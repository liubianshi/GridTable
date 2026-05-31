Cell <- function(tbl, i, j) {
    info <- cell_merge_info(tbl, i, j)
    if (isTRUE(info$merged) && isFALSE(info$first_cell)) {
        return(NULL)
    }
    info <- c(info, cell_position_info(tbl, info$i, info$j))
    info$align <- attr(tbl, "align")[info$j[1]]
    info$content <- cell_content(tbl, info = info)


    edges <- purrr::map(info$row$start:info$row$end, \(rowno) {
        node_symbol  <- if      (rowno == info$row$start) SYMBOL$VERTICE
                        else if (rowno == info$row$end)   SYMBOL$VERTICE
                        else                              SYMBOL$SIDE

        leftnode     <- Node(Coordinate(rowno, info$col$start), node_symbol)
        rightnode    <- Node(Coordinate(rowno, info$col$end),   node_symbol)

        edge_content <-
            if (rowno == info$row$start) {
                if      (info$row$isHeaderLine$start) SYMBOL$HEADER
                else if (info$row$isFooterLine$start) SYMBOL$FOOTER
                else                                  SYMBOL$LINE
            } else if (rowno == info$row$end) {
                if      (info$row$isHeaderLine$end)   SYMBOL$HEADER
                else if (info$row$isFooterLine$end)   SYMBOL$FOOTER
                else                                  SYMBOL$LINE
            }
            else {
                purrr::pluck(info$content, rowno - info$row$start, .default = "")
            }

        Edge(leftnode, rightnode, edge_content, info$align)
    })

    structure(edges, start = info$row$start, end = info$row$end, class = "Cell")
}

cell_merge_info <- function(tbl, i, j) {
    stopifnot(inherits(tbl, "GridTable"))
    stopifnot(length(i) == 1 && length(j) == 1)

    meta <- MERGED_CELL_OPTION
    meta$merged <- FALSE
    meta$i <- toInteger(i)
    meta$j <- toInteger(j)

    for (m in attr(tbl, "merged_cells")) {
        if (meta$i %in% m$rows[1]:m$rows[2] && meta$j %in% m$cols[1]:m$cols[2]) {
            meta$merged <- TRUE
            meta$first_cell <- (meta$i == m$rows[1] && meta$j == m$cols[1])
            purrr::iwalk(m, \(x, y) if (y %in% names(meta)) meta[[y]] <<- x)
            meta$i <- m$rows[1]:m$rows[2]
            meta$j <- m$cols[1]:m$cols[2]
            break
        }
    }
    meta
}

cell_position_info <- function(tbl, i, j) {
    height <- attr(tbl, "height")
    width  <- attr(tbl, "width")
    get_row_nums <- function(i) sum(height[i] + 1) + 1
    get_col_nums <- function(j) sum(width[j] + 3) + 1
    col <- list(start = if (j[1] == 1) 1 else get_col_nums(1:(j[1] - 1)),
                num   = get_col_nums(j),
                end   = get_col_nums(1:j[length(j)]))
    row <- list(start = if (i[1] == 1) 1 else get_row_nums(1:(i[1] - 1)),
                num   = get_row_nums(i),
                end   = get_row_nums(1:i[length(i)]))

    table_header <- attr(tbl, "header")
    row$isHeaderLine$start = FALSE
    row$isHeaderLine$end   = row$end == sum(height[1:table_header] + 1) + 1

    table_footer_rows <- purrr::map(attr(tbl, "footer"), \(f) {
        if (is.null(f) || is.infinite(f) || f <= 0) return(c(-1, -1))
        c(sum(height[1:(f-1)] + 1) + 1, sum(height + 1) + 1)
    })
    row$isFooterLine$start <- row$start %in% purrr::map_int(table_footer_rows, 1)
    row$isFooterLine$end   <- row$end %in% purrr::map_int(table_footer_rows, 2)

    list(col = col, row = row)
}

cell_content <- function(tbl, i = NULL, j = NULL, info = NULL) {
    if ((is.null(i) || is.null(j)) && is.null(info)) {
        stop("Need set i and j or set info", call. = FALSE)
    }
    if (is.null(i) || is.null(j)) {
        i <- info$i
        j <- info$j
    }
    if (is.null(info)) {
        info <- c(cell_merge_info(tbl, i, j), cell_position_info(tbl, i, j))
    }
    content <- if (isTRUE(info$drop_content)) {
        tbl[i[1], j[1]]
    } else {
        subm <- as.matrix(tbl)[i, j]
        dim(subm) <- c(length(i), length(j))
        apply(subm, 2, paste, collapse = "\n")
    }

    content <- unlist(strsplit(as.character(content), "\n"))
    content <- content[!grepl("^\\s*(&nbsp;)?\\s*$", content, perl = TRUE)]
    if (length(content) == 0) return("")

    if (length(content) > info$row$num - 2) {
        set_attr(tbl, height = paste0(i[1], "+", length(content) - info$row$num + 2))
        stop("Adjust the height", call. = FALSE)
    }
    if (isTRUE(info$wrap) && length(content) > 1) {
        content[-length(content)] <- pandoc_wrap(content[-length(content)])
    }
    if (isTRUE(info$middle)) {
        content <- c(rep("", (info$row$num - 2 - length(content)) / 2), content)
    }

    max_content_width <- max(purrr::map_int(content, str_width))
    if (max_content_width + 4 > info$col$num) {
        shorter        <- max_content_width + 4 - info$col$num
        inc            <- ceiling(shorter / length(j))
        table_width    <- attr(tbl, "width")
        table_width[j] <- table_width[j] + inc
        data.table::setattr(tbl, "width", table_width)
        stop("Adjust the width", call. = FALSE)
    }

    content
}

get_cells_from <- function(gtable) {
    rownum <- nrow(gtable)
    colnum <- ncol(gtable)
    cells <- purrr::map(seq_len(rownum), \(i) {
        purrr::map(seq_len(colnum), \(j) {
          Cell(gtable, i, j)
      })
    })
    do.call(c, cells)
}

merge_cell_list <- function(cells) {
    cells <- cells[!purrr::map_lgl(cells, is.null)]
    purrr::walk(cells, \(c) stopifnot(inherits(c, "Cell")))

    longest <- max(purrr::map_int(cells, ~ attr(.x, "end")))
    new_rows <- purrr::map(seq_len(longest), \(i) {
        edges <- purrr::map(cells, \(c) {
            start <- attr(c, "start")
            end   <- attr(c, "end")
            if (i %in% start:end) c[[i - start + 1]] else NULL
        })
        edges <- edges[!purrr::map_lgl(edges, is.null)]
        do.call(Row, c(integrate_edge_list(edges), row_no = i))
    })
    do.call(Table, new_rows)
}
