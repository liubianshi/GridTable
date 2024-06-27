SYMBOL <- list(VERTICE = "+",
               SIDE    = "|",
               LINE    = "-",
               HEADER  = "=",
               FOOTER  = "=",
               ALIGN   = ":")

MERGED_CELL_OPTION <- list(drop_content = FALSE,
                           middle       = FALSE,
                           wrap         = FALSE)

`<.Node` <- function(n1, n2) {
    stopifnot(row_no(n1) == row_no(n2))
    col_no(n1) < col_no(n2)
}

`>.Node` <- function(n1, n2) {
    stopifnot(row_no(n1) == row_no(n2))
    col_no(n1) > col_no(n2)
}

`==.Node` <- function(n1, n2) {
    all(n1$coordinate == n2$coordinate)
}

`==.Edge` <- function(e1, e2) {
    e1$leftnode == e2$leftnode && e1$rightnode == e2$rightnode
}

`<.Edge` <- function(e1, e2) {
    e1$leftnode < e2$leftnode
}

cal_column_width <- function(x) {
    x <- x[!is.na(x)]
    trimws(x) |>
    str_width() |>
    max()
}

Cell <- function(table, i, j) {
    info <- cell_merge_info(table, i, j)
    if (isTRUE(info$merged) && isFALSE(info$first_cell)) {
        return(NULL)
    }
    info <- c(info, cell_position_info(table, info$i, info$j))
    info$align <- attr(table, "align")[info$j[1]]
    info$content <- cell_content(table, info = info)

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

cell_merge_info <- function(table, i, j) {
    stopifnot(inherits(table, "GridTable"))
    stopifnot(length(i) == 1 && length(j) == 1)

    meta <- MERGED_CELL_OPTION
    meta$merged <- FALSE
    meta$i <- toInteger(i)
    meta$j <- toInteger(j)

    for (m in attr(table, "merged_cells")) {
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

cell_position_info <- function(table, i, j) {
    height <- attr(table, "height")
    width  <- attr(table, "width")
    get_row_nums <- function(i) sum(height[i] + 1) + 1
    get_col_nums <- function(j) sum(width[j] + 3) + 1
    col <- list(start = if (j[1] == 1) 1 else get_col_nums(1:(j[1] - 1)),
                num   = get_col_nums(j),
                end   = get_col_nums(1:j[length(j)]))
    row <- list(start = if (i[1] == 1) 1 else get_row_nums(1:(i[1] - 1)),
                num   = get_row_nums(i),
                end   = get_row_nums(1:i[length(i)]))

    table_header <- attr(table, "header")
    row$isHeaderLine$start = FALSE
    row$isHeaderLine$end   = row$end == sum(height[1:table_header] + 1) + 1

    table_footer_rows <- purrr::map(attr(table, "footer"), \(f) {
        if (is.null(f) || is.infinite(f) || f <= 0) return(c(-1, -1))
        c(sum(height[1:(f-1)] + 1) + 1, sum(height + 1) + 1)
    })
    row$isFooterLine$start <- row$start %in% purrr::map_int(table_footer_rows, 1)
    row$isFooterLine$end   <- row$end %in% purrr::map_int(table_footer_rows, 2)

    list(col = col, row = row)
}

cell_content <- function(table, i = NULL, j = NULL, info = NULL) {
    if ((is.null(i) || is.null(j)) && is.null(info)) {
        stop("Need set i and j or set info", call. = FALSE)
    }
    if (is.null(i) || is.null(j)) {
        i <- info$i
        j <- info$j
    }
    if (is.null(info)) {
        info <- c(cell_merge_info(table, i, j), cell_position_info(table, i, j))
    }

    content <- if (isTRUE(info$drop_content)) {
        table[i[1], j[1]]                   
    } else {
        apply(as.matrix(table[i, j]), 2, paste, collapse = "\n")
    }
    content <- unlist(strsplit(content, "\n"))
    content <- content[!grepl("^\\s*(&nbsp;)?\\s*$", content, perl = TRUE)]
    
    if (length(content) == 0) return("")

    if (length(content) > info$row$num - 2) {
        set_attr(table, height = paste0(i[1], "+", length(content) - info$row$num + 2))
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
        table_width    <- attr(table, "width")
        table_width[j] <- table_width[j] + inc
        data.table::setattr(table, "width", table_width)
        stop("Adjust the width", call. = FALSE)
    }

    content
}

column_start_end_points <-  function (line, sep = " ") {
    line_width <- str_width(line)
    sep_p <- which(strsplit(line, "")[[1]] == sep)
    if (length(sep_p) == 0) 
        return(NULL)
    keep <- purrr::map_lgl(seq_along(sep_p), function(x) {
        if (x == 1) 
            return(TRUE)
        if (x == length(sep_p)) 
            return(TRUE)
        if (sep_p[x] - sep_p[x - 1] == 1 && sep_p[x + 1] - sep_p[x] == 1) 
            return(FALSE)
        return(TRUE)
    })
    sep_p <- sep_p[keep]
    start_end_points <- unlist(purrr::map(seq_along(sep_p), function(i) {
        if (i == 1) {
            return(if (all(sep_p[1:2] == 1:2)) NULL else if (sep_p[1] == 
                1) 2 else c(1, sep_p[1] - 1))
        }
        if (i == length(sep_p)) {
            if (all(sep_p[length(sep_p) - 1:0] == line_width - 
                1:0)) {
                return(NULL)
            }
            if (sep_p[length(sep_p)] == line_width) {
                return(line_width - 1)
            }
            return(c(sep_p[length(sep_p)] + 1, line_width))
        }
        if (sep_p[i] == sep_p[i - 1] + 1) 
            return(sep_p[i] + 1)
        if (sep_p[i] == sep_p[i + 1] - 1) 
            return(sep_p[i] - 1)
        return(c(sep_p[i] - 1, sep_p[i] + 1))
    }))
    purrr::map(seq_len(length(start_end_points)/2),
               function(i) start_end_points[2 * i - 1:0])
}
col_no <- function(x) {
    switch(class(x),
        Coordinate = x[2],
        Node       = col_no(x$coordinate),
        Edge       = c(col_no(x$leftnode), col_no(x$rightnode)),
        list       = purrr::map_int(x, col_no),
        default    = stop("wrong object")
    )
}


Coordinate <- function(x, y) {
    structure(c(toInteger(x), toInteger(y)), class = "Coordinate")
}

Edge <- function(leftnode, rightnode, content, align = NULL) {
    stopifnot(class(leftnode)  == "Node" &&
              class(rightnode) == "Node" &&
              is.character(content) &&
              length(content) == 1 &&
              (is.null(align) || isTRUE(align %in% c("l", "r", "c"))) &&
              row_no(leftnode) == row_no(rightnode) &&
              col_no(leftnode) <  col_no(rightnode))
    type <- "Normal"
    symbol <- NULL
    if (leftnode$symbol != SYMBOL$SIDE && rightnode$symbol != SYMBOL$SIDE) {
        symbol <- local({
            cont <- trimws(content)
            unique(strsplit(cont, "")[[1]])
        })
        stopifnot(str_width(symbol) == 1)
        type <- unlist(purrr::imap(SYMBOL, ~ if (.x == symbol) .y else NULL))
    } else if (!grepl("\\S", content, perl = TRUE)) {
        type <- "Empty"
        symbol <- " "
    }

    structure(list(leftnode  = leftnode,
                   rightnode = rightnode,
                   content   = content,
                   align     = align,
                   type      = type,
                   symbol    = symbol),
              class = "Edge")
}

edge_left_extend <- function(e1, e2) {
    updated_node <- node_merge(e1$rightnode, e2$leftnode)
    return(list(edge_update(e1, rightnode = updated_node),
                edge_update(e2, leftnode = updated_node)))
}

edge_merge <- function(e1, e2) {
    if (col_no(e1$leftnode) > col_no(e2$leftnode)) return(edge_merge(e2, e1))
    switch(edge_relation(e1, e2),
        LEFT = {
            list(e1, Edge(e1$rightnode, e2$leftnode, ""), e2)
        },
        ADJACENT_LEFT = {
            edge_left_extend(e1, e2)
        },
        CONTAIN_SAME_END = {
            left_part    <- edge_update(e1, rightnode = e2$leftnode)
            overlap_part <- edge_update(e1, leftnode = e2$leftnode, align = e2$align)
            c(list(excess_part), edge_merge(overlap_part, e2))
        },
        OVERLAP_LEFT = {
            left_part    <- edge_update(e1, rightnode = e2$leftnode)
            overlap_p1   <- edge_update(e1, leftnode  = e2$leftnode)
            overlap_p2   <- edge_update(e2, rightnode = e1$rightnode)
            right_part   <- edge_update(e2, leftnode  = e1$rightnode)
            c(list(excess_part), edge_merge(overlap_p1, eoverlap_p2),
              list(right_part))
        },
        IN_SAME_START = {
            overlap_part <- edge_update(e2, rightnode = e1$rightnode, align = e1$align)
            right_part   <- edge_update(e2, leftnode  = e1$rightnode)
            c(edge_merge(e1, overlap_part), list(right_part))
        },
        EQUAL = {
            e1 <- edge_update(e1, leftnode = node_merge(e1$leftnode, e2$leftnode),
                                  rightnode = node_merge(e1$rightnode, e2$rightnode))
            if ((e1$type[1] == "Normal" && e2$type[1] != "Empty") ||
                (e1$type[1] != "Empty"  && e2$type[1] == "Normal")) {
                stop("Content Overlapping", call. = FALSE)
            } 
            if (e1$type[1] == "Empty" || sum(c("HEADER", "FOOTER") %in% e2$type)) {
                e1 <- edge_update(e1, content = e2$content,
                                      type = e2$type,
                                      symbol = e2$symbol)
            } 
            list(e1)
        },
        CONTAIN = {
            left_part    <- edge_update(e1, rightnode = e2$leftnode)
            overlap_part <- edge_update(e1, leftnode  = e2$leftnode,
                                            rightnode = e2$rightnode,
                                            align     = e2$align)
            right_part   <- edge_update(e1, leftnode  = e2$rightnode)
            c(list(left_part), edge_merge(overlap_part, e2), list(right_part))
        },
        CONTAIN_SAME_START = {
            overlap_part <- edge_update(e1, rightnode = e2$rightnode, align = e2$align)
            right_part   <- edge_update(e1, leftnode  = e2$rightnode)
            c(edge_merge(overlap_part, e2), list(right_part))
        }
    )
}

edge_relation <- function(e1, e2) {
    stopifnot(inherits(e1, "Edge") && inherits(e2, "Edge"))
    stopifnot(row_no(e1) == row_no(e2))
    range_relation(col_no(e1), col_no(e2))
}
edge_update <- function(e, ...) {
    attrs <- list(...)
    for (i in seq_along(attrs)) {
        e[[names(attrs)[i]]] <- attrs[[i]]
    }
    e
}

empty_row <- function(row_no, left_col_no = NULL, right_col_no = NULL) {
    stopifnot(is.null(left_col_no) == is.null(right_col_no))
    if (is.null(left_col_no) || is.null(right_col_no)) {
        return(structure(list(nodes = NULL,
                              edges = NULL,
                              n = 0,
                              row_no = row_no),
                              class = "Row"))
    }
    leftnode  <- Node(Coordinate(row_no, left_col_no),  SYMBOL$SIDE)
    rightnode <- Node(Coordinate(row_no, right_col_no), SYMBOL$SIDE)
    Row(Edge(leftnode, rightnode, " "))
}

format_column <- function(x, digits = 3L, ...) {
    stopifnot(is.atomic(x))
    if (is.character(x)) return(trimws(x))
    if (is.integer(x))   return(as.character(x))

    max_origin_digits <-
        strsplit(as.character(x), "\\.")  |>
        purrr::map_chr(\(r) purrr::pluck(r, 2, .default = "")) |>
        purrr::map_int(nchar) |>
        max()

    if (max_origin_digits == 0L) return(as.character(x))

    purrr::map_chr(x, format_one_num, digits = digits, ...) |>
    trimws()
}

format_one_num <- function(z, digits, nsmall = 3L,
                           width = NULL, na.replace = "", big.mark = ",") {
    stopifnot(is.numeric(z) && length(z) == 1L)
    if (is.null(digits)) stop("Must set digits", call. = FALSE)
    stopifnot(is.null(width) || width > digits) 
    
    if (is.na(z))      return(na.replace)
    if (is.integer(z)) return(format(z, width = width, big.mark = big.mark))
    if (is.null(width)) width = digits + 3

    digits <- as.integer(digits)
    decbits <- if (abs(z) < 1) width - 0
               else            width - as.integer(log10(abs(round(z, digits = 0L)))) - 2
    
    if (decbits >= digits) {
        round(z, digits = digits) |>
        format(digits = digits, nsmall = digits, width = width)
    } else if (decbits > 0) {
        round(z, digits = digits) |>
        format(digits = decbits, nsmall = decbits,
               width = width, bit.mark = big.mark)
    } else {
        round(z, digits = 0) |>
        fomart(width = width, big.mark = big.mark)
    }
}





get_cells_from <- function(gtable) {
    rownum <- nrow(gtable)
    colnum <- ncol(gtable)
    cells <- purrr::map(seq_len(rownum), \(i) {
        purrr::map(seq_len(colnum), \(j) Cell(gtable, i, j))
    }) 
    do.call(c, cells)
}

#' @export
GridTable <- function(data, align = NULL,
                      header = NULL, footer = NULL, ...) {
    if (!inherits(data, "data.table")) data <- data.table::as.data.table(data)
    args <- list(...)
    align <- valid_align(data, align)

    data <- purrr::map_dfc(data, format_column) |> data.table::setDT()
    if (is.null(header) || header == 0L) {
        data <- rbind(as.list(colnames(data)), data)
        header <- 1L
    }

    if (is.null(footer)) footer <- Inf
    width <- purrr::map_int(data, cal_column_width)
    names(width) <- NULL
    height <- height_of(data)

    class(data) <- c("GridTable", class(data))
    data.table::setattr(data, "height", height)
    data.table::setattr(data, "width",  width)
    data.table::setattr(data, "align",  align)
    data.table::setattr(data, "header", header)
    data.table::setattr(data, "footer", footer)
    purrr::iwalk(args, \(x, y) data.table::setattr(data, y, x))
    data
}

height_of <- function(df, base = NULL) {
    h <- purrr::map_int(1:nrow(df), \(i) {
        purrr::map_int(
            df[i, ],
            \(x) if (is.na(x)) 0 else sum(gregexpr("\n", x)[[1]] > 0)
        ) |>
        max()
    }) + 1
    if (is.null(base)) return(h)
    else               return(ifelse(h > base, h, base))
}

ifthen <- function(x, y) {
    if (is.null(x)) y else x
}

integrate_edge_list <- function(edge_list) {
    old <- sort_edge_list(edge_list)
    if (length(edge_list) == 1) return(old)

    new <- list(shift(old))
    temp <- list(shift(old)) 
    while (length(old) > 0 || length(temp) > 0) {
        e <- if (length(temp) == 0)         shift(old)
             else if (length(old)  == 0)    shift(temp)
             else if (old[[1]] < temp[[1]]) shift(old)
             else                           shift(temp)
        m <- edge_merge(new[[length(new)]], e)
        if (e < m[[length(m)]]) {
            new <- c(new[-length(new)], m[-length(m)])
            temp <- sort_edge_list(c(m[length(m)], temp))
        } else {
            new <- c(new[-length(new)], m)
        }
    }
    new
}

is_overlaped <- function(x, y) {
    stopifnot(length(x) == 2 && length(y) == 2)
    get <- function(x, i) if (is.atomic(x))        x[i]     else x[[i]] 
    min <- function(z)    if (get(z,1) < get(z,2)) get(z,1) else get(z,2)
    max <- function(z)    if (get(z,1) < get(z,2)) get(z,2) else get(z,1)
    !(min(x) > max(y) || min(y) > max(x))
}


#' @export
kable_to_grid <- function(kbl, ...) {
    stopifnot(inherits(kbl, "knitr_kable"))
    format <- attr(kbl, "format")
    stopifnot(format %in% c("simple", "pipe"))

    if (grepl("^Table:", kbl[1])) {
        caption <- kbl[1]
        kbl     <- kbl[-(1:2)]
    } else {
        caption <- NULL
    }
    sep <- " "
    if (format == "pipe") {
        sepline <- kbl[2]
        table   <- if (grepl("[^\\|\\s]", kbl[1], perl = TRUE)) kbl[-2] else kbl[-(1:2)]
        sep     <-  "|"
    } else if (grepl("[^\\-\\s]", kbl[length(kbl)], perl = TRUE)) {
        sepline <- kbl[2]
        table   <- kbl[-2]
    } else {
        sepline <- kbl[1]
        table   <- kbl[-c(1, length(kbl))]
    }

    start_end_points <- column_start_end_points(sepline, sep)
    stopifnot(!is.null(start_end_points))
    data <- purrr::map(table, \(line) {
        purrr::map_chr(start_end_points, \(x) substr_width(line, x[1], x[2]))
    })
    data <- do.call(rbind, data)

    args <- list(...)
    args$data <- data
    if (is.null(args$header)) args$header = 1L
    if (is.null(args$caption)) args$caption = caption
    do.call(GridTable, args)
}

last_node <- function(row) {
    if (row$n == 0) return(NULL)
    else            return(row$nodes[[row$n]])
}

#' @export
merge_cells <- function(table, i = NULL, j = NULL, cancel = NULL, ...) {
    old_merged_cell <- attr(table, "merged_cells")
    old_merged_cell_names <- names(old_merged_cell)

    if (is.null(i) != is.null(j)) {
        stop("Need to set both i and j", call. = FALSE)
    }

    if (is.null(i) && is.null(j) && is.null(cancel)) {
        purrr::iwalk(old_merged_cell, \(v, n) {
            cat("Name:", n, "\n")
            purrr::iwalk(v, ~ cat("\t", .y, ": ", toString(.x), "\n", sep = ""))
        })
        return(invisible(table))
    }
    
    merged_cell_name <- if (!is.null(i) && !is.null(j)) {
        paste(paste(unique(minmax(i)), collapse = ":"),
              paste(unique(minmax(j)), collapse = ":"),
              sep = ",")
    }

    if (!is.null(cancel)) {
        if (isTRUE(cancel) && !is.null(merged_cell_name)) {
            cancel <- merged_cell_name
        }
        if(!is.character(cancel)) {
            stop("Cancel needed to be TRUE, FALSE or names", call. = FALSE)
        }
        old_merged_cell_names <- setdiff(old_merged_cell_names, cancel)
        old_merged_cell <- old_merged_cell[old_merged_cell_names]
        data.table::setattr(table, "merged_cells", old_merged_cell)
        return(invisible(table))
    }
    
    merged_cell <- c(valid_merged_cell(i, j, table), list(...))
    purrr::walk(old_merged_cell, \(m1, m2) {
        if (is_overlaped(m1$rows, m2$rows) && is_overlaped(m1$cols, m2$cols)) {
            stop("There is overlap", call. = FALSE)
        }
    }, m2 = merged_cell)

    newattrs <- c(old_merged_cell, list(merged_cell))
    names(newattrs) <- c(old_merged_cell_names, merged_cell_name)
    data.table::setattr(table, "merged_cells", newattrs)
    return(invisible(table))
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

minmax <- function(x, na.rm = TRUE) {
    stopifnot(is.numeric(x))
    c(min(x, na.rm = na.rm), max(x, na.rm = na.rm))
}

Node <- function(coordinate, symbol = "+") {
    stopifnot(class(coordinate) == "Coordinate" && is.character(symbol))
    structure(list(coordinate = coordinate,
                   symbol = symbol),
              class = "Node")
}


node_merge <- function(n1, n2) {
    if (is.null(n1)) return(n2)
    if (is.null(n2)) return(n1)
    stopifnot(n1 == n2)
    if (n2$symbol == SYMBOL$VERTICE) return(n2)
    return(n1)
}

node_list_merge <- function(nodes1, nodes2) {
    stopifnot(do.call(row_no_allequal, c(nodes1, nodes2)))
    col_nos1 <- col_no(nodes1)
    col_nos2 <- col_no(nodes2)
    col_nos <- sort(unique(col_nos1, col2_nos1))
    purrr::map(col_nos, \(i) {
        node_merge(get_node_where(nodes1, i), get_node_where(nodes2, i))
    })
}


pandoc_wrap <- function(x) {
    nms <- names(x)
    names(x) <- NULL
    x <- purrr::imap_chr(strsplit(x, "[^\\]"), \(v, i) {
        if (nchar(v[length(v)]) %% 2 == 0) paste0(x[i], "\\")
        else                                  x[i]
    })
    names(x) <- nms
    x
}


parse_number_adjust <- function(num, x) {
    stopifnot(is.character(x))
    if (!is.atomic(x)) {
        x <- purrr::imap_chr(x, \(val,name) gettextf("%s%s", name, val))
    }
    x <- toupper(x)

    if (length(x) > 1) {
        return(parse_number_adjust(parse_number_adjust(num, x[1]), x[-1]))
    }
    elements <- str_match(x, "^([0-9a-zA-Z]+)([-+*/=]?)([0-9]+)")[[1]]
    if (length(elements) == 0 || is.na(elements[1])) {
        return(NULL)
    }
    else {
        elements <- elements[2:4]
    }

    index <- if (grepl("^[0-9]+$", elements[1])) {
        as.integer(elements[1])
    } else if (grepl("^[A-Za-z]+$", elements[1])) {
        which(LETTERS == strsplit(toupper(elements[1]), "")[[1]]) |> sum()
    } 
    if (is.null(index)) return(NULL)

    stopifnot(index <= length(num))
    operand <- as.integer(elements[3])
    operator <- if (is.na(elements[2])) "=" else elements[2]
    switch(operator,
        `=` = { num[index] <- operand },
        `+` = { num[index] <- num[index] + operand },
        `-` = { num[index] <- num[index] - operand },
        `*` = { num[index] <- num[index] * operand },
        `/` = { num[index] <- as.integer(num[index] / operand) },
        default = stop("+-*/= or omit", call. = FALSE)
    )
    num
}

#' @export
print.GridTable <- function(gtable, drop_empty_line = TRUE, ...) {
    content <- toString(gtable, drop_empty_line = drop_empty_line, ...)
    cat(content, sep = "\n")
    invisible(content)
}

#' @export
row_no <- function(x) {
    switch(class(x),
        Coordinate = x[1],
        Node       = row_no(x$coordinate),
        Edge       = row_no(x$leftnode),
        Row        = x$row_no,
        default    = stop("wrong object")
    )
}

range_relation <- function(s1, s2, na.rm = TRUE) {
    l1 <- min(s1, na.rm = na.rm)
    l2 <- min(s2, na.rm = na.rm)
    r1 <- max(s1, na.rm = na.rm)
    r2 <- max(s2, na.rm = na.rm)

    if (r1 < l2) {
        return("LEFT")
    } else if (r1 == l2) {
        return("ADJACENT_LEFT")
    } else if (r1 > l2 & r1 < r2) {
        return(
            if      (l1 < l2)  "OVERLAP_LEFT"
            else if (l1 == l2) "IN_SAME_START"
            else               "In"
        )
    } else if (r1 == r2) {
        return(
            if      (l1 < l2)  "CONTAIN_SAME_END"
            else if (l1 == l2) "EQUAL"
            else               "IN_SAME_END"
        )
    } else {
        return(
            if      (l1 <  l2)           "CONTAIN"
            else if (l1 == l2)           "CONTAIN_SAME_START"
            else if (l1 > l2 && l1 < r2) "OVERLAP_RIGHT"
            else if (l1 == r2)           "ADJACENT_RIGHT"
            else                         "RIGHT"
        )
    }
}

row_no_allequal <- function(...) {
    args <- list(...)
    row_nos <- purrr::map(args, row_no) |> unlist() |> unique()
    length(row_nos) == 1L
}

Row <- function(..., row_no = NULL) {
    edges <- list(...)
    stopifnot(!(length(edges) == 0L && is.null(row_no)))
    if (length(edges) == 0L) return(empty_row(row_no))

    purrr::walk(edges, ~ stopifnot(class(.x) == "Edge"))
    edges <- local({
        edge_no <- purrr::map_int(edges, ~ .x$leftnode$coordinate[2])
        stopifnot(anyDuplicated(edge_no) == 0)
        purrr::map(seq_along(edges), ~ edges[[which(order(edge_no) == .x)]])
    })

    nodes <- vector("list", length(edges) + 1)
    for (i in seq_along(edges)) {
        if (i != 1) {
            stopifnot(edges[[i]]$leftnode == edges[[i-1]]$rightnode)
            edges[[i-1]]$rightnode <- node_merge(edges[[i]]$leftnode, edges[[i-1]]$rightnode)
            edges[[i]]$leftnode    <- edges[[i-1]]$rightnode
        }
        if (i == length(edges)) nodes[[i+1]] = edges[[i]]$rightnode
        nodes[[i]] = edges[[i]]$leftnode
    }
    
    structure(list(nodes = nodes,
                   edges = edges,
                   n = length(nodes),
                   row_no = row_no(edges[[1]])), class = "Row")
}


#' @export
set_attr <- function(table, attr = NULL, value = NULL, ...) {
    stopifnot(inherits(table, "GridTable"))
    purrr::iwalk(list(...), \(value, attr) set_attr(table, attr, value))
    if (is.null(attr)) return(invisible(table))

    switch(attr,
        align  = data.table::setattr(table, attr, valid_align(table, value)),
        height =,
        width  = {
            if (is.numeric(value)) {
                if (attr == "height") stopifnot(length(value) != nrow(table))
                if (attr == "width")  stopifnot(length(value) != ncol(table))
                data.table::setattr(table, attr, value)
            } else {
                data.table::setattr(table, attr,
                                    parse_number_adjust(attr(table, attr), value))
            }
        },
        data.table::setattr(table, attr, value)
    )
    invisible(table)
}

shift <- function (x, drop = TRUE) {
    if (length(x) == 0) 
        return(NULL)
    outer_x <- as.character(substitute(x))
    shiftret <- if (isTRUE(drop) && is.list(x)) {
        x[[1]]
    }
    else {
        x[1, drop = drop]
    }
    assign(as.character(substitute(x)), x[-1], parent.frame())
    shiftret
}

sort_edge_list <- function(edge_list) {
    edge_list <- edge_list[!purrr::map_lgl(edge_list, is.null)]
    lcol_nos <- purrr::map_int(edge_list, \(e) col_no(e$leftnode))
    rcol_nos <- purrr::map_int(edge_list, \(e) col_no(e$rightnode))
    index <- lcol_nos * max(rcol_nos) + rcol_nos
    edge_list[order(index)]
}

str_width <- function(x) nchar(x, type = "width")
str_match <- function(x, pattern) regmatches(x, regexec(pattern, x))

substr_width <- function(x, start, end) {
    width     <- end - start + 1
    pre_width <- start - 1

    pre       <- substr(x, 1, start - 1)
    while (start >= 0 && str_width(pre) > pre_width) {
        start <- start - 1
        end   <- end - 1
        pre   <- substr(pre, 1, start - 1)
        if (str_width(pre) < pre_width) {
            stop("Start point cut character", call. = FALSE)
        }
    }

    subs <- substr(x, start, end)
    end  <- width
    while (end >= 0 && str_width(subs) > width) {
        end <- end - 1
        subs <- substr(subs, 1, end)
        if (str_width(subs) < width) {
            stop("End point cut character", call. = FALSE)
        }
    }

    subs
}

toInteger <- function(x) {
    stopifnot(!grepl(".", as.character(x + 0), fixed = TRUE))
    as.integer(x)
}

Table <- function(...) {
    rows    <- list(...)
    row_nos <- purrr::map_int(rows, "row_no")
    stopifnot(anyDuplicated(row_nos) == 0)

    length  <- max(row_nos)
    width   <- rows |>
               purrr::map_int(\(x) if (x$n == 0) 0 else col_no(last_node(x))) |>
               max()
    rows    <- purrr::map(seq_len(length), ~ {
        which_row <- which(row_nos == .x)
        if (length(which_row) != 0)   rows[[which_row]]
        else                          Row(row_no = .x)
        
    })
    structure(list(rows = rows, length = length, width = width),
              class = "Table")
}


toString.Edge <- function(edge) {
    symbol_l <- if (col_no(edge$leftnode) == 1L) edge$leftnode$symbol else ""
    symbol_r <- edge$rightnode$symbol
    width    <- col_no(edge$rightnode) - col_no(edge$leftnode) - 1
    if (edge$type[1] == "Normal") {
        content   <- trimws(edge$content)
        space_num <- width - 1 - str_width(content)
        content   <- paste0(" ", content, strrep(" ", space_num))
        return(gettextf("%s%s%s", symbol_l, content, symbol_r))
    }

    if ((!is.null(edge$align)) && "HEADER" %in% edge$type ) {
        align_symbol <- switch(edge$align,
            l = c(SYMBOL$ALIGN, ""),
            r = c("",           SYMBOL$ALIGN),
            c = c(SYMBOL$ALIGN, SYMBOL$ALIGN),
            default = stop("Align invalid", call. = FALSE)
        )
        edge_symbol_num <- width - sum(str_width(align_symbol))
        return(gettextf("%s%s%s%s%s", symbol_l, align_symbol[1],
                       strrep(edge$symbol, edge_symbol_num),
                       align_symbol[2], symbol_r))
    }
    return(gettextf("%s%s%s", symbol_l, strrep(edge$symbol, width), symbol_r))
}

toString.Row <- function(row) {
    paste0(purrr::map_chr(row$edges, toString.Edge), collapse = "")
}

#' @export
toString.GridTable <- function(gtable, ...) {
    rownum <- nrow(gtable)
    colnum <- ncol(gtable)
    args <- list(...)

    cells <- try(get_cells_from(gtable), silent = TRUE)
    if (inherits(cells, "try-error")) {
        msg <- geterrmessage()
        if (grepl("Adjust the width", msg, fixed = TRUE)) {
            message("Message: Adjusted table width attribute due to lengthy content")
        } else if (grepl("Adjust the height", msg, fixed = TRUE)) {
            message("Message: Adjusted table height attribute due to multiline content")
        }
        else {
            stop(message, call. = FALSE)
        }
        return(do.call(toString.GridTable, c(list(gtable), args)))
    }

    Table <- merge_cell_list(cells)
    content <- do.call(toString.Table, c(list(Table), args))
    if (!is.null(attr(gtable, "caption"))) {
        content <- c(attr(gtable, "caption"), "", content)
    }

    structure(content, class = "GridTable_output")
}

toString.Table <- function(table, drop_empty_line = TRUE, ...) {
    table_content <- purrr::map_chr(table$rows, ~ toString.Row(.x))
    if (isTRUE(drop_empty_line)) {
        table_content <- table_content[grepl("[^|\\s]", table_content, perl = TRUE)]
    }
    table_content
}


valid_align <- function(data, align = NULL) {
    if (is.null(align)) {
        align <- 
            purrr::map_chr(data, \(x) {
                switch(class(x),
                    character = "l",
                    numeric = "r",
                    stop("Weird Column", call. = FALSE))
            })
    } else if (is.character(align)) {
        stopifnot(length(align) %in% c(1, length(data)))
        if (length(align) == 1)  {
            align <- strsplit(align, "")[[1]]
            if (length(align) == 1) align <- rep(align, length(data))
        }
        stopifnot(all(align %in% c("l", "r", "c")))
    } else {
        stop("align needed to be a character or character vector", call. = FALSE)
    }
    align
}

valid_merged_cell <- function(rows, cols, gridtable) {
    stopifnot(is.numeric(rows) && is.numeric(cols))
    stopifnot(inherits(gridtable, "GridTable"))

    merged_cell <-
        purrr::map(list(rows = rows, cols = cols), \(x) {
            mM <- minmax(x) 
            if (length(x) > 2) stopifnot(all(sort(x) == mM[1]:mM[2]))
            mM
        })

    stopifnot(all(merged_cell$rows %in% 1:nrow(gridtable)))
    stopifnot(all(merged_cell$cols %in% 1:ncol(gridtable)))
    if (merged_cell$rows[1] < attr(gridtable, "header") &&
        merged_cell$rows[2] > attr(gridtable, "header")) {
        stop("Cannot Span header row", call. = FALSE)
    }

    merged_cell
}

# vim: set fdm=expr:
