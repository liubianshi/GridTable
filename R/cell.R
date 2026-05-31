#' Build the set of edges for one logical cell
#'
#' Produce the bounding-box edges (top border, content lines, bottom border) of
#' the cell at table position `(i, j)`, already aware of merging and of header/
#' footer separator lines. For a non-first cell of a merged region it returns
#' `NULL`.
#'
#' @param tbl A `GridTable`.
#' @param i,j Logical table row/column indices.
#' @return A `Cell` (list of `Edge`s with `start`/`end` row attributes), or
#'   `NULL`.
#' @noRd
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

#' Resolve a cell's merge membership
#'
#' Determine whether `(i, j)` belongs to a registered merged region; if so,
#' expand `i`/`j` to the region's full row/column spans, flag whether this is the
#' region's first (top-left) cell, and fold in the region's merge options
#' (`drop_content`/`middle`/`wrap`).
#'
#' @param tbl A `GridTable`.
#' @param i,j Scalar logical row/column indices.
#' @return A metadata list: `merged`, `first_cell`, expanded `i`/`j`, and the
#'   merge options.
#' @noRd
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

#' Map a logical cell to character-grid spans
#'
#' Convert table coordinates `(i, j)` into character-grid `col`/`row` `start`/
#' `num`/`end` spans using the `height`/`width` attributes, and flag whether the
#' span's top/bottom edge coincides with the header or a footer separator line.
#'
#' @param tbl A `GridTable`.
#' @param i,j Logical row/column index spans.
#' @return A list with `col` and `row` span info (including `isHeaderLine` /
#'   `isFooterLine`).
#' @noRd
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

#' Compute a cell's content lines, growing the table to fit
#'
#' Assemble the (possibly multi-row, multi-column) text for a cell, split it into
#' lines, drop blank/`&nbsp;` lines, and optionally `wrap`/vertically-centre
#' (`middle`) it.
#'
#' **Auto-fit via exceptions (intentional).** If the content is taller than the
#' cell, this function grows the table's `height` attribute in place (through
#' [set_attr()]) and `stop()`s with "Adjust the height"; if it is wider, it grows
#' the `width` attribute in place and `stop()`s with "Adjust the width".
#' [toString.GridTable()] catches these and retries. Do not "fix" this into a
#' plain error path — the mutate-then-`stop` is the resize signal.
#'
#' @param tbl A `GridTable`.
#' @param i,j Logical row/column index spans (optional if `info` is given).
#' @param info Pre-computed merge + position info (optional).
#' @return A character vector of content lines (`""` when empty).
#' @noRd
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

#' Build a flat list of cells for the whole table
#'
#' Construct a `Cell` for every `(i, j)` (row-major) and flatten to one list;
#' merged non-first cells come back as `NULL`.
#'
#' @param gtable A `GridTable`.
#' @return A flat list of `Cell`s (with `NULL`s for absorbed merge cells).
#' @noRd
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

#' Merge per-cell edges into a unified table
#'
#' Drop `NULL` cells, regroup every cell's edges by character row, and run
#' `integrate_edge_list()` on each row so shared borders between neighbouring
#' cells collapse into single characters; assemble the merged rows into a
#' `Table`.
#'
#' @param cells A list of `Cell`s (as from `get_cells_from()`).
#' @return A `Table` of merged geometry, ready to render.
#' @noRd
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
