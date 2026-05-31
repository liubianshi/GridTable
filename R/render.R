# Local-decision renderer + the GridTable adapter.
#
# Given a sized occupancy map this turns every border character into one local
# question — "do the two neighbouring positions belong to the same cell?" — and
# every text line into a left-to-right walk that groups same-cell columns into a
# single field. There is no edge geometry and no merge step.
#
# `toString.GridTable()` / `print.GridTable()` are the public entry points: they
# translate a `GridTable` (a data.table carrying width/height/align/header/
# footer/merged_cells attributes) into the core's inputs, render, and prepend
# any caption. All width handling is CJK-aware via `str_width()` (utils.R) and
# the box characters come from `SYMBOL` (constants.R).

#' Pad cell text to a field width, honouring alignment
#'
#' Place `text` inside a `width`-character field with at least one space of
#' padding on the left. `l` hugs the left, `r` the right, `c` centres (extra
#' space to the right). Unlike the previous engine — whose body was always
#' left-aligned — the per-column `align` now governs the body, not just the
#' header rule's colons.
#'
#' @param text The cell text for this line.
#' @param width The field width (chars between the two `|`).
#' @param alignment `"l"`, `"r"`, or `"c"`.
#' @return A `width`-character string.
#' @noRd
pad <- function(text, width, alignment) {
    gap   <- width - str_width(text)
    left  <- switch(alignment, l = 1L, r = gap - 1L, c = gap %/% 2L,
                    stop("Align invalid", call. = FALSE))
    left  <- max(left, 1L)
    right <- max(width - str_width(text) - left, 0L)
    paste0(strrep(" ", left), text, strrep(" ", right))
}

#' Render one horizontal rule (the gap between two table rows)
#'
#' Gap 0 is the table's top edge, gap `nrow` its bottom edge; gap `k` otherwise
#' sits between rows `k` and `k+1`. Each character is decided locally: a `+`
#' wherever a vertical border meets the rule, the fill character (`-`, or `=` on
#' a header/footer rule) wherever a horizontal border runs, and a space where a
#' rowspan crosses the rule uninterrupted. On a `=` rule the per-column
#' alignment colons (`:`) are written onto the fill, matching pandoc's
#' `:---`, `---:`, `:--:` markers.
#'
#' @param occ Occupancy map.
#' @param gap The gap index (0..nrow).
#' @param field,align Column field widths and alignment codes.
#' @param header Gap index carrying the header `=` rule (0 = none).
#' @param footer Integer vector of gap indices carrying a footer `=` rule.
#' @return One rendered line (string).
#' @noRd
border_line <- function(occ, gap, field, align, header, footer) {
    n_cols <- occ$ncol
    n_rows <- occ$nrow
    rule   <- (header >= 1L && gap == header) || gap %in% footer
    fill   <- if (rule) SYMBOL$HEADER else SYMBOL$LINE
    above  <- gap
    below  <- gap + 1L
    out    <- ""
    for (k in 0:n_cols) {
        vertical_above <- above >= 1L     && !occ_same_cell(occ, above, k, above, k + 1L)
        vertical_below <- below <= n_rows && !occ_same_cell(occ, below, k, below, k + 1L)
        if (vertical_above || vertical_below) {
            out <- paste0(out, SYMBOL$VERTICE)
        } else {
            horiz_left  <- k >= 1L     && !occ_same_cell(occ, above, k,      below, k)
            horiz_right <- k <  n_cols && !occ_same_cell(occ, above, k + 1L, below, k + 1L)
            out <- paste0(out, if (horiz_left || horiz_right) fill else " ")
        }
        if (k < n_cols) {
            col <- k + 1L
            if (occ_same_cell(occ, above, col, below, col)) {
                out <- paste0(out, strrep(" ", field[col]))
            } else {
                seg <- rep(fill, field[col])
                if (rule) {
                    if (align[col] %in% c("l", "c")) seg[1] <- SYMBOL$ALIGN
                    if (align[col] %in% c("r", "c")) seg[length(seg)] <- SYMBOL$ALIGN
                }
                out <- paste0(out, paste(seg, collapse = ""))
            }
        }
    }
    out
}

#' Render one text line of a table row
#'
#' Walk the row left to right, grouping consecutive columns that belong to one
#' cell (a colspan) into a single padded field. For a rowspan the text is read
#' from the anchor and indexed by the line's offset from the anchor's top; a
#' `middle` anchor first reserves leading blank lines so its content is centred
#' within the rows it spans.
#'
#' @param occ Occupancy map.
#' @param content Prepared cell-text matrix.
#' @param row,offset The table row and the 1-based text line within it.
#' @param field,align,height Column widths, alignment codes, row heights.
#' @param col_x,row_top Pre-computed character-grid offsets.
#' @param middle Logical matrix; `TRUE` at anchors to vertically centre.
#' @return One rendered line (string).
#' @noRd
content_line <- function(occ, content, row, offset, field, align, height,
                         col_x, row_top, middle) {
    out <- SYMBOL$SIDE
    col <- 1L
    while (col <= occ$ncol) {
        a_row <- occ$anchor_row[row, col]
        a_col <- occ$anchor_col[row, col]
        run_end <- col
        while (run_end < occ$ncol &&
               occ$anchor_row[row, run_end + 1L] == a_row &&
               occ$anchor_col[row, run_end + 1L] == a_col) {
            run_end <- run_end + 1L
        }
        width        <- col_x[run_end + 1L] - col_x[col] - 1L
        line_in_cell <- (row_top[row] + offset - 1L) - row_top[a_row] + 1L
        lines        <- cell_lines(content, a_row, a_col)
        if (isTRUE(middle[a_row, a_col])) {
            slots        <- sum(height[a_row:occ$last_row[a_row, a_col]])
            line_in_cell <- line_in_cell - (slots - length(lines)) %/% 2L
        }
        text <- if (line_in_cell >= 1L && line_in_cell <= length(lines)) {
            lines[line_in_cell]
        } else {
            ""
        }
        out <- paste0(out, pad(text, width, align[col]), SYMBOL$SIDE)
        col <- run_end + 1L
    }
    out
}

#' Render a sized occupancy map to grid-table lines
#'
#' The render half of the engine: interleave a `border_line()` (top edge, then
#' one after every row) with each row's `content_line()`s. Optionally drops
#' lines that carry no visible content (only side bars / whitespace), matching
#' the previous renderer's `drop_empty_line`.
#'
#' @param occ Occupancy map.
#' @param content Prepared cell-text matrix.
#' @param field,height,align Column widths, row heights, alignment codes.
#' @param header,footer Header/footer rule gap indices.
#' @param middle Logical matrix of vertically-centred anchors.
#' @param drop_empty_line Drop all-blank content lines when `TRUE`.
#' @return A character vector of rendered lines.
#' @noRd
occupancy_render <- function(occ, content, field, height, align,
                             header, footer, middle, drop_empty_line = TRUE) {
    col_x   <- c(0L, cumsum(field + 1L))
    row_top <- c(1L, 1L + cumsum(height)[seq_len(occ$nrow - 1L)])

    rendered <- border_line(occ, 0L, field, align, header, footer)
    for (row in seq_len(occ$nrow)) {
        for (offset in seq_len(height[row])) {
            rendered <- c(rendered, content_line(occ, content, row, offset, field,
                                                 align, height, col_x, row_top, middle))
        }
        rendered <- c(rendered, border_line(occ, row, field, align, header, footer))
    }
    if (isTRUE(drop_empty_line)) {
        rendered <- rendered[grepl("[^|[:space:]]", rendered)]
    }
    rendered
}

#' Translate a GridTable into the occupancy core's inputs
#'
#' Pull the cell text and the width/height/align/header/footer/merged_cells
#' render state off a `GridTable` and reshape them for `occupancy_render()`:
#' build the merge rectangles, fold each merged region's content into its anchor
#' (joining members unless `drop_content`, dropping blank/`&nbsp;` lines, adding
#' pandoc continuation `\` when `wrap`), and map the header/footer row indices to
#' rule gaps.
#'
#' @param gtable A `GridTable`.
#' @return A named list of arguments for `occupancy_render()`.
#' @noRd
grid_inputs <- function(gtable) {
    n_rows <- nrow(gtable)
    n_cols <- ncol(gtable)

    content <- as.matrix(gtable)
    storage.mode(content) <- "character"
    content[is.na(content)] <- ""
    dimnames(content) <- NULL

    align  <- attr(gtable, "align")
    names(align) <- NULL
    header <- as.integer(attr(gtable, "header"))

    ftr        <- attr(gtable, "footer")
    ftr        <- ftr[is.finite(ftr) & ftr >= 1L & ftr <= n_rows]
    footer     <- if (length(ftr)) as.integer(unique(c(ftr - 1L, n_rows))) else integer(0)

    merges <- list()
    middle <- matrix(FALSE, n_rows, n_cols)
    wrap_anchor <- list()
    for (m in attr(gtable, "merged_cells")) {
        r1 <- m$rows[1]; r2 <- m$rows[2]
        c1 <- m$cols[1]; c2 <- m$cols[2]
        merges[[length(merges) + 1L]] <- c(r1, c1, r2, c2)

        cell_text <- if (isTRUE(m$drop_content)) {
            content[r1, c1]
        } else {
            paste(as.vector(content[r1:r2, c1:c2]), collapse = "\n")
        }
        content[r1:r2, c1:c2] <- ""        # clear the whole region first ...
        content[r1, c1] <- cell_text       # ... then write the folded text to the anchor

        if (isTRUE(m$middle)) middle[r1, c1] <- TRUE
        if (isTRUE(m$wrap))   wrap_anchor[[length(wrap_anchor) + 1L]] <- c(r1, c1)
    }

    occ <- build_occupancy(n_rows, n_cols, merges)

    is_wrap <- function(r, c) {
        any(vapply(wrap_anchor, \(a) a[1] == r && a[2] == c, logical(1)))
    }
    for (cell in occ_anchor_cells(occ)) {
        r <- cell[1]; c <- cell[2]
        lines <- strsplit(content[r, c], "\n", fixed = TRUE)[[1]]
        lines <- lines[!grepl("^\\s*(&nbsp;)?\\s*$", lines, perl = TRUE)]
        if (length(lines) == 0L) {
            content[r, c] <- ""
            next
        }
        if (length(wrap_anchor) && is_wrap(r, c) && length(lines) > 1L) {
            lines[-length(lines)] <- pandoc_wrap(lines[-length(lines)])
        }
        content[r, c] <- paste(lines, collapse = "\n")
    }

    field_min  <- attr(gtable, "width") + 2L
    height_min <- attr(gtable, "height")

    field  <- compute_field(occ, content, field_min)
    height <- compute_height(occ, content, height_min)

    list(occ = occ, content = content, field = field, height = height,
         align = align, header = header, footer = footer, middle = middle)
}

#' Render and Print a Grid Table
#'
#' `toString.GridTable()` renders a [GridTable] to a character vector of
#' grid-table lines; `print.GridTable()` renders and `cat`s it. Both run the
#' occupancy-map engine: `grid_inputs()` folds the render state into a sized
#' occupancy map, `occupancy_render()` draws it, and any `caption` attribute is
#' prepended. All sizing is computed up front — there is no resize retry loop.
#'
#' @param x A `GridTable` object.
#' @param drop_empty_line When `TRUE` (default), drop rendered lines that carry
#'   no visible content.
#' @param ... Unused, for S3 signature compatibility.
#'
#' @return `toString.GridTable()` returns a character vector with class
#'   `GridTable_output`. `print.GridTable()` returns it invisibly after printing.
#'
#' @examples
#' tbl <- GridTable(data.frame(a = c("中文", "x"), b = c(1.5, 22)))
#' toString(tbl)
#' print(tbl)
#'
#' @rdname print.GridTable
#' @export
toString.GridTable <- function(x, drop_empty_line = TRUE, ...) {
    args    <- grid_inputs(x)
    content <- occupancy_render(args$occ, args$content, args$field, args$height,
                                args$align, args$header, args$footer, args$middle,
                                drop_empty_line = drop_empty_line)
    if (!is.null(attr(x, "caption"))) {
        content <- c(attr(x, "caption"), "", content)
    }
    structure(content, class = "GridTable_output")
}

#' @rdname print.GridTable
#' @export
print.GridTable <- function(x, drop_empty_line = TRUE, ...) {
    content <- toString(x, drop_empty_line = drop_empty_line, ...)
    cat(content, sep = "\n")
    invisible(content)
}
