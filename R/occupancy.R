# Occupancy-map geometry + forward-pass sizing core.
#
# This file is the heart of the rewritten engine. It replaces the old
# bottom-up "build an Edge ring per cell, then merge neighbouring edges"
# machinery (geometry.R / cell.R / merge-engine.R) with a top-down, *local*
# model borrowed from the reference renderer:
#
#   * an occupancy map records, for every character-grid position, which
#     logical cell owns it (its anchor = top-left corner) and how far that
#     cell's merge extends (its last row / col);
#   * sizing is a single forward pass per axis: narrower spans settle first, a
#     span only grows the tracks it covers when its content does not fit;
#   * rendering (render.R) is then a pile of local "do these two neighbouring
#     positions belong to the same cell?" decisions.
#
# There is no edge merging and no "mutate-then-stop()" resize retry: every
# size is computed up front. All width measurement is CJK-aware via
# `str_width()` (utils.R).

#' Build the occupancy map for a rectangular grid
#'
#' Record, for every `(row, col)` position, the anchor (top-left corner) of the
#' logical cell that owns it, plus — at each anchor — how far that cell spans
#' (`last_row` / `last_col`). A non-merged cell is its own anchor with span 1.
#'
#' @param nrow,ncol Grid dimensions.
#' @param merges A list of integer `c(first_row, first_col, last_row, last_col)`
#'   rectangles (1-based, inclusive); one entry per merged region.
#' @return A list with `nrow`, `ncol`, and the `anchor_row`/`anchor_col`/
#'   `last_row`/`last_col` integer matrices.
#' @noRd
build_occupancy <- function(nrow, ncol, merges = list()) {
    anchor_row <- matrix(seq_len(nrow), nrow, ncol)
    anchor_col <- matrix(rep(seq_len(ncol), each = nrow), nrow, ncol)
    last_row   <- anchor_row
    last_col   <- anchor_col
    for (m in merges) {
        anchor_row[m[1]:m[3], m[2]:m[4]] <- m[1]
        anchor_col[m[1]:m[3], m[2]:m[4]] <- m[2]
        last_row[m[1], m[2]] <- m[3]
        last_col[m[1], m[2]] <- m[4]
    }
    list(nrow = nrow, ncol = ncol,
         anchor_row = anchor_row, anchor_col = anchor_col,
         last_row = last_row, last_col = last_col)
}

#' Is `(row, col)` the anchor (top-left) of its cell?
#'
#' @param occ An occupancy map from `build_occupancy()`.
#' @param row,col Grid position.
#' @return `TRUE` when the position is its cell's top-left corner.
#' @noRd
occ_is_anchor <- function(occ, row, col) {
    occ$anchor_row[row, col] == row && occ$anchor_col[row, col] == col
}

#' Do two positions belong to the same logical cell?
#'
#' The single predicate the renderer is built on. Off-grid positions count as a
#' different cell, so the table's outer rectangle is always drawn as a border.
#'
#' @param occ An occupancy map.
#' @param r1,c1,r2,c2 Two grid positions.
#' @return `TRUE` when both positions share one anchor (one cell).
#' @noRd
occ_same_cell <- function(occ, r1, c1, r2, c2) {
    on_grid <- function(r, c) r >= 1 && r <= occ$nrow && c >= 1 && c <= occ$ncol
    if (!on_grid(r1, c1) || !on_grid(r2, c2)) return(FALSE)
    occ$anchor_row[r1, c1] == occ$anchor_row[r2, c2] &&
        occ$anchor_col[r1, c1] == occ$anchor_col[r2, c2]
}

#' List every anchor position, row-major
#'
#' @param occ An occupancy map.
#' @return A list of `c(row, col)` integer pairs, one per logical cell.
#' @noRd
occ_anchor_cells <- function(occ) {
    cells <- list()
    for (row in seq_len(occ$nrow)) {
        for (col in seq_len(occ$ncol)) {
            if (occ_is_anchor(occ, row, col)) {
                cells[[length(cells) + 1L]] <- c(row, col)
            }
        }
    }
    cells
}

#' Split a cell's text into display lines
#'
#' @param content A character matrix of cell text.
#' @param row,col Grid position (should be an anchor).
#' @return A character vector of lines (at least `""`).
#' @noRd
cell_lines <- function(content, row, col) {
    parts <- strsplit(content[row, col], "\n", fixed = TRUE)[[1]]
    if (length(parts)) parts else ""
}

#' Display width of a cell's widest line
#'
#' @param content A character matrix of cell text.
#' @param row,col Grid position.
#' @return Integer display width of the widest line.
#' @noRd
cell_width <- function(content, row, col) {
    max(vapply(cell_lines(content, row, col),
               \(s) if (nzchar(s)) str_width(s) else 0L, 1L))
}

#' Spread a deficit as evenly as possible across spanned tracks
#'
#' If `needed` exceeds what `tracks` currently provide (plus the borders a span
#' absorbs), enlarge those tracks, distributing the shortfall as evenly as
#' possible (earlier tracks take the remainder). Monotone: never shrinks.
#'
#' @param sizes Integer vector of per-track sizes (widths or heights).
#' @param tracks Integer indices of the tracks the cell spans.
#' @param needed The size the cell's content requires.
#' @param absorbed_borders Borders internal to the span that become usable space
#'   (one per track boundary the span swallows).
#' @return The updated `sizes` vector.
#' @noRd
grow_to_fit <- function(sizes, tracks, needed, absorbed_borders) {
    available <- sum(sizes[tracks]) + absorbed_borders
    if (needed <= available) return(sizes)
    deficit  <- needed - available
    n        <- length(tracks)
    add      <- rep(deficit %/% n, n)
    leftover <- deficit %% n
    if (leftover) add[seq_len(leftover)] <- add[seq_len(leftover)] + 1L
    sizes[tracks] <- sizes[tracks] + add
    sizes
}

#' Forward-pass column widths
#'
#' `field[c]` is the number of characters between a column's two `|` borders
#' (cell text plus its two padding spaces). Single-column cells set their column
#' directly; multi-column (colspan) cells are processed narrowest-span-first and
#' only widen their tracks when their content does not already fit. `field_min`
#' is a per-column floor (e.g. a width the user pinned via `set_attr`).
#'
#' @param occ An occupancy map.
#' @param content A character matrix of cell text.
#' @param field_min Integer per-column minimum field width.
#' @return An integer vector of column field widths.
#' @noRd
compute_field <- function(occ, content, field_min) {
    field <- pmax(3L, as.integer(field_min))
    anchors <- occ_anchor_cells(occ)
    for (cell in anchors) {
        row <- cell[1]; col <- cell[2]
        if (occ$last_col[row, col] == col) {
            field[col] <- max(field[col], cell_width(content, row, col) + 2L)
        }
    }
    spanning <- Filter(\(cell) occ$last_col[cell[1], cell[2]] > cell[2], anchors)
    spanning <- spanning[order(vapply(
        spanning, \(cell) occ$last_col[cell[1], cell[2]] - cell[2], numeric(1)))]
    for (cell in spanning) {
        row <- cell[1]; col <- cell[2]; lc <- occ$last_col[row, col]
        field <- grow_to_fit(field, col:lc, cell_width(content, row, col) + 2L, lc - col)
    }
    field
}

#' Forward-pass row heights
#'
#' The width algorithm transposed: `height[r]` is the number of text lines in
#' row `r`. Single-row cells set their row from their line count; multi-row
#' (rowspan) cells are processed shortest-span-first and only grow their tracks
#' when their content does not already fit. `height_min` is a per-row floor.
#'
#' Unlike column widths, a rowspan's *internal* horizontal borders are NOT
#' usable space: the renderer always draws them as a (blank-in-this-column)
#' rule and only places text on each row's own lines. So `grow_to_fit` is called
#' with `absorbed_borders = 0` here — counting them (as the reference renderer
#' did) over-estimates the room and silently drops the cell's last line(s).
#'
#' @param occ An occupancy map.
#' @param content A character matrix of cell text.
#' @param height_min Integer per-row minimum height.
#' @return An integer vector of row heights.
#' @noRd
compute_height <- function(occ, content, height_min) {
    height <- pmax(1L, as.integer(height_min))
    anchors <- occ_anchor_cells(occ)
    for (cell in anchors) {
        row <- cell[1]; col <- cell[2]
        if (occ$last_row[row, col] == row) {
            height[row] <- max(height[row], length(cell_lines(content, row, col)))
        }
    }
    spanning <- Filter(\(cell) occ$last_row[cell[1], cell[2]] > cell[1], anchors)
    spanning <- spanning[order(vapply(
        spanning, \(cell) occ$last_row[cell[1], cell[2]] - cell[1], numeric(1)))]
    for (cell in spanning) {
        row <- cell[1]; col <- cell[2]; lr <- occ$last_row[row, col]
        height <- grow_to_fit(height, row:lr, length(cell_lines(content, row, col)), 0L)
    }
    height
}
