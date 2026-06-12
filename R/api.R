#' Render a Data Frame as a Pandoc Grid Table
#'
#' Build a `GridTable` object from a `data.frame`/`data.table`. The result is a
#' `data.table` carrying all render state (column widths, row heights,
#' alignment, header/footer positions) as attributes; print it with
#' [print.GridTable()] to obtain the plain-text grid table. The layout engine is
#' CJK-width aware (it measures everything with `nchar(type = "width")`), so
#' columns containing Chinese or other wide characters stay aligned.
#'
#' @param data A `data.frame` or `data.table` (anything coercible by
#'   [data.table::as.data.table()]). Numeric columns are formatted with
#'   `format_one_num()`; character columns are trimmed.
#' @param align Column alignment. Either `NULL` (default: left for character
#'   columns, right for numeric), a single character such as `"l"`, `"r"` or
#'   `"c"` recycled to every column, or a string/vector of per-column codes such
#'   as `"lcr"`.
#' @param header Number of leading rows that form the table header: the header
#'   separator line (drawn with `=`) is placed below row `header`. When `NULL`
#'   or `0`, the column names are prepended as the first row and become the
#'   header (`header = 1`); user-facing row indices then start at 2 for the
#'   first data row.
#' @param footer Row index of the first table-foot row: a `=` separator line
#'   is drawn above that row, so the rows from `footer` to the bottom print as
#'   the table foot (pandoc's table foot, the analogue of HTML `<tfoot>` --
#'   e.g. a totals row). This is *not* a footnote mechanism. Defaults to `Inf`
#'   (no table foot).
#' @param ... Additional attributes stored on the returned object via
#'   [data.table::setattr()]. The most useful are `caption`, a string rendered
#'   above the table, and `note_style`, a paragraph style name wrapping the
#'   footnote block (see [add_footnote()]).
#'
#' @return An object of class `GridTable` (a `data.table` with render-state
#'   attributes). Use [print.GridTable()] or [toString.GridTable()] to render
#'   it.
#'
#' @examples
#' df <- data.frame(name = c("Alice", "Bob"), score = c(1.5, 22))
#' print(GridTable(df))
#'
#' # Per-column alignment plus a caption (passed through `...`)
#' print(GridTable(df, align = "lc", caption = "Table: scores"))
#'
#' # CJK-width awareness keeps wide characters aligned
#' print(GridTable(data.frame(a = c("中文", "x"), b = c(1.5, 22))))
#'
#' @seealso [merge_cells()] to merge cells, [set_attr()] to tweak widths and
#'   heights, [kable_to_grid()] to build from a `knitr::kable`.
#' @export
GridTable <- function(data, align = NULL,
                      header = NULL, footer = NULL, ...) {
    if (!inherits(data, "data.table")) data <- data.table::as.data.table(data)
    args <- list(...)
    align <- valid_align(data, align)

    data <- data.table::as.data.table(purrr::map(data, format_column))
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

#' Build a Grid Table from a 'knitr' kable
#'
#' Reverse-parse a `knitr::kable()` (in `"pipe"` or `"simple"` format) back into
#' a `GridTable`. The kable separator line is used to recover the column
#' boundaries (via `column_start_end_points()`); each data line is then sliced
#' by display width (via `substr_width()`) so the original column contents are
#' recovered as character cells. A leading `Table:` caption line, if present, is
#' carried over.
#'
#' @param kbl A `knitr_kable` object produced with `format = "pipe"` or
#'   `format = "simple"`.
#' @param ... Further arguments forwarded to [GridTable()] (e.g. `align`,
#'   `footer`, `caption`). `header` defaults to `1` and `caption` defaults to
#'   the kable's own `Table:` line when not supplied.
#'
#' @return An object of class `GridTable`.
#'
#' @examples
#' if (requireNamespace("knitr", quietly = TRUE)) {
#'   df  <- data.frame(name = c("Alice", "Bob"), score = c(1, 22))
#'   kbl <- knitr::kable(df, format = "pipe")
#'   print(kable_to_grid(kbl))
#' }
#'
#' @seealso [GridTable()]
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
        tbl   <- if (grepl("[^\\|\\s]", kbl[1], perl = TRUE)) kbl[-2] else kbl[-(1:2)]
        sep     <-  "|"
    } else if (grepl("[^\\-\\s]", kbl[length(kbl)], perl = TRUE)) {
        sepline <- kbl[2]
        tbl   <- kbl[-2]
    } else {
        sepline <- kbl[1]
        tbl   <- kbl[-c(1, length(kbl))]
    }

    start_end_points <- column_start_end_points(sepline, sep)
    stopifnot(!is.null(start_end_points))
    data <- purrr::map(tbl, \(line) {
        purrr::map_chr(start_end_points, \(x) substr_width(line, x[1], x[2]))
    })
    data <- do.call(rbind, data)

    args <- list(...)
    args$data <- data
    if (is.null(args$header)) args$header = 1L
    if (is.null(args$caption)) args$caption = caption
    do.call(GridTable, args)
}

#' Register, Cancel, or List Merged Cell Regions
#'
#' Manage the rectangular merged-cell regions of a `GridTable`. Regions are
#' stored as a named-list attribute and consumed by the rendering engine, which
#' drops the internal borders so the region prints as a single cell. All forms
#' modify `tbl` by reference and return it invisibly.
#'
#' Behaviour depends on which arguments are supplied:
#'
#' * `i` **and** `j` given: register a new region spanning rows `i` and columns
#'   `j`. The region may not straddle the header separator and may not overlap
#'   an existing region.
#' * `cancel` given: remove a region. Use `cancel = TRUE` together with `i`/`j`
#'   to drop the region they describe, or pass the region name(s) as a character
#'   vector.
#' * nothing given: print the currently registered regions and return invisibly.
#'
#' @param tbl A `GridTable` object.
#' @param i Integer row indices of the region (must be supplied together with
#'   `j`).
#' @param j Integer column indices of the region.
#' @param cancel `TRUE` to cancel the region described by `i`/`j`, or a character
#'   vector of region names to cancel.
#' @param ... Merge options stored with the region: `drop_content` (keep only
#'   the top-left cell's content), `middle` (vertically centre the content),
#'   `wrap` (pandoc line-wrap the content). See `MERGED_CELL_OPTION` for the
#'   defaults.
#'
#' @return The (invisibly returned) `GridTable`, modified by reference.
#'
#' @examples
#' df  <- data.frame(a = c("x", "y", "z"), b = c(1, 2, 3))
#' tbl <- GridTable(df)
#' merge_cells(tbl, i = 2:3, j = 1)   # merge column 1 across rows 2-3
#' print(tbl)
#'
#' merge_cells(tbl)                   # list registered regions
#' merge_cells(tbl, i = 2:3, j = 1, cancel = TRUE)  # undo the merge
#'
#' @seealso [GridTable()]
#' @export
merge_cells <- function(tbl, i = NULL, j = NULL, cancel = NULL, ...) {
    old_merged_cell <- attr(tbl, "merged_cells")
    old_merged_cell_names <- names(old_merged_cell)

    if (is.null(i) != is.null(j)) {
        stop("Need to set both i and j", call. = FALSE)
    }

    if (is.null(i) && is.null(j) && is.null(cancel)) {
        purrr::iwalk(old_merged_cell, \(v, n) {
            cat("Name:", n, "\n")
            purrr::iwalk(v, ~ cat("\t", .y, ": ", toString(.x), "\n", sep = ""))
        })
        return(invisible(tbl))
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
        data.table::setattr(tbl, "merged_cells", old_merged_cell)
        return(invisible(tbl))
    }

    merged_cell <- c(valid_merged_cell(i, j, tbl), list(...))
    purrr::walk(old_merged_cell, \(m1, m2) {
        if (is_overlaped(m1$rows, m2$rows) && is_overlaped(m1$cols, m2$cols)) {
            stop("There is overlap", call. = FALSE)
        }
    }, m2 = merged_cell)

    newattrs <- c(old_merged_cell, list(merged_cell))
    names(newattrs) <- c(old_merged_cell_names, merged_cell_name)
    data.table::setattr(tbl, "merged_cells", newattrs)
    return(invisible(tbl))
}

#' Add a Footnote Below a Grid Table
#'
#' Register a footnote on a `GridTable`. Footnotes are stored in the `notes`
#' attribute and rendered by [toString.GridTable()] as paragraphs *below* the
#' table block, mirroring how `caption` is placed above it -- the table
#' geometry itself is untouched (no extra rows are inserted). When `ref` is
#' given, the note is prefixed with the pandoc superscript marker `^ref^` and
#' the cells selected by `i`/`j` get the same marker appended to their content
#' (column widths grow automatically at render time). Modifies `tbl` by
#' reference and returns it invisibly.
#'
#' @details
#' When the table carries a `note_style` attribute (set it with
#' `GridTable(..., note_style = "Table Note")` or
#' `set_attr(tbl, note_style = "Table Note")`), the rendered notes are wrapped
#' in a pandoc fenced div `::: {custom-style="..."}`. Pandoc applies custom
#' styles in docx, odt and ICML output (the style must exist in the reference
#' document); other writers ignore the attribute and the notes degrade to
#' plain paragraphs. Without `note_style` the notes are emitted as bare
#' paragraphs, keeping terminal output free of fence noise.
#'
#' Cell markers are baked into the cell text at call time, while the note
#' itself lives in the `notes` attribute: clearing that attribute afterwards
#' (e.g. `set_attr(tbl, notes = NULL)`) does not remove markers already
#' placed in cells.
#'
#' @param tbl A `GridTable` object.
#' @param note Character string: the footnote text. The reference marker is
#'   added automatically from `ref`; do not include it here.
#' @param ref Optional character string: a reference symbol such as `"a"` or
#'   `"*"`, rendered as a pandoc superscript `^ref^`.
#' @param i,j Optional integer vectors selecting the cells to mark with the
#'   `^ref^` marker (every combination of `i` rows and `j` columns). Must be
#'   supplied together, and require `ref`. Row indices follow the user-facing
#'   convention: when the column names were prepended (`header = NULL`), row 1
#'   is the header row.
#'
#' @return The (invisibly returned) `GridTable`, modified by reference.
#'
#' @examples
#' df  <- data.frame(term = c("x1", "x2"), est = c(1.34, 2.1))
#' tbl <- GridTable(df)
#' add_footnote(tbl, "Standard errors in parentheses.", ref = "a", i = 1, j = 2)
#' add_footnote(tbl, "Source: simulated data.")
#' print(tbl)
#'
#' # Wrap the notes in a custom-style div for docx output
#' set_attr(tbl, note_style = "Table Note")
#' print(tbl)
#'
#' @seealso [GridTable()], [set_attr()]
#' @export
add_footnote <- function(tbl, note, ref = NULL, i = NULL, j = NULL) {
    stopifnot(inherits(tbl, "GridTable"))
    if (!is.character(note) || length(note) != 1L || is.na(note)) {
        stop("`note` must be a single character string", call. = FALSE)
    }
    if (is.null(i) != is.null(j)) {
        stop("Need to set both i and j", call. = FALSE)
    }
    marker <- NULL
    if (!is.null(ref)) {
        if (!is.character(ref) || length(ref) != 1L || is.na(ref) || !nzchar(ref)) {
            stop("`ref` must be a single non-empty character string", call. = FALSE)
        }
        marker <- paste0("^", ref, "^")
        note   <- paste0(marker, " ", note)
    }
    if (!is.null(i)) {
        if (is.null(marker)) {
            stop("`ref` is required when marking cells with i/j", call. = FALSE)
        }
        i <- as.integer(i)
        j <- as.integer(j)
        stopifnot(all(i >= 1L), all(i <= nrow(tbl)),
                  all(j >= 1L), all(j <= ncol(tbl)))
        for (r in i) {
            for (col in j) {
                data.table::set(tbl, r, col, paste0(tbl[[col]][r], marker))
            }
        }
    }
    data.table::setattr(tbl, "notes", c(attr(tbl, "notes"), note))
    invisible(tbl)
}

#' Adjust Render-State Attributes of a Grid Table
#'
#' Set or adjust an attribute of a `GridTable` by reference. Besides the generic
#' "set this attribute to this value" behaviour, the `width` and `height`
#' attributes accept a small adjustment DSL so individual columns/rows can be
#' grown or shrunk relative to their current value. Multiple attributes may be
#' set at once through `...`.
#'
#' @details
#' For `width` and `height`, `value` may be:
#'
#' * a numeric vector, used as the new attribute verbatim; or
#' * a character adjustment string of the form `<index><op><operand>`, parsed by
#'   `parse_number_adjust()`. The `index` is either a number or a letter
#'   (`A` = 1, `B` = 2, ...); `op` is one of `+ - * / =` (omitting it means `=`);
#'   `operand` is an integer. For example `"2/2"` halves the second value,
#'   `"b-3"` subtracts 3 from the second, and `"1+1"` increments the first. A
#'   vector of such strings applies the adjustments in turn.
#'
#' For `align`, `value` is validated by `valid_align()`. Any other attribute is
#' set verbatim.
#'
#' @param tbl A `GridTable` object.
#' @param attr Name of the attribute to set (e.g. `"width"`, `"height"`,
#'   `"align"`). May be `NULL` when only `...` is used.
#' @param value New value, or an adjustment string/vector for `width`/`height`.
#' @param ... Further `attr = value` pairs, each applied as an additional call.
#'
#' @return The (invisibly returned) `GridTable`, modified by reference.
#'
#' @examples
#' df  <- data.frame(a = c("x", "y"), b = c(1, 2))
#' tbl <- GridTable(df)
#' set_attr(tbl, "width", "1+3")   # widen column 1 by 3 display columns
#' set_attr(tbl, align = "c")      # centre every column (through `...`)
#' print(tbl)
#'
#' @seealso [GridTable()]
#' @export
set_attr <- function(tbl, attr = NULL, value = NULL, ...) {
    stopifnot(inherits(tbl, "GridTable"))
    purrr::iwalk(list(...), \(value, attr) set_attr(tbl, attr, value))
    if (is.null(attr)) return(invisible(tbl))

    switch(attr,
        align  = data.table::setattr(tbl, attr, valid_align(tbl, value)),
        height =,
        width  = {
            if (is.numeric(value)) {
                if (attr == "height") stopifnot(length(value) == nrow(tbl))
                if (attr == "width")  stopifnot(length(value) == ncol(tbl))
                data.table::setattr(tbl, attr, value)
            } else {
                data.table::setattr(tbl, attr,
                                    parse_number_adjust(attr(tbl, attr), value))
            }
        },
        data.table::setattr(tbl, attr, value)
    )
    invisible(tbl)
}
