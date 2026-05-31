#' Render Low-Level Grid Geometry to Text
#'
#' Internal `toString` methods that turn the merged grid geometry into character
#' strings, applied bottom-up: an `Edge` becomes one horizontal slice of a line
#' (border run or padded content), a `Row` concatenates its edges, and a `Table`
#' maps every row to a line. These are building blocks of
#' [toString.GridTable()]; users normally never call them directly.
#'
#' @param edge An `Edge` object.
#' @param row A `Row` object.
#' @param tbl A `Table` object.
#' @param drop_empty_line When `TRUE`, drop lines that carry no visible content
#'   (only side bars/whitespace).
#' @param ... Unused, for S3 signature compatibility.
#'
#' @return A character string (`Edge`/`Row`) or character vector (`Table`).
#'
#' @rdname grid-tostring-internal
#' @keywords internal
#' @export
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

#' @rdname grid-tostring-internal
#' @keywords internal
#' @export
toString.Row <- function(row) {
    paste0(purrr::map_chr(row$edges, toString.Edge), collapse = "")
}

#' @rdname grid-tostring-internal
#' @keywords internal
#' @export
toString.Table <- function(tbl, drop_empty_line = TRUE, ...) {
    table_content <- purrr::map_chr(tbl$rows, ~ toString.Row(.x))
    if (isTRUE(drop_empty_line)) {
        table_content <- table_content[grepl("[^|\\s]", table_content, perl = TRUE)]
    }
    table_content
}

#' Render and Print a Grid Table
#'
#' `toString.GridTable()` renders a [GridTable] object to a character vector of
#' grid-table lines; `print.GridTable()` renders and `cat`s it to the console.
#' Both drive the build/merge pipeline (`get_cells_from()` →
#' `merge_cell_list()` → [toString.Table()]) and prepend any `caption`
#' attribute.
#'
#' Rendering runs an **auto-fit retry loop**: while building cells the engine may
#' discover that content is too tall or too wide, grow the table's `height`/
#' `width` attribute in place, and signal this by `stop()`ing with "Adjust the
#' ...". `toString.GridTable()` catches that condition and re-invokes itself
#' until everything fits. Exceptions are used as control flow here by design.
#'
#' @param gtable A `GridTable` object.
#' @param drop_empty_line When `TRUE` (default), drop rendered lines that carry
#'   no visible content.
#' @param ... Passed through to [toString.Table()].
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

#' @rdname print.GridTable
#' @export
print.GridTable <- function(gtable, drop_empty_line = TRUE, ...) {
  content <- toString(gtable, drop_empty_line = drop_empty_line, ...)
  cat(content, sep = "\n")
  invisible(content)
}
