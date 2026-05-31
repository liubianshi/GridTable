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

#' @export
toString.Row <- function(row) {
    paste0(purrr::map_chr(row$edges, toString.Edge), collapse = "")
}

#' @export
toString.Table <- function(tbl, drop_empty_line = TRUE, ...) {
    table_content <- purrr::map_chr(tbl$rows, ~ toString.Row(.x))
    if (isTRUE(drop_empty_line)) {
        table_content <- table_content[grepl("[^|\\s]", table_content, perl = TRUE)]
    }
    table_content
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

#' @export
print.GridTable <- function(gtable, drop_empty_line = TRUE, ...) {
  content <- toString(gtable, drop_empty_line = drop_empty_line, ...)
  cat(content, sep = "\n")
  invisible(content)
}
