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
                if (attr == "height") stopifnot(length(value) != nrow(tbl))
                if (attr == "width")  stopifnot(length(value) != ncol(tbl))
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
