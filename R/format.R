#' Format one table column to character
#'
#' Trim character columns, stringify integer columns, and format numeric columns
#' with `format_one_num()` (skipping the per-number formatting when the column
#' has no decimal digits).
#'
#' @param x An atomic column vector.
#' @param digits Significant digits for numeric formatting.
#' @param ... Forwarded to `format_one_num()`.
#' @return A character vector.
#' @noRd
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

#' Format a single number for display
#'
#' Render one numeric scalar to a fixed-width string, choosing the number of
#' decimal places from the magnitude and target width, grouping thousands with
#' `big.mark`, and substituting `na.replace` for `NA`.
#'
#' @param z A length-1 numeric.
#' @param digits Significant digits.
#' @param nsmall Minimum decimal places.
#' @param width Target field width (defaults to `digits + 3`).
#' @param na.replace String used for `NA`.
#' @param big.mark Thousands separator.
#' @return A length-1 formatted string.
#' @noRd
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
               width = width, big.mark = big.mark)
    } else {
        round(z, digits = 0) |>
        format(width = width, big.mark = big.mark)
    }
}

#' Display width needed for a column
#'
#' @param x A character column.
#' @return The maximum display width across non-`NA` (trimmed) entries.
#' @noRd
cal_column_width <- function(x) {
    x <- x[!is.na(x)]
    trimws(x) |>
    str_width() |>
    max()
}

#' Per-row height (line count) of a data frame
#'
#' Each row's height is one plus the largest number of embedded newlines across
#' its cells, optionally floored at `base`.
#'
#' @param df A data frame of character cells.
#' @param base Optional minimum height per row.
#' @return An integer vector of row heights.
#' @noRd
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

#' Apply a width/height adjustment expression
#'
#' Parse an adjustment string of the form `<index><op><operand>` and apply it to
#' the numeric vector `num`. The `index` is a number or a letter (`A` = 1,
#' `B` = 2, ...); `op` is one of `+ - * / =` (omitted means `=`); `operand` is an
#' integer. A vector of expressions is applied left to right. Backs the
#' `width`/`height` DSL of [set_attr()].
#'
#' @param num The current numeric vector (existing widths or heights).
#' @param x One adjustment string, or a vector of them.
#' @return The adjusted numeric vector, or `NULL` if `x` does not parse.
#' @noRd
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

#' Recover column boundaries from a kable separator line
#'
#' Reverse-parse the `---`/`===` separator (or space-delimited rule) of a
#' `knitr::kable` into per-column `c(start, end)` display-width spans, used by
#' [kable_to_grid()] to slice each data line.
#'
#' @param line The kable separator line.
#' @param sep The column separator character (`"|"` for pipe, `" "` for simple).
#' @return A list of length-2 `c(start, end)` spans, or `NULL` if none found.
#' @noRd
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

#' Append pandoc continuation backslashes
#'
#' Ensure each wrapped content line ends with an escaping backslash so pandoc
#' treats the following line as a continuation, accounting for trailing
#' backslashes already present.
#'
#' @param x A character vector of content lines.
#' @return The lines with continuation backslashes appended where needed.
#' @noRd
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

#' Validate and expand per-column alignment
#'
#' Resolve the `align` argument to a length-`ncol` vector of `l`/`r`/`c`:
#' default from column type (`l` for character, `r` for numeric), recycle a
#' single code, or split a string like `"lcr"`.
#'
#' @param data The (column) data the alignment applies to.
#' @param align `NULL`, a single code, or a per-column string/vector.
#' @return A character vector of alignment codes, one per column.
#' @noRd
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

#' Validate a merged-cell region
#'
#' Normalise `rows`/`cols` to `c(min, max)` spans, require contiguity when more
#' than two indices are given, check they fall inside the table, and forbid a
#' region straddling the header separator.
#'
#' @param rows,cols Numeric row/column indices of the region.
#' @param gridtable The `GridTable` the region belongs to.
#' @return A list with normalised `rows` and `cols` spans.
#' @noRd
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
