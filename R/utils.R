#' Null-coalescing helper
#'
#' @param x,y Any values; returns `x` unless it is `NULL`, otherwise `y`.
#' @return `x` if non-`NULL`, else `y`.
#' @noRd
ifthen <- function(x, y) {
    if (is.null(x)) y else x
}

#' Minimum and maximum of a numeric vector
#'
#' @param x A numeric vector.
#' @param na.rm Drop `NA`s before reducing.
#' @return A length-2 vector `c(min, max)`.
#' @noRd
minmax <- function(x, na.rm = TRUE) {
    stopifnot(is.numeric(x))
    c(min(x, na.rm = na.rm), max(x, na.rm = na.rm))
}

#' Coerce to integer, refusing fractional input
#'
#' @param x A number expected to hold no fractional part.
#' @return The value as an integer (errors if it had decimals).
#' @noRd
toInteger <- function(x) {
    stopifnot(!grepl(".", as.character(x + 0), fixed = TRUE))
    as.integer(x)
}

#' Pop the first element of a list in place
#'
#' Returns the head of `x` **and** reassigns `x` to its tail in the caller's
#' frame (via `substitute` + `assign`). This is a side-effecting "shift": after
#' `h <- shift(stack)`, `stack` has lost its first element. Used to consume
#' worklists in the edge-merge loop.
#'
#' @param x A list or vector variable (passed by name).
#' @param drop Whether to drop dimensions / unwrap a single list element.
#' @return The removed first element, or `NULL` if `x` was empty.
#' @noRd
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

#' Do two closed intervals overlap?
#'
#' Order-insensitive overlap test for two length-2 ranges (each given as
#' endpoints in either order).
#'
#' @param x,y Length-2 numeric ranges (atomic or list).
#' @return `TRUE` if the intervals intersect.
#' @noRd
is_overlaped <- function(x, y) {
    stopifnot(length(x) == 2 && length(y) == 2)
    get <- function(x, i) if (is.atomic(x))        x[i]     else x[[i]]
    min <- function(z)    if (get(z,1) < get(z,2)) get(z,1) else get(z,2)
    max <- function(z)    if (get(z,1) < get(z,2)) get(z,2) else get(z,1)
    !(min(x) > max(y) || min(y) > max(x))
}

#' Display width of a string (CJK-aware)
#'
#' @param x A character vector.
#' @return Integer display width, counting wide characters as 2.
#' @noRd
str_width <- function(x) nchar(x, type = "width")

#' Extract the first regex match with capture groups
#'
#' @param x A string.
#' @param pattern A regular expression.
#' @return The full match followed by its capture groups.
#' @noRd
str_match <- function(x, pattern) regmatches(x, regexec(pattern, x))

#' Substring by display width
#'
#' Slice `x` between display-width positions `start` and `end`, nudging the
#' bounds inward when they would fall in the middle of a wide character (raising
#' an error if a clean cut is impossible). Used by [kable_to_grid()] to recover
#' columns from a kable line.
#'
#' @param x A string.
#' @param start,end Inclusive display-width positions.
#' @return The sliced substring.
#' @noRd
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
