ifthen <- function(x, y) {
    if (is.null(x)) y else x
}

minmax <- function(x, na.rm = TRUE) {
    stopifnot(is.numeric(x))
    c(min(x, na.rm = na.rm), max(x, na.rm = na.rm))
}

toInteger <- function(x) {
    stopifnot(!grepl(".", as.character(x + 0), fixed = TRUE))
    as.integer(x)
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

is_overlaped <- function(x, y) {
    stopifnot(length(x) == 2 && length(y) == 2)
    get <- function(x, i) if (is.atomic(x))        x[i]     else x[[i]]
    min <- function(z)    if (get(z,1) < get(z,2)) get(z,1) else get(z,2)
    max <- function(z)    if (get(z,1) < get(z,2)) get(z,2) else get(z,1)
    !(min(x) > max(y) || min(y) > max(x))
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
