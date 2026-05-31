#' Classify how two 1-D spans relate
#'
#' Compare two ranges by their endpoints and return a constant naming their
#' relationship, used to drive `edge_merge()`. Possible values:
#' `LEFT`, `ADJACENT_LEFT`, `OVERLAP_LEFT`, `IN_SAME_START`, `In`,
#' `CONTAIN_SAME_END`, `EQUAL`, `IN_SAME_END`, `CONTAIN`, `CONTAIN_SAME_START`,
#' `OVERLAP_RIGHT`, `ADJACENT_RIGHT`, `RIGHT`.
#'
#' @param s1,s2 Numeric spans (endpoints in either order).
#' @param na.rm Drop `NA`s before reducing.
#' @return A length-1 relationship constant (character).
#' @noRd
range_relation <- function(s1, s2, na.rm = TRUE) {
    l1 <- min(s1, na.rm = na.rm)
    l2 <- min(s2, na.rm = na.rm)
    r1 <- max(s1, na.rm = na.rm)
    r2 <- max(s2, na.rm = na.rm)

    if (r1 < l2) {
        return("LEFT")
    } else if (r1 == l2) {
        return("ADJACENT_LEFT")
    } else if (r1 > l2 & r1 < r2) {
        return(
            if      (l1 < l2)  "OVERLAP_LEFT"
            else if (l1 == l2) "IN_SAME_START"
            else               "In"
        )
    } else if (r1 == r2) {
        return(
            if      (l1 < l2)  "CONTAIN_SAME_END"
            else if (l1 == l2) "EQUAL"
            else               "IN_SAME_END"
        )
    } else {
        return(
            if      (l1 <  l2)           "CONTAIN"
            else if (l1 == l2)           "CONTAIN_SAME_START"
            else if (l1 > l2 && l1 < r2) "OVERLAP_RIGHT"
            else if (l1 == r2)           "ADJACENT_RIGHT"
            else                         "RIGHT"
        )
    }
}

#' Relationship between two edges on the same row
#'
#' `range_relation()` applied to the column spans of two edges sharing a row.
#'
#' @param e1,e2 `Edge`s on the same row.
#' @return A relationship constant (see `range_relation()`).
#' @noRd
edge_relation <- function(e1, e2) {
    stopifnot(inherits(e1, "Edge") && inherits(e2, "Edge"))
    stopifnot(row_no(e1) == row_no(e2))
    range_relation(col_no(e1), col_no(e2))
}

#' Merge two overlapping or adjacent edges
#'
#' The core of the geometry engine: combine two edges that share a row into the
#' minimal set of non-overlapping edges, collapsing shared `+` vertices,
#' splitting partial overlaps into left/overlap/right pieces, and resolving
#' borders (e.g. a header `=` overriding a `-`). The inputs are first normalised
#' so `e1` starts no later than `e2`, then dispatched on `edge_relation()`:
#'
#' * `LEFT` — disjoint; insert an empty gap edge between them.
#' * `ADJACENT_LEFT` — touch at a node; join via `edge_left_extend()`.
#' * `CONTAIN_SAME_END` / `CONTAIN` / `CONTAIN_SAME_START` — one contains the
#'   other; recurse on the overlapping middle.
#' * `OVERLAP_LEFT` — partial overlap; split into left / merged-overlap / right.
#' * `IN_SAME_START` — shared start; merge the common head, keep the tail.
#' * `EQUAL` — identical span; merge content/type/symbol, raising "Content
#'   Overlapping" when two real contents collide.
#'
#' Note: the right-leaning relations (`In`, `IN_SAME_END`, `OVERLAP_RIGHT`,
#' `ADJACENT_RIGHT`, `RIGHT`) have no `switch` branch because normalisation is
#' expected to turn them into their left-leaning mirror; unusual merge layouts
#' that still reach them fall through to `NULL`. Several branches are lightly
#' exercised — fix the specific case you actually hit rather than assuming the
#' whole table of cases is covered.
#'
#' @param e1,e2 `Edge`s on the same row.
#' @return A list of merged `Edge`s.
#' @noRd
edge_merge <- function(e1, e2) {
    if (col_no(e1$leftnode) > col_no(e2$leftnode)) return(edge_merge(e2, e1))
    switch(edge_relation(e1, e2),
        LEFT = {
            list(e1, Edge(e1$rightnode, e2$leftnode, ""), e2)
        },
        ADJACENT_LEFT = {
            edge_left_extend(e1, e2)
        },
        CONTAIN_SAME_END = {
            left_part    <- edge_update(e1, rightnode = e2$leftnode)
            overlap_part <- edge_update(e1, leftnode = e2$leftnode, align = e2$align)
            c(list(left_part), edge_merge(overlap_part, e2))
        },
        OVERLAP_LEFT = {
            left_part    <- edge_update(e1, rightnode = e2$leftnode)
            overlap_p1   <- edge_update(e1, leftnode  = e2$leftnode)
            overlap_p2   <- edge_update(e2, rightnode = e1$rightnode)
            right_part   <- edge_update(e2, leftnode  = e1$rightnode)
            c(list(left_part), edge_merge(overlap_p1, overlap_p2),
              list(right_part))
        },
        IN_SAME_START = {
            overlap_part <- edge_update(e2, rightnode = e1$rightnode, align = e1$align)
            right_part   <- edge_update(e2, leftnode  = e1$rightnode)
            c(edge_merge(e1, overlap_part), list(right_part))
        },
        EQUAL = {
            e1 <- edge_update(e1, leftnode = node_merge(e1$leftnode, e2$leftnode),
                                  rightnode = node_merge(e1$rightnode, e2$rightnode))
            if ((e1$type[1] == "Normal" && e2$type[1] != "Empty") ||
                (e1$type[1] != "Empty"  && e2$type[1] == "Normal")) {
                stop("Content Overlapping", call. = FALSE)
            }
            if (e1$type[1] == "Empty" || sum(c("HEADER", "FOOTER") %in% e2$type)) {
                e1 <- edge_update(e1, content = e2$content,
                                      type = e2$type,
                                      symbol = e2$symbol)
            }
            list(e1)
        },
        CONTAIN = {
            left_part    <- edge_update(e1, rightnode = e2$leftnode)
            overlap_part <- edge_update(e1, leftnode  = e2$leftnode,
                                            rightnode = e2$rightnode,
                                            align     = e2$align)
            right_part   <- edge_update(e1, leftnode  = e2$rightnode)
            c(list(left_part), edge_merge(overlap_part, e2), list(right_part))
        },
        CONTAIN_SAME_START = {
            overlap_part <- edge_update(e1, rightnode = e2$rightnode, align = e2$align)
            right_part   <- edge_update(e1, leftnode  = e2$rightnode)
            c(edge_merge(overlap_part, e2), list(right_part))
        }
    )
}

#' Integrate a row's edges into a non-overlapping sequence
#'
#' Sort the edges left-to-right and sweep through them, repeatedly
#' `edge_merge()`ing the running result with the next edge. When a merge yields a
#' trailing remainder that still overlaps later edges, the remainder is pushed
#' back onto the worklist (via `shift()`/`sort_edge_list()`) and reconsidered.
#'
#' @param edge_list A list of `Edge`s from one character row.
#' @return A list of merged, ordered, non-overlapping `Edge`s.
#' @noRd
integrate_edge_list <- function(edge_list) {
    old <- sort_edge_list(edge_list)
    if (length(edge_list) == 1) return(old)

    new <- list(shift(old))
    temp <- list(shift(old))
    while (length(old) > 0 || length(temp) > 0) {
        e <- if (length(temp) == 0)         shift(old)
             else if (length(old)  == 0)    shift(temp)
             else if (old[[1]] < temp[[1]]) shift(old)
             else                           shift(temp)
        m <- edge_merge(new[[length(new)]], e)
        if (e < m[[length(m)]]) {
            new <- c(new[-length(new)], m[-length(m)])
            temp <- sort_edge_list(c(m[length(m)], temp))
        } else {
            new <- c(new[-length(new)], m)
        }
    }
    new
}

#' Order edges by position within a row
#'
#' Drop `NULL`s and sort edges by left column, breaking ties by right column (via
#' a packed `left * maxRight + right` key).
#'
#' @param edge_list A list of `Edge`s.
#' @return The list sorted left-to-right.
#' @noRd
sort_edge_list <- function(edge_list) {
    edge_list <- edge_list[!purrr::map_lgl(edge_list, is.null)]
    lcol_nos <- purrr::map_int(edge_list, \(e) col_no(e$leftnode))
    rcol_nos <- purrr::map_int(edge_list, \(e) col_no(e$rightnode))
    index <- lcol_nos * max(rcol_nos) + rcol_nos
    edge_list[order(index)]
}
