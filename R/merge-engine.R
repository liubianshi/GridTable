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

edge_relation <- function(e1, e2) {
    stopifnot(inherits(e1, "Edge") && inherits(e2, "Edge"))
    stopifnot(row_no(e1) == row_no(e2))
    range_relation(col_no(e1), col_no(e2))
}

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

sort_edge_list <- function(edge_list) {
    edge_list <- edge_list[!purrr::map_lgl(edge_list, is.null)]
    lcol_nos <- purrr::map_int(edge_list, \(e) col_no(e$leftnode))
    rcol_nos <- purrr::map_int(edge_list, \(e) col_no(e$rightnode))
    index <- lcol_nos * max(rcol_nos) + rcol_nos
    edge_list[order(index)]
}
