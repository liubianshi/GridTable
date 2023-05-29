SYMBOL <- list(VERTICE = "+",
               SIDE    = "|",
               LINE    = "-",
               HEADER  = "=",
               FOOTER  = "=",
               ALIGN   = ":")

`<.Node` <- function(n1, n2) {
    stopifnot(row_no(n1) == row_no(n2))
    col_no(n1) < col_no(n2)
}

`>.Node` <- function(n1, n2) {
    stopifnot(row_no(n1) == row_no(n2))
    col_no(n1) > col_no(n2)
}

`==.Node` <- function(n1, n2) {
    all(n1$coordinate == n2$coordinate)
}

`==.Edge` <- function(e1, e2) {
    e1$leftnode == e2$leftnode && e1$rightnode == e2$rightnode
}

`<.Edge` <- function(e1, e2) {
    e1$leftnode < e2$leftnode
}

`%between%` <- function(x, a) x >= a[1] & x <= a[2]

#' @export
bind_cell <- function(table, row_range = NULL, col_range = NULL, cancel = NULL, ...) {
    oldattrs <- attr(table, "merged_cells")
    old_merged_cell_names <- names(oldattrs)

    if (is.null(row_range) && is.null(col_range) && is.null(cancel)) {
        cat(old_merged_cell_names, sep = "\n")
        return(invisible(table))
    }
    
    if (!is.null(cancel)) {
        old_merged_cell_names <- setdiff(old_merged_cell_names, cancel)
        oldattrs <- oldattrs[old_merged_cell_names]
        if (is.null(row_range) || is.null(col_range)) {
            data.table::setattr(table, "merged_cells", oldattrs)
            return(invisible(table))
        }
    }
    
    if (is.null(row_range) != is.null(col_range)) {
        stop("Need to set both row_range and col_range", call. = FALSE)
    }

    merged_cell <- valid_merged_cell(list(rows = row_range,
                                          cols = col_range),
                                     table)
    merged_cell <- c(merged_cell, list(...))
    merged_cell_name <-
        merged_cell |>
        purrr::map_chr(\(x) paste(unique(x), collapse = ":")) |>
        paste(collapse = ",")

    purrr::walk(oldattrs, \(m1, m2) {
        if (is_overlaped(m1$rows, m2$row) && is_overlaped(m1$cols, m2$cols))
            stop("There is overlap", call. = FALSE)
    }, m2 = merged_cell)

    newattrs <- c(oldattrs, list(merged_cell))
    names(newattrs) <- c(old_merged_cell_names, merged_cell_name)
    data.table::setattr(table, "merged_cells", newattrs)
    return(invisible(table))
}

cal_column_width <- function(x) {
        stringr::str_trim(x) |>
        stringr::str_width() |>
        max()
}

Cell <- function(table, i, j) {
    stopifnot(inherits(table, "GridTable"))
    stopifnot(length(i) == 1 && length(j) == 1)
    i <- toInteger(i)
    j <- toInteger(j)
    drop_content <- FALSE
    middle <- FALSE

    in_merged_cell <- function(i, j, m) i %between% m$rows && j %between% m$cols
    first_cell_in_merged_cell <- function(i, j, m) i == m$rows[1] && j == m$cols[1]


    for (merged_cell in attr(table, "merged_cells")) {
        if (first_cell_in_merged_cell(i, j, merged_cell)) {
            i <- merged_cell$rows[1]:merged_cell$rows[2]
            j <- merged_cell$cols[1]:merged_cell$cols[2]
            drop_content <- merged_cell$drop_content
            middle <- merged_cell$middle
            break
        } else if (in_merged_cell(i, j, merged_cell)) {
            return(NULL)
        }
    }

    width     <- attr(table, "width")
    colnums   <- sum(width[j] + 3) + 1
    rownums   <- length(i) * 2 + 1
    start_row <- 2 * i[1] - 1
    end_row   <- start_row + rownums - 1
    start_col <- if (j[1] == 1) 1 else sum(width[1:(j[1] - 1)] + 3) + 1
    end_col   <- start_col + colnums - 1
    align     <- attr(table, "align")[j[1]]

    content  <- 
        if (isTRUE(drop_content)) {
            table[i[1], j[1]]           |>
            stringr::str_split("\n")         |>
            unlist()
        } else {
            table[i, j]   |>
            as.matrix() |>
            apply(2, paste, collapse = "\n") |>
            stringr::str_split("\n")         |>
            unlist()
        }
    if (length(content) > rownums - 2)
        content[rownums - 2] <- paste(content[(rownums - 2):length(content)],
                                      collapse = " ")
    if (isTRUE(middle)) {
        content <- c(rep("", (rownums - 2 - length(content)) / 2), content)
    }
    if (max(purrr::map_int(content, stringr::str_width)) > colnums - 4) {
        stop(gettextf("Width of column %s is too small",
                      paste(j, collapse = ",")), call. = FALSE)
    }

    edges <- purrr::map(start_row:end_row, \(rowno) {
        isBoundary   <- rowno %in% c(start_row, end_row)
        isHeaderLine <- rowno == 2 *  attr(table, "header") + 1 && rowno == end_row
        isFooterLine <- (rowno == 2 * attr(table, "footer") - 1 && rowno == start_row) ||
                        (rowno == 2 * attr(table, "footer") + 1 && rowno == end_row)
        node_symbol  <- if (isBoundary) SYMBOL$VERTICE else SYMBOL$SIDE
        leftnode     <- Node(Coordinate(rowno, start_col), node_symbol)
        rightnode    <- Node(Coordinate(rowno, end_col),   node_symbol)
        edge_content <-
            if      (isHeaderLine) SYMBOL$HEADER
            else if (isFooterLine) SYMBOL$FOOTER
            else if (isBoundary)   SYMBOL$LINE
            else                   purrr::pluck(content, rowno - start_row, .default = "") 
        Edge(leftnode, rightnode, edge_content, align)
    })
    structure(edges, start = start_row, end = end_row, class = "Cell")
}


column_start_end_points <-  function (line, sep = " ") {
    line_width <- stringr::str_width(line)
    sep_p <- which(stringr::str_split(line, "")[[1]] == sep)
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
col_no <- function(x) {
    switch(class(x),
        Coordinate = x[2],
        Node       = col_no(x$coordinate),
        Edge       = c(col_no(x$leftnode), col_no(x$rightnode)),
        list       = purrr::map_int(x, col_no),
        default    = stop("wrong object")
    )
}


Coordinate <- function(x, y) {
    structure(c(toInteger(x), toInteger(y)), class = "Coordinate")
}

Edge <- function(leftnode, rightnode, content, align = NULL) {
    stopifnot(class(leftnode)  == "Node" &&
              class(rightnode) == "Node" &&
              is.character(content) &&
              length(content) == 1 &&
              (is.null(align) || isTRUE(align %in% c("l", "r", "c"))) &&
              row_no(leftnode) == row_no(rightnode) &&
              col_no(leftnode) <  col_no(rightnode))
    type <- "Normal"
    symbol <- NULL
    if (leftnode$symbol != SYMBOL$SIDE && rightnode$symbol != SYMBOL$SIDE) {
        symbol <- local({
            cont <- stringr::str_trim(content)
            unique(stringr::str_split(cont, "")[[1]])
        })
        stopifnot(stringr::str_width(symbol) == 1)
        type <- unlist(purrr::imap(SYMBOL, ~ if (.x == symbol) .y else NULL))
    } else if (!grepl("\\S", content, perl = TRUE)) {
        type <- "Empty"
        symbol <- " "
    }

    structure(list(leftnode  = leftnode,
                   rightnode = rightnode,
                   content   = content,
                   align     = align,
                   type      = type,
                   symbol    = symbol),
              class = "Edge")
}

edge_left_extend <- function(e1, e2) {
    updated_node <- node_merge(e1$rightnode, e2$leftnode)
    return(list(edge_update(e1, rightnode = updated_node),
                edge_update(e2, leftnode = updated_node)))
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
            c(list(excess_part), edge_merge(overlap_part, e2))
        },
        OVERLAP_LEFT = {
            left_part    <- edge_update(e1, rightnode = e2$leftnode)
            overlap_p1   <- edge_update(e1, leftnode  = e2$leftnode)
            overlap_p2   <- edge_update(e2, rightnode = e1$rightnode)
            right_part   <- edge_update(e2, leftnode  = e1$rightnode)
            c(list(excess_part), edge_merge(overlap_p1, eoverlap_p2),
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

edge_relation <- function(e1, e2) {
    stopifnot(inherits(e1, "Edge") && inherits(e2, "Edge"))
    stopifnot(row_no(e1) == row_no(e2))
    range_relation(col_no(e1), col_no(e2))
}
edge_update <- function(e, ...) {
    attrs <- list(...)
    for (i in seq_along(attrs)) {
        e[[names(attrs)[i]]] <- attrs[[i]]
    }
    e
}

empty_row <- function(row_no, left_col_no = NULL, right_col_no = NULL) {
    stopifnot(is.null(left_col_no) == is.null(right_col_no))
    if (is.null(left_col_no) || is.null(right_col_no)) {
        return(structure(list(nodes = NULL,
                              edges = NULL,
                              n = 0,
                              row_no = row_no),
                              class = "Row"))
    }
    leftnode  <- Node(Coordinate(row_no, left_col_no),  SYMBOL$SIDE)
    rightnode <- Node(Coordinate(row_no, right_col_no), SYMBOL$SIDE)
    Row(Edge(leftnode, rightnode, " "))
}

format_column <- function(x, digits = 3L, ...) {
    stopifnot(is.atomic(x))
    if (is.character(x)) return(stringr::str_trim(x))
    if (is.integer(x))   return(as.character(x))

    max_origin_digits <-
        stringr::str_split(as.character(x), "\\.")  |>
        purrr::map_chr(\(r) purrr::pluck(r, 2, .default = "")) |>
        purrr::map_int(nchar) |>
        max()

    if (max_origin_digits == 0L) return(as.character(x))

    lbs::stformat(x, digits = min(digits, max_origin_digits)) |>
    stringr::str_trim()
}

#' @export
GridTable <- function(data, align = NULL,
                      header = NULL, footer = NULL, ...) {
    if (!inherits(data, "data.frame")) data <- as.data.frame(data)
    args <- list(...)

    align <- valid_align(data, align)
    data <- vapply(data, format_column, character(nrow(data)))
    if (is.null(header) || header == 0L) {
        data <- rbind(colnames(data), data)
        header <- 1L
    }
    if (is.null(footer)) footer <- Inf
    width <- vapply(as.data.frame(data), cal_column_width, integer(1))
    names(width) <- NULL
    data <- data.table::as.data.table(data)
    class(data) <- c("GridTable", class(data))
    data.table::setattr(data, "width", width)
    data.table::setattr(data, "align", align)
    data.table::setattr(data, "header", header)
    data.table::setattr(data, "footer", footer)
    purrr::iwalk(args, \(x, y) data.table::setattr(data, y, x))
    data
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

is_overlaped <- function(x, y) {
    stopifnot(length(x) == 2 && length(y) == 2)
    get <- function(x, i) if (is.atomic(x))        x[i]     else x[[i]] 
    min <- function(z)    if (get(z,1) < get(z,2)) get(z,1) else get(z,2)
    max <- function(z)    if (get(z,1) < get(z,2)) get(z,2) else get(z,1)
    !(min(x) > max(y) || min(y) > max(x))
}

last_node <- function(row) {
    if (row$n == 0) return(NULL)
    else            return(row$nodes[[row$n]])
}

merge_cell_list <- function(cells) {
    cells <- cells[!purrr::map_lgl(cells, is.null)]
    purrr::walk(cells, \(c) stopifnot(inherits(c, "Cell")))

    longest <- max(purrr::map_int(cells, ~ attr(.x, "end")))
    new_rows <- purrr::map(seq_len(longest), \(i) {
        edges <- purrr::map(cells, \(c) {
            start <- attr(c, "start")
            end   <- attr(c, "end")
            if (i %in% start:end) c[[i - start + 1]] else NULL
        })
        edges <- edges[!purrr::map_lgl(edges, is.null)]
        do.call(Row, c(integrate_edge_list(edges), row_no = i))
    })
    do.call(Table, new_rows)
}

Node <- function(coordinate, symbol = "+") {
    stopifnot(class(coordinate) == "Coordinate" && is.character(symbol))
    structure(list(coordinate = coordinate,
                   symbol = symbol),
              class = "Node")
}


node_merge <- function(n1, n2) {
    if (is.null(n1)) return(n2)
    if (is.null(n2)) return(n1)
    stopifnot(n1 == n2)
    if (n2$symbol == SYMBOL$VERTICE) return(n2)
    return(n1)
}

node_list_merge <- function(nodes1, nodes2) {
    stopifnot(do.call(row_no_allequal, c(nodes1, nodes2)))
    col_nos1 <- col_no(nodes1)
    col_nos2 <- col_no(nodes2)
    col_nos <- sort(unique(col_nos1, col2_nos1))
    purrr::map(col_nos, \(i) {
        node_merge(get_node_where(nodes1, i), get_node_where(nodes2, i))
    })
}


parse_number_adjust <- function(num, x) {
    stopifnot(is.character(x))
    if (!is.atomic(x)) {
        x <- purrr::imap_chr(x, \(val,name) gettextf("%s%s", name, val))
    }
    x <- stringr::str_to_upper(x)

    if (length(x) > 1) {
        return(parse_number_adjust(parse_number_adjust(num, x[1]), x[-1]))
    }

    elements <- stringr::str_match(x, "^([A-Z]+)([-+*/=]?)([0-9]+)")[1, ]
    if (is.na(elements[1])) return(NULL) else elements <- elements[2:4]

    index <- which(LETTERS == stringr::str_split(elements[1], "")[[1]]) |> sum()
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

#' @export
print.GridTable <- function(gtable, drop_empty_line = TRUE, ...) {
    content <- toString(gtable, ...)
    cat(content, sep = "\n")
    invisible(content)
}

#' @export
row_no <- function(x) {
    switch(class(x),
        Coordinate = x[1],
        Node       = row_no(x$coordinate),
        Edge       = row_no(x$leftnode),
        Row        = x$row_no,
        default    = stop("wrong object")
    )
}

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

row_no_allequal <- function(...) {
    args <- list(...)
    row_nos <- purrr::map(args, row_no) |> unlist() |> unique()
    length(row_nos) == 1L
}

Row <- function(..., row_no = NULL) {
    edges <- list(...)
    stopifnot(!(length(edges) == 0L && is.null(row_no)))
    if (length(edges) == 0L) return(empty_row(row_no))

    purrr::walk(edges, ~ stopifnot(class(.x) == "Edge"))
    edges <- local({
        edge_no <- purrr::map_int(edges, ~ .x$leftnode$coordinate[2])
        stopifnot(anyDuplicated(edge_no) == 0)
        purrr::map(seq_along(edges), ~ edges[[which(order(edge_no) == .x)]])
    })

    nodes <- vector("list", length(edges) + 1)
    for (i in seq_along(edges)) {
        if (i != 1) {
            stopifnot(edges[[i]]$leftnode == edges[[i-1]]$rightnode)
            edges[[i-1]]$rightnode <- node_merge(edges[[i]]$leftnode, edges[[i-1]]$rightnode)
            edges[[i]]$leftnode    <- edges[[i-1]]$rightnode
        }
        if (i == length(edges)) nodes[[i+1]] = edges[[i]]$rightnode
        nodes[[i]] = edges[[i]]$leftnode
    }
    
    structure(list(nodes = nodes,
                   edges = edges,
                   n = length(nodes),
                   row_no = row_no(edges[[1]])), class = "Row")
}


#' @export
set_attr <- function(table, attr, value) {
    stopifnot(inherits(table, "GridTable"))
    switch(attr,
        align = data.table::setattr(table, "align", valid_align(table, value)),
        width = {
            if (is.numeric(value)) {
                stopifnot(length(value) != length(table))
                width <- value
            } else {
                width <- parse_number_adjust(attr(table, "width"), value)
            }
            data.table::setattr(table, "width", width)
        },
        default = data.table::setattr(table, attr, value)
    )
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

sort_edge_list <- function(edge_list) {
    edge_list <- edge_list[!purrr::map_lgl(edge_list, is.null)]
    lcol_nos <- purrr::map_int(edge_list, \(e) col_no(e$leftnode))
    rcol_nos <- purrr::map_int(edge_list, \(e) col_no(e$rightnode))
    index <- lcol_nos * max(rcol_nos) + rcol_nos
    edge_list[order(index)]
}

#' @export
simple_to_grid <- function(kbl, ...) {
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
        table   <- if (grepl("[^\\|\\s]", kbl[1], perl = TRUE)) kbl[-2] else kbl[-(1:2)]
        sep     <-  "|"
    } else if (grepl("[^\\-\\s]", kbl[length(kbl)], perl = TRUE)) {
        sepline <- kbl[2]
        table   <- kbl[-2]
    } else {
        sepline <- kbl[1]
        table   <- kbl[-c(1, length(kbl))]
    }

    start_end_points <- column_start_end_points(sepline, sep)
    stopifnot(!is.null(start_end_points))
    data <- purrr::map(table, \(line) {
        purrr::map_chr(start_end_points, \(x) substr_width(line, x[1], x[2]))
    })
    data <- do.call(rbind, data)

    args <- list(...)
    args$data <- data
    if (is.null(args$header)) args$header = 1L
    if (is.null(args$caption)) args$caption = caption
    do.call(GridTable, args)
}

substr_width <- function(x, start, end) {
    width     <- end - start + 1
    pre_width <- start - 1

    pre       <- substr(x, 1, start - 1)
    while (start >= 0 && stringr::str_width(pre) > pre_width) {
        start <- start - 1
        end   <- end - 1
        pre   <- substr(pre, 1, start - 1)
        if (stringr::str_width(pre) < pre_width) {
            stop("Start point cut character", call. = FALSE)
        }
    }

    subs <- substr(x, start, end)
    end  <- width
    while (end >= 0 && stringr::str_width(subs) > width) {
        end <- end - 1
        subs <- substr(subs, 1, end)
        if (stringr::str_width(subs) < width) {
            stop("End point cut character", call. = FALSE)
        }
    }

    subs
}

toInteger <- function(x) {
    stopifnot(!grepl(".", as.character(x + 0), fixed = TRUE))
    as.integer(x)
}

Table <- function(...) {
    rows    <- list(...)
    row_nos <- purrr::map_int(rows, "row_no")
    stopifnot(anyDuplicated(row_nos) == 0)

    length  <- max(row_nos)
    width   <- rows |>
               purrr::map_int(\(x) if (x$n == 0) 0 else col_no(last_node(x))) |>
               max()
    rows    <- purrr::map(seq_len(length), ~ {
        which_row <- which(row_nos == .x)
        if (length(which_row) != 0)   rows[[which_row]]
        else                          Row(row_no = .x)
        
    })
    structure(list(rows = rows, length = length, width = width),
              class = "Table")
}


toString.Edge <- function(edge) {
    symbol_l <- if (col_no(edge$leftnode) == 1L) edge$leftnode$symbol else ""
    symbol_r <- edge$rightnode$symbol
    width    <- col_no(edge$rightnode) - col_no(edge$leftnode) - 1
    if (edge$type[1] == "Normal") {
        content   <- stringr::str_trim(edge$content)
        space_num <- width - 1 - stringr::str_width(content)
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
        edge_symbol_num <- width - sum(stringr::str_width(align_symbol))
        return(gettextf("%s%s%s%s%s", symbol_l, align_symbol[1],
                       strrep(edge$symbol, edge_symbol_num),
                       align_symbol[2], symbol_r))
    }
    return(gettextf("%s%s%s", symbol_l, strrep(edge$symbol, width), symbol_r))
}

toString.Row <- function(row) {
    paste0(purrr::map_chr(row$edges, toString.Edge), collapse = "")
}

#' @export
toString.GridTable <- function(gtable, ...) {
    rownum <- nrow(gtable)
    colnum <- ncol(gtable)

    cells <- purrr::map(seq_len(rownum), \(i) {
        purrr::map(seq_len(colnum), \(j) Cell(gtable, i, j))
    }) 
    cells <- do.call(c, cells)  
    Table <- merge_cell_list(cells)

    # for (i in seq_len(rownum)) {
    #     for (j in seq_len(colnum)) {
    #         if (i == 1 && j == 1) next
    #         Table <- Table + Cell(gtable, i, j)
    #     }
    # }

    content <- toString(Table, ...)
    if (!is.null(attr(gtable, "caption"))) {
        content <- c(attr(gtable, "caption"), "", content)
    }

    structure(content, class = "GridTable_output")
}

toString.Table <- function(table, drop_empty_line = TRUE, ...) {
    table_content <- purrr::map_chr(table$rows, ~ toString.Row(.x))
    if (isTRUE(drop_empty_line)) {
        table_content <- table_content[grepl("[^|\\s]", table_content, perl = TRUE)]
    }
    table_content
}


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
        if (length(align) == 1) 
            align <- rep(strsplit(align, "")[[1]], length(data))
        stopifnot(all(align %in% c("l", "r", "c")))
    } else {
        stop("align needed to be a character or character vector", call. = FALSE)
    }
    align
}

valid_merged_cell <- function(merged_cell, gridtable) {
    stopifnot(inherits(merged_cell, "list"))
    stopifnot(all(c("rows", "cols") %in% names(merged_cell)))

    merged_cell <-
        purrr::map(merged_cell[c("rows", "cols")], \(x) {
            x <- toInteger(x)
            m <- min(x, na.rm = TRUE)
            M <- max(x, na.rm = TRUE)
            if (length(x) > 2) stopifnot(identical(sort(x), m:M))
            c(m, M)
        })
    stopifnot(all(merged_cell$rows %in% 1:nrow(gridtable)))
    stopifnot(all(merged_cell$cols %in% 1:ncol(gridtable)))

    if (merged_cell$rows[1] < attr(gridtable, "header") &&
        merged_cell$rows[2] > attr(gridtable, "header")) {
        stop("Cannot Span header row", call. = FALSE)
    }

    merged_cell
}

# vim: set fdm=expr:
