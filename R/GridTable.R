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

`+.Row` <- function(r1, r2) {
    if (r1$row_no != r2$row_no) stop("rows must have same row number", call. = FALSE)
    if (r1$n == 0)                     return (r2)
    if (r2$n == 0)                     return (r1)
    if (r1$nodes[[1]] > r2$nodes[[1]]) return (`+.Row`(r2, r1))
    if (r1$nodes[[r1$n]] < r2$nodes[[1]]) {
        stop("must be adjacent or overlapping", call. = FALSE)
    }
    for (edge in r2$edges) {
        r1 <- row_update(r1, edge)
    }
    r1
}

`+.Table` <- function(t1, t2) {
    if (is.null(t1)) return(t2)
    if (is.null(t2)) return(t1)
    if (t1$length < t2$length) {
        return(`+.Table`(t2, t1))
    }
    new_rows <- purrr::map(
        seq_len(t1$length),
        \(.x) {
            if (.x <= t2$length) t1$rows[[.x]] + t2$rows[[.x]]
            else                 t1$rows[[.x]]
        }
    )
    do.call(Table, new_rows)
}

`%between%` <- function(x, a) x >= a[1] & x <= a[2]

#' @export
bind_cell <- function(table, row_range = NULL, col_range = NULL, cancel = NULL) {
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

Cell <- function(table, i, j, drop_content = FALSE) {
    stopifnot(inherits(table, "GridTable"))
    stopifnot(length(i) == 1 && length(j) == 1)

    in_merged_cell <- function(i, j, m) i %between% m$rows && j %between% m$cols
    first_cell_in_merged_cell <- function(i, j, m) i == m$rows[1] && j == m$cols[1]

    i <- toInteger(i)
    j <- toInteger(j)

    for (merged_cell in attr(table, "merged_cells")) {
        if (first_cell_in_merged_cell(i, j, merged_cell)) {
            i <- merged_cell$rows[1]:merged_cell$rows[2]
            j <- merged_cell$cols[1]:merged_cell$cols[2]
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
            table[i, j, drop = FALSE]   |>
            apply(2, paste, collapse = "\n") |>
            stringr::str_split("\n")         |>
            unlist()
        }
    if (length(content) > rownums - 2)
        content[rownums - 2] <- paste(content[(rownums - 2):length(content)],
                                      collapse = " ")
    if (max(purrr::map_int(content, stringr::str_width)) > colnums - 4) {
        stop(gettextf("Width of column %s is too small",
                      paste(j, collapse = ",")), call. = FALSE)
    }

    rows <- purrr::map(start_row:end_row, \(rowno) {
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
        Row(Edge(leftnode, rightnode, edge_content, align))
    })
    do.call(Table, rows)
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

col_no <- function(x) {
    switch(class(x),
        Coordinate = x[2],
        Node       = col_no(x$coordinate),
        Edge       = c(col_no(x$leftnode), col_no(x$rightnode)),
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

edge_update <- function(e, ...) {
    attrs <- list(...)
    for (i in seq_along(attrs)) {
        e[[names(attrs)[i]]] <- attrs[[i]]
    }
    e
}

edge_split <- function(edge, node, align_l = edge$align,
                                   align_r = edge$align) {
    if (is.null(edge$symbol)) stop("Normal edge cannot be split")
    list(left  = edge_update(edge, rightnode = node, align = align_l),
         right = edge_update(edge, leftnode  = node, align = align_r))
}

edge_left_extend <- function(e1, e2) {
    updated_node <- node_merge(e1$rightnode, e2$leftnode)
    return(list(left  = edge_update(e1, rightnode = updated_node),
                right = edge_update(e2, leftnode = updated_node)))
}

edge_left_merge <- function(e1, e2) {
    # not overlap
    if (e1$rightnode <  e2$leftnode)  return(list(e1))
    if (e1$leftnode  >  e2$rightnode) return(list(e1))
    if (e1$rightnode == e2$leftnode)  {
        updated_node <- node_merge(e1$rightnode, e2$leftnode)
        return(list(edge_update(e1, rightnode = updated_node)))
    }
    if (e1$leftnode  == e2$rightnode) {
        updated_node <- node_merge(e1$leftnode, e2$rightnode)
        return(list(edge_update(e1, leftnode = updated_node)))
    }

    # overlapping
    if (e1$leftnode == e2$leftnode) {
        e1$leftnode  <- node_merge(e1$leftnode, e2$leftnode)
    }
    if (e1$rightnode == e2$rightnode) {
        e1$rightnode <- node_merge(e1$rightnode, e2$rightnode)
    }
    if (e1$leftnode < e2$leftnode) {
        excess_part  <- edge_update(e1, rightnode = e2$leftnode)
        overlap_part <- edge_update(e1, leftnode = e2$leftnode, align = e2$align)
        return(c(list(excess_part), edge_left_merge(overlap_part, e2)))
    }
    if (e1$rightnode > e2$rightnode) {
        overlap_part <- edge_update(e1, rightnode = e2$rightnode, align = e2$align)
        excess_part  <- edge_update(e1, leftnode = e2$rightnode)
        return(c(edge_left_merge(overlap_part, e2), list(excess_part)))
    }

    if ((e1$type[1] == "Normal" && e2$type[1] != "Empty") ||
        (e1$type[1] != "Empty"  && e2$type[1] == "Normal")) {
        stop("Content Overlapping", call. = FALSE)
    } 

    if (e1$type[1] == "Empty" || sum(c("HEADER", "FOOTER") %in% e2$type)) {
        e1 <- edge_update(e1, content = e2$content, type = e2$type, symbol = e2$symbol)
    }
    return(list(e1))
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

#' @export
GridTable <- function(data, align = NULL, header = NULL, footer = NULL) {
    if (!inherits(data, "data.frame")) data <- as.data.frame(data)

    align <- valid_align(data, align)
    data <- vapply(data, format_column, character(nrow(data)))
    if (is.null(header) || header == 0L) {
        data <- rbind(colnames(data), data)
        header <- 1L
    }
    if (is.null(footer)) footer <- Inf
    width <- vapply(as.data.frame(data), cal_column_width, integer(1))
    structure(data,
              width  = width,
              align  = align,
              header = header,
              footer = footer,
              class  = "GridTable")
}

last_edge <- function(row) {
    if (row$n == 0) return(NULL)
    else            return(row$edges[[row$n - 1]])
}

last_node <- function(row) {
    if (row$n == 0) return(NULL)
    else            return(row$nodes[[row$n]])
}

Node <- function(coordinate, symbol = "+") {
    stopifnot(class(coordinate) == "Coordinate" && is.character(symbol))
    structure(list(coordinate = coordinate,
                   symbol = symbol),
              class = "Node")
}

node_merge <- function(n1, n2) {
    stopifnot(n1 == n2)
    if (n2$symbol == SYMBOL$VERTICE) return(n2)
    return(n1)
}

is_overlaped <- function(x, y) {
    stopifnot(length(x) == 2 && length(y) == 2)
    get <- function(x, i) if (is.atomic(x))        x[i]     else x[[i]] 
    min <- function(z)    if (get(z,1) < get(z,2)) get(z,1) else get(z,2)
    max <- function(z)    if (get(z,1) < get(z,2)) get(z,2) else get(z,1)
    !(min(x) > max(y) || min(y) > max(x))
}

#' @export
print.GridTable <- function(gtable, drop_content = FALSE) {
    rownum <- nrow(gtable)
    colnum <- ncol(gtable)
    purrr::map(seq_len(rownum), \(i) {
        purrr::map(seq_len(colnum), \(j) Cell(gtable, i, j, drop_content = FALSE)) |>
        purrr::reduce(`+.Table`)
    }) |>
    purrr::reduce(`+.Table`)
}

#' @export
print.Table <- function(table) {
    purrr::walk(table$rows, ~  cat(toString(.x), "\n"))
}

row_extend <- function(row, edge) {
    stopifnot(last_node(row) == edge$leftnode)
    row$nodes[[row$n]]     <- node_merge(row$nodes[[row$n]], edge$leftnode)
    row$nodes[[row$n + 1]] <- edge$rightnode
    row$edges[[row$n]]     <- edge_update(edge, leftnode = row$nodes[[row$n]])
    row$n                  <- row$n + 1
    return(row)
}

row_no <- function(x) {
    switch(class(x),
        Coordinate = x[1],
        Node       = row_no(x$coordinate),
        Edge       = row_no(x$leftnode),
        default    = stop("wrong object")
    )
}

row_update <- function(row, edge) {
    if (edge$leftnode > last_node(row)) {
        stop("must be adjacent or overlapping", call. = FALSE)
    }
    if (edge$leftnode == last_node(row)) {
        return(row_extend(row, edge))
    }

    if (edge$rightnode > last_node(row)) {
        edges <- edge_split(edge, last_node(row),
                            align_l = last_edge(row)$align)
        return(row_extend(row_update(row, edges$left), edges$right))
    }

    new_row_edges <- local({
        edges_list_of_list <- purrr::map(row$edges, ~ edge_left_merge(.x, edge))
        do.call(c, edges_list_of_list)
    })

    do.call(Row, new_row_edges)
}

Row <- function(..., row_no = NULL) {
    edges <- list(...)
    stopifnot(!(length(edges) == 0L && is.null(row_no)))
    if (length(edges) == 0L) {
        empty_row <- structure(list(nodes = NULL,
                                    edges = NULL,
                                    n = 0,
                                    row_no = row_no),
                               class = "Row")
        return(empty_row)
    }

    purrr::walk(edges, ~ stopifnot(class(.x) == "Edge"))
    edges <- local({
        edge_no <- purrr::map_int(edges, ~ .x$leftnode$coordinate[2])
        stopifnot(anyDuplicated(edge_no) == 0)
        purrr::map(seq_along(edges), ~ edges[[which(order(edge_no) == .x)]])
    })

    nodes <- local({
        n <- length(edges)
        l <- purrr::map(edges, ~ .x$leftnode)
        r <- purrr::map(edges, ~ .x$rightnode)
        purrr::map2(l[-1], r[-n], ~ stopifnot(.x == .y && .x$symbol == .y$symbol))
        c(l, r[n])
    })
    
    structure(list(nodes = nodes,
                   edges = edges,
                   n = length(nodes),
                   row_no = row_no(edges[[1]])), class = "Row")
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

toInteger <- function(x) {
    stopifnot(!grepl(".", as.character(x + 0), fixed = TRUE))
    as.integer(x)
}

toString.Edge <- function(edge) {
    symbol_l <- if (col_no(edge$leftnode) == 1L) edge$leftnode$symbol else ""
    symbol_r <- edge$rightnode$symbol
    width <- col_no(edge$rightnode) - col_no(edge$leftnode) - 1
    if (edge$type[1] == "Normal") {
        content <- stringr::str_trim(edge$content)
        return(gettextf(gettextf("%%s %%-%ds%%s", width - 1),
                        symbol_l, content, symbol_r))
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



# vim: set fdm=expr:
