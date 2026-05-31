Coordinate <- function(x, y) {
    structure(c(toInteger(x), toInteger(y)), class = "Coordinate")
}

Node <- function(coordinate, symbol = "+") {
    stopifnot(class(coordinate) == "Coordinate" && is.character(symbol))
    structure(list(coordinate = coordinate,
                   symbol = symbol),
              class = "Node")
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
    if (!grepl("\\S", content, perl = TRUE)) {
        # Whitespace-only content is an empty span (e.g. the gap edge_merge's
        # LEFT branch inserts between two cells). Resolve it to Empty *before*
        # trying to derive a border symbol, otherwise vertice-bounded empty
        # edges crash in the symbol-derivation branch below.
        type <- "Empty"
        symbol <- " "
    } else if (leftnode$symbol != SYMBOL$SIDE && rightnode$symbol != SYMBOL$SIDE) {
        symbol <- local({
            cont <- trimws(content)
            unique(strsplit(cont, "")[[1]])
        })
        stopifnot(str_width(symbol) == 1)
        type <- unlist(purrr::imap(SYMBOL, ~ if (.x == symbol) .y else NULL))
    }

    structure(list(leftnode  = leftnode,
                   rightnode = rightnode,
                   content   = content,
                   align     = align,
                   type      = type,
                   symbol    = symbol),
              class = "Edge")
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

last_node <- function(row) {
    if (row$n == 0) return(NULL)
    else            return(row$nodes[[row$n]])
}

node_merge <- function(n1, n2) {
    if (is.null(n1)) return(n2)
    if (is.null(n2)) return(n1)
    stopifnot(n1 == n2)
    if (n2$symbol == SYMBOL$VERTICE) return(n2)
    return(n1)
}

edge_update <- function(e, ...) {
    attrs <- list(...)
    for (i in seq_along(attrs)) {
        e[[names(attrs)[i]]] <- attrs[[i]]
    }
    e
}

edge_left_extend <- function(e1, e2) {
    updated_node <- node_merge(e1$rightnode, e2$leftnode)
    return(list(edge_update(e1, rightnode = updated_node),
                edge_update(e2, leftnode = updated_node)))
}

#' @export
`<.Node` <- function(n1, n2) {
    stopifnot(row_no(n1) == row_no(n2))
    col_no(n1) < col_no(n2)
}

#' @export
`>.Node` <- function(n1, n2) {
    stopifnot(row_no(n1) == row_no(n2))
    col_no(n1) > col_no(n2)
}

#' @export
`==.Node` <- function(n1, n2) {
    all(n1$coordinate == n2$coordinate)
}

#' @export
`==.Edge` <- function(e1, e2) {
    e1$leftnode == e2$leftnode && e1$rightnode == e2$rightnode
}

#' @export
`<.Edge` <- function(e1, e2) {
    e1$leftnode < e2$leftnode
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

col_no <- function(x) {
    switch(class(x),
        Coordinate = x[2],
        Node       = col_no(x$coordinate),
        Edge       = c(col_no(x$leftnode), col_no(x$rightnode)),
        list       = purrr::map_int(x, col_no),
        default    = stop("wrong object")
    )
}

row_no_allequal <- function(...) {
    args <- list(...)
    row_nos <- purrr::map(args, row_no) |> unlist() |> unique()
    length(row_nos) == 1L
}
