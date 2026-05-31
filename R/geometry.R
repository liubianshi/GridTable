#' Construct a character-grid coordinate
#'
#' A `(row, col)` pair indexing the **character grid** (not logical table
#' cells). Both components are coerced to whole integers.
#'
#' @param x,y Integer-valued row and column positions.
#' @return A length-2 integer vector of class `Coordinate`.
#' @noRd
Coordinate <- function(x, y) {
    structure(c(toInteger(x), toInteger(y)), class = "Coordinate")
}

#' Construct a grid node
#'
#' A point on the character grid carrying a drawing symbol: `+` for a vertex or
#' `|` for a side.
#'
#' @param coordinate A `Coordinate`.
#' @param symbol The node symbol, usually `SYMBOL$VERTICE` or `SYMBOL$SIDE`.
#' @return A `Node` object.
#' @noRd
Node <- function(coordinate, symbol = "+") {
    stopifnot(class(coordinate) == "Coordinate" && is.character(symbol))
    structure(list(coordinate = coordinate,
                   symbol = symbol),
              class = "Node")
}

#' Construct a grid edge
#'
#' A horizontal segment between two nodes on the **same** row. It carries the
#' span's `content`, `align`, and a derived `type` (`Normal`/`Empty`/`HEADER`/
#' `FOOTER`/`LINE`). Whitespace-only content resolves to `Empty` before any
#' border-symbol derivation; border edges (bounded by vertices/sides) derive
#' their `type`/`symbol` from the single drawing character in `content`.
#'
#' @param leftnode,rightnode `Node`s bounding the segment; left must precede
#'   right and share its row.
#' @param content A length-1 string: the cell text or a border run.
#' @param align Optional alignment code (`l`/`r`/`c`).
#' @return An `Edge` object.
#' @noRd
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

#' Construct a grid row
#'
#' An ordered set of edges plus the nodes shared between adjacent edges. Edges
#' are sorted by column; adjacent edges must meet (left node of one equals right
#' node of the previous), and their shared nodes are merged via `node_merge()`.
#'
#' @param ... `Edge` objects (none if building an empty row by `row_no`).
#' @param row_no Row number; required when no edges are supplied.
#' @return A `Row` object.
#' @noRd
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

#' Assemble a grid table from rows
#'
#' A list of `Row`s padded to the full height: missing row numbers are filled
#' with empty rows so the table spans `1:max(row_no)`. Records the overall
#' character `length` (rows) and `width` (columns).
#'
#' @param ... `Row` objects with unique `row_no`s.
#' @return A `Table` object.
#' @noRd
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

#' Construct an empty row
#'
#' Either a node-less placeholder row (when no column bounds are given) or a row
#' holding a single whitespace edge spanning the given columns.
#'
#' @param row_no Row number.
#' @param left_col_no,right_col_no Optional column bounds for a spanning empty
#'   edge; both or neither.
#' @return A `Row` object.
#' @noRd
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

#' Last (right-most) node of a row
#'
#' @param row A `Row`.
#' @return The right-most `Node`, or `NULL` for an empty row.
#' @noRd
last_node <- function(row) {
    if (row$n == 0) return(NULL)
    else            return(row$nodes[[row$n]])
}

#' Merge two coincident nodes
#'
#' Collapse two nodes sharing a coordinate into one, preferring a vertex (`+`)
#' over a side (`|`). `NULL` operands pass through.
#'
#' @param n1,n2 `Node`s (or `NULL`); must share a coordinate when both present.
#' @return A single `Node`.
#' @noRd
node_merge <- function(n1, n2) {
    if (is.null(n1)) return(n2)
    if (is.null(n2)) return(n1)
    stopifnot(n1 == n2)
    if (n2$symbol == SYMBOL$VERTICE) return(n2)
    return(n1)
}

#' Return an edge with some fields replaced
#'
#' Functional update: copy `e` and overwrite the named list elements (e.g.
#' `leftnode`, `rightnode`, `content`, `align`, `type`, `symbol`).
#'
#' @param e An `Edge`.
#' @param ... Named fields to overwrite.
#' @return The updated `Edge`.
#' @noRd
edge_update <- function(e, ...) {
    attrs <- list(...)
    for (i in seq_along(attrs)) {
        e[[names(attrs)[i]]] <- attrs[[i]]
    }
    e
}

#' Join two adjacent edges at their shared node
#'
#' Merge the right node of `e1` with the left node of `e2` and write the merged
#' node back into both, so the pair shares a single boundary node.
#'
#' @param e1,e2 Adjacent `Edge`s (right node of `e1` coincides with left of `e2`).
#' @return A length-2 list of the two updated edges.
#' @noRd
edge_left_extend <- function(e1, e2) {
    updated_node <- node_merge(e1$rightnode, e2$leftnode)
    return(list(edge_update(e1, rightnode = updated_node),
                edge_update(e2, leftnode = updated_node)))
}

#' Comparison Operators for Grid Nodes and Edges
#'
#' S3 comparison operators defined so that `Node` and `Edge` objects can be
#' ordered and compared inside the edge-merge algorithm (see `edge_merge()` and
#' `integrate_edge_list()`). Nodes compare by column number within the same row;
#' edges compare by their left node. Equality compares grid coordinates.
#'
#' @param n1,n2 `Node` objects (must share a row for `<`/`>`).
#' @param e1,e2 `Edge` objects.
#'
#' @return A logical scalar.
#'
#' @rdname grid-compare-operators
#' @keywords internal
#' @export
`<.Node` <- function(n1, n2) {
    stopifnot(row_no(n1) == row_no(n2))
    col_no(n1) < col_no(n2)
}

#' @rdname grid-compare-operators
#' @keywords internal
#' @export
`>.Node` <- function(n1, n2) {
    stopifnot(row_no(n1) == row_no(n2))
    col_no(n1) > col_no(n2)
}

#' @rdname grid-compare-operators
#' @keywords internal
#' @export
`==.Node` <- function(n1, n2) {
    all(n1$coordinate == n2$coordinate)
}

#' @rdname grid-compare-operators
#' @keywords internal
#' @export
`==.Edge` <- function(e1, e2) {
    e1$leftnode == e2$leftnode && e1$rightnode == e2$rightnode
}

#' @rdname grid-compare-operators
#' @keywords internal
#' @export
`<.Edge` <- function(e1, e2) {
    e1$leftnode < e2$leftnode
}

#' Row Number of a Grid Geometry Object
#'
#' Extract the character-grid row number of a geometry object. It dispatches on
#' the object's class: a `Coordinate`'s first element, a `Node`/`Edge`'s
#' coordinate, or a `Row`'s stored `row_no`.
#'
#' @param x A `Coordinate`, `Node`, `Edge`, or `Row` object.
#'
#' @return An integer row number.
#'
#' @examples
#' # The geometry classes are internal; reach one to demonstrate dispatch.
#' co <- GridTable:::Coordinate(3, 1)
#' row_no(co)
#'
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

#' Column number(s) of a grid geometry object
#'
#' Companion to [row_no()]: a `Coordinate`'s column, a `Node`'s column, an
#' `Edge`'s `c(left, right)` columns, or the column of each element of a list.
#'
#' @param x A `Coordinate`, `Node`, `Edge`, or list thereof.
#' @return An integer (or integer vector) column number.
#' @noRd
col_no <- function(x) {
    switch(class(x),
        Coordinate = x[2],
        Node       = col_no(x$coordinate),
        Edge       = c(col_no(x$leftnode), col_no(x$rightnode)),
        list       = purrr::map_int(x, col_no),
        default    = stop("wrong object")
    )
}

#' Do all objects lie on the same row?
#'
#' @param ... Geometry objects accepted by [row_no()].
#' @return `TRUE` if every object shares one row number.
#' @noRd
row_no_allequal <- function(...) {
    args <- list(...)
    row_nos <- purrr::map(args, row_no) |> unlist() |> unique()
    length(row_nos) == 1L
}
