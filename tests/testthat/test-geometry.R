# Unit tests for the geometry primitives in R/geometry.R: Coordinate / Node /
# Edge / Row / Table, their accessors, and the S3 comparison operators. The
# operators are the focus of robustness fix #1 — they must dispatch when
# GridTable is loaded as a real package, not just when source()'d.

test_that("row_no / col_no read coordinates off a Node", {
  n <- Node(Coordinate(2L, 7L), "+")
  expect_equal(row_no(n), 2L)
  expect_equal(col_no(n), 7L)
})

test_that("col_no on an Edge returns both endpoints", {
  e <- Edge(Node(Coordinate(1L, 1L), "+"), Node(Coordinate(1L, 9L), "+"), "-")
  expect_equal(col_no(e), c(1L, 9L))
})

test_that("Node comparison operators dispatch in the package environment", {
  n1 <- Node(Coordinate(1L, 1L), "+")
  n2 <- Node(Coordinate(1L, 5L), "+")
  expect_true(n1 < n2)
  expect_false(n1 > n2)
  expect_false(n1 == n2)
  expect_true(n1 == Node(Coordinate(1L, 1L), "+"))
  # Comparing nodes on different rows is undefined and must error.
  expect_error(n1 < Node(Coordinate(2L, 5L), "+"))
})

test_that("Edge comparison operators dispatch in the package environment", {
  e1 <- Edge(Node(Coordinate(1L, 1L), "+"), Node(Coordinate(1L, 5L), "+"), "-")
  e2 <- Edge(Node(Coordinate(1L, 5L), "+"), Node(Coordinate(1L, 9L), "+"), "-")
  expect_true(e1 == e1)
  expect_false(e1 == e2)
  expect_true(e1 < e2)            # ordered by left node
})

test_that("node_merge keeps the vertice over a side regardless of order", {
  side <- Node(Coordinate(1L, 3L), "|")
  vert <- Node(Coordinate(1L, 3L), "+")
  expect_equal(node_merge(side, vert)$symbol, "+")
  expect_equal(node_merge(vert, side)$symbol, "+")
  expect_equal(node_merge(NULL, side)$symbol, "|")  # NULL is identity
})

test_that("Edge constructor classifies content into a type", {
  expect_equal(Edge(S(1), S(9), "hi")$type[1], "Normal")
  expect_equal(Edge(S(1), S(9), "  ")$type[1], "Empty")
  expect_equal(Edge(V(1), V(9), "-")$type[[1]], "LINE")
  expect_equal(Edge(V(1), V(9), "=")$type[["HEADER"]], "HEADER")
})

test_that("edge_update overwrites only the named fields", {
  e  <- Edge(Node(Coordinate(1L, 1L), "+"), Node(Coordinate(1L, 9L), "+"), "-")
  e2 <- edge_update(e, align = "c")
  expect_equal(e2$align, "c")
  expect_equal(e2$content, "-")  # untouched
})

test_that("Row stitches edges and shares the boundary node", {
  row <- Row(Edge(S(1), S(5), " "), Edge(S(5), S(9), " "))
  expect_equal(row$n, 3)              # 2 edges -> 3 nodes
  expect_equal(row_no(row), 1L)
  expect_true(row_no_allequal(S(1), S(5)))
})

test_that("Table pads missing rows and reports its width", {
  tbl <- Table(Row(Edge(S(1, 1), S(9, 1), " ")),    # row 1, cols 1..9
               Row(Edge(S(1, 3), S(9, 3), " ")))    # row 3, cols 1..9
  expect_equal(tbl$length, 3)          # row 2 was padded in
  expect_equal(tbl$width, 9L)
  expect_equal(tbl$rows[[2]]$n, 0)     # padded row is empty
})

test_that("empty_row and last_node handle the degenerate row", {
  er <- empty_row(4L)
  expect_equal(er$n, 0)
  expect_null(last_node(er))
})
