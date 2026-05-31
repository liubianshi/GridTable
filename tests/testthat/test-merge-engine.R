# Unit tests for the edge-merge core in R/merge-engine.R. range_relation is the
# truth table that classifies how two spans relate; edge_merge resolves each
# case. Both are exercised exhaustively here because the whole layout engine
# rests on them, and several branches never fire on the common rendering path.

test_that("range_relation classifies the full span-relation matrix", {
  expect_equal(range_relation(c(1, 5),  c(7, 9)),  "LEFT")
  expect_equal(range_relation(c(1, 5),  c(5, 9)),  "ADJACENT_LEFT")
  expect_equal(range_relation(c(1, 9),  c(5, 12)), "OVERLAP_LEFT")
  expect_equal(range_relation(c(1, 9),  c(1, 12)), "IN_SAME_START")
  expect_equal(range_relation(c(5, 9),  c(1, 12)), "In")
  expect_equal(range_relation(c(1, 12), c(5, 12)), "CONTAIN_SAME_END")
  expect_equal(range_relation(c(1, 9),  c(1, 9)),  "EQUAL")
  expect_equal(range_relation(c(5, 12), c(1, 12)), "IN_SAME_END")
  expect_equal(range_relation(c(1, 9),  c(3, 5)),  "CONTAIN")
  expect_equal(range_relation(c(1, 9),  c(1, 5)),  "CONTAIN_SAME_START")
  expect_equal(range_relation(c(3, 12), c(1, 9)),  "OVERLAP_RIGHT")
  expect_equal(range_relation(c(5, 9),  c(1, 5)),  "ADJACENT_RIGHT")
  expect_equal(range_relation(c(7, 9),  c(1, 5)),  "RIGHT")
})

# V() / S() (vertice / side nodes) and all_edges() come from helper-geometry.R.

test_that("edge_relation delegates to range_relation on edge spans", {
  expect_equal(edge_relation(Edge(V(1), V(9), "-"), Edge(V(1), V(5), "-")),
               "CONTAIN_SAME_START")
})

test_that("edge_merge LEFT inserts an Empty gap edge between the two", {
  merged <- edge_merge(Edge(V(1), V(5), "-"), Edge(V(7), V(9), "-"))
  expect_length(merged, 3)
  expect_equal(merged[[2]]$type[1], "Empty")   # the synthesized gap
  expect_true(all_edges(merged))
})

test_that("edge_merge ADJACENT_LEFT fuses the touching boundary", {
  merged <- edge_merge(Edge(V(1), V(5), "-"), Edge(V(5), V(9), "-"))
  expect_length(merged, 2)
  expect_true(all_edges(merged))
})

test_that("edge_merge IN_SAME_START returns Edge segments", {
  merged <- edge_merge(Edge(V(1), V(9), "-"), Edge(V(1), V(12), "-"))
  expect_length(merged, 2)
  expect_true(all_edges(merged))
})

test_that("edge_merge CONTAIN_SAME_END returns Edge segments", {
  merged <- edge_merge(Edge(V(1), V(12), "-"), Edge(V(5), V(12), "-"))
  expect_length(merged, 2)
  expect_true(all_edges(merged))
})

test_that("edge_merge OVERLAP_LEFT returns Edge segments", {
  merged <- edge_merge(Edge(V(1), V(9), "-"), Edge(V(5), V(12), "-"))
  expect_length(merged, 3)
  expect_true(all_edges(merged))
})

test_that("edge_merge CONTAIN splits into left / overlap / right", {
  merged <- edge_merge(Edge(V(1), V(12), "-"), Edge(V(4), V(8), "-"))
  expect_length(merged, 3)
  expect_true(all_edges(merged))
})

test_that("edge_merge CONTAIN_SAME_START returns Edge segments", {
  merged <- edge_merge(Edge(V(1), V(12), "-"), Edge(V(1), V(8), "-"))
  expect_length(merged, 2)
  expect_true(all_edges(merged))
})

test_that("edge_merge EQUAL collapses two identical borders into one", {
  merged <- edge_merge(Edge(V(1), V(9), "-"), Edge(V(1), V(9), "-"))
  expect_length(merged, 1)
  expect_equal(merged[[1]]$type[[1]], "LINE")
})

test_that("edge_merge EQUAL refuses to overlap two content cells", {
  expect_error(edge_merge(Edge(S(1), S(9), "x"), Edge(S(1), S(9), "y")),
               "Content Overlapping")
})

test_that("edge_merge is order-insensitive (reorders by left node)", {
  a <- edge_merge(Edge(V(1), V(5), "-"), Edge(V(5), V(9), "-"))
  b <- edge_merge(Edge(V(5), V(9), "-"), Edge(V(1), V(5), "-"))
  expect_equal(length(a), length(b))
})

test_that("sort_edge_list orders edges left-to-right and drops NULLs", {
  e_left  <- Edge(V(1), V(5), "-")
  e_right <- Edge(V(5), V(9), "-")
  sorted <- sort_edge_list(list(e_right, NULL, e_left))
  expect_length(sorted, 2)
  expect_true(sorted[[1]] == e_left)
})
