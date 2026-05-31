# Unit tests for the occupancy-map geometry + forward-pass sizing core
# (R/occupancy.R). These pin the pure-function building blocks the renderer
# stands on: the anchor/last matrices, the same_cell predicate, and the two
# width/height forward passes (including the rowspan content-fit regression
# the reference renderer got wrong).

test_that("a plain grid is its own anchor everywhere with span 1", {
  occ <- build_occupancy(2, 3, list())
  expect_true(occ_is_anchor(occ, 2, 3))
  expect_equal(occ$last_row[1, 1], 1)
  expect_equal(occ$last_col[1, 1], 1)
  expect_length(occ_anchor_cells(occ), 6)   # every cell is an anchor
})

test_that("a merge records anchor + extent and hides covered positions", {
  occ <- build_occupancy(3, 3, list(c(2, 1, 3, 2)))   # rows 2-3 x cols 1-2
  expect_true(occ_is_anchor(occ, 2, 1))               # top-left is the anchor
  expect_false(occ_is_anchor(occ, 2, 2))              # covered
  expect_false(occ_is_anchor(occ, 3, 1))              # covered
  expect_equal(occ$last_row[2, 1], 3)
  expect_equal(occ$last_col[2, 1], 2)
  expect_length(occ_anchor_cells(occ), 6)             # 3 (row1) + 1 (BIG) + 2 (col3)
})

test_that("same_cell is true within a merge, false across cells and off-grid", {
  occ <- build_occupancy(3, 3, list(c(2, 1, 3, 2)))
  expect_true(occ_same_cell(occ, 2, 1, 3, 2))    # both inside BIG
  expect_true(occ_same_cell(occ, 2, 1, 2, 2))
  expect_false(occ_same_cell(occ, 2, 1, 2, 3))   # BIG vs the col-3 cell
  expect_false(occ_same_cell(occ, 1, 1, 2, 1))   # header cell vs BIG
  expect_false(occ_same_cell(occ, 2, 1, 2, 0))   # off-grid is a different cell
  expect_false(occ_same_cell(occ, 2, 1, 4, 1))
})

test_that("compute_field: single cells fit content, CJK counts as width 2", {
  occ     <- build_occupancy(2, 2, list())
  content <- matrix(c("中文", "x", "ab", "cd"), 2, 2, byrow = TRUE)
  # col 1: max(中文 = 4, ab = 2) + 2 padding = 6; col 2: max(x, cd) = 2 + 2 = 4
  expect_equal(compute_field(occ, content, rep(0L, 2)), c(6L, 4L))
})

test_that("compute_field: field_min acts as a floor", {
  occ     <- build_occupancy(1, 2, list())
  content <- matrix(c("x", "y"), 1, 2)
  expect_equal(compute_field(occ, content, c(10L, 0L)), c(10L, 3L))
})

test_that("compute_field: a colspan only widens when it does not fit", {
  occ     <- build_occupancy(2, 2, list(c(1, 1, 1, 2)))  # row1 spans both cols
  content <- matrix(c("a wide spanning title", "", "x", "y"), 2, 2, byrow = TRUE)
  field   <- compute_field(occ, content, rep(0L, 2))
  # the colspan field = field[1] + field[2] + 1 internal border must hold its text
  expect_gte(field[1] + field[2] + 1L, cell_width(content, 1, 1) + 2L)
})

test_that("compute_height: a rowspan grows rows so all lines fit (no dropped line)", {
  occ     <- build_occupancy(2, 2, list(c(1, 1, 2, 1)))   # col1 spans rows 1-2
  content <- matrix(c("a\nb\nc\nd", "x", "", "y"), 2, 2, byrow = TRUE)
  height  <- compute_height(occ, content, rep(0L, 2))
  # 4 content lines must fit on the spanned rows' *text* lines only — the
  # internal border line is NOT usable (that was the reference renderer's bug).
  expect_equal(sum(height[1:2]), 4L)
})

test_that("compute_height: height_min acts as a floor", {
  occ     <- build_occupancy(2, 1, list())
  content <- matrix(c("x", "y"), 2, 1)
  expect_equal(compute_height(occ, content, c(3L, 1L)), c(3L, 1L))
})
