# Unit tests for the local-decision renderer and the GridTable -> core adapter
# (R/render.R). The end-to-end rendered output is pinned in test-snapshot.R;
# here we check the smaller pieces: padding/alignment, a couple of border-rule
# decisions, and the input reshaping done by grid_inputs() (footer-gap mapping,
# merge folding, drop_content, the middle matrix).

test_that("pad honours alignment and keeps >=1 leading space", {
  expect_equal(pad("ab", 6, "l"), " ab   ")
  expect_equal(pad("ab", 6, "r"), "   ab ")
  expect_equal(pad("ab", 6, "c"), "  ab  ")
  expect_equal(pad("", 4, "l"),   "    ")    # empty cell is all spaces
  expect_equal(nchar(pad("xyz", 5, "r")), 5) # never overflows the field
})

test_that("border_line draws a header '=' rule with alignment colons", {
  occ  <- build_occupancy(2, 2, list())
  line <- border_line(occ, gap = 1, field = c(3L, 3L), align = c("l", "r"),
                      rule_gaps = 1L)
  expect_equal(line, "+:==+==:+")
})

test_that("border_line draws a plain '-' rule away from header/footer", {
  occ  <- build_occupancy(2, 2, list())
  line <- border_line(occ, gap = 0, field = c(3L, 3L), align = c("l", "l"),
                      rule_gaps = 1L)
  expect_equal(line, "+---+---+")
})

test_that("border_line: a rowspan crosses the rule as blank, not a '-' run", {
  occ  <- build_occupancy(2, 1, list(c(1, 1, 2, 1)))   # col1 spans rows 1-2
  line <- border_line(occ, gap = 1, field = 5L, align = "l",
                      rule_gaps = integer(0))
  expect_equal(line, "+     +")    # no horizontal rule where the cell continues
})

test_that("grid_inputs maps header and footer indices to unified rule gaps", {
  gt   <- GridTable(data.frame(a = c("x", "y", "s"), b = c(1, 2, 3)), footer = 3)
  args <- grid_inputs(gt)
  # header = 1 -> gap 1; footer = N draws a rule above row N (gap N-1) and along
  # the table bottom (gap nrow). All fold into a single rule_gaps set.
  expect_setequal(args$rule_gaps, c(1L, 2L, nrow(gt)))
})

test_that("grid_inputs folds drop_content to the anchor's own cell", {
  m  <- matrix(c("H1", "H2", "A", "B", "C", "D"), ncol = 2, byrow = TRUE)
  gt <- GridTable(m, header = 1)
  merge_cells(gt, i = 2:3, j = 1, drop_content = TRUE)
  args <- grid_inputs(gt)
  expect_equal(args$content[2, 1], "A")   # anchor, not the data.table dt[i,j] trap
  expect_equal(args$content[3, 1], "")    # covered position cleared
})

test_that("grid_inputs joins a merged region's members when not dropping", {
  m  <- matrix(c("H1", "H2", "A", "B", "C", "D"), ncol = 2, byrow = TRUE)
  gt <- GridTable(m, header = 1)
  merge_cells(gt, i = 2:3, j = 1)         # default drop_content = FALSE
  args <- grid_inputs(gt)
  expect_equal(args$content[2, 1], "A\nC")
})

test_that("grid_inputs records the middle flag on the anchor only", {
  gt <- GridTable(data.frame(a = c("x", "y", "z"), b = c(1, 2, 3)))
  merge_cells(gt, i = 2:3, j = 1, middle = TRUE)
  args <- grid_inputs(gt)
  expect_true(args$middle[2, 1])
  expect_false(args$middle[3, 1])
})

test_that("grid_inputs drops blank / &nbsp; lines from a cell", {
  gt   <- GridTable(data.frame(a = c("one\n&nbsp;\ntwo", "z")))
  args <- grid_inputs(gt)
  expect_equal(args$content[2, 1], "one\ntwo")
})
