# Tests for the exported constructors / mutators in R/api.R. GridTable's
# attribute wiring and the set_attr / merge_cells DSLs are checked with ordinary
# assertions; kable_to_grid (a whole exported constructor that was previously
# untested) is pinned with a snapshot of its reverse-parsed output.

test_that("GridTable attaches the render-state attributes", {
  gt <- GridTable(data.table::data.table(a = c("x", "y"), b = c(1.5, 2.0)))
  expect_s3_class(gt, "GridTable")
  expect_equal(unname(attr(gt, "align")), c("l", "r"))
  expect_equal(attr(gt, "header"), 1L)        # auto header row prepended
  expect_equal(nrow(gt), 3)                    # colnames became row 1
  expect_equal(length(attr(gt, "width")), 2)
  expect_true(is.infinite(attr(gt, "footer"))) # default footer = Inf
})

test_that("GridTable stores extra attributes passed via ...", {
  gt <- GridTable(data.table::data.table(a = "x"), caption = "Table: T1")
  expect_equal(attr(gt, "caption"), "Table: T1")
})

test_that("set_attr width DSL adjusts one column by letter index", {
  gt <- GridTable(data.table::data.table(a = "x", b = "y"))
  before <- attr(gt, "width")[2]
  set_attr(gt, width = "B+3")          # column B (= 2): width + 3
  expect_equal(attr(gt, "width")[2], before + 3L)
})

test_that("set_attr replaces align through its validator", {
  gt <- GridTable(data.table::data.table(a = "x", b = "y"))
  set_attr(gt, align = "cc")
  expect_equal(attr(gt, "align"), c("c", "c"))
})

test_that("merge_cells registers a named merged region", {
  gt <- GridTable(rbind(c("h1", "h2", "h3"), c("a", "b", "c")), header = 1)
  merge_cells(gt, 1, c(2, 3))
  mc <- attr(gt, "merged_cells")
  expect_named(mc, "1,2:3")
  expect_equal(mc[["1,2:3"]]$rows, c(1, 1))
  expect_equal(mc[["1,2:3"]]$cols, c(2, 3))
})

test_that("merge_cells refuses overlapping regions and cancels by name", {
  gt <- GridTable(rbind(c("h1", "h2", "h3"), c("a", "b", "c")), header = 1)
  merge_cells(gt, 1, c(2, 3))
  expect_error(merge_cells(gt, 1, c(3, 3)), "overlap")
  merge_cells(gt, cancel = "1,2:3")
  expect_length(attr(gt, "merged_cells"), 0)
})

test_that("kable_to_grid reverse-parses a pipe kable", {
  skip_if_not_installed("knitr")
  kbl <- knitr::kable(
    data.frame(City = c("Beijing", "Xian"), Pop = c(2189, 1295)),
    format = "pipe"
  )
  expect_snapshot(print(kable_to_grid(kbl)))
})

test_that("add_footnote stores notes and marks the selected cells", {
  gt <- GridTable(data.frame(a = c("x", "y"), b = c(1, 2)))
  add_footnote(gt, "Clustered at the province level.", ref = "a", i = 1, j = 2)
  expect_equal(attr(gt, "notes"), "^a^ Clustered at the province level.")
  expect_equal(gt[[2]][1], "b^a^")             # header cell got the marker

  add_footnote(gt, "Source: census.")          # no ref -> bare note, no marker
  expect_equal(attr(gt, "notes")[2], "Source: census.")
})

test_that("add_footnote validates its inputs", {
  gt <- GridTable(data.frame(a = "x"))
  expect_error(add_footnote(gt, c("a", "b")), "single character")
  expect_error(add_footnote(gt, "n", i = 1), "both i and j")
  expect_error(add_footnote(gt, "n", i = 1, j = 1), "ref")
  expect_error(add_footnote(gt, "n", ref = "a", i = 99, j = 1))
})
