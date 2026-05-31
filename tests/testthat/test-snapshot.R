# Golden-master rendering snapshots for the GridTable engine.
#
# This file pins the *fully rendered* plain-text output of every representative
# scenario the package supports. It is the regression net for the geometry
# engine: any visual change to the layout / cell-merge / render pipeline shows
# up here as a snapshot diff.
#
# When the engine is deliberately changed (e.g. body content now honours the
# per-column alignment, or width/height allocation numbers shift), review each
# diff by eye and rebaseline with `testthat::snapshot_accept("snapshot")`.
# A *structural* break (misaligned borders, broken header rules) is a bug, not
# a rebaseline.

test_that("simple table: header rule + numeric formatting", {
  df <- data.frame(name = c("Alice", "Bob"), score = c(91.5, 88))
  expect_snapshot(print(GridTable(df)))
})

test_that("per-column alignment (lcr)", {
  df <- data.frame(a = c("x", "yy"), b = c("p", "q"), c = c("m", "n"))
  expect_snapshot(print(GridTable(df, align = "lcr")))
})

test_that("CJK (wide) columns stay aligned, mixed alignment", {
  df <- data.frame(a = c("中文", "x"), b = c(1.5, 22))
  expect_snapshot(print(GridTable(df, align = "lr")))
})

test_that("colspan header spanning two columns", {
  m <- matrix(c("Location",   "Temp", "Temp",
                "city",       "min",  "max",
                "Antarctica", "-89.2", "19.8"),
              ncol = 3, byrow = TRUE)
  gt <- GridTable(m, header = 2, align = "lrr")
  merge_cells(gt, i = 1, j = 2:3)
  expect_snapshot(print(gt))
})

test_that("rowspan with vertical centring (middle)", {
  gt <- GridTable(data.frame(a = c("x", "y", "z"), b = c(1, 2, 3)))
  merge_cells(gt, i = 2:3, j = 1, middle = TRUE)
  expect_snapshot(print(gt))
})

test_that("row + column span together", {
  m <- matrix(c("H1",  "H2",  "H3",
                "BIG", "BIG", "x",
                "BIG", "BIG", "y"),
              ncol = 3, byrow = TRUE)
  gt <- GridTable(m, header = 1)
  merge_cells(gt, i = 2:3, j = 1:2)
  expect_snapshot(print(gt))
})

test_that("merged region keeping only the anchor (drop_content)", {
  m <- matrix(c("H1", "H2", "A", "B", "C", "D"), ncol = 2, byrow = TRUE)
  gt <- GridTable(m, header = 1)
  merge_cells(gt, i = 2:3, j = 1, drop_content = TRUE)
  expect_snapshot(print(gt))
})

test_that("footer separator line", {
  df <- data.frame(a = c("x", "y", "sum"), b = c(1, 2, 3))
  expect_snapshot(print(GridTable(df, footer = 3)))
})

test_that("caption is rendered above the table", {
  df <- data.frame(a = "x", b = 1)
  expect_snapshot(print(GridTable(df, caption = "Table: demo")))
})

test_that("multi-line cell grows the row height", {
  df <- data.frame(a = c("one\ntwo\nthree", "z"), b = c("p", "q"))
  expect_snapshot(print(GridTable(df)))
})

test_that("wrap option marks continuation lines with backslashes", {
  m <- matrix(c("H1",                              "H2",
                "first line\nsecond line\nthird",  "y"),
              ncol = 2, byrow = TRUE)
  gt <- GridTable(m, header = 1)
  merge_cells(gt, i = 2, j = 1, wrap = TRUE)
  expect_snapshot(print(gt))
})

test_that("kable_to_grid reverse-parses a pipe kable", {
  skip_if_not_installed("knitr")
  kbl <- knitr::kable(
    data.frame(City = c("Beijing", "Xian"), Pop = c(2189, 1295)),
    format = "pipe"
  )
  expect_snapshot(print(kable_to_grid(kbl)))
})

test_that("set_attr width DSL widens one column", {
  gt <- GridTable(data.frame(a = c("x", "y"), b = c(1, 2)))
  set_attr(gt, "width", "1+3")
  expect_snapshot(print(gt))
})
