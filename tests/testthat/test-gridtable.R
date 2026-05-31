# End-to-end rendering snapshots for GridTable.
#
# The package's whole job is to draw correct ASCII grid tables, so the fully
# rendered output is captured with expect_snapshot() (see _snaps/gridtable.md).
# A visual regression anywhere in the layout / cell-merge / render pipeline
# therefore fails loudly here. The unit tests for the individual subsystems live
# in test-utils / test-geometry / test-merge-engine / test-cell / test-format /
# test-api, mirroring the R/ source layout.

test_that("a basic table renders with header line and numeric formatting", {
  dt <- data.table::data.table(Name = c("Alice", "Bob"), Score = c(91.5, 88))
  expect_snapshot(print(GridTable(dt)))
})

test_that("CJK (wide) columns stay aligned", {
  dt <- data.table::data.table(
    "城市" = c("北京", "上海"),   # 城市 / 北京,上海
    "人口" = c(2189, 2487)                          # 人口
  )
  expect_snapshot(print(GridTable(dt, align = "lr")))
})

test_that("merge_cells spans a header cell across columns", {
  m <- rbind(
    c("Location",   "Temperature", "Temperature"),
    c("",           "min",         "max"),
    c("Antarctica", "-89.2",       "19.8"),
    c("Earth",      "-50.1",       "56.7")
  )
  gt <- GridTable(m, header = 2, align = "lrr") |>
    merge_cells(1, c(2, 3))
  expect_snapshot(print(gt))
})
