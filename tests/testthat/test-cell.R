# Tests for the cell-layout layer in R/cell.R. These exercise the table->cell
# coordinate translation and, crucially, the auto-fit retry mechanism: when a
# cell's content is too tall or too wide, cell_content() mutates the table's
# height/width attribute in place and stop()s so toString.GridTable can re-run.

test_that("cell_merge_info reports a plain cell as un-merged", {
  gt <- GridTable(data.table::data.table(a = c("x", "y"), b = c("p", "q")))
  info <- cell_merge_info(gt, 2, 1)
  expect_false(info$merged)
  expect_equal(info$i, 2L)
})

test_that("cell_merge_info flags the anchor and members of a merged region", {
  gt <- GridTable(rbind(c("h1", "h2", "h3"), c("a", "b", "c")), header = 1) |>
    merge_cells(1, c(2, 3))
  anchor <- cell_merge_info(gt, 1, 2)
  member <- cell_merge_info(gt, 1, 3)
  expect_true(anchor$merged)
  expect_true(anchor$first_cell)
  expect_true(member$merged)
  expect_false(member$first_cell)
  expect_equal(anchor$j, c(2, 3))   # expanded to the full span
})

test_that("cell_position_info marks the header separator line", {
  gt <- GridTable(data.table::data.table(a = c("x", "y"), b = c("p", "q")))
  pos <- cell_position_info(gt, 1, 1)
  expect_true(pos$row$isHeaderLine$end)    # header rule under row 1
  expect_false(pos$row$isHeaderLine$start)
})

test_that("cell_content grows the row height when content overflows", {
  gt <- GridTable(data.table::data.table(a = "one\ntwo\nthree"))
  data.table::setattr(gt, "height", c(1L, 1L))   # force the data row too short
  expect_error(cell_content(gt, 2, 1), "Adjust the height")
  expect_gt(attr(gt, "height")[2], 1L)            # mutated upward in place
})

test_that("cell_content grows the column width when content is too wide", {
  gt <- GridTable(data.table::data.table(a = "hello"))
  data.table::setattr(gt, "width", 2L)            # force the column too narrow
  expect_error(cell_content(gt, 2, 1), "Adjust the width")
  expect_gt(attr(gt, "width")[1], 2L)             # mutated wider in place
})

test_that("get_cells_from yields one Cell slot per table coordinate", {
  gt <- GridTable(data.table::data.table(a = c("x", "y"), b = c("p", "q")))
  cells <- get_cells_from(gt)
  expect_length(cells, nrow(gt) * ncol(gt))       # 3 rows x 2 cols
  non_null <- cells[!vapply(cells, is.null, logical(1))]
  expect_true(all(vapply(non_null, inherits, logical(1), "Cell")))
})
