# Unit tests for the formatting / validation helpers in R/format.R: column
# formatting, the number formatter, the width/height adjustment DSL parser, and
# the input validators. These encode the trickiest non-geometry logic.

test_that("format_column dispatches on column type", {
  expect_equal(format_column(c(" a ", "b ")), c("a", "b"))     # char: trimmed
  expect_equal(format_column(c(1L, 2L)),      c("1", "2"))     # integer
  expect_equal(format_column(c(1, 2, 3)),     c("1", "2", "3")) # no decimals
  expect_equal(format_column(c(1.5, 22.25)),  c("1.500", "22.250")) # decimals
})

test_that("format_one_num formats a large number with a thousands separator", {
  # Pins the large-number branch that the bit.mark->big.mark / fomart->format
  # fixes restored: the exact output proves both the separator and the rounding.
  expect_equal(trimws(format_one_num(1234567.89, digits = 3)), "1,234,568")
})

test_that("format_one_num handles NA and integers", {
  expect_equal(format_one_num(NA_real_, digits = 3), "")
  expect_equal(format_one_num(NA_real_, digits = 3, na.replace = "-"), "-")
  expect_equal(trimws(format_one_num(5L, digits = 3)), "5")
})

test_that("parse_number_adjust applies each operator at a numeric index", {
  base <- c(10L, 20L)
  expect_equal(parse_number_adjust(base, "2+1"), c(10L, 21L))
  expect_equal(parse_number_adjust(base, "2-1"), c(10L, 19L))
  expect_equal(parse_number_adjust(base, "1*3"), c(30L, 20L))
  expect_equal(parse_number_adjust(base, "2/2"), c(10L, 10L))
  expect_equal(parse_number_adjust(base, "2=9"), c(10L,  9L))
})

test_that("parse_number_adjust accepts letter indices and chains adjustments", {
  expect_equal(parse_number_adjust(c(10L, 20L), "B+5"), c(10L, 25L))  # B = col 2
  expect_equal(parse_number_adjust(c(10L, 20L), c("A+1", "B+2")),
               c(11L, 22L))
})

test_that("parse_number_adjust returns NULL for unparseable input", {
  expect_null(parse_number_adjust(c(10L, 20L), "!!!"))
  expect_null(parse_number_adjust(c(10L, 20L), "B"))   # no operand
})

test_that("cal_column_width measures the widest (display) cell, ignoring NA", {
  expect_equal(cal_column_width(c("ab", "abcd", NA)), 4L)
  expect_equal(cal_column_width(c("北京", "x")), 4L)   # CJK width
})

test_that("height_of counts the tallest multiline cell per row", {
  df <- data.table::data.table(a = c("x", "y\nz"), b = c("p\nq\nr", "s"))
  expect_equal(height_of(df), c(3L, 2L))
  expect_equal(height_of(df, base = 4L), c(4L, 4L))  # floor at base
})

test_that("valid_align defaults by column type and expands a single code", {
  expect_equal(unname(valid_align(data.table::data.table(a = "x", b = 1.0))),
               c("l", "r"))
  expect_equal(valid_align(data.table::data.table(a = 1, b = 2, c = 3), "c"),
               c("c", "c", "c"))
  expect_equal(valid_align(data.table::data.table(a = 1, b = 2), "lr"),
               c("l", "r"))
  expect_error(valid_align(data.table::data.table(a = 1), 99))
})

test_that("valid_merged_cell rejects a region straddling the header line", {
  # header = 2: the separator sits below row 2, so spanning rows 1:3 crosses it.
  gt <- GridTable(rbind(c("h1", "h2"), c("s1", "s2"), c("a", "b")), header = 2)
  expect_error(valid_merged_cell(c(1, 3), c(1, 1), gt), "header")
  ok <- valid_merged_cell(c(2, 3), c(1, 1), gt)
  expect_equal(ok$rows, c(2, 3))
})
