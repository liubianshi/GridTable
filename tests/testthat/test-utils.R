# Unit tests for the leaf helpers in R/utils.R. These are pure and
# deterministic, and several encode width-aware (CJK) logic with hard error
# paths, so they are worth pinning directly rather than only through rendering.

test_that("minmax returns the min and max as a length-2 vector", {
  expect_equal(minmax(c(3, 1, 2)), c(1, 3))
  expect_equal(minmax(c(5, 5)),    c(5, 5))
})

test_that("toInteger accepts whole numbers and rejects fractionals", {
  expect_identical(toInteger(5),   5L)
  expect_identical(toInteger(2.0), 2L)
  expect_error(toInteger(2.5))
})

test_that("is_overlaped detects span overlap, touching and disjoint", {
  expect_true(is_overlaped(c(1, 5), c(4, 9)))   # overlap
  expect_true(is_overlaped(c(1, 5), c(5, 9)))   # touch at 5
  expect_false(is_overlaped(c(1, 5), c(6, 9)))  # disjoint
})

test_that("shift pops the first element and mutates the caller's binding", {
  x <- list(10, 20, 30)
  first <- shift(x)
  expect_equal(first, 10)
  expect_equal(length(x), 2)        # x was mutated in this frame
  expect_equal(x[[1]], 20)
  expect_null(shift(list()))
})

test_that("str_width measures display width (CJK = 2)", {
  expect_equal(str_width("ab"), 2L)
  expect_equal(str_width("北"), 2L)
  expect_equal(str_width("北京"), 4L)
})

test_that("substr_width slices by display width and respects CJK boundaries", {
  expect_equal(substr_width("北京1234", 1, 4), "北京")  # two wide chars
  expect_equal(substr_width("abcdef",   2, 4), "bcd")
  # Cutting through the middle of a wide character is an error, not a silent
  # half-glyph.
  expect_error(substr_width("北京", 1, 1), "cut character")
})
