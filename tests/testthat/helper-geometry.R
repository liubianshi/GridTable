# Shared constructors for the geometry / merge-engine unit tests. testthat
# auto-sources helper-*.R before every test file, so these are available across
# the whole suite without being re-declared per file.

# A vertice ("+") / side ("|") Node at the given column. Row defaults to 1 (the
# common single-row case); pass it explicitly as the second argument when a
# multi-row layout is needed, e.g. S(col = 9, row = 3).
V <- function(col, row = 1L) Node(Coordinate(row, col), "+")
S <- function(col, row = 1L) Node(Coordinate(row, col), "|")

# TRUE iff every element of x is an Edge.
all_edges <- function(x) all(vapply(x, inherits, logical(1), "Edge"))
