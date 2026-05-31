#' @keywords internal
"_PACKAGE"

# Import the symbols used internally. All call sites still use explicit
# `data.table::` / `purrr::` prefixes; these imports exist so the package's
# namespace declares its dependencies (clearing the "Imports not imported from"
# check note) and, crucially, so importing from data.table marks this package as
# "data.table-aware" (`cedta()`), keeping `[`/`[<-` on the GridTable data.table
# in their data.table semantics now that data.table is in Imports, not Depends.
#' @importFrom data.table as.data.table setattr
#' @importFrom purrr imap imap_chr iwalk map map_chr map_int map_lgl pluck walk
NULL
