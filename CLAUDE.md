# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this package does

`GridTable` is an R package that renders a `data.frame`/`data.table` (or a
`knitr::kable`) into a plain-text **pandoc grid table** — ASCII boxes drawn with
`+ | - = :`. It is CJK-width aware (uses `nchar(type = "width")` everywhere), so
tables containing Chinese/wide characters stay aligned. It supports merged cells,
header/footer separator lines, per-column alignment, content wrapping, and
automatic column-width / row-height growth so content always fits.

The entire implementation lives in a **single file**: `R/GridTable.R` (~970
lines). There is no `man/`; the doc comments are only `#' @export` tags consumed
by roxygen2 to regenerate `NAMESPACE`.

## Commands

This is a standard R package layout, but the test harness is currently
aspirational (see Gotchas), so day-to-day work is done interactively.

```r
# Interactive development — load the package functions into the session.
# Most functions (Node, Edge, Row, Table, Cell, edge_merge, ...) are NOT
# exported, so you cannot reach them after library(GridTable). To poke at
# internals, source the file directly:
library(purrr); library(data.table)
source("R/GridTable.R")

# Or use devtools for the package workflow:
devtools::load_all()      # load exported + internal functions
devtools::document()      # regenerate NAMESPACE from #' @export tags
devtools::test()          # run tests/testthat (see Gotchas)
devtools::check()         # full R CMD check
```

```bash
# Package-level build / check from the shell
R CMD INSTALL .
R CMD build .
R CMD check GridTable_*.tar.gz
```

There is no lint config; match the existing style (4-space indent, native pipe
`|>`, `purrr::map*`, explicit `data.table::` / `purrr::` prefixes).

## Architecture

The render pipeline is `toString.GridTable` → cells → merged rows → text. Read
it in this order:

1. **Construction** — `GridTable(data, align, header, footer, ...)`
   coerces to `data.table`, formats every column (`format_column` /
   `format_one_num`), prepends the column names as row 1 when `header` is unset,
   then attaches all render state as attributes via `data.table::setattr`:
   `height`, `width` (per-column display widths), `align` (`l`/`r`/`c`),
   `header` (row index of the header separator), `footer`, plus optional
   `caption` and `merged_cells`. `kable_to_grid()` is the alternate constructor:
   it reverse-parses a kable's separator line to recover column boundaries
   (`column_start_end_points`) and slices each line by display width
   (`substr_width`).

2. **Two coordinate systems** — table coordinates `(i, j)` index logical cells;
   `Coordinate(row, col)` indexes the **character grid**. `cell_position_info`
   converts `(i, j)` into character `start/end` spans using the `height`/`width`
   attributes, and decides whether a span coincides with a header/footer line.

3. **Geometry primitives** (in dependency order):
   - `Node` = a `Coordinate` + a symbol (`+` vertex / `|` side).
   - `Edge` = a horizontal segment between two nodes on the **same** row,
     carrying `content`, `align`, and a `type` (`Normal`/`Empty`/`HEADER`/
     `FOOTER`/`LINE`). Custom S3 operators `<.Node`, `==.Node`, `<.Edge`,
     `==.Edge` let edges/nodes be sorted and compared in the merge algorithm.
   - `Row` = ordered edges + the shared nodes between them.
   - `Table` = a list of `Row`s, padded to full height.
   - `Cell(tbl, i, j)` = the set of edges (top border, content lines, bottom
     border) for one cell's bounding box, already aware of merge/header/footer.

4. **Edge merging — the core algorithm.** `get_cells_from` builds a `Cell` for
   every `(i, j)`; `merge_cell_list` regroups their edges by character row and
   calls `integrate_edge_list`, which repeatedly `edge_merge`s overlapping or
   adjacent edges from neighbouring cells. `range_relation(s1, s2)` classifies
   how two spans relate (`LEFT`, `ADJACENT_LEFT`, `OVERLAP_LEFT`, `CONTAIN`,
   `EQUAL`, `IN_SAME_START`, …) and `edge_merge`'s `switch` resolves each case
   (e.g. shared `+` vertices, header `=` overriding `-`, conflicting content
   raising "Content Overlapping"). This is where shared borders between cells
   collapse into single characters.

5. **Auto-fit retry loop.** While computing a cell, `cell_content` may discover
   the content is too tall or too wide and `stop()`s with "Adjust the height" or
   "Adjust the width" *after* mutating the table's `height`/`width` attribute in
   place. `toString.GridTable` wraps `get_cells_from` in `try()`, catches those
   messages, and re-invokes itself — growing the table until everything fits.
   **Exceptions are used as control flow here; do not "fix" it into a plain
   error path.**

6. **Rendering** — `toString.Table` → `toString.Row` → `toString.Edge` turn the
   merged geometry into character strings; `print.GridTable` just `cat`s them.

## Attribute mutation & the `set_attr` DSL

Because the table is a `data.table`, all state changes go through
`data.table::setattr` (by reference). `set_attr()` / the `...` of `GridTable()`
expose a small adjustment DSL for `width` and `height`: pass a numeric vector to
set absolutely, or a string like `"2/2"`, `"2+1"`, `"b-3"`, `"B/2"` to adjust an
existing value. `parse_number_adjust` reads it as `<index><op><operand>` where
the index may be a number or a letter (`A`=1, `B`=2, …) and the op is one of
`+ - * / =`. `merge_cells()` registers/cancels merged regions (options
`drop_content`, `middle`, `wrap`) as a named list attribute.

## Gotchas

- **Test harness is broken/aspirational.** `tests/testthat.R` calls
  `library(gridtable)` / `test_check("gridtable")` (lowercase) but the package is
  `GridTable`. `tests/testthat/test-element.R` uses the removed testthat 2.x
  `context()`, has **no assertions** (it just runs and prints), and reads an
  `.Rds` from `$NUTSTORE` that does not exist in CI. Treat it as a scratch
  example, not a passing suite. If you add real tests, fix the package-name case
  first and rewrite with `expect_*`.
- **`DESCRIPTION` is still the template** (placeholder Title/Description/Author/
  License). Update it before any real release; the package currently has no
  declared license.
- **Latent bugs in unexercised `edge_merge` branches.** Several `switch` cases
  reference undefined names (`excess_part`, `eoverlap_p2`) or typos
  (`fomart`, `bit.mark`, `col2_nos1` in `node_list_merge`). They don't fire on
  the common paths but will error on more complex merge layouts — fix the branch
  you actually hit rather than assuming the whole algorithm is exercised.
- **Most functions are internal.** Only `GridTable`, `kable_to_grid`,
  `merge_cells`, `row_no`, `set_attr`, `print.GridTable`, `toString.GridTable`
  are exported. Reach the rest via `devtools::load_all()` or `source()`.

## Knowledge graph (this repo)

The user-global `~/CLAUDE.md` asks you to prefer the `code-review-graph` MCP
tools over Grep/Read. For this repo the graph is currently empty — run
`build_or_update_graph_tool` first, or just read `R/GridTable.R` directly (it is
a single self-contained file).
