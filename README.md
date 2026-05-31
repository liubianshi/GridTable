<!-- README.md — hand-written. The rendered tables below are real
     `devtools::load_all()` output pasted verbatim; re-run the examples and
     refresh them if you change the rendering engine. -->

# GridTable

Render a `data.frame`, `data.table`, or `knitr::kable` into a plain-text
**pandoc grid table** — the ASCII boxes drawn with `+ | - = :` that pandoc
understands as a real table.

The layout engine is **CJK-width aware** (it measures text with
`nchar(type = "width")`), so tables mixing Chinese / wide and narrow characters
stay aligned. It supports merged cells (colspan, rowspan, and both at once),
header / footer separator lines, per-column alignment, content wrapping,
vertical centring, and automatic column-width / row-height growth so content
always fits.

## Installation

```r
# install.packages("remotes")
remotes::install_github("liubianshi/GridTable")
```

## Quick start

```r
library(GridTable)

df <- data.frame(name = c("Alice", "Bob"), score = c(91.5, 88))
print(GridTable(df))
```

```
+-------+--------+
| name  |  score |
+:======+=======:+
| Alice | 91.500 |
+-------+--------+
| Bob   | 88.000 |
+-------+--------+
```

A few things happen automatically: the **column names** become the first row
with a header rule (`=`) under them, the numeric `score` column is formatted to
a fixed number of decimals, and every column gets a sensible width.

A `GridTable` is just a `data.table` carrying its render state as attributes.
You build it once, tweak it **by reference**, and `print()` it (or call
`toString()` to get the lines as a character vector).

## Feature tour

### Per-column alignment

Pass `align` as one code per column (`l`eft, `r`ight, `c`entre), or a single
code recycled to all. Numeric columns default to right, character to left. The
alignment governs the **body** text as well as the header rule's colons.

```r
df <- data.frame(item = c("apples", "pie"), qty = c(4, 12), note = c("fresh", "warm"))
print(GridTable(df, align = "lcr"))
```

```
+--------+-----+-------+
| item   | qty |  note |
+:=======+:===:+======:+
| apples |  4  | fresh |
+--------+-----+-------+
| pie    | 12  |  warm |
+--------+-----+-------+
```

### Header and footer rules

`footer` draws a rule above the given row and along the table bottom — handy
for a totals row.

```r
sales <- data.frame(region = c("North", "South", "Total"), amount = c(120, 95, 215))
print(GridTable(sales, footer = 3))
```

```
+--------+--------+
| region | amount |
+:=======+=======:+
| North  |    120 |
+:=======+=======:+
| South  |     95 |
+--------+--------+
| Total  |    215 |
+:=======+=======:+
```

### Merging cells

`merge_cells(tbl, i, j)` registers a rectangular merged region spanning rows `i`
and columns `j`, **by reference**. With the default auto-header the first *data*
row is row 2.

A column span (`drop_content = TRUE` keeps only the anchor cell's text):

```r
m <- matrix(c("Location", "Temp",  "Temp",
              "city",     "min",   "max",
              "Antarctica", "-89.2", "19.8"), ncol = 3, byrow = TRUE)
gt <- GridTable(m, header = 2, align = "lrr")
merge_cells(gt, i = 1, j = 2:3, drop_content = TRUE)
print(gt)
```

```
+------------+--------------+
| Location   |         Temp |
+------------+-------+------+
| city       |   min |  max |
+:===========+======:+=====:+
| Antarctica | -89.2 | 19.8 |
+------------+-------+------+
```

A row span:

```r
gt <- GridTable(data.frame(group = c("A", "A", "B"), value = c(1, 2, 3)))
merge_cells(gt, i = 2:3, j = 1)
print(gt)
```

```
+-------+-------+
| group | value |
+:======+======:+
| A     |     1 |
+       +-------+
| A     |     2 |
+-------+-------+
| B     |     3 |
+-------+-------+
```

`merge_cells()` also takes `middle = TRUE` (vertical centring) and `wrap = TRUE`
(append pandoc continuation `\` to every line but the last). Call
`merge_cells(tbl)` with no `i`/`j` to list regions, or `cancel = TRUE` to remove
one.

### Adjusting widths/heights — the `set_attr` DSL

The engine sizes everything to fit, but you can pin or nudge a column width or
row height. Besides a plain numeric vector, `width`/`height` accept an
adjustment string `<index><op><operand>`, where the index is a number or a
letter (`A` = 1, `B` = 2, …) and the op is one of `+ - * / =`:

```r
gt <- GridTable(data.frame(a = c("x", "y"), b = c(1, 2)))
set_attr(gt, "width", "1+3")   # widen column 1 by three display columns
print(gt)
```

### Starting from a `knitr::kable`

`kable_to_grid()` reverse-parses a pipe- or simple-format kable (recovering the
column boundaries from its separator line) into a `GridTable` you can then
merge, align, or caption.

### CJK width awareness

Because widths are measured in display columns, wide characters count as two and
stay aligned:

```r
df <- data.frame(city = c("北京", "上海", "x"), pop = c(2189, 2487, 5))
print(GridTable(df, align = "lr"))
```

```
+------+------+
| city |  pop |
+:=====+=====:+
| 北京 | 2189 |
+------+------+
| 上海 | 2487 |
+------+------+
| x    |    5 |
+------+------+
```

## API surface

The public surface is small:

| Function | Purpose |
|----------|---------|
| `GridTable()` | Build a table from a `data.frame` / `data.table` / `matrix`. |
| `kable_to_grid()` | Build a table from a `knitr::kable`. |
| `merge_cells()` | Register / cancel merged regions (`drop_content` / `middle` / `wrap`). |
| `set_attr()` | Pin or adjust widths, heights, alignment, captions. |
| `print()` / `toString()` | Render the table. |

See the vignette for the full walkthrough:

```r
vignette("GridTable")
```

## License

MIT © liu.bian.shi. See [LICENSE](LICENSE).
