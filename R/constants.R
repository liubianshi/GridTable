# Box-drawing characters used throughout the grid renderer:
#   VERTICE "+"  corner / junction vertex
#   SIDE    "|"  vertical cell border
#   LINE    "-"  ordinary horizontal border
#   HEADER  "="  header separator line
#   FOOTER  "="  footer separator line
#   ALIGN   ":"  alignment marker placed on a header line (`:--`, `--:`, `:-:`)
SYMBOL <- list(VERTICE = "+",
               SIDE    = "|",
               LINE    = "-",
               HEADER  = "=",
               FOOTER  = "=",
               ALIGN   = ":")

# Default options for a merged-cell region (see `merge_cells()`):
#   drop_content  keep only the top-left cell's content
#   middle        vertically centre the content
#   wrap          pandoc line-wrap the content
MERGED_CELL_OPTION <- list(drop_content = FALSE,
                           middle       = FALSE,
                           wrap         = FALSE)
