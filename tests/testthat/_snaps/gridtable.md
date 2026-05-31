# a basic table renders with header line and numeric formatting

    Code
      print(GridTable(dt))
    Output
      +-------+--------+
      | Name  |  Score |
      +:======+=======:+
      | Alice | 91.500 |
      +-------+--------+
      | Bob   | 88.000 |
      +-------+--------+

# CJK (wide) columns stay aligned

    Code
      print(GridTable(dt, align = "lr"))
    Output
      +------+------+
      | 城市 | 人口 |
      +:=====+=====:+
      | 北京 | 2189 |
      +------+------+
      | 上海 | 2487 |
      +------+------+

# merge_cells spans a header cell across columns

    Code
      print(gt)
    Output
      +------------+---------------------------+
      | Location   |               Temperature |
      |            |               Temperature |
      +------------+-------------+-------------+
      |            |         min |         max |
      +:===========+============:+============:+
      | Antarctica |       -89.2 |        19.8 |
      +------------+-------------+-------------+
      | Earth      |       -50.1 |        56.7 |
      +------------+-------------+-------------+

