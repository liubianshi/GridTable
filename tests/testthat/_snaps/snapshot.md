# simple table: header rule + numeric formatting

    Code
      print(GridTable(df))
    Output
      +-------+--------+
      | name  |  score |
      +:======+=======:+
      | Alice | 91.500 |
      +-------+--------+
      | Bob   | 88.000 |
      +-------+--------+

# per-column alignment (lcr)

    Code
      print(GridTable(df, align = "lcr"))
    Output
      +----+---+---+
      | a  | b | c |
      +:===+:=:+==:+
      | x  | p | m |
      +----+---+---+
      | yy | q | n |
      +----+---+---+

# CJK (wide) columns stay aligned, mixed alignment

    Code
      print(GridTable(df, align = "lr"))
    Output
      +------+--------+
      | a    |      b |
      +:=====+=======:+
      | 中文 |  1.500 |
      +------+--------+
      | x    | 22.000 |
      +------+--------+

# colspan header spanning two columns

    Code
      print(gt)
    Output
      +------------+--------------+
      | Location   |         Temp |
      |            |         Temp |
      +------------+-------+------+
      | city       |   min |  max |
      +:===========+======:+=====:+
      | Antarctica | -89.2 | 19.8 |
      +------------+-------+------+

# rowspan with vertical centring (middle)

    Code
      print(gt)
    Output
      +---+---+
      | a | b |
      +:==+==:+
      | x | 1 |
      +   +---+
      | y | 2 |
      +---+---+
      | z | 3 |
      +---+---+

# row + column span together

    Code
      print(gt)
    Output
      +-----+-----+----+
      | H1  | H2  | H3 |
      +:====+:====+:===+
      | BIG       | x  |
      | BIG       |    |
      +           +----+
      | BIG       | y  |
      | BIG       |    |
      +-----------+----+

# merged region keeping only the anchor (drop_content)

    Code
      print(gt)
    Output
      +----+----+
      | H1 | H2 |
      +:===+:===+
      | A  | B  |
      +    +----+
      |    | D  |
      +----+----+

# footer separator line

    Code
      print(GridTable(df, footer = 3))
    Output
      +-----+---+
      | a   | b |
      +:====+==:+
      | x   | 1 |
      +:====+==:+
      | y   | 2 |
      +-----+---+
      | sum | 3 |
      +:====+==:+

# caption is rendered above the table

    Code
      print(GridTable(df, caption = "Table: demo"))
    Output
      Table: demo
      
      +---+---+
      | a | b |
      +:==+==:+
      | x | 1 |
      +---+---+

# multi-line cell grows the row height

    Code
      print(GridTable(df))
    Output
      +-------------+---+
      | a           | b |
      +:============+:==+
      | one         | p |
      | two         |   |
      | three       |   |
      +-------------+---+
      | z           | q |
      +-------------+---+

# wrap option marks continuation lines with backslashes

    Code
      print(gt)
    Output
      +----------------------------+----+
      | H1                         | H2 |
      +:===========================+:===+
      | first line\                | y  |
      | second line\               |    |
      | third                      |    |
      +----------------------------+----+

# kable_to_grid reverse-parses a pipe kable

    Code
      print(kable_to_grid(kbl))
    Output
      +---------+------+
      | City    | Pop  |
      +:========+:=====+
      | Beijing | 2189 |
      +---------+------+
      | Xian    | 1295 |
      +---------+------+

# set_attr width DSL widens one column

    Code
      print(gt)
    Output
      +------+---+
      | a    | b |
      +:=====+==:+
      | x    | 1 |
      +------+---+
      | y    | 2 |
      +------+---+

