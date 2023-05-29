context("Test element")

e11 <- Edge(r11, r12, "-")
e12 <- Edge(r11, r13, "-")
edge_relation(e11, e12)
edge_merge(e11, e12)

r11 <- Node(Coordinate(1L, 1L), "+")
r12 <- Node(Coordinate(1L, 9L), "+")
r13 <- Node(Coordinate(1L, 12L), "+")
r21 <- Node(Coordinate(2L, 1L), "|")
r22 <- Node(Coordinate(2L, 9L), "|")
r31 <- Node(Coordinate(3L, 1L), "+")
r32 <- Node(Coordinate(3L, 9L), "+")
e1  <- Row(Edge(r11, r12, "-", "l"))
e2  <- Row(Edge(r21, r22, "test"))
e3  <- Row(Edge(r31, r32, "-"))
T <- Table(e1, e2, e3)
edge_relation(e1, e2)



dt <- data.table::fread("
Location            | Temperature 1961-1990 ||
                    | in degree Celsius     ||
                    | min   | mean  | max   
Antarctica          | -89.2 | N/A   | 19.8  
Earth               | -89.2 | 14    | 56.7  
", sep = "|")

gdt <- GridTable(dt, header = 3, align = "lrrr") |>
    merge_cells(c(1,3), 1, inline_block = TRUE) |>
    merge_cells(c(1,2), c(2,4), inline_block = TRUE) |>
    merge_cells(c(4,5), 2, drop_content = TRUE, middle = TRUE)

gdt[1, V1 := paste0(V1, "\n", "Test")]
merge_cells(gdt, 1:3, 1, cancel = TRUE)
str(gdt)
merge_cells(gdt)

toString.GridTable(gdt) %>% str()

set_attr(gdt, "width", "2/2")
set_attr(gdt, "width", "b-3")

print(gdt, drop_empty_line = TRUE)

out <- file.path(Sys.getenv("NUTSTORE"),
                            "论文/IIA 和国际直接投资/写作/iia_ma",
                            "基本模型回归结果.Rds") |>
        readRDS()
out <- kable_to_grid(out$out)

system.time(print(out))


grepl(valid_regex, simple[2], perl = TRUE)
    valid_regex <- paste0("^[\\s\\", sep, "\\", symbol, "]+$")

gdt <-  do.call(rbind, re) |>
        as.data.table() |>
        GridTable(header = 2, align = "lrrrrrr")
merge_cells(gdt, 1, 2:7)

