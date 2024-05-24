# funções auxiliares para acordos em tidy data

# bibliotecas ------------------------------------------------------------

require(dplyr)
require(tidyxl)
require(unpivotr)


# funcoes -----------------------------------------------------------------

get_acordos_pos_matrix <- function(data) {
  # posições (i, f) de cada acordo em tidyxl dataframe
  ind <- stringr::str_detect(data$character, stringr::regex("Acordo \\d+"))
  i <- which(ind)
  f <- c(which(ind)[-1] -2L, length(ind))
  cbind(i, f)
}

each_acordo <- function(data, pos_matrix, ...) {
  # separa em lista cada acordo conforme matriz de posições (i, f)
  count <- seq(1, nrow(pos_matrix))
  purrr::map(count, \(i) data[pos_matrix[i,][1]:pos_matrix[i,][2],], ...)
}

# wrapper para separar cada arquivo em n acordos conforme posições

split_acordos <- function(data) {
  each_acordo(data, pos_matrix = get_acordos_pos_matrix(data))
}

# cabeçalho de cada acordo

# usa tbl com mais linhas como exemplo
max_row_tbl <- df$split_content$`data/2023.xlsx` |> 
  map(nrow) |>
  list_simplify() |>
  which.max()
ex <- df$split_content$`data/2023.xlsx`[max_row_tbl][[1]]

# header
View(ex)
head_info <- c(4, 6, 8)
head_names <- c("unidade", "acordo", "data_acordo")
corners <- ex |>
  filter(character %in% c("Cobranças originais", "Parcelas do acordo"))
  
partition(ex, corners)$cells[[2]] |> 
  behead("up-left", info) |>
  behead("up", name = "i", types = "value")
unpivotr::rectify(ex) |> print(n = 100)
  tidyr::pivot_wider()
  
