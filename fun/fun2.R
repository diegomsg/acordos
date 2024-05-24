# tidyxl/unpivotr approach to tidy acordos


# dependencias ------------------------------------------------------------

require(dplyr)
require(tidyxl)
require(unpivotr)
require(purrr)


# funcoes -----------------------------------------------------------------

load_acordo_file <- function(file) {
  xlsx_cells(file) |>
    filter(!is_blank) |>
    select(row:character, -is_blank)
}

valida_xlsx_cells <- function(tbl) {
  #valida 
  col_names <- c("row", "col", "content", "data_type", "error", "logical", "numeric", "date", "character")
  names(tbl) |>
    setdiff(col_names) |>
    length() == 0L
}

partition_acordos <- function(data) {
  # particiona cada arquivo de acordo em blocos de acordo individial
  stopifnot(
    tibble = is.data.frame(data),
    valida = valida_xlsx_cells(data)
    )
  l1_corners_text <- c(stringr::regex("Acordo \\d+"))
  l1_corners <- filter(data, stringr::str_detect(character, l1_corners_text))
  partition(data, l1_corners) |>
    select(acordo = character, acordo_full_data = cells)
}

partition_acordos_l2 <- function(data) {
  # particiona cada conjunto de acordo nos 4 blocos
  stopifnot(
    tibble = is.data.frame(data),
    valida = valida_xlsx_cells(data)
  )
  l2_corners_text <- c("Detalhes", "Cobranças originais", 
                       "Acréscimos", "Parcelas do acordo")
  l2_corners <- filter(ex_acordo, character %in% l2_corners_text)
  partition(ex_acordo, l2_corners) |>
    select(name = character, cells)
}

check_block <- function(data, type) {
  data$character[1] == type
}

# example -----------------------------------------------------------------

files <- fs::path("data") |>
  fs::dir_ls(glob = "*.xlsx")

ex_file <- files[file.size(files) |> 
                   which.max()]

# test --------------------------------------------------------------------

# full pipe

data_pipe <- load_acordo_file(ex_file) |>
  partition_acordos() |>
  mutate(blocos = map(acordo_full_data, partition_acordos_l2)) |>
  select(-acordo_full_data) |>
  tidyr::unnest_longer(blocos)

# teste parcial

ex_blocos <- data_pipe[[3]][[1]]

# detalhes

ex_blocos$cells[[1]] |>
  slice(-1) |>
  # pegar pares de linhas e separar em 2 colunas
