# tidyxl/unpivotr approach to tidy acordos


# dependencias ------------------------------------------------------------

require(dplyr)
require(tidyxl)
require(unpivotr)
require(tidyr)
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
  l2_corners <- filter(data, character %in% l2_corners_text)
  partition(data, l2_corners) |>
    select(name = character, cells)
}

check_block <- function(data, type) {
  data$character[1] == type
}

tidy_detalhes <- function(cells) {
  # arruma bloco detalhes
  # 1 linha de saída
  stopifnot(
    check_block(cells, "Detalhes")
  )
  cells |>
    slice(-1) |>
    behead(direction = "left", tipo) |> 
    select(tipo, character) |> 
    pivot_wider(names_from = tipo, values_from = character) |>
    janitor::clean_names() |>
    mutate(cod_acordo = as.integer(cod_acordo),
           efetuado_em = lubridate::dmy(efetuado_em))
}

list_pick <- function(list, pos) {
  list[[pos]]
}

tidy_det_acordos <- function(cells) {
  # arruma bloco detalhes a partir de cells de acordos
  list_pick(cells, 1) |>
    tidy_detalhes()
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
  unnest_wider(blocos) |>
  mutate(detalhes = map(cells, tidy_det_acordos)) |> 
  #unnest_longer(c(name, cells)) |>
  #rename(info = name) |>
  unnest_wider(detalhes)

# teste parcial
#TODO: COBRANÇAS
ex_cobrancas <- data_pipe$cells[[1]][[2]]

# detalhes
check_block(ex_detalhe, "Detalhes")
ex_detalhe |>
  slice(-1) |>
  behead(direction = "left", tipo) |> 
  select(tipo, character) |> 
  pivot_wider(names_from = tipo, values_from = character) |>
  janitor::clean_names() |>
  mutate(cod_acordo = as.integer(cod_acordo),
         efetuado_em = lubridate::dmy(efetuado_em))
  # pegar pares de linhas e separar em 2 colunas
data_pipe$name[1]
