# tidyxl/unpivotr approach to tidy acordos


# dependencias ------------------------------------------------------------

require(dplyr)
require(tidyxl)
require(unpivotr)
require(tidyr)
require(purrr)


# parametros --------------------------------------------------------------

br_locale <- readr::locale(decimal_mark = ",", grouping_mark = ".")


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

  l2_corners_text <- c("Detalhes", "Cobranças originais", "Parcelas do acordo")
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

tidy_det_acordos <- function(blocos) {
  # arruma bloco detalhes a partir de blocos em nested list of tbl
  # 1 tbl = 1 line
  pull(blocos, cells) |>
    list_pick(1) |>
    tidy_detalhes()
}

tidy_cobrancas <- function(cells) {
  # arruma bloco cobrancas
  # 1 linha de saída
  stopifnot(
    check_block(cells, "Cobranças originais")
  )
  
  acrescimos <- cells |>
    slice_tail(n = 4) |>
    behead(direction = "left", header) |>
    select(-(row:content)) |>
    spatter(header) |> 
    janitor::clean_names() |>
    select(acrescimos, total_devido) |>
    mutate(across(everything(), ~readr::parse_number(.x, locale = br_locale)))
  
  cells |>
    slice(-1) |>
    slice(1:(n()-5)) |> 
    behead(direction = "up", header) |> 
    select(row, data_type, numeric, character, header) |>
    spatter(header) |> 
    select(-row) |>
    janitor::clean_names() |>
    mutate(competencia = lubridate::my(competencia),
           composicao = as.numeric(sub(",", ".", composicao, fixed = TRUE)),
           vencimento = lubridate::dmy(vencimento)) |>
    fill(everything()) |> 
    nest(cobrancas = everything()) |>
    bind_cols(acrescimos)
}

tidy_cob_acordos <- function(blocos) {
  # arruma bloco cobranças a partir de blocos em nested list of tbl
  # 1 tbl = 1 line
  pull(blocos, cells) |>
    list_pick(2) |>
    tidy_cobrancas()
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
  mutate(detalhes = map(blocos, tidy_det_acordos),
         cobrancas = map(blocos, tidy_cob_acordos)) |> 
  unnest(c(detalhes, cobrancas))

# teste parcial
#TODO: PARCELAS
ex_parcelas <- data_pipe$blocos[[1]] |>
  pull(cells) |>
  list_pick(3)

# parcelas
check_block(ex_cobrancas, "Cobranças originais")
ex_cobrancas |>
  slice(-1) |>
  slice(1:(n()-5)) |> 
  behead(direction = "up", header) |> 
  select(row, data_type, numeric, character, header) |>
  spatter(header) |> 
  select(-row) |>
  janitor::clean_names() |>
  mutate(competencia = lubridate::my(competencia),
         composicao = as.numeric(sub(",", ".", composicao, fixed = TRUE)),
         vencimento = lubridate::dmy(vencimento)) |>
  fill(everything()) |> 
  nest(cobrancas = everything()) |>
  bind_cols(acrescimos) |> View()

acrescimos <- ex_cobrancas |>
  slice_tail(n = 4) |>
  behead(direction = "left", header) |>
  select(-(row:content)) |>
  spatter(header) |> 
  janitor::clean_names() |>
  select(acrescimos, total_devido) |>
  mutate(across(everything(), ~readr::parse_number(.x, locale = br_locale)))
data_pipe$name[1]

tidy_cobrancas(ex_cobrancas)
