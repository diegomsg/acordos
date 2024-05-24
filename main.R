# relatórios excel de acordos condominiais em tidy
# 2024-05-16

# bibliotecas -------------------------------------------------------------

library(tidyxl)
library(dplyr)
library(stringr)
library(purrr)
library(unpivotr)


# dependencias ------------------------------------------------------------

source("fun/fun.R")


# parametros --------------------------------------------------------------

acordos_dir <- fs::path("data")


# acordos --------------------------------------------------------------

acordos_files <- fs::dir_ls(acordos_dir, glob = "*.xlsx")

# testes com maior arquivo, múltiplos acordos no período
acordos_max_pos <- acordos_files |> 
  file.size() |> 
  which.max()
test_file <- acordos_files[acordos_max_pos]


# tidy data ---------------------------------------------------------------

df <- tibble(files = acordos_files,
       content = map(files, ~xlsx_cells(.x) |>
                       filter(!is_blank) |>
                       select(sheet:character)),
       acordo_data = map(content, split_acordos))

df$acordo_data
