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


# tidy data ---------------------------------------------------------------

data_pipe <- tibble(files = acordos_files,
                    content = map(files, load_acordo_file),
                    blocos = map(content, partition_acordos)) |>
  unnest(blocos) |>
  select(-files, -content) |>
  mutate(blocos = map(acordo_full_data, partition_acordos_l2)) |>
  select(-acordo_full_data) |>
  mutate(detalhes = map(blocos, tidy_det_acordos),
         cobrancas = map(blocos, tidy_cob_acordos),
         parcelas = map(blocos, tidy_parc_acordos)) |> 
  unnest(c(detalhes, cobrancas, parcelas)) |>
  mutate(cobrado = map_dbl(cobrancas, ~with(.x, sum(composicao)))) |>
  relocate(cobrado, .before = acrescimos) |>
  mutate(total_emitido = map_dbl(parcelas, ~sum(.x$emitido)),
         total_pago = map_dbl(parcelas, ~sum(.x$pago) |> 
                                         coalesce(0)),
         quitado = total_pago >= total_emitido,
         vencido = map_vec(parcelas, ~max(.x$vencimento) <= Sys.Date()) & !quitado)
