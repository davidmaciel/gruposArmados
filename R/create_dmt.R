
create_dmt <- function(file_raw, file_class, write = T, file_name = "dtm", id_class){
s0 <- readr::read_csv2(file_raw, locale = readr::locale(encoding = "ISO-8859-1"))
# names(s0) <- c("den_cd","den_texto")

s0_class <- read.csv2(file_class)
x <- s0_class %>% rename("den_cd" = denuncia) %>%
  inner_join(milic) %>% select(den_cd, den_texto) %>%
  distinct()

s0_class <- s0_class %>%
  rename("den_cd" = denuncia) %>%
  tidyr::pivot_longer(cols = -1,
                      names_to = "codigo",
                      values_to = "presente") %>%
   dplyr::filter(presente > 0) %>%
  dplyr::select(-presente) %>%
  dplyr::mutate(
    codigo = dplyr::case_when(
      stringr::str_detect(codigo, "controle") ~ "controle",
      stringr::str_detect(codigo, "violento") ~ "empreendedorismo_violento",
      stringr::str_detect(codigo, "for\u00e7a") ~ "uso_da_forca",
      stringr::str_detect(codigo, "[eE]mpreendedorismo\\.\\.\\.") ~ "empreendedorismo_nao_violento",
      stringr::str_detect(codigo, "PRESEN\u00c7A") ~ "presenca",
      T ~ "outro"
    )
  ) %>% dplyr::distinct() %>%
  tidyr::pivot_wider(
    names_from = codigo,
    values_from = codigo,
    values_fn = function(x) {
      ifelse(is.na(x), 0, 1)
    },
    values_fill = 0
  ) %>%
    dplyr::select(den_cd, controle, empreendedorismo_violento,
                  empreendedorismo_nao_violento, presenca, uso_da_forca)

# s0_class$den_cd <- as.numeric(str_remove(s0_class$den_cd, "0$"))
s0_class <- left_join(x, s0_class)
classified <-
  dplyr::inner_join(s0, s0_class, by = c("den_cd")) %>% distinct() %>%
  rename("den_texto" = den_texto.x) %>%
  select(-den_texto.y)

classified[is.na(classified)] <- 0
if(write == T){
  readr::write_csv(classified, file = file.path("data-raw/processados/milicia/", paste0(file_name, ".csv")))


  }
classified
}

# create_dmt(file_raw = "data-raw/milic_sample_1_e_2_raw.CSV",
#            file_class = "data-raw/milic_sample_1_e_2_classified.CSV")
# create_dmt(file_raw, file_class, file_name = "dtm_liminares_1_caio")
# x <- create_dmt(file_raw = "data-raw/milic_sample_1_e_2_raw.CSV",
#                 file_class = "data-raw/processados/dtm_milicia_amostra_1_e_2.CSV",
#                 file_name = "data-raw/processados/dtm_milica_amostra_1_e_2_caio")
file_raw <- "data-raw/cru/liminares/liminares_03_atlas.CSV"
file_class <- "data-raw/processados/milicia/dtm_milic_s1_2_e_3.CSV"
file_name <- "dtm_milica_s1_2_e_3_caio"
