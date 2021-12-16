
creat_ctm <- function(file, hiperlinks, write = T, file_name = "ctm"){
cits <- read.csv2(file)
names(cits) <- c("id","text","tag")


hipers <- read.csv2(hiperlinks)
names(hipers) <- c("id_fonte","text_fonte","id_dest","text_dest")

x <- hipers %>% dplyr::select(id_fonte, text_dest) %>%
  dplyr::right_join(cits, by = c("id_fonte" = "id"))
x <- hipers %>% dplyr::select(id_dest, text_fonte) %>%
  dplyr::right_join(x, by = c("id_dest" = "id_fonte"))
x[is.na(x)] <- ""
cits_clean <- x %>% tidyr::unite(col = "text", text_fonte, text, text_dest, sep = " ")
cits <- cits_clean %>%
  tidyr::separate(col = tag,
                  into = c("tag", "subtag"),
                  sep = ":") %>%
  dplyr::mutate(
    tag = stringr::str_trim(tag) %>%
      stringr::str_squish() %>%
      stringr::str_to_lower(),
    subtag = stringr::str_trim(subtag) %>%
      stringr::str_squish() %>%
      stringr::str_to_lower()
  ) %>%
  dplyr::filter(
    tag %in% c(
      "presença",
      "empreendedorismo",
      "controle territorial",
      "empreendedorismo violento",
      "uso da for\u00e7a"
    )
  ) %>% select(-subtag)

if(write == T){
  readr::write_csv(cits, file = file.path("data-raw", paste0(file_name, ".csv")))
}
cits

}
