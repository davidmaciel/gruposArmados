ajeita <- function(x){
  stringr::str_trim(x) %>%
    stringr::str_squish() %>%
    stringr::str_replace_all("\\s","\\+")
}

build_url_here <- function(street,houseNumber,district,city,state,country){
  paste0(
    "https://geocode.search.hereapi.com/v1/geocode?qq=street=",
    street, ";",
    "houseNumber=",
    houseNumber, ";",
    "district=",
    district, ";",
    "city=",
    city, ";",
    "state=",
    state, ";",
    "country=",
    country, "",
    "&apiKey={",Sys.getenv("HERE"),"}")

}
