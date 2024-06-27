library(pdftools)
library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(httr)
library(jsonlite)

eval(parse("R/load_packages.R", encoding="UTF-8"))
eval(parse("R/abstimmungen_functions.R", encoding="UTF-8"))
eval(parse("R/general_functions.R", encoding="UTF-8"))
eval(parse("R/grgeko_functions.R", encoding="UTF-8"))
eval(parse("R/mitglieder_functions.R", encoding="UTF-8"))
eval(parse("R/sitzungsprotokolle_functions.R", encoding="UTF-8"))



current_legislatur <- 2024



## Mitglieder scrape


mitglieder <- get_mitglieder()
message("Mitglieder crawled")

mitglieder_full <- check_mitglieder(mitglieder)
message("Mitglieder checked")

geschaefte <- get_vorstossdaten(legislatur=current_legislatur, mitglieder_df=mitglieder_full)
message("Vorstossdaten crawled")

get_abstimmungen(mitglieder_df=mitglieder_full)
message("Abstimmungen crawled")


get_sitzungsprotokolle()
message("Sitzungsprotokolle crawled")
