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


last_protocol <- readRDS("vars/last_update.rds")
last_abstimmung <- readRDS("data/last_abstimmung.rds")

pdf_df <- get_pdf_list(from_date = last_protocol)

pdf_df_abst <- pdf_df %>% 
  filter(text>last_abstimmung)



get_abstimmungen(mitglieder_df=mitglieder_full, pdf_df = pdf_df_abst)
message("Abstimmungen crawled")


get_sitzungsprotokolle(pdf_df = pdf_df)
message("Sitzungsprotokolle crawled")
