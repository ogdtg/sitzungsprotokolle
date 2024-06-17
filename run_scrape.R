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
mitglieder_full <- check_mitglieder(mitglieder)
# get_vorstossdaten(legislatur=current_legislatur, mitglieder_df=mitglieder_full)
get_abstimmungen(mitglieder_df=mitglieder_full)
get_sitzungsprotokolle()
# # Download
# download_mitglieder_pdf()
# 
# # Dataset
# mitglieder_list <- create_mitglieder_df("mitglieder.pdf")
# 
# # Crawl Page
# scrape_mg <- crawl_mitglieder_page()
#   
# 
# mitglieder <- mitglieder_list$data %>% 
#   mutate(fullname = paste0(Vorname," ",Name)) %>% 
#   mutate(fullname = str_replace_all(fullname,intToUtf8(8217),intToUtf8(39))) %>% 
#   left_join(scrape_mg %>% 
#               select(-Name), by = c("fullname"="name")) %>% 
#   select(-fullname) %>% 
#   mutate(datenstand = mitglieder_list$stand) %>% 
#   mutate_if(is.character,str_trim) %>% 
#   mutate(partei = str_replace(partei,"SP","SP und Gewerkschaften"))
# 
# names(mitglieder) <- tolower(names(mitglieder)) %>% str_replace_all("\\.","_") %>% str_replace_all("-","_")
# 
# # Save
# write.table(mitglieder, file = "data/gr_mitglieder.csv", quote = T, sep = ",", dec = ".", 
#             row.names = F, na="",fileEncoding = "utf-8")
# 
# saveRDS(scrape_mg,"data/scrape_gr_mg.rds")

# Abgleich und Erstellung einer kompletten Mitgliederliste inklusive Änderungen
# mitglieder_old <- readRDS("data/mitglieder_full.rds") %>% mutate_if(is.character,str_trim)
# mitglieder_changes_total_old <- readRDS("data/mitglieder_changes_total.rds") %>% mutate_if(is.character,str_trim)
# mitglieder_changes_name_party_fraktion_old <- readRDS("data/mitglieder_changes_name_party_fraktion.rds") %>% mutate_if(is.character,str_trim)
# 
# # Gesamtdaten
# mitglieder_full <- mitglieder %>% 
#   bind_rows(mitglieder_old) %>% 
#   distinct(name,vorname,partei,fraktion,.keep_all = T)
# 
# # Generelle Änderungen
# mitglieder_changes_total <- mitglieder %>% 
#   anti_join(mitglieder_old) %>% 
#   mutate(change_date = Sys.Date()) 
# 
# mitglieder_changes_total_full <- mitglieder_changes_total %>% 
#   bind_rows(mitglieder_changes_total_old)
#   
# # Änderung an Namen, Partei oder Fraktion
# mitglieder_changes_name_party_fraktion <- mitglieder %>% 
#   anti_join(mitglieder_old, by = c("name","vorname","partei","fraktion")) %>% 
#   mutate(change_date = Sys.Date()) 
# 
# mitglieder_changes_name_party_fraktion_full <- mitglieder_changes_name_party_fraktion %>% 
#   bind_rows(mitglieder_changes_name_party_fraktion_old)
# 
# if (nrow(mitglieder_changes_name_party_fraktion)>0){
#   issue_body_mg <- mitglieder_changes_name_party_fraktion %>% 
#     mutate(issue_body = paste0(vorname," ",name," (",partei,")", " [",fraktion,"]")) %>% 
#     pull(issue_body) %>% 
#     paste0(.,collapse = "\n")
#   
#   create_gh_issue(
#     title = paste0("Neue/veraenderte Mitglieder ",Sys.Date()),
#     body = paste0(
#       "Folgende Eintragungen sind neu oder wurden veraendert:\n\n",
#       issue_body_mg
#     )
#   )
#   
#   
# }  
#   
# 
# saveRDS(mitglieder_full,"data/mitglieder_full.rds")
# saveRDS(mitglieder_changes_total_full,"data/mitglieder_changes_total.rds")
# saveRDS(mitglieder_changes_name_party_fraktion_full,"data/mitglieder_changes_name_party_fraktion.rds")

# ## GRGEKO Scrape
# geschafte_list <- scrape_grgeko(legislatur = current_legislatur)
# 
# # Daten aufbereiten für OGD
# geschaefte_prep <- prepare_ogd_vorstoesse(geschafte_list)
# 
# 
# if (nrow(geschaefte_prep$vorstoesser)>0){
#   
# 
#   compare_df_old <- readRDS("data/compare_df.rds")
#   
#   compare_df <- geschaefte_prep$vorstoesser %>% 
#     distinct(nachname,vorname,partei) %>% 
#     anti_join(mitglieder_full %>% 
#                 rename(nachname = "name"))
#   
#   compare_df_new <- compare_df %>% 
#     anti_join(compare_df_old) 
#   
#   existing_issues <- readRDS("data/existing_issues.rds")
#   
#   compare_df_new <- compare_df_new %>% 
#     left_join(geschaefte_prep$vorstoesser) %>% 
#     anti_join(existing_issues)
#   
#   if (nrow(compare_df_new)>0){
#     
# 
#    
#     gnum <- compare_df_new %>% distinct(geschaeftsnummer) %>% pull()
#     
#     if (length(gnum)>3){
#       gnum_string <- "mehreren Geschaeften"
#     } else {
#       gnum_string <- paste0(gnum, collapse = ", ")
#     }
#     
#     
#     
#     issue_body <- compare_df_new %>% 
#       mutate(issue_body = paste0(nachname,", ",vorname," (",geschaeftsnummer,")")) %>% 
#       pull(issue_body) %>% 
#       paste0(.,collapse="\n")
#     
#     create_gh_issue(
#       title = paste0("Unbekannte Sprecher bei ",gnum_string),
#       body = paste0(
#         "Folgende Vorstoesser konnten keinem Eintrag aus der Mitgliederliste zugeordnet werden:\n\n",
#         issue_body
#       )
#     ) 
#   }
#     
# }

# Asbtimmungsdaten Scrape



# 
# pdf_df <- get_pdf_list(from_date = readRDS("data/last_abstimmung.rds"))
# 
# pdf_df_abst <- pdf_df %>% 
#   filter(str_detect(name,"Trakt"))
# 
# if (nrow(pdf_df_abst)>0){
#   abstimmungen_old <- readRDS("data/abstimmungen_ogd.rds")
#   
#   abstimmungen_new <- lapply(pdf_df_abst$pdf, prepare_abstimmung_pdf) %>%
#     bind_rows() %>%
#     mutate(traktandum = str_remove(traktandum,"Traktandum:") %>% str_trim()) %>%
#     mutate(fraktion = str_replace(fraktion,"SP UND GEW.","SP und Gewerkschaften"),
#            fraktion = str_replace(fraktion,"EDU/AUFTG","EDU/Aufrecht")) %>%
#     mutate_if(is.character, ~str_replace_all(.x,intToUtf8(39),intToUtf8(8217)))
#   
# 
#   
#   # Abgleich mit bereits gespeicherten Abstimmungen
#   abstimmungen <- abstimmungen_new %>% 
#     anti_join(abstimmungen_old, by = c("datum","geschaeftsnummer","traktandum")) %>% 
#     bind_rows(abstimmungen_old) 
#     
#   
#   # Abgleich mit Mitgliederliste
#   abstimmungen_new_mod <- abstimmungen_new %>% 
#     left_join(mitglieder_df %>% 
#                 mutate(name_vorname=paste0(name," ",vorname)), by = c("name_vorname","fraktion"))
#   
#   compare_df_new <- abstimmungen_new_mod %>% 
#     filter(is.na(name)) %>% 
#     distinct(name_vorname,datum,geschaeftsnummer)
#   
#   
#   if (nrow(compare_df_new)>0){
#     
#     
#     
#     gnum <- compare_df_new %>% distinct(geschaeftsnummer) %>% pull()
#     
#     if (length(gnum)>3){
#       gnum_string <- "mehreren Geschaeften"
#     } else {
#       gnum_string <- paste0(gnum, collapse = ", ")
#     }
#     
#     
#     
#     issue_body <- compare_df_new %>% 
#       mutate(issue_body = paste0(nachname,", ",vorname," (",geschaeftsnummer,")")) %>% 
#       pull(issue_body) %>% 
#       paste0(.,collapse="\n")
#     
#     create_gh_issue(
#       title = paste0("Abstimmungen: Unbekannte Namen bei ",gnum_string),
#       body = paste0(
#         "Folgende Mitglieder konnten keinem Eintrag aus der Mitgliederliste zugeordnet werden:\n\n",
#         issue_body
#       )
#     )
#     message("GitHub Issue created (Abstimmungen)")
#   }
# }

# 
# saveRDS(abstimmungen,"data/abstimmungen_ogd.rds")
# write.table(abstimmungen, file = "data/abstimmungen_ogd.csv", quote = T, sep = ",", dec = ".", 
#             row.names = F, na="",fileEncoding = "utf-8")
# 
# saveRDS(max(abstimmungen$datum),"data/last_abstimmung.rds")

## SITZUNGSPROTOKOLLE Scrape

# if (file.exists("vars/last_update.rds")){
#   last_update <- readRDS("vars/last_update.rds")
#   last_id <- readRDS("vars/last_id.rds")
# } else {
#   last_update <- as.Date("1990-01-01")
# }
# 
# 
# current_data <- tryCatch({
#   # Attempt to get the current data
#   get_current_data()
# }, error = function(e) {
#   # If an error occurs, print the error message and set current_data to NULL
#   list(pdf_date = last_update-1)
# })
# 
# 
# pdf_data_text <- prepare_pdf_data(pdf_link = current_data$pdf_link)
# current_data$pdf_date <- get_protocol_date(pdf_data_text)
# 
# 
# if (current_data$pdf_date > last_update){
#   pdf_df <- extract_speaker_text(pdf_data_text)
#   pdf_df_final <- prepare_text_data(pdf_df, date = current_data$pdf_date)
# 
#   gr <- last_id + 1
#   datum <- current_data$pdf_date
#   part_id <- case_when(
#     str_length(gr)==5 ~ paste0(gr),
#     str_length(gr)==4 ~ paste0("0",gr),
#     str_length(gr)==3 ~ paste0("00",gr),
#     str_length(gr)==2 ~ paste0("000",gr),
#     str_length(gr)==1 ~ paste0("0000",gr),
#     TRUE ~ paste0(lubridate::year(datum),gr)
#   )
# 
#   pdf_df_final <- pdf_df_final %>%
#     mutate(gr_id = 1:nrow(.)) %>%
#     mutate(rede_id = case_when(
#       str_length(gr_id)==3 ~ paste0(part_id,"0",gr_id),
#       str_length(gr_id)==2 ~ paste0(part_id,"00",gr_id),
#       str_length(gr_id)==1 ~ paste0(part_id,"000",gr_id),
#       TRUE~paste0(part_id,gr_id)
#     )) %>% 
#     rename(geschaeftsnummer = "registraturnummer",
#            fraktion = "partei") %>% 
#     mutate(partei = ifelse(is.na(fraktion),"",fraktion))
# 
#   # Define the URL and parameters
#   url <- "https://data.tg.ch/api/push/1.0/sk-stat-137/echtzeit/push/"
# 
#   # Send the POST request
#   response <- httr::POST(
#     url = url,
#     query = list(pushkey=Sys.getenv("PUSHKEY")),
#     body = jsonlite::toJSON(pdf_df_final),
#     httr::add_headers("Content-Type" = "application/json")
#   )
#   message(paste0("Data pushed with status code ", response$status_code))
# 
#   saveRDS(gr,"vars/last_id.rds")
#   saveRDS(datum,"vars/last_update.rds")
# 
# } else{
#   last_run <- Sys.time()
#   saveRDS(last_run,"vars/last_run.rds")
#   message("No new data")
# }
