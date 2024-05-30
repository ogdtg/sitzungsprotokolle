library(pdftools)
library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(httr)
library(jsonlite)


eval(parse("R/extract_functions.R", encoding="UTF-8"))




# Download
download_mitglieder_pdf()

# Dataset
mitglieder_list <- create_mitglieder_df("mitglieder.pdf")

# Crawl Page
scrape_mg <- crawl_mitglieder_page()

mitglieder <- mitglieder_list$data %>% 
  mutate(fullname = paste0(Vorname," ",Name)) %>% 
  mutate(fullname = str_replace_all(fullname,intToUtf8(8217),intToUtf8(39))) %>% 
  left_join(scrape_mg, by = c("fullname"="name")) %>% 
  select(-fullname) %>% 
  mutate(datenstand = mitglieder_list$stand)

names(mitglieder) <- tolower(names(mitglieder)) %>% str_replace_all("\\.","_")

# Save
write.table(mitglieder, file = "gr_mitglieder.csv", quote = T, sep = ",", dec = ".", 
            row.names = F, na="",fileEncoding = "utf-8")

saveRDS(scrape_mg,"scrape_gr_mg.rds")


if (file.exists("vars/last_update.rds")){
  last_update <- readRDS("vars/last_update.rds")
  last_id <- readRDS("vars/last_id.rds")
} else {
  last_update <- as.Date("1990-01-01")
}


current_data <- tryCatch({
  # Attempt to get the current data
  get_current_data()
}, error = function(e) {
  # If an error occurs, print the error message and set current_data to NULL
  list(pdf_date = last_update-1)
})

# 
# current_data <- get_data_by_url(pdf_link = "https://parlament.tg.ch/public/upload/assets/156926/240228_Ausfuehrliches-Protokoll.pdf?fp=1710943302687",
#                                 tagesordnung_link = "https://parlament.tg.ch/public/upload/assets/154806/Tagesordnung_Nr_72_vom_2024-02-28.pdf?fp=1712567498190")


# current_data <- get_data_by_url(pdf_link = "https://parlament.tg.ch/public/upload/assets/158537/240320_Ausfuehrliches_Protokoll.pdf?fp=1713259270913",
#                                 tagesordnung_link = "https://parlament.tg.ch/public/upload/assets/156843/Tagesordnung_Nr_73_vom_2024-03-20.pdf?fp=1712567540951")


# current_data <- get_data_by_url(pdf_link = "https://parlament.tg.ch/public/upload/assets/159336/240417_Ausfuehrliches_Protokoll.pdf?fp=1715083621043",
#                                 tagesordnung_link = "https://parlament.tg.ch/public/upload/assets/158452/Tagesordnung_Nr_74_vom_2024-04-17.pdf?fp=1713505901912")
# 


if (current_data$pdf_date > last_update){
  pdf_data_text <- prepare_pdf_data(pdf_link = current_data$pdf_link)
  pdf_df <- extract_speaker_text(pdf_data_text)
  pdf_df_final <- prepare_text_data(pdf_df, date = current_data$pdf_date)

  gr <- last_id + 1
  datum <- current_data$pdf_date
  part_id <- case_when(
    str_length(gr)==5 ~ paste0(gr),
    str_length(gr)==4 ~ paste0("0",gr),
    str_length(gr)==3 ~ paste0("00",gr),
    str_length(gr)==2 ~ paste0("000",gr),
    str_length(gr)==1 ~ paste0("0000",gr),
    TRUE ~ paste0(lubridate::year(datum),gr)
  )

  pdf_df_final <- pdf_df_final %>%
    mutate(gr_id = 1:nrow(.)) %>%
    mutate(rede_id = case_when(
      str_length(gr_id)==3 ~ paste0(part_id,"0",gr_id),
      str_length(gr_id)==2 ~ paste0(part_id,"00",gr_id),
      str_length(gr_id)==1 ~ paste0(part_id,"000",gr_id),
      TRUE~paste0(part_id,gr_id)
    ))

  # Define the URL and parameters
  url <- "https://data.tg.ch/api/push/1.0/sk-gr-1/echtzeit/push/"


  # Send the POST request
  response <- httr::POST(
    url = url,
    query = list(pushkey=Sys.getenv("PUSHKEY")),
    body = jsonlite::toJSON(pdf_df_final),
    httr::add_headers("Content-Type" = "application/json")
  )
  message(paste0("Data pushed with status code ", response$status_code))

  saveRDS(gr,"vars/last_id.rds")
  saveRDS(datum,"vars/last_update.rds")

} else{
  last_run <- Sys.time()
  saveRDS(last_run,"vars/last_run.rds")
  message("No new data")
}
