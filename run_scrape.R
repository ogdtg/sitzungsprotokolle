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
mitglieder <- create_mitglieder_df("mitglieder.pdf")

# Save
write.table(mitglieder$data, file = "gr_mitglieder.csv", quote = T, sep = ",", dec = ".", 
            row.names = F, na="",fileEncoding = "utf-8")




if (file.exists("vars/last_update.rds")){
  last_update <- readRDS("vars/last_update.rds")
  last_id <- readRDS("vars/last_id.rds")
} else {
  last_update <- as.Date("1990-01-01")
}


current_data <- get_current_data()

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
