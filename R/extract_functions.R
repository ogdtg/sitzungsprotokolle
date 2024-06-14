# Functions
if (!require("janitor")) {
  install.packages("janitor")
  library(janitor)
}
library(pdftools)
library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(lubridate)

#' Retrieve protocol date from pdf
#'
#' @param prepared_data 
#'
#' @return
#' @export
#'
#' @examples
get_protocol_date <- function(prepared_data){
  pdf_data_text %>% 
    filter(page==1) %>% 
    filter(str_detect(font_name,"[B|b]old")) %>% 
    filter(font_size>12) %>% 
    group_by(y) %>% 
    summarise(line = paste0(text, collapse = " ")) %>% 
    filter(str_detect(line, "\\d{1,2}\\. .+? \\d{4}")) %>% 
    pull(line) %>% 
    str_extract("\\d{1,2}\\. .+? \\d{4}") %>% 
    replace_german_months() %>% 
    lubridate::dmy()
}



#' Function to replace german month names
#'
#' @param text 
#'
#' @return
#' @export
#'
replace_german_months <- function(text) {
  # Create a named vector with German month names as keys and English month names as values
  months_german_to_english <- c(
    "Januar" = "January",
    "Februar" = "February",
    "März" = "March",
    "April" = "April",
    "Mai" = "May",
    "Juni" = "June",
    "Juli" = "July",
    "August" = "August",
    "September" = "September",
    "Oktober" = "October",
    "November" = "November",
    "Dezember" = "December"
  )
  
  # Replace each German month name with its English equivalent
  for (german_month in names(months_german_to_english)) {
    english_month <- months_german_to_english[german_month]
    text <- gsub(german_month, english_month, text, ignore.case = TRUE)
  }
  
  return(text)
}















#' GitHub Issues erstellen
#'
#' @param token 
#' @param owner 
#' @param repo 
#' @param title 
#' @param body 
#' @param assignees 
#' @param milestone 
#' @param labels 
#'
#' @return
#' @export
#'
#' @examples
create_gh_issue <- function(token = Sys.getenv("PAT"), owner ="ogdtg", repo="sitzungsprotokolle", title, body, assignees=list("FLorenz","dkoltg"), milestone=NULL, labels=list("question")) {
  url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/issues")
  
  headers <- c(
    "Accept" = "application/json",
    "Authorization" = paste("Bearer", token),
    "X-GitHub-Api-Version" = "2022-11-28"
  )
  
  body_data <- list(
    title = title,
    body = body,
    assignees = assignees,
    milestone = milestone,
    labels = labels
  )
  response <- POST(url, body = body_data, encode = "json", add_headers(.headers = headers))
  res_list <- response$content %>% rawToChar() %>% jsonlite::fromJSON()
  
  return(res_list$url)
}







