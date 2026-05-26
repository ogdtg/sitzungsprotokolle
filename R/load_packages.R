# Functions
if (!require("janitor")) {
  install.packages("janitor")
  library(janitor)
}
if (!require("arrow")) {
  install.packages("arrow")
  library(arrow)
}
# if (!requireNamespace("fuzzyjoin", quietly = TRUE)) {
#   install.packages("fuzzyjoin")
# }
#install.packages("arrow")

packages <- c("xml2", "tibble", "purrr", "dplyr")

installed <- rownames(installed.packages())
missing <- packages[!packages %in% installed]

if (length(missing) > 0) install.packages(missing)

library(arrow)
library(pdftools)
library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(lubridate)
library(zoo)
library(jsonlite)
library(httr)
library(xml2)
library(tibble)
library(purrr)
library(httr)



























