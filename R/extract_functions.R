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

#' extract_tagesordnung
#'
#' Extracts the Tagesordnung (agenda) from a PDF document specified by the pdf_link parameter
#'
#' @param pdf_link The URL or file path of the PDF document.
#'
#' @return The function returns a dataframe containing the extracted Tagesordnung.
#' @export
#'
extract_tagesordnung <- function(pdf_link){

  #Creaet tempfile
  temp <- tempfile()

  #downlaod_pdf
  download.file(pdf_link,
                destfile = temp,mode = "wb")

  # Extact pdf data with font info
  tago_data <- pdftools::pdf_data(temp, font_info = T)
  # tago_text <- pdftools::pdf_text(temp)
  #
  # text_data <- str_split(tago_text,"\\n|\\s|\\r")[[1]]
  # text_data <- text_data[text_data!=""]
  # text_df <- data.frame(text = text_data,
  #                       order = 1:length(text_data))


  # Add page variable
  tago_data <- lapply(seq_along(tago_data), function(i){
    tago_data[[i]]$page <- i
    return(tago_data[[i]])
  })


  # bind rows and add the separator
  full_tago_data <- tago_data %>%
    bind_rows() %>%
    mutate(sep = ifelse(space," ","\n")) %>%
    mutate(order = 1:nrow(.))

  # if (nrow(full_tago_data)!=nrow(text_df)){
  #   warning("Not the same length for data and text")
  # }

  # Join with text for correct order
  full_tago_data <- full_tago_data %>%
    # left_join(text_df) %>%
    # distinct(order,.keep_all = T) %>%
    arrange(order)

  # Concatenate text with separator for creating the final text from the words
  full_tago_data <- full_tago_data %>%
    mutate(lag_sep = lag(sep)) %>%
    mutate(text_string =paste0(text,sep))


  # Detect where the Table of Content starts
  start_of_tagesordnung <- full_tago_data %>%
    mutate(tago_num = case_when(
      (str_detect(text,"\\d+\\.") & (x>=82 & x<=88)) | (str_detect(text,"\\d+.\\d+") & (x>=100 & x<=108)) ~ TRUE,
      TRUE~FALSE)) %>%
    mutate(start = case_when(
      tago_num & str_detect(font_name,"[B|b]old") & text=="1." ~ TRUE,
      TRUE~FALSE
    )) %>%
    mutate(to_num = ifelse(tago_num,text,NA))


  # Extract the row index of the start
  start_index <- which(start_of_tagesordnung$start)


  # Slice the dataset and complete the column with the group number
  start_of_tagesordnung_red <- start_of_tagesordnung %>%
    slice(c(start_index:nrow(.))) %>%
    mutate(to_num = zoo::na.locf(to_num,na.rm = F))


  # Find where the tagesordnung ends
  end_index <- start_of_tagesordnung_red %>%
    mutate(to_num = ifelse(str_detect(to_num,"^\\d+\\.$"),paste0(to_num,0),to_num)) %>%
    separate(to_num, into = c("first","last"),sep = "\\.",remove = F) %>% #create numeric variables for sorting
    mutate(first = as.numeric(first),
           last = as.numeric(last)) %>%
    group_by(first) %>%
    mutate(max_last = max(last)) %>% #
    ungroup() %>%
    mutate(text_x = case_when(
      tago_num ~ lead(x),
      TRUE~NA_integer_
    )) %>%
    mutate(text_x = zoo::na.locf(text_x,na.rm = F)) %>%
    mutate(test = case_when(
      first==max(first) & last == max_last &!space~TRUE & lead(x) != text_x,
      TRUE~FALSE
    ))

  # Extract the row index of the end of the tagesordnung
  end_index_num <- which(end_index$test) %>% min()


  # Create the final dataset
  tagesordnung_final <- end_index %>%
    slice(c(1:end_index_num)) %>% #Slice the dataset
    group_by(first,last,to_num) %>%
    summarise(full_text = paste0(text_string,collapse = "")) %>% # Create the full text
    ungroup() %>%
    arrange(first,last) %>% #sort it n correct order
    select(-c(first,last))  # remove the numerics


  return(tagesordnung_final)
}




#' Prepare PDF Data
#'
#' Prepares the PDF data with font information for further processing.
#'
#' @param pdf_link The URL or file path of the PDF document.
#'
#' @return The function returns a dataframe containing the PDF data with font information.

#' @export
#'
prepare_pdf_data <- function(pdf_link){

  #Creaet tempfile
  temp <- tempfile()

  #downlaod_pdf
  download.file(pdf_link,
                destfile = temp,mode = "wb")

  # Extact pdf data with font info
  tago_data <- pdftools::pdf_data(temp, font_info = T)
  # tago_text <- pdftools::pdf_text(temp)
  #
  # text_data <- str_split(tago_text,"\\n|\\s|\\r")[[1]]
  # text_data <- text_data[text_data!=""]
  # text_df <- data.frame(text = text_data,
  #                       order = 1:length(text_data))


  # Add page variable
  tago_data <- lapply(seq_along(tago_data), function(i){
    tago_data[[i]]$page <- i
    return(tago_data[[i]])
  })


  # bind rows and add the separator
  full_tago_data <- tago_data %>%
    bind_rows() %>%
    mutate(sep = ifelse(space," ","\n")) %>%
    mutate(text_string = paste0(text,sep))

  return(full_tago_data)

}



# Extract Sitzungsdaten

#' Extract Sitzungsdaten from PDF data
#'
#' This function extracts Sitzungsdaten (session data) from PDF data.
#'
#' @param pdf_data A data frame containing the PDF data.
#' @return A data frame containing the extracted Sitzungsdaten.
#' @export
extract_sitzungsdaten <- function(pdf_data){

  index <-  which(pdf_data$text == "Tagesordnung" &
                    str_detect(pdf_data$font_name, "[B|b]old"))[1]

  sitzungsdaten <- pdf_data %>%
    slice(c(1:(index-1))) %>%
    arrange(page,y,x) %>%
    filter(font_size<14)


  sitzungsdaten_bold <- sitzungsdaten %>%
    filter(str_detect(font_name, "[B|b]old")) %>%
    mutate(element = 1:nrow(.))

  sitzungsdaten <- sitzungsdaten %>%
    left_join(sitzungsdaten_bold) %>%
    mutate(element = zoo::na.locf(element,na.rm = F))


  sitzungsdaten_text <- sitzungsdaten %>%
    filter(!str_detect(font_name, "[B|b]old")) %>%
    group_by(element) %>%
    summarise(text_content  = paste0(text_string,collapse = ""))

  sitzungsdaten_final <- sitzungsdaten_bold %>%
    select(text,element) %>%
    left_join(sitzungsdaten_text,by = "element") %>%
    select(-element) %>%
    tidyr::pivot_wider(names_from = text,values_from = text_content)


  return(sitzungsdaten_final)
}







#' Extract Speaker Text from PDF data
#'
#' This function extracts speaker text from PDF data.
#'
#' @param pdf_data_text A data frame containing the PDF text data.
#' @return A data frame containing the extracted speaker text.
#'
extract_speaker_text <- function(pdf_data_text){

  content_base <- pdf_data_text %>%
    mutate(lag_sep = lag(sep)) %>%
    mutate(rnum = 1:nrow(.)) %>%
    mutate(lag_lag_sep = lag(lag_sep,default = NA)) %>%
    arrange(rnum)


  all_speaker <- content_base %>%
    mutate(lag_text = lag(text)) %>%
    mutate(lead_font = lead(font_name)) %>%
    mutate(speaker = case_when(
      str_detect(font_name,"[B|b]old") & (lag_sep =="\n"| lag_lag_sep == "\n" |lag_text=="Dr.") & stringr::str_detect(text,"\\:|\\,") ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    mutate(speaker = case_when(
      speaker & (lag_sep =="\n"| lag_lag_sep == "\n") & stringr::str_detect(text,"\\,") & str_detect(lead_font,"[B|b]old") ~ FALSE,
      speaker & str_detect(text,",") & !(str_detect(lead(text),":") | str_detect(lead(lead(text)),":")) ~ FALSE, #Sonderfälle
      speaker & str_detect(text,":") & (str_detect(text,"^[a-z]") | str_detect(text,"\\d")) ~ FALSE, #Sonderfälle
      speaker & (lag_sep==" " | lag(y)==y) & str_detect(lag_text,"^[a-z]") & !str_detect(lag(font_name),"[B|b]old")~ FALSE,
      speaker & str_detect(text,"^I+?:") & str_detect(lag_text,"Teil") ~ FALSE,
      speaker & str_detect(text,":")  & str_detect(lead_font,"[B|b]old") ~ FALSE,
      speaker & str_detect(text,"Antrag:")  & str_detect(lead_font,"[B|b]old") ~ FALSE,
      speaker & str_detect(text,"Antrag:")  & str_detect(lag_text,"folgenden") ~ FALSE,
      str_detect(font_name,"[B|b]old") & str_detect(text,":") & lag_lag_sep=="\n" & str_detect(lag_text,"Präsident")  & str_detect(lag(font_name),"[B|b]old") ~ TRUE,
      str_detect(font_name,"[B|b]old") & str_detect(text,":") & lag(lag_lag_sep)=="\n"& str_detect(lag(lag_text),"Präsident")  & str_detect(lag(font_name),"[B|b]old") & str_detect(lag(lag(font_name)),"[B|b]old")~ TRUE,

      TRUE~ speaker
    )) %>%
    mutate(full_speaker = case_when(
      speaker & lag_lag_sep == "\n" & lag_sep != "\n" ~ paste0(lag(text_string),text_string),
      speaker & lag_sep == "\n" ~ paste0(text_string),
      speaker & lag_text =="Dr." ~ paste0(lag(lag(text_string)),lag(text_string),text_string),
      speaker & str_detect(lag(lag_text),"Präsident") & str_detect(lag(lag(font_name)),"[B|b]old")~ paste0(lag(lag(text_string)),lag(text_string),text_string),

      TRUE ~ NA_character_
    )) %>%
    mutate(party = case_when(
      speaker & (lag_sep =="\n"| lag_lag_sep == "\n") & stringr::str_detect(text,"\\,") ~ TRUE,
      TRUE ~ FALSE
    ))



  party_index <- all_speaker %>%
    filter(party) %>%
    pull(rnum)

  if (length(party_index>0)) {
    colon_list <- list()

    for (num in party_index) {
      base = num
      colon = FALSE
      while (!colon){
        num = num + 1
        colon = stringr::str_detect(all_speaker$text[num],"\\:")
        if (num -base >= 15){
          colon_list[[as.character(base)]] <- NA
          break
        }
      }
      if (length(colon_list[[as.character(base)]])>0){
        if (colon_list[[as.character(base)]] %>% is.na()){
          next
        }
      }
      colon_list[[as.character(base)]] <- num
    }

    colon_df <- colon_list %>% unlist() %>% data.frame(rnum = as.numeric(names(.)),colon = ., row.names = 1:length(.))

    all_speaker <- all_speaker %>%
      left_join(colon_df)


    party_speaker <- all_speaker %>%
      filter(!is.na(colon))

    party_speaker$party_name <- NA

    for (i in 1:nrow(party_speaker)){
      start <- party_speaker$rnum[i]+1
      end <- party_speaker$colon[i]
      party_speaker$party_name[i] <- paste0(all_speaker$text_string[start:end],collapse = "") %>%
        stringr::str_remove("\\:") %>%
        stringr::str_trim()
    }

    all_speaker <- all_speaker %>%
      filter(is.na(colon)) %>%
      bind_rows(party_speaker) %>%
      arrange(rnum) %>%
      mutate(speaker = case_when(
        speaker & stringr::str_detect(text,"\\,") & is.na(colon) ~ FALSE,
        TRUE ~ speaker
      ))

  } else {
    all_speaker$party_name <- NA
  }



  # What words are also present in bold besides the speaker

  all_speaker <- all_speaker %>%
    mutate(bold_no_speaker = case_when(
      !speaker & str_detect(font_name,"[B|b]old") ~ TRUE,
      TRUE~FALSE
    ))



  # Headings

  all_speaker <- all_speaker %>%
    mutate(heading = case_when(
      (x==79 | x==100) & str_detect(font_name,"[B|b]old") & !speaker & lag_sep =="\n" & sep == "\n" ~ TRUE,
      (x==79 | x==100) & str_detect(font_name,"[B|b]old") & !speaker & lag_sep =="\n" & str_detect(lead_font,"[B|b]old") & !stringr::str_detect(text,"^\\d")~ TRUE,
      TRUE~FALSE
    ))

  all_speaker$part_of_heading = NA
  i=1
  index = 1
  while (i <= nrow(all_speaker)){
    if(all_speaker$heading[i]){
      all_speaker$part_of_heading[i] <- index

      k=i+1
      if (all_speaker$space[i]) {
        while(str_detect(all_speaker$font_name[k],"[B|b]old")){
          all_speaker$part_of_heading[k] <- index
          k = k+1
        }
      }

      i = k
      index = index + 1
    }
    i = i+1
  }

  # Extract the Tagesordnung for better oversight

  all_speaker <- all_speaker %>%
    mutate(start_tagesordnung = case_when(
      (x==79 | x==100) & str_detect(font_name,"[B|b]old") & lag_sep == "\n" & stringr::str_detect(text,"^(\\d+\\.\\d+|\\d+\\.)$") & !stringr::str_detect(lead(text),"Lesung")~ TRUE,
      TRUE ~ FALSE
    ))

  all_speaker$part_of_tagesordnung = NA
  i=1
  index = 1
  while (i <= nrow(all_speaker)){
    if(all_speaker$start_tagesordnung[i]){
      all_speaker$part_of_tagesordnung[i] <- index

      k=i+1
      if (all_speaker$space[i]) {
        while((str_detect(all_speaker$font_name[k],"[B|b]old") | stringr::str_detect(all_speaker$text[k],"\\(|\\)|\\/"))){
          all_speaker$part_of_tagesordnung[k] <- index
          if (!all_speaker$space[k] & !str_detect(all_speaker$font_name[k],"[B|b]old")){
            k = k+1
            break
          }
          k = k+1

        }
      }

      i = k
      index = index + 1
    }
    i = i+1
  }

  # Complete the tagesordnungen column
  all_speaker <- all_speaker %>%
    mutate(tagesordnung = part_of_tagesordnung,
           part_of_tagesordnung = zoo::na.locf(part_of_tagesordnung,na.rm = F))


  # Complete the heading column
  all_speaker <- all_speaker %>%
    mutate(ueberschrift = zoo::na.locf(part_of_heading,na.rm = F))


  # complete speaker column

  all_speaker$part_of_speaker = NA
  i=1
  index = 1
  while (i <= nrow(all_speaker)) {

    if (all_speaker$speaker[i]) {
      all_speaker$part_of_speaker[i] <- index

      k = i + 1
      while (!all_speaker$heading[k] & !all_speaker$start_tagesordnung[k] & !all_speaker$speaker[k]) {
        all_speaker$part_of_speaker[k] <- index
        k = k + 1
        if (k>nrow(all_speaker)){
          break
        }
        if(all_speaker$speaker[k]){
          break
        }
      }
      i = k-1
      index = index + 1
    }
    i = i + 1
  }
  # Complete the speaker and party column


  speaker_df <- all_speaker %>%
    select(full_speaker,party_name,part_of_speaker) %>%
    filter(!is.na(part_of_speaker)) %>%
    filter(!is.na(full_speaker)) %>%
    distinct()

  # Complete the heading column
  all_speaker_mod <- all_speaker %>%
    select(-c(party_name,full_speaker)) %>%
    left_join(speaker_df)


  # Tagesordnung
  tord_df <- all_speaker %>%
    filter(!is.na(tagesordnung)) %>%
    group_by(tagesordnung) %>%
    summarise(name_tagesordnung = paste0(text_string,collapse = ""))

  # Complete the heading column
  all_speaker_mod <- all_speaker %>%
    select(-c(party_name,full_speaker)) %>%
    left_join(speaker_df)



  # Build a text dataset
  all_speaker_text <- all_speaker_mod %>%
    filter(!is.na(part_of_speaker)) %>%
    filter(font_size>=11.3) %>%
    group_by(part_of_speaker,part_of_tagesordnung,ueberschrift,full_speaker,party_name) %>%
    summarise(full_text = paste0(text_string,collapse = "")) %>%
    left_join(tord_df,by = c("part_of_tagesordnung"="tagesordnung")) %>%
    filter(
      !full_speaker %in% c(
        "Abstimmung: ",
        "Wahl: ",
        "Wahl:\n",
        "Wahlen:\n",
        "Abstimmungen: ",
        "Abstimmungen:\n",
        "Abstimmung:\n",
        "Schlussabstimmungen:\n",
        "Schlussabstimmung:\n",
        "Beantwortung:\n"
      )
    ) %>%
    ungroup() %>%
    mutate(full_text = case_when(
      !is.na(party_name) ~ str_remove(full_text,"^.+?,.+?: "),
      is.na(party_name) ~ str_remove(full_text,"^.+?: "),
      TRUE ~ full_text)
      ) %>%
    mutate(full_text = str_remove_all(full_text,"-\n"),
           full_text = str_replace_all(full_text,"\n", " ") %>% str_trim()) %>%
    mutate(remove_string = lead(full_speaker)#,
           # remove_string =  purrr::map_chr(str_split(remove_string, " "), 1),
           # remove_string = paste0("\n",remove_string," "),
           # full_text = str_remove(full_text,paste0(remove_string,"$"))
           )

  return(all_speaker_text)
}


get_current_data <- function(){
  page <- read_html("https://parlament.tg.ch/sitzungen-protokolle/ausfuehrliche-protokolle.html/4483")

  urls <- page %>%
    html_elements("#content") %>%
    html_elements("div") %>%
    html_elements("li") %>%
    html_elements("a") %>%
    html_attr("href")


  pdf_link <- urls[stringr::str_detect(urls,"[P|p]rotokoll")]
  pdf_date <- str_extract(pdf_link,"\\d+\\.\\d+\\.\\d{2,4}|\\d+\\-\\d+\\-\\d{2,4}") %>% lubridate::dmy()
  
  if (is.na(pdf_date)){
    pdf_date <- str_extract(pdf_link,"(?<!fp=)\\d{6}(?!/).|(?<!fp=)\\d{8}(?!/).") %>% lubridate::ymd()
    
  }
  
  page <- read_html("https://parlament.tg.ch/sitzungen-protokolle/tagesordnungen.html/4481")

  tagesordnung_link <- page %>%
    html_elements("#content") %>%
    html_elements("p") %>%
    html_elements("a") %>%
    html_attr("href")

  to_date <- str_replace_all(tagesordnung_link,"%20"," ") %>% str_extract("\\d+\\.\\d+\\.\\d+|\\d+\\-\\d+\\-\\d+") %>% lubridate::dmy()

  if (is.na(to_date)){
    to_date <- str_replace_all(tagesordnung_link,"%20"," ") %>% str_extract("\\d+\\.\\d+\\.\\d+|\\d+\\-\\d+\\-\\d+") %>% lubridate::ymd()
    
  }
  
  result <- list(pdf_link = pdf_link,
                 pdf_date = pdf_date,
                 tagesordnung_link = tagesordnung_link,
                 to_date = to_date)
  return(result)
}


get_data_by_url <- function(pdf_link,tagesordnung_link){

  pdf_date <- str_extract(pdf_link,"\\d+\\.\\d+\\.\\d{2,4}|\\d+\\-\\d+\\-\\d{2,4}") %>% lubridate::dmy()
  
  if (is.na(pdf_date)){
    pdf_date <- str_extract(pdf_link,"(?<!fp=)\\d{6}(?!/).|(?<!fp=)\\d{8}(?!/).") %>% lubridate::ymd()
    
  }
  
  to_date <- str_replace_all(tagesordnung_link,"%20"," ") %>% str_extract("\\d+\\.\\d+\\.\\d+|\\d+\\-\\d+\\-\\d+") %>% lubridate::dmy()
  
  if (is.na(to_date)){
    to_date <- str_replace_all(tagesordnung_link,"%20"," ") %>% str_extract("\\d+\\.\\d+\\.\\d+|\\d+\\-\\d+\\-\\d+") %>% lubridate::ymd()
    
  }
  
  
  result <- list(pdf_link = pdf_link,
                 pdf_date = pdf_date,
                 tagesordnung_link = tagesordnung_link,
                 to_date = to_date)
  return(result)
}


scrape_docs <- function(range = c(1:50)){
  full_speaker_text <- list()
  sitzungsdaten_full <- list()

  for (j in range) {
    print(j)

    if (is.na(protocols$pdf_link[j])) {
      sitzungsdaten_full[[j]] <- NULL
      full_speaker_text[[j]] <- NULL

    } else {
      pdf_data_prepared <- tryCatch({
        prepare_pdf_data(protocols$pdf_link[j])
      }, error = function(cond){
        NULL
      })

      speaker_text_temp <- tryCatch({
        extract_speaker_text(pdf_data_prepared) %>%
          select(-c(1:3)) %>%
          mutate(date = protocols$datum[j])
      }, error = function(cond){
        NULL
      })

      sitzungsdaten_temp <- tryCatch({
        extract_sitzungsdaten(pdf_data_prepared) %>%
          mutate(date = protocols$datum[j])
      }, error = function(cond){
        NULL
      })

      full_speaker_text[[j]] <- speaker_text_temp
      sitzungsdaten_full[[j]] <- sitzungsdaten_temp
    }
  }

  sd_df <- lapply(sitzungsdaten_full, function(x) {
    if (!is.null(x)) {
      x <- x %>%
        mutate_all(as.character) %>%
        select(c(Vorsitz:date))
      return(x)
    }
  }) %>% bind_rows()

  text_df <- full_speaker_text %>% bind_rows()

  return(list(sitzungsdaten = sd_df, text = text_df))
}


prepare_text_data <- function(pdf_df, date){
  pdf_df$date <- date

  pdf_df_mod <- pdf_df %>%
    select(-remove_string) %>%
    arrange(date) %>%
    mutate(id = 1:nrow(.)) %>%
    group_by(date) %>%
    mutate(gr = cur_group_id()) %>%
    mutate(gr_id = 1:n()) %>%
    mutate(gr_id = case_when(
      str_length(gr_id)==3 ~ paste0(lubridate::year(date),gr,"0",gr_id),
      str_length(gr_id)==2 ~ paste0(lubridate::year(date),gr,"00",gr_id),
      str_length(gr_id)==1 ~ paste0(lubridate::year(date),gr,"000",gr_id),
      TRUE~paste0(lubridate::year(date),gr,gr_id)
    )) %>%
    ungroup() %>%
    mutate(
      speaker_type = str_extract(
        full_speaker,
        "Kommisionspräsidentin|Regierungsrat|Regierungsrätin|Präsidentin|Präsident|Alterspräsidentin|Alterspräsident|Kommissionspräsident"
      )
    ) %>%
    select(-c(part_of_speaker,part_of_tagesordnung,ueberschrift,id,gr)) %>%
    rename("rede_id" = gr_id,
           "tagesordnungspunkt" = name_tagesordnung,
           "partei" = party_name,
           "sprecher" = full_speaker,
           "sprecher_typ" = speaker_type,
           "datum" = date,
           "rede" = full_text) %>%
    select(rede_id,datum, tagesordnungspunkt, sprecher, sprecher_typ,partei,rede) 
  
  pdf_df_mod <- pdf_df_mod %>% 
    mutate(rede = str_remove(rede,paste0(" ",lead(sprecher_typ),"$"))) %>% 
    mutate(rede = str_remove(rede,"Diskussion - nicht benützt.*?$"))

  pdf_df_mod <- pdf_df_mod %>%
    mutate(registraturnummer = str_extract(tagesordnungspunkt,"\\(\\d\\d/[A-Z].+?/.+?\\)")) %>%
    mutate(registraturnummer = str_remove_all(registraturnummer,"\\(|\\)")) %>%
    group_by(datum) %>%
    mutate(reg_mod = zoo::na.locf(registraturnummer,na.rm = F)) %>%
    mutate(point = str_extract(tagesordnungspunkt,"^\\d\\.")) %>%
    mutate(registraturnummer = case_when(
      is.na(registraturnummer) & str_detect(tagesordnungspunkt,point) ~ reg_mod,
      TRUE ~ registraturnummer
    )) %>%
    select(-c(reg_mod,point)) %>%
    ungroup() %>%
    relocate(registraturnummer,.before = tagesordnungspunkt) %>%
    mutate(sprecher = str_remove_all(sprecher,"\\:|\\,")) %>%
    mutate(sprecher_typ = case_when(
      is.na(sprecher_typ) & !is.na(partei) ~ "Mitglied GR",
      TRUE ~ sprecher_typ
    )) %>%
    mutate(sprecher = case_when(
      !str_detect(sprecher_typ,"Präsident|Präsidentin") ~ str_remove(sprecher,sprecher_typ) %>% str_trim(),
      TRUE ~ str_trim(sprecher)
    )) %>%
    mutate(sprecher = str_remove(sprecher, "^in ")) %>%
    mutate(sprecher_typ = str_replace_all(sprecher_typ,"rätin","rat")) %>%
    mutate(sprecher_typ = str_replace_all(sprecher_typ,"dentin","dent"))

  return(pdf_df_mod)
}



#' Prepare (local) PDF File
#'
#' Prepares the PDF data with font information for further processing.
#'
#' @param filepath The path to the local PDF File
#'
#' @return The function returns a dataframe containing the PDF data with font information.

#' @export
#'
prepare_pdf_file <- function(filepath){
  
  # Extact pdf data with font info
  tago_data <- pdftools::pdf_data(filepath, font_info = T)
  # tago_text <- pdftools::pdf_text(temp)
  #
  # text_data <- str_split(tago_text,"\\n|\\s|\\r")[[1]]
  # text_data <- text_data[text_data!=""]
  # text_df <- data.frame(text = text_data,
  #                       order = 1:length(text_data))
  
  
  # Add page variable
  tago_data <- lapply(seq_along(tago_data), function(i){
    tago_data[[i]]$page <- i
    return(tago_data[[i]])
  })
  
  
  # bind rows and add the separator
  full_tago_data <- tago_data %>%
    bind_rows() %>%
    mutate(sep = ifelse(space," ","\n")) %>%
    mutate(text_string = paste0(text,sep))
  
  return(full_tago_data)
  
}



#' Replace German Months with English names to create a proper datetime bject
#'
#' @param date_string  
#'
#' @return string
#'
replace_german_months <- function(date_string) {
  # Named vector with German month names as names and English month names as values
  months <- c(
    Januar = "January", 
    Februar = "February", 
    März = "March", 
    April = "April", 
    Mai = "May", 
    Juni = "June", 
    Juli = "July", 
    August = "August", 
    September = "September", 
    Oktober = "October", 
    November = "November", 
    Dezember = "December"
  )
  
  # Replace each German month name with its English counterpart
  for (month in names(months)) {
    date_string <- str_replace_all(date_string, month, months[[month]])
  }
  
  return(date_string)
}

#' Create data.frame of Mitglieder des Grossen Rats from the respective PDF
#'
#' @param file 
#'
#' @return
#' @export
#'
#' @examples
create_mitglieder_df <- function(file){
  
  info <- pdf_data(file,font_info = T) 
  
  stand_index <- which(info[[1]]$text=="Stand")[1]
  stand <- info[[1]] %>% 
    slice((stand_index+1):(stand_index+3)) %>% 
    pull(text) %>% 
    paste0(collapse = " ") %>% 
    replace_german_months() %>% 
    dmy()
  
  info_full <- lapply(seq_along(info), function(i){
    info[[i]] %>% 
      mutate(page = i)
  }) %>% bind_rows() %>% 
    arrange(page,y,x) #correct order of words
  
  # Get x positions of the variables
  positions <- info_full %>% 
    filter(text %in% c("Name","Vorname","Beruf","Fraktion","GR-Eintritt","E-Mail","Bezirk","Wohnort","Postadresse","Telefon")) %>% 
    distinct(x,text) %>% 
    setNames(c("x","category"))
  
  
  # retrieve the name for grouping the variables later on
  nachname <- info_full %>%  filter(x== 51) %>% 
    mutate(lead_y = lead(y, default = max(info_full$y)))
  
  
  # Prepare tthe info df
  info_mod <- info_full %>% 
    filter(font_size<=7) %>% #remove all titles, pages, etc 
    left_join(nachname) %>% # join for later grouping and identification of the rows
    mutate(sep = ifelse(space," ","\n")) %>% # init separateor (whitespace or newline)
    mutate(lead_y = zoo::na.locf(lead_y)) %>% 
    left_join(positions, by = "x") %>% # Join categories
    mutate(category = zoo::na.locf(category))
  
  
  # Create the content from the single words
  mitglieder_df <- info_mod %>% 
    mutate(text_mod = paste0(text,sep)) %>% 
    group_by(lead_y,category,page) %>% 
    summarise(content = paste0(text_mod,collapse = ""))
  
  
  # Produce  wide data.frame that corresponds to the PDF
  mitglieder_df_wide <- mitglieder_df %>% 
    ungroup() %>% 
    pivot_wider(names_from = category, values_from = content) %>% 
    select(-lead_y,page) %>% 
    mutate_all(~str_remove_all(.x,'\\n(?=[a-z])')) %>% 
    mutate_all(~str_remove_all(.x,'\\n$')) %>% 
    mutate_all(~str_replace_all(.x,'\\n'," ")) %>% 
    filter(Name!= "testuser") %>% 
    select(all_of(positions$category))
  
  list(data = mitglieder_df_wide,
       stand = stand)
  
}




#' Download and Save Mitglieder PDF
#'
#'
download_mitglieder_pdf <- function(){
  url_mitglieder <- "https://parlament.tg.ch/mitglieder/mitgliederliste.html/12745"
  html_mg <- read_html(url_mitglieder)
  
  pdf_link <- html_mg %>% 
    html_elements("a") %>% 
    html_attr("href") %>% 
    str_subset("itglied") %>% 
    str_subset(".pdf")
  
  
  
  
  # Destination file path
  destfile <- "mitglieder.pdf"
  
  # Download the PDF file
  response <- GET(pdf_link, write_disk(destfile, overwrite = TRUE))
}



#' Scrape the webpage with the Mitglieder and retrieve name, party and image link
#'
#' @param url default is https://parlament.tg.ch/mitglieder/mitgliederliste.html/12745
#'
#'
crawl_mitglieder_page <- function(url = "https://parlament.tg.ch/mitglieder/mitgliederliste.html/12745"){
  html_mg <- read_html(url)
  
  entries <- html_mg %>% 
    html_elements("li.mod-entry") 
  
  text_entries <- entries %>% 
    html_elements(".col-3")
  
  img_entries <- entries %>% 
    html_elements(".mod-contact-img") %>% 
    html_attr("src")
  
  df <- lapply(entries, function(x){
    name <- x %>% html_element("h3") %>% html_text()
    partei <- x %>% html_element(".mod-contact-function") %>% html_text() %>% str_extract("(?<=Partei: )\\w+")
    
    data.frame(name=name,partei=partei)
  }) %>% bind_rows() %>% 
    mutate(Img = img_entries) %>% 
    mutate(Name =  str_replace_all(name,intToUtf8(8217),intToUtf8(39)))
  
  return(df)
}



# GRGEKO Scrape
scrape_grgeko <- function(legislatur = 2020) {
  
  # Variablen Laden um Felder zu identifizieren
  variables <- readRDS("vars/variables.rds")
  
  # Datensatz mit Abkürzungen der einzelnen Geschaefte für späteren Join laden
  join_ga <- readRDS("vars/kennung.rds")
  
  # Listen initialisieren
  
  document_list <- list() # Dokumente
  data_list <- list() # Daten zu Pol. Geschaefte
  names_list <- list() # VorstoesserInnen
  
  # Counter für Nummern ohne Dokument (relevant für Abbruchbedingung)
  nodoc_counter <- 0
  
  
  for (i in 1:10000) {
    # print(i)
    # Seite lesen, wenn möglich
    html <- tryCatch({
      rvest::read_html(glue::glue("https://grgeko.tg.ch/view?legislatur={legislatur}&grgnum={i}"))
    },
    error=function(cond) {
      NA
    })
    
    if(is.na(html)) {
      print(glue::glue("Kein Dokument mit grgr-Nummer {i}"))
      nodoc_counter <- nodoc_counter+1
      print(paste0("Bisher ",nodoc_counter," aufeinanderfolgende fehlende grg Nummern"))
      
    }
    
    # Wenn mehr als 20 aufeinander folgende Nummern kein Dokument liefern sind alle Dokumente gescraped -> Funktion bricht ab
    if (nodoc_counter == 40) {
      print("keine weiteren Eintragungen")
      break
    }
    
    # Zur nächsten Nummer, wenn keine Seite existiert
    if (is.na(html)) {
      next
    } 
    
    # Counter nullen wenn Seite gefunden -> zahelung startet von vorne
    nodoc_counter <-0
    
    
    # Alle Felder
    divs = html %>%
      html_nodes(xpath = "//div[contains(@class, 'ui-g-')]")
    
    text = divs %>%
      html_text() %>%
      gsub("\\n", "", .) %>%
      str_trim()
    
    # Wenn keine Felder -> next
    if (length(text) == 0) {
      next
    }
    
    names = html %>%
      html_nodes("div.tg-bc-contact") %>%
      html_nodes("div.ui-g")
    
    # VorstösserInnen extrahieren
    nam_list = lapply(names, function(x) {
      temp <- x %>%
        html_nodes("div.ui-g-2") %>%
        html_text() %>%
        gsub("\\n", "", .) %>%
        gsub("Vorstösser/Vorstösserin:", "", .) %>%
        str_trim()
      
      temp[temp == ""] = NA
      return(temp)
      
    })
    
    names_df = do.call(rbind, nam_list) %>%
      data.frame() %>%
      select(-X1) %>%
      setNames(.[1, ]) %>%
      filter(!row_number() %in% c(1))
    
    # Daten extrahieren
    temp_list = list()
    for (index in 3:(length(text) - 1)) {
      first <- text[index]
      second <- text[index + 1]
      
      if (!first %in% variables) {
        next
      }
      if (second %in% variables) {
        temp_list[[first]] = c(NA)
        next
      } else {
        temp_list[[first]] = c(second)
        
      }
      
      
    }
    
    # Datensatz erstellen
    temp_df <- temp_list %>% bind_rows()
    
    # Titel definieren
    title <- temp_df[variables[10]][[1]]
    
    temp_df$Titel <- title
    
    names_df$Titel <- title
    
    # Dokumente und Links extrahieren
    docs <- html %>%
      html_element("dl.ui-datalist-data") %>% 
      html_elements("a")
    
    doc_title <-
      docs %>% 
      html_text()
    
    if (length(doc_title) == 0) {
      doc_df = data.frame(doc_title = NA,
                          doc_link = NA,
                          titel = title)
      document_list[[title]] <- doc_df
      data_list[[title]] <- temp_df
      next
    }
    
    doc_link <- docs %>%
      html_attr("href") %>% 
      paste0("https://grgeko.tg.ch",.)
    
    doc_df = data.frame(doc_title, doc_link, titel = title)
    
    # Einzelne Datensätze zu Liste hinzufügen
    document_list[[paste0(title,"_",i)]] <- doc_df
    data_list[[paste0(title,"_",i)]] <- temp_df
    names_list[[paste0(title,"_",i)]] <- names_df
    Sys.sleep(.2)
  }
  
  # Zusammenhängende Datensätze erstellen
  documents <- document_list %>% bind_rows() %>% janitor::clean_names()
  data_df <- data_list %>% bind_rows() %>% janitor::clean_names()
  names_df <- names_list %>% bind_rows() %>% janitor::clean_names()
  
  
  data_df$legislatur_nr = legislatur
  data_df <- data_df %>% 
    left_join(join_ga) %>% 
    mutate(registraturnummer  = paste0(legislatur_nr,"/",kennung," ",laufnummer,"/",grg_nummer))
  
  join_reg <- data_df %>% 
    distinct(registraturnummer,titel)
  
  names_df <- names_df %>% 
    left_join(join_reg) %>% 
    tidyr::separate_rows(nachname,vorname,partei,ort, sep = ", ")
  
  documents <- documents %>% 
    left_join(join_reg)
  
  # saveRDS(names_df,paste0("Y:\\SK\\SKStat\\R\\Prozesse\\pol\\Parlamentsdienste\\vorstoesse_gr\\data\\neu\\vorstoesser_",legislatur,".rds"))
  # saveRDS(data_df,paste0("Y:\\SK\\SKStat\\R\\Prozesse\\pol\\Parlamentsdienste\\vorstoesse_gr\\data\\neu\\geschaefte_",legislatur,".rds"))
  # saveRDS(documents,paste0("Y:\\SK\\SKStat\\R\\Prozesse\\pol\\Parlamentsdienste\\vorstoesse_gr\\data\\neu\\dokumente_",legislatur,".rds"))
  
  message(paste0("Dokumente gespeichert unter ",getwd()))
  
  # Datensätze in Liste gepackt zurückgeben
  return(list(data_df,names_df,documents))
}



prepare_ogd_vorstoesse <- function(data_list){
  # Vorstoesser
  vorstoesse <- data_list[[1]] %>% 
    mutate(datum_geschaeft_eingang = lubridate::dmy(eintrittsdatum)) %>% 
    select(datum_geschaeft_eingang,registraturnummer,grg_nummer,geschaftstitel,geschaftsart,kennung,sachbegriff,status,departement) %>% 
    rename(geschaeftsnummer = "registraturnummer")
  
  max_splits <- max(sapply(strsplit(vorstoesse$sachbegriff, ", \\d"), length))
  
  vorstoesse_wide <- vorstoesse %>% 
    separate(sachbegriff, into = paste0("sachbegriff_grgeko_", 1:max_splits), sep = "(?=, \\d)", extra = "merge", fill = "right") %>% 
    mutate_at(vars(str_subset(names(.),"sachbegriff")),~str_remove(.x,"^, ") %>% str_trim())
  
  # Vorstoesse
  vorstoesser <- data_list[[2]] %>% 
    filter(!if_all(c(nachname, vorname, partei), is.na))  %>% 
    rename(geschaeftsnummer = "registraturnummer")
  
  
  
  # 
  # column_names_vs <- colnames(vorstoesser)
  # 
  # max_digit_vs <- column_names_vs %>% str_extract("\\d+") %>% as.numeric() %>% max(na.rm = T)
  # 
  # new_order_vs <- c("nachname","vorname","partei","ort") %>% 
  #   expand.grid(.,1:max_digit_vs) %>% 
  #   mutate(name = paste0(Var1,"_",Var2)) %>% 
  #   pull(name) %>% 
  #   c("titel", "registraturnummer",.)
  # 
  # vorstoesser_wide <- vorstoesser %>% 
  #   select(all_of(new_order_vs))
  # 
  
  # Dokumente
  
  dokumente <- data_list[[3]] %>%
    filter(!is.na(doc_link)) %>% 
    rename(geschaeftsnummer = "registraturnummer")
  
  # group_by(titel,registraturnummer) %>% 
  # mutate(person_id = row_number()) %>%
  # ungroup() %>% 
  # tidyr::pivot_wider(
  #   names_from = person_id,
  #   values_from = c(doc_title,doc_link),
  #   names_sep = "_"
  # ) 
  # 
  
  # column_names_doc <- colnames(vorstoesser)
  
  # max_digit_doc <- column_names_doc %>% str_extract("\\d+") %>% as.numeric() %>% max(na.rm = T)
  # 
  # new_order_doc <- c("doc_title","doc_link") %>% 
  #   expand.grid(.,1:max_digit_doc) %>% 
  #   mutate(name = paste0(Var1,"_",Var2)) %>% 
  #   pull(name) %>% 
  #   c("titel", "registraturnummer",.)
  # 
  # dokumente_wide <- dokumente %>% 
  #   select(all_of(new_order_doc))
  # 
  # 
  # full_vs_data <- vorstoesse_wide %>% 
  #   left_join(vorstoesser_wide, by = c("geschaftstitel"="titel","registraturnummer")) %>% 
  #   left_join(dokumente_wide, by = c("geschaftstitel"="titel","registraturnummer")) %>% 
  #   rename(geschaeftsnummer = "registraturnummer",
  #          grg_nr = "grg_nummer",
  #          vorstoss_bezeichnung = "geschaftstitel",
  #          vorstossart_bezeichnung = "geschaftsart",
  #          vorstossart_kennung = "kennung")
  
  
  write.table(vorstoesser, file = "data/vorstoesser.csv", quote = T, sep = ",", dec = ".", 
              row.names = F, na="",fileEncoding = "utf-8")
  write.table(dokumente, file = "data/dokumente.csv", quote = T, sep = ",", dec = ".", 
              row.names = F, na="",fileEncoding = "utf-8")
  write.table(vorstoesse_wide, file = "data/geschaefte.csv", quote = T, sep = ",", dec = ".", 
              row.names = F, na="",fileEncoding = "utf-8")
  
  return(list(vorstoesser=vorstoesser,dokumente=dokumente,vorstoesse = vorstoesse_wide))
  
  
}




scrape_grgeko_single <- function(legislatur = 2020,grg_num) {
  
  # Variablen Laden um Felder zu identifizieren
  variables <- readRDS("variables.rds")
  
  # Datensatz mit Abkürzungen der einzelnen Geschaefte für späteren Join laden
  join_ga <- readRDS("kennung.rds")
  
  # Listen initialisieren
  
  document_list <- list() # Dokumente
  data_list <- list() # Daten zu Pol. Geschaefte
  names_list <- list() # VorstoesserInnen
  
  # Counter für Nummern ohne Dokument (relevant für Abbruchbedingung)
  nodoc_counter <- 0
  
  
  for (i in grg_num) {
    # print(i)
    # Seite lesen, wenn möglich
    html <- tryCatch({
      rvest::read_html(glue::glue("https://grgeko.tg.ch/view?legislatur={legislatur}&grgnum={i}"))
    },
    error=function(cond) {
      NA
    })
    
    if(is.na(html)) {
      print(glue::glue("Kein Dokument mit grgr-Nummer {i}"))
      nodoc_counter <- nodoc_counter+1
      print(paste0("Bisher ",nodoc_counter," aufeinanderfolgende fehlende grg Nummern"))
      
    }
    
    # Wenn mehr als 20 aufeinander folgende Nummern kein Dokument liefern sind alle Dokumente gescraped -> Funktion bricht ab
    if (nodoc_counter == 40) {
      print("keine weiteren Eintragungen")
      break
    }
    
    # Zur nächsten Nummer, wenn keine Seite existiert
    if (is.na(html)) {
      next
    } 
    
    # Counter nullen wenn Seite gefunden -> zahelung startet von vorne
    nodoc_counter <-0
    
    
    # Alle Felder
    divs = html %>%
      html_nodes(xpath = "//div[contains(@class, 'ui-g-')]")
    
    text = divs %>%
      html_text() %>%
      gsub("\\n", "", .) %>%
      str_trim()
    
    # Wenn keine Felder -> next
    if (length(text) == 0) {
      next
    }
    
    names = html %>%
      html_nodes("div.tg-bc-contact") %>%
      html_nodes("div.ui-g")
    
    # VorstösserInnen extrahieren
    nam_list = lapply(names, function(x) {
      temp <- x %>%
        html_nodes("div.ui-g-2") %>%
        html_text() %>%
        gsub("\\n", "", .) %>%
        gsub("Vorstösser/Vorstösserin:", "", .) %>%
        str_trim()
      
      temp[temp == ""] = NA
      return(temp)
      
    })
    
    names_df = do.call(rbind, nam_list) %>%
      data.frame() %>%
      select(-X1) %>%
      setNames(.[1, ]) %>%
      filter(!row_number() %in% c(1))
    
    # Daten extrahieren
    temp_list = list()
    for (index in 3:(length(text) - 1)) {
      first <- text[index]
      second <- text[index + 1]
      
      if (!first %in% variables) {
        next
      }
      if (second %in% variables) {
        temp_list[[first]] = c(NA)
        next
      } else {
        temp_list[[first]] = c(second)
        
      }
      
      
    }
    
    # Datensatz erstellen
    temp_df <- temp_list %>% bind_rows()
    
    # Titel definieren
    title <- temp_df[variables[10]][[1]]
    
    temp_df$Titel <- title
    
    names_df$Titel <- title
    
    # Dokumente und Links extrahieren
    docs <- html %>%
      html_element("dl.ui-datalist-data") %>% 
      html_elements("a")
    
    doc_title <-
      docs %>% 
      html_text()
    
    if (length(doc_title) == 0) {
      doc_df = data.frame(doc_title = NA,
                          doc_link = NA,
                          titel = title)
      document_list[[title]] <- doc_df
      data_list[[title]] <- temp_df
      next
    }
    
    doc_link <- docs %>%
      html_attr("href") %>% 
      paste0("https://grgeko.tg.ch",.)
    
    doc_df = data.frame(doc_title, doc_link, titel = title)
    
    # Einzelne Datensätze zu Liste hinzufügen
    document_list[[paste0(title,"_",i)]] <- doc_df
    data_list[[paste0(title,"_",i)]] <- temp_df
    names_list[[paste0(title,"_",i)]] <- names_df
    Sys.sleep(.2)
  }
  
  # Zusammenhängende Datensätze erstellen
  documents <- document_list %>% bind_rows() %>% janitor::clean_names()
  data_df <- data_list %>% bind_rows() %>% janitor::clean_names()
  names_df <- names_list %>% bind_rows() %>% janitor::clean_names()
  
  
  data_df$legislatur_nr = legislatur
  data_df <- data_df %>% 
    left_join(join_ga) %>% 
    mutate(registraturnummer  = paste0(legislatur_nr,"/",kennung," ",laufnummer,"/",grg_nummer))
  
  join_reg <- data_df %>% 
    distinct(registraturnummer,titel)
  
  names_df <- names_df %>% 
    left_join(join_reg) %>% 
    tidyr::separate_rows(nachname,vorname,partei,ort, sep = ", ")
  
  documents <- documents %>% 
    left_join(join_reg)
  
  # saveRDS(names_df,paste0("Y:\\SK\\SKStat\\R\\Prozesse\\pol\\Parlamentsdienste\\vorstoesse_gr\\data\\neu\\vorstoesser_",legislatur,".rds"))
  # saveRDS(data_df,paste0("Y:\\SK\\SKStat\\R\\Prozesse\\pol\\Parlamentsdienste\\vorstoesse_gr\\data\\neu\\geschaefte_",legislatur,".rds"))
  # saveRDS(documents,paste0("Y:\\SK\\SKStat\\R\\Prozesse\\pol\\Parlamentsdienste\\vorstoesse_gr\\data\\neu\\dokumente_",legislatur,".rds"))
  
  message(paste0("Dokumente gespeichert unter ",getwd()))
  
  # Datensätze in Liste gepackt zurückgeben
  return(list(data_df,names_df,documents))
}



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



