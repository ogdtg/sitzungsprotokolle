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