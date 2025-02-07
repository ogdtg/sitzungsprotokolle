#' Download PDF
#'
#' @return
#' @export
#'
#' @examples
download_pdf <- function(url){
  
  
  
  # Destination file path
  destfile <- "temp.pdf"
  
  # Download the PDF file
  response <- GET(pdf_link, write_disk(destfile, overwrite = TRUE))
}


extract_kommisions_data <- function(pdf = "temp.pdf"){
  pdf_df <- pdf_data(pdf) %>% bind_rows() %>%
    mutate(y = ifelse(height == 13,y+3,y)) %>%
    mutate(type = case_when(
      str_detect(text,"Präsidium:|Präsident:") ~ "Präsidium",
      str_detect(text,"Mitglieder:") ~ "Mitglied",
      str_detect(text,"Beobachter/in:|Beobachter:") ~ "Beobachter/in",
      str_detect(text,"Vizepräsidium:") ~ "Vizepräsidium",
      TRUE~NA
    )) %>%
    filter(nchar(text) > 1 | text %in% c(",","-")) %>%
    mutate(type = zoo::na.locf(type, na.rm=F)) %>%
    filter(!text %in% c("Präsidium:","Mitglieder:","Beobachter/in:","Vizepräsidium:","Beobachter:","Präsident:")) %>%
    group_by(type,y) %>%
    summarise(full_text = paste0(text,collapse = " ")) %>%
    arrange(y) %>%
    ungroup() %>%
    mutate(dif_y = lead(y)-y) %>%
    mutate(dif_y = replace_na(dif_y,0)) %>%
    mutate(full_text_check = case_when(
      lag(dif_y)<=14 & !str_detect(full_text,"-") ~ TRUE,
      TRUE~FALSE
    )) %>%
    mutate(full_text = case_when(
      lead(full_text_check) ~ paste0(full_text," ",lead(full_text)),
      TRUE~full_text
    )) %>%
    filter(!full_text_check)
  pdf_df$kom_art <- pdf_df$full_text[2]
  if (pdf_df$full_text[2] == "Ständige Kommissionen"){
    pdf_df$kom_name <- pdf_df$full_text[3]
  } else {
    index <- which(str_detect(pdf_df$full_text,"bestellt:$"))+1
    pdf_df$kom_name <- pdf_df$full_text[index]
  }
  # Gehe davon aus dass es nur einen Beobachter gibt
  result <- tryCatch({
    pdf_df %>%
      filter(!is.na(type)) %>%
      mutate(
        name_vorname = str_extract(full_text, "^[^,]+") %>% str_remove("^-") %>% str_trim(),
        fraktion = NA
      ) %>%
      filter(!(lag(type)==type & lag(dif_y>20))) %>%
      select(kom_name, kom_art, type, name_vorname, fraktion) %>%
      mutate(stand = Sys.Date())
  }, error = function(cond){
  })
  return(result)
}


scrape_st_komissions_pdf <- function(url = "https://parlament.tg.ch/organe/kommissionen/staendige-kommissionen.html/12217"){
  st_kom_urls <- read_html(url) %>%
    html_element("main") %>%
    html_elements("a") %>%
    html_attr("href") %>%
    str_subset("https://parlament.tg.ch/organe/kommissionen")
  lapply(st_kom_urls, function(x){
    pdf_link_base <-  x %>%
      read_html() %>%
      html_element("main") %>%
      html_elements("a")
    pdf_names <- pdf_link_base %>% html_text()
    pdf_link <- pdf_link_base[which(str_detect(pdf_names,"^Mitglieder"))] %>%
      html_attr("href")
    download_pdf(pdf_link)
    extract_kommisions_data()
  })
}



extract_kommisions_data2 <- function(pdf = "temp.pdf"){
  pdf_data <- pdf_data(pdf) %>% bind_rows() %>%
    mutate(y = ifelse(height == 13,y+3,y)) %>%
    mutate(type = case_when(
      str_detect(text,"Präsidium:|Präsident:") ~ "Präsidium",
      str_detect(text,"Mitglieder:") ~ "Mitglied",
      str_detect(text,"Beobachter/in:|Beobachter:") ~ "Beobachter/in",
      str_detect(text,"Vizepräsidium:") ~ "Vizepräsidium",
      TRUE~NA
    ))
}

scrape_spez_komissions_pdf <- function(url = "https://parlament.tg.ch/organe/kommissionen/spezialkommissionen.html/12218"){
  spez <- read_html(url) %>%
    html_element("main") %>%
    html_elements("a") %>%
    html_attr("href")
  lapply(spez, function(x){
    print(x)
    download_pdf(x)
    extract_kommisions_data()
  })
}




scrape_spez_komissions_pdf()
