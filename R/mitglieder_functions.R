
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
    filter(text %in% c("Nr.","Name","Vorname","Beruf","Partei","Fraktion","Eintritt","E-Mail","Bezirk","Wohnort","Postadresse","Telefon","Wahlbezirk")) %>% 
    distinct(x,text) %>% 
    setNames(c("x","category"))
  
  nr_x <- positions %>% 
    filter(category=="Nr.") %>% 
    pull(x)
  
  # retrieve the nr for grouping the variables later on
  nr <- info_full %>%  filter(x== nr_x) %>% 
    mutate(lead_y = lead(y, default = max(info_full$y)))
  
  
  # Prepare tthe info df
  info_mod <- info_full %>% 
    filter(font_size<=7) %>% #remove all titles, pages, etc 
    left_join(nr) %>% # join for later grouping and identification of the rows
    mutate(sep = ifelse(space," ","\n")) %>% # init separateor (whitespace or newline)
    mutate(lead_y = zoo::na.locf(lead_y)) %>% 
    left_join(positions, by = "x") %>% 
    mutate(category = case_when(
      is.na(category) & str_detect(text,"^P$|^M$|^G$") & (str_detect(lead(text),"^\\d\\d\\d$")|str_detect(lead(text),"^\\d\\d$")) ~ "Telefon",
      is.na(category) & str_detect(text,"^P$|^M$|^G$") & str_detect(lead(text),"@") ~ "E-Mail",
      TRUE ~ category
    )) %>% 
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
    mutate_all(~str_replace_all(.x,'Aufre cht',"Aufrecht")) %>% 
    mutate_all(~str_trim(.x)) %>% 
    filter(Name!= "testuser") %>% 
    select(all_of(positions$category))
  
  list(data = mitglieder_df_wide,
       stand = stand)
  
}





#' Download Mitglieder PDF
#'
#' @return
#' @export
#'
#' @examples
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
    partei <- x %>% html_element(".mod-contact-function") %>% html_text() %>% str_extract("(?<=Partei: ).*?(?=,)")
    
    data.frame(name=name,partei=partei)
  }) %>% bind_rows() %>% 
    mutate(Img = img_entries) %>% 
    mutate(Name =  str_replace_all(name,intToUtf8(8217),intToUtf8(39)))
  
  return(df)
}



get_mitglieder <- function(file="mitglieder.pdf"){
  # Download
  download_mitglieder_pdf()
  
  # Dataset
  mitglieder_list <- create_mitglieder_df(file)
  
  # Crawl Page
  scrape_mg <- crawl_mitglieder_page()
  
  
  mitglieder <- mitglieder_list$data %>% 
    mutate_at(vars(Vorname,Name), ~str_replace_all(.x,intToUtf8(8217),intToUtf8(39))) %>% 
    mutate(fullname = paste0(Vorname," ",Name)) %>% 
    left_join(scrape_mg %>% 
                select(-c(Name,partei)), by = c("fullname"="name")) %>% 
    select(-fullname) %>% 
    mutate(datenstand = mitglieder_list$stand) %>% 
    mutate_if(is.character,str_trim) #%>% 
    # mutate(partei = str_replace(partei,"SP","SP und Gewerkschaften"))
  
  names(mitglieder) <- tolower(names(mitglieder)) %>% str_replace_all("\\.","_") %>% str_replace_all("-","_")
  
  
  # Wird nicht mehr benötigt sobald nr auch im Mitgliederfile vorhanden ist
  if (!"nr" %in% names(mitglieder)){
    mitglieder_old <- readRDS("data/mitglieder_full.rds")
    
    mitglieder_mod <- mitglieder %>% left_join(mitglieder_old %>% 
                                                 distinct(nr, name,vorname,partei)) %>% 
      relocate(nr)
  } else {
    mitglieder_mod <- mitglieder
  }
  

    
  
  # Save
  write.table(mitglieder_mod, file = "data/gr_mitglieder.csv", quote = T, sep = ",", dec = ".", 
              row.names = F, na="",fileEncoding = "utf-8")
  write_parquet(mitglieder_mod,"parquet/mitglieder.parquet")
  
  
  saveRDS(mitglieder_mod,"data/gr_mitglieder.rds")
  saveRDS(scrape_mg,"data/scrape_gr_mg.rds")
  message("Mitglieder Data prepared")
  
  return(mitglieder)
}


check_mitglieder <- function(mitglieder_df){
  
  
  # Abgleich ob alle Mitglieder einer Nummer zugeordnet werden können
  # Nach Einführung der Nummer ins PDF kann dies gelöscht/verändert werden
  mitglieder_old <- readRDS("data/mitglieder_full.rds") %>% mutate_if(is.character,str_trim)
  
  mitglieder_join_nr <- mitglieder_df %>% 
    left_join(mitglieder_old %>% 
                select(c("name", "vorname","partei","fraktion","nr")), by = c("name", "vorname","partei","fraktion"))
  
  
  
  # Abgleich und Erstellung einer kompletten Mitgliederliste inklusive Änderungen
  mitglieder_changes_total_old <- readRDS("data/mitglieder_changes_total.rds") %>% mutate_if(is.character,str_trim)
  mitglieder_changes_name_party_fraktion_old <- readRDS("data/mitglieder_changes_name_party_fraktion.rds") %>% mutate_if(is.character,str_trim)
  
  # Gesamtdaten
  mitglieder_full <- mitglieder_join_nr %>% 
    bind_rows(mitglieder_old) %>% 
    distinct(nr,name,vorname,partei,fraktion,.keep_all = T)
  
  # Generelle Änderungen
  mitglieder_changes_total <- mitglieder_join_nr %>% 
    anti_join(mitglieder_old) %>% 
    mutate(change_date = Sys.Date()) 
  
  mitglieder_changes_total_full <- mitglieder_changes_total %>% 
    bind_rows(mitglieder_changes_total_old)
  
  # Änderung an Namen, Partei oder Fraktion
  mitglieder_changes_name_party_fraktion <- mitglieder_join_nr %>% 
    anti_join(mitglieder_old, by = c("nr","name","vorname","partei","fraktion")) %>% 
    mutate(change_date = Sys.Date()) 
  
  mitglieder_changes_name_party_fraktion_full <- mitglieder_changes_name_party_fraktion %>% 
    bind_rows(mitglieder_changes_name_party_fraktion_old)
  
  if (nrow(mitglieder_changes_name_party_fraktion)>0){
    issue_body_mg <- mitglieder_changes_name_party_fraktion %>% 
      mutate(issue_body = paste0(vorname," ",name," (",partei,")", " [",fraktion,"]")) %>% 
      pull(issue_body) %>% 
      paste0(.,collapse = "\n")


    # message("Issue erstellt")
    
    
  } else {
    issue_body_mg <- ""
  }
  
  saveRDS(issue_body_mg, "data/gh_issue_mitgl_issue_body_mg.rds")
  saveRDS(mitglieder_full,"data/mitglieder_full.rds")
  saveRDS(mitglieder_changes_total_full,"data/mitglieder_changes_total.rds")
  saveRDS(mitglieder_changes_name_party_fraktion_full,"data/mitglieder_changes_name_party_fraktion.rds")
  
  message("Mitglieder Check completed")
  
  return(mitglieder_full)
}





#' Creates GitHub Issue in Case Vorstösser oder Mtglieder sind unbekannt
#'
#' @return
#' @export
#'
#' @examples
create_issues <- function(){
  gnum_string <- readRDS("data/gh_issue_vorst_gnum_string.rds")
  issue_body <- readRDS( "data/gh_issue_vorst_issue_body.rds")
  
  if (gnum_string != "" & issue_body != ""){
    create_gh_issue(
      title = paste0("GRGEKO: Unbekannte Vorstoesser bei ",gnum_string),
      body = paste0(
        "Folgende Vorstoesser konnten keinem Eintrag aus der Mitgliederliste zugeordnet werden:\n\n",
        issue_body
      )
    )
  }
  
  
  issue_body_mg <- readRDS( "data/gh_issue_mitgl_issue_body_mg.rds")
  
  if (issue_body_mg != ""){
    create_gh_issue(
      title = paste0("Neue/veraenderte Mitglieder ",Sys.Date()),
      body = paste0(
        "Folgende Eintragungen sind neu oder wurden veraendert:\n\n",
        issue_body_mg
      )
    )
  }
}



#' Mitglieder Daten updaten
#'
#' @description
#' Bereitet die Mitgliederdaten auf. Sollte es zu einem Fehler kommen werden die Daten wieder auf den Ausgangsstand zurückgesetzt
#' 
#' @return data.frame Gesamtmitglieder
#' @export
#'
#' @examples
update_mitglieder <- function(){
  
  scrape_mg_pre <- readRDS("data/scrape_gr_mg.rds")
  mitglieder_ogd_pre <- readRDS("data/gr_mitglieder.rds")
  issue_body_mg_pre <- readRDS( "data/gh_issue_mitgl_issue_body_mg.rds")
  mitglieder_full_pre <- readRDS("data/mitglieder_full.rds")
  mitglieder_changes_total_full_pre <- readRDS("data/mitglieder_changes_total.rds")
  mitglieder_changes_name_party_fraktion_full_pre <- readRDS("data/mitglieder_changes_name_party_fraktion.rds")

  tryCatch({
    mitglieder <- get_mitglieder()
    message("Mitglieder crawled")
    
    mitglieder_full <- check_mitglieder(mitglieder)
    message("Mitglieder checked")
    mitglieder_full
  }, error = function(cond){
    
    saveRDS(scrape_mg_pre,"data/scrape_gr_mg.rds")
    saveRDS(mitglieder_ogd_pre,"data/gr_mitglieder.rds")
    saveRDS(issue_body_mg_pre, "data/gh_issue_mitgl_issue_body_mg.rds")
    saveRDS(mitglieder_full_pre,"data/mitglieder_full.rds")
    saveRDS(mitglieder_changes_total_full_pre,"data/mitglieder_changes_total.rds")
    saveRDS(mitglieder_changes_name_party_fraktion_full_pre,"data/mitglieder_changes_name_party_fraktion.rds")
    
    
    write.table(mitglieder_ogd_pre, file = "data/gr_mitglieder.csv", quote = T, sep = ",", dec = ".", 
                row.names = F, na="",fileEncoding = "utf-8")
    
    stop(paste0("Mitglieder Update schlug fehl:", cond,"\nSämtliche Daten werden auf den Ursprungswert zurückgesetzt."))
  })
  

  
}





