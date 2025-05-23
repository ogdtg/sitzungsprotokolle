#' Function to expand directories on parlament.tg.ch
#'
#' @param category_id the ID of the category that needs to be unpacked
#' @param ajax_url ajay_url needed (default is set to https://parlament.tg.ch/route/cms-index-renderAjaxBlock/blockId/517949/ignore_wrapped_content/1)
#'
#' @return xml nodeset of <tr>
#'
dmsLoadCategory <- function(category_id, ajax_url="https://parlament.tg.ch/route/cms-index-renderAjaxBlock/blockId/517949/ignore_wrapped_content/1") {
  print(category_id)
  # Define the data to be posted
  data_collapsed <- list(dcategory = category_id)
  data_expanded <- list(dcategory = category_id, isInclude = TRUE)
  Sys.sleep(10)
  # POST request when collapsed
  response_collapsed <- POST(ajax_url, body = data_collapsed, encode = "form")
  
  # Check the response
  if (status_code(response_collapsed) != 200) {
    stop("Failed to post data for collapsed state: ", status_code(response_collapsed))
  }
  
  # Parse the response content
  content_collapsed <- content(response_collapsed, as = "text")
  
  fromJSON(content_collapsed)$content %>% 
    rvest::read_html() %>% 
    html_elements("tr")
  
}

#' Retrieve category ID of collapsed folder on parlament.tg.ch
#'
#' @param loaded_cat result of dmsLoadCategory()
#' @param url base url (default is https://parlament.tg.ch/protokolle/sitzungsunterlagen.html/16604)
#'
#' @return data.frame with category_id and its name
#' @export
#'
#' @examples
get_category_id <- function(loaded_cat=NULL, url = "https://parlament.tg.ch/protokolle/sitzungsunterlagen.html/16604"){
  if (is.null(loaded_cat)){
    loaded_cat <- read_html(url) %>% 
      html_elements("tr")
  }
  
  cat_vec <- loaded_cat %>% 
    html_attr("data-category-id")
  
  parcat_vec <- loaded_cat %>% 
    html_attr("data-parentcategory-id")
  
  cat_text <- loaded_cat %>% 
    html_text() %>% 
    stringr::str_replace_all("\\n","") %>% 
    stringr::str_trim() %>% 
    stringr::str_remove("\\s+\\d{1,2}\\. [A-Za-z]+ \\d{4}$")
  
  cat_df <- data.frame(id =cat_vec,text =  cat_text, parent = parcat_vec) %>% 
    filter(!is.na(id))
}


#' Get a list of downloadable PDFs on parlament.tg.ch 
#'
#' @param url base url (default is https://parlament.tg.ch/protokolle/sitzungsunterlagen.html/16604)
#' @param from_date return only documents that belong to a session after this date (default is NULL -> get all available documents)
#'
#' @return data.frame with all documents, the session date and the download URL
#' @export
#'
#' @examples
get_pdf_list <- function(url = "https://parlament.tg.ch/protokolle/sitzungsunterlagen.html/16604", from_date = NULL){
  
  
  # Legislatur
  cat1 <- get_category_id(url = url) %>% 
    mutate_if(is.character, ~str_remove_all(.x, "Ebene \\d+:") %>% str_trim())
  
  loaded_cat_list <- lapply(cat1$id,dmsLoadCategory)
  
  
  # Amtsjahr
  cat2 <- lapply(loaded_cat_list,get_category_id) %>% bind_rows() %>% 
    mutate_if(is.character, ~str_remove_all(.x, "Ebene \\d+:") %>% str_trim()) %>% 
    anti_join(cat1) 
  
  
  loaded_cat_list2 <- lapply(cat2$id,dmsLoadCategory)

  #Sitzungstermine
  cat3 <- lapply(loaded_cat_list2,get_category_id) %>% bind_rows() %>% 
    mutate_if(is.character, ~str_remove_all(.x, "Ebene \\d+:") %>% str_trim()) %>% 
    anti_join(cat1)
  
  
  cat3 <- tryCatch({
    cat3 %>% 
      mutate(text = as.character(as.Date(text)))
  }, error = function(cond){
    cat3 %>% 
      mutate(text = stringr::str_extract(text,"\\d\\d\\d\\d-\\d\\d-\\d\\d"))
  }) %>% distinct()
  
  
  if (!is.null(from_date)){
    cat3 <- cat3 %>% 
      filter(as.Date(text)>as.Date(from_date))
  }
  
  # Dokumente
  loaded_cat_list_final <- lapply(cat3$id,dmsLoadCategory)
  



  
  
  
  pdf_df <- lapply(loaded_cat_list_final, function(x){
    url <- x %>% 
      html_elements("a") %>% 
      html_attr("href") 
    
    index <- which(stringr::str_detect(url,"\\.pdf"))
    
    text <- x %>% 
      html_elements("a") %>% 
      html_text() %>% 
      stringr::str_remove_all("\n") %>% 
      stringr::str_remove_all("\r") %>% 
      stringr::str_trim()
    
    parcat <- x %>% 
      html_attr("data-parentcategory-id") %>% 
      .[!is.na(.)]
    
    data.frame(name = text[index],pdf = url[index], parent = parcat[index])
    
  }) %>% bind_rows() %>% 
    left_join(cat3 %>% 
                select(-parent), by = c("parent"="id"))
  
  return(pdf_df)
  
}



#' Download PDF and retrieve PDF data
#'
#' @param url PDF-Download URL
#'
#' @return data.frame with result of pdftools::pdf_data()
#' @export
#'
#' @examples
crawl_pdf <- function(url){
  
  path <- tempfile(fileext = ".pdf")
  
  # Send the GET request and handle the response
  response <- GET(url)
  
  # Check if the request was successful (status code 200)
  if (status_code(response) == 200) {
    # Save the content as a binary file to avoid corruption
    writeBin(content(response, "raw"), path)
  } else {
    stop("Failed to download PDF: ", status_code(response),"\n",rawToChar(content(response)))
  }
  
  
  # Download the PDF file
  # response <- GET(url, write_disk(path , overwrite = TRUE))
  
  df_list <- pdftools::pdf_data(path, font_info = T)
  unlink(path)
  
  lapply(seq_along(df_list), function(x){
    df_list[[x]]$page <- x
    df_list[[x]]
  }) %>% bind_rows()
}


create_abst_data <- function(pdf_data_abst_red, var_data, substract_one = T,substract_one_nr=F,substract_one_stimme = T,datum,traktandum){
  

  # Prepare Datasets
  pdf_df_red <- pdf_data_abst_red %>% 
    anti_join(var_data)
  
  var_data_mod <- var_data %>% 
    select(text,x) |> 
    setNames(c("cat","x"))
  
  if (str_detect(traktandum,"Präsenzerfassung")){
    return(NULL)
  }
  
  
  result <- fuzzy_left_join(
    pdf_df_red,
    var_data_mod,
    by = c("x" = "x"),
    match_fun = function(x, y) abs(x - y) <= 1
  ) |> 
    mutate(cat = zoo::na.locf(cat)) %>% 
    arrange(page,y) |>
    group_by(page) |> 
    mutate(group_id = cumsum(c(1, diff(y)) > 1)) |>
    mutate(group_id = paste0(page,"_",group_id)) |> 
    ungroup() |>
    group_by(cat,group_id,page,y) %>% 
    summarise(text = paste0(text, collapse = " ")) %>% 
    ungroup() |> 
    arrange(page,y,cat) |> 
    select(-c(page,y)) |> 
    pivot_wider(names_from = cat, values_from = text) %>% 
    select(-group_id) |> 
    mutate(datum = datum,
           geschaeftsnummer = str_extract(traktandum,"\\((\\d.*?\\d)\\)") %>% str_remove("\\(") %>% str_remove("\\)"),
           traktandum = traktandum) %>% 
    mutate_if(is.character,str_trim) %>% 
    mutate_if(is.character,~str_replace_all(.x,'"',"'")) %>% 
    rename(fraktion = "Fraktion",
           name_vorname="Name",
           stimme = "Stimme",
           nr = "Nr.")
  
  if (is.na(result$geschaeftsnummer[1])){
    return(NULL)
  }
  
  has_na <- anyNA(result)
  
  if (has_na){
    stop("Dataframe contains NA")
  } else {
    return(result)
  }
  
}



#' Downloads und prepares Abstimmungs PDF
#'
#' @param url PDF-Download URL
#'
#' @return data.frame with the respective Abstimmung
#' @export
#'
#' @examples
prepare_abstimmung_pdf <- function(url){
  Sys.sleep(5) # to avoid tooo many requestsa
  print(url)
  pdf_data_abst <- crawl_pdf(url)
  
  start_index <- which(pdf_data_abst$text=="Abstimmungsprotokoll")[1]
  if (is.na(start_index)){
    warning(paste0(url," scheint kein Abstimmungsprotokoll zu sein."))
    return(NULL)
  }
  
  traktandum_index <- which(pdf_data_abst$text=="Traktandum:")[1]
  
  traktandum_y <- pdf_data_abst$y[traktandum_index]
  traktandum_font <- pdf_data_abst$font_name[traktandum_index]
  traktandum_size <- pdf_data_abst$font_size[traktandum_index]
  
  
  traktandum <- pdf_data_abst %>% 
    filter(font_name==traktandum_font) %>% 
    filter(font_size==traktandum_size) %>% 
    filter(y<200) %>% 
    pull(text) %>% 
    paste0(collapse = " ") %>% 
    str_remove_all("\\s\\s+")
  
  datum_y <- which(pdf_data_abst$text=="Datum:")[1]
  datum <- pdf_data_abst$text[datum_y+1] %>% 
    as.Date(format = "%d.%m.%Y")
  
  pdf_data_abst_red <- pdf_data_abst %>% 
    slice(-c(1:start_index))
  
  var_data <- pdf_data_abst %>% 
    filter(text %in% c("Name","Nr.","Fraktion","Stimme")&font_size>min(font_size)&page==1) 
  
  if (nrow(var_data)==0){
    warning(traktandum," is not a valid Abstimmung. Maybe the voting software did not work.")
    return(NULL)
  }
  
  if (traktandum=="" & nrow(var_data)){
    warning(url," is not a Abstimmung.")
    return(NULL)
  }

  abst_data <- tryCatch({
    create_abst_data(pdf_data_abst_red, var_data, substract_one = T,substract_one_nr=F,traktandum = traktandum,datum = datum)
  }, error = function(cond){
    tryCatch({
      create_abst_data(pdf_data_abst_red, var_data, substract_one = F,substract_one_nr=F,traktandum = traktandum,datum = datum)
    }, error = function(cond){
      tryCatch({
        create_abst_data(pdf_data_abst_red, var_data, substract_one = F,substract_one_nr=T,traktandum = traktandum,datum = datum)
      }, error = function(cond){
        create_abst_data(pdf_data_abst_red, var_data, substract_one = T,substract_one_nr=T,traktandum = traktandum,datum = datum)
      })
    })
  })
  
  abst_data$url <- url
  
  return(abst_data)
}



get_abstimmungen <- function(mitglieder_df,geschaefte_df = readRDS("data/geschaefte.rds"),pdf_df){
  # pdf_df <- get_pdf_list(from_date = readRDS("data/last_abstimmung.rds"))
  
  # pdf_df_abst <- pdf_df %>% 
  #   filter(str_detect(name,"Trakt"))
  
  pdf_df_abst <- pdf_df %>%
    filter(!str_detect(name,"Präsenz Vormittag$")) |> 
    filter(!str_detect(name,"Präsenz Nachmittag$")) |> 
    filter(!str_detect(name,"Präsenz Abend$")) |> 
    filter(!str_detect(name,"Kurzprotokoll$")) |> 
    filter(!str_detect(name,"Tagesordnung$")) 
  
  
  
  if (nrow(pdf_df_abst)>0){
    abstimmungen_old <- readRDS("data/abstimmungen_ogd.rds") %>% 
      mutate(datum = as.Date(datum))
    
    abstimmungen_new <- lapply(pdf_df_abst$pdf, prepare_abstimmung_pdf) %>%
      bind_rows() %>%
      mutate(traktandum = str_remove(traktandum,"Traktandum:") %>% str_trim()) %>%
      mutate(fraktion = str_replace(fraktion,"SP UND GEW.","SP und Gewerkschaften"),
             fraktion = str_replace(fraktion,"EDU/AUFTG","EDU/Aufrecht")) %>%
      mutate_if(is.character, ~str_replace_all(.x,intToUtf8(39),intToUtf8(8217)))
    
    # Join with Geschaefte df
    abstimmungen_join <- abstimmungen_new %>% 
      left_join(geschaefte_df %>% 
                  mutate(geschaeftsnummer = str_remove(geschaeftsnummer,"^20(?=[0-9])")) %>% 
                  select(geschaeftsnummer,geschaftstitel) %>% 
                  distinct(), by = "geschaeftsnummer") %>% 
      rename(geschaeftstitel = "geschaftstitel")
    
    # Abgleich mit bereits gespeicherten Abstimmungen
    abstimmungen <- abstimmungen_join %>% 
      anti_join(abstimmungen_old, by = c("datum","geschaeftsnummer","traktandum")) %>% 
      bind_rows(abstimmungen_old) |> 
      filter(geschaeftsnummer!="") |> 
      filter(!is.na(geschaeftsnummer)) 
    
    
    # Abgleich mit Mitgliederliste
    abstimmungen_new_mod <- abstimmungen_new %>% 
      left_join(mitglieder_df %>% 
                  mutate(name_vorname=paste0(name," ",vorname)), by = c("name_vorname","fraktion"))
    
    compare_df_new <- abstimmungen_new_mod %>% 
      filter(is.na(name)) %>% 
      distinct(name_vorname,datum,geschaeftsnummer)
    
    
    if (nrow(compare_df_new)>0){
      
      
      
      gnum <- compare_df_new %>% distinct(geschaeftsnummer) %>% pull()
      
      if (length(gnum)>3){
        gnum_string <- "mehreren Geschaeften"
      } else {
        gnum_string <- paste0(gnum, collapse = ", ")
      }
      
      
      
      issue_body <- compare_df_new %>% 
        mutate(issue_body = paste0(name_vorname," (",geschaeftsnummer,")")) %>% 
        pull(issue_body) %>% 
        paste0(.,collapse="\n")
      
      create_gh_issue(
        title = paste0("Abstimmungen: Unbekannte Namen bei ",gnum_string),
        body = paste0(
          "Folgende Mitglieder konnten keinem Eintrag aus der Mitgliederliste zugeordnet werden:\n\n",
          issue_body
        )
      )
      message("GitHub Issue created (Abstimmungen)")
    }

    
    saveRDS(abstimmungen,"data/abstimmungen_ogd.rds")
    write.table(abstimmungen, file = "data/abstimmungen_ogd.csv", quote = T, sep = ",", dec = ".", 
                row.names = F, na="",fileEncoding = "utf-8")
    
    write_parquet(abstimmungen,"parquet/abstimmungen.parquet")
    
    

    saveRDS(max(abstimmungen$datum),"data/last_abstimmung.rds")
  }
}
