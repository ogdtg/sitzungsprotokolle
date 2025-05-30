# GRGEKO scrape pure

#' GRGEKO scrape pure
#'
#' @param current_legislatur aktuelle Legilsatur
#' @param old_legislatur letzte Legislatur. Aus dieser werden die noch nicht abgeschlossenen Geschäfte gescraped
#'
#' @return
#' @export
#'
#' @examples
scrape_grgeko_pure <- function(current_legislatur,old_legislatur){
  # Variablen Laden um Felder zu identifizieren
  variables <- readRDS("vars/variables.rds")
  
  # Datensatz mit Abkürzungen der einzelnen Geschaefte für späteren Join laden
  join_ga <- readRDS("vars/kennung.rds")
  
  # Listen initialisieren
  
  document_list <- list() # Dokumente
  data_list <- list() # Daten zu Pol. Geschaefte
  names_list <- list() # VorstoesserInnen
  

  
  
  # Vorstossdaten laden
  vorst_df <- readRDS("data/geschaefte.rds") |> distinct()
  
  # GRG Nummer der nicht abgeschlossenen Geschäfte in der letzen Legislaturperiode
  grg_num_old_leg <- vorst_df |> 
    filter(substr(geschaeftsnummer,1,2)==substr(old_legislatur,3,4)) |> 
    filter(status !="abgeschlossen") |> 
    pull(grg_nummer) |> 
    as.numeric() 
  
  for (legislatur in c(old_legislatur,current_legislatur)){
    # Counter für Nummern ohne Dokument (relevant für Abbruchbedingung)
    nodoc_counter <- 0
    if (legislatur == old_legislatur){
      start = min(grg_num_old_leg,na.rm = T)
    } else {
      rm(i)
      start = 1
    }
    for (i in start:10000) {
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
      
      names_fields = html %>%
        html_nodes("div.tg-bc-contact") %>%
        html_nodes("div.ui-g")
      
      # VorstösserInnen extrahieren
      nam_list = lapply(names_fields, function(x) {
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
      
      
      temp_df$legislatur <- substr(legislatur,3,4)
      
      
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
      document_list[[paste0(legislatur,"_",i)]] <- doc_df
      data_list[[paste0(legislatur,"_",i)]] <- temp_df
      names_list[[paste0(legislatur,"_",i)]] <- names_df
      Sys.sleep(.2)
    }
  }
  
  return(list(names_list=names_list,
         document_list = document_list,
         data_list=data_list))
  
}




# GRGEKO Scrape
#' GRGEKO Scrapen
#'
#' @param legislatur 
#'
#' @return
#' @export
#'
#' @examples
scrape_grgeko <- function(current_legislatur,old_legislatur) {
  
  raw_data <- scrape_grgeko_pure(current_legislatur,old_legislatur)
    
  # Datensatz mit Abkürzungen der einzelnen Geschaefte für späteren Join laden
  join_ga <- readRDS("vars/kennung.rds")
  
  # Zusammenhängende Datensätze erstellen
  documents <- raw_data$document_list %>% bind_rows() %>% janitor::clean_names()
  data_df <- raw_data$data_list %>% bind_rows() %>% janitor::clean_names()
  names_df <- raw_data$names_list %>% bind_rows() %>% janitor::clean_names()
  
  
  data_df$legislatur
  data_df <- data_df %>% 
    left_join(join_ga,by ="geschaftsart") %>% 
    mutate(registraturnummer  = paste0(legislatur,"/",kennung," ",laufnummer,"/",grg_nummer))
  
  join_reg <- data_df %>% 
    distinct(registraturnummer,titel)
  
  names_df <- names_df %>% 
    left_join(join_reg,by = "titel") %>% 
    tidyr::separate_rows(nachname,vorname,partei,ort, sep = ", ")
  
  documents <- documents %>% 
    left_join(join_reg,by = "titel")
  
  # saveRDS(names_df,paste0("Y:\\SK\\SKStat\\R\\Prozesse\\pol\\Parlamentsdienste\\vorstoesse_gr\\data\\neu\\vorstoesser_",legislatur,".rds"))
  # saveRDS(data_df,paste0("Y:\\SK\\SKStat\\R\\Prozesse\\pol\\Parlamentsdienste\\vorstoesse_gr\\data\\neu\\geschaefte_",legislatur,".rds"))
  # saveRDS(documents,paste0("Y:\\SK\\SKStat\\R\\Prozesse\\pol\\Parlamentsdienste\\vorstoesse_gr\\data\\neu\\dokumente_",legislatur,".rds"))
  

  # Datensätze in Liste gepackt zurückgeben
  return(list(data_df,names_df,documents))
}



#' Vorstossdatena aufbereiten
#'
#' @param data_list 
#'
#' @return
#' @export
#'
#' @examples
prepare_ogd_vorstoesse <- function(data_list, mitglieder_df){
  # Vorstoesser
  vorstoesse <- data_list[[1]] %>% 
    mutate(datum_geschaeft_eingang = lubridate::dmy(eintrittsdatum)) %>% 
    select(datum_geschaeft_eingang,registraturnummer,grg_nummer,geschaftstitel,geschaftsart,kennung,sachbegriff,status,departement,anzahl_mitunterzeichnende,bemerkungen) %>% 
    rename(geschaeftsnummer = "registraturnummer")
  
  max_splits <- max(sapply(strsplit(vorstoesse$sachbegriff, ", \\d"), length))
  
  vorstoesse_wide <- vorstoesse %>% 
    separate(sachbegriff, into = paste0("sachbegriff_grgeko_", 1:max_splits), sep = "(?=, \\d)", extra = "merge", fill = "right") %>% 
    mutate_at(vars(str_subset(names(.),"sachbegriff")),~str_remove(.x,"^, ") %>% str_trim()) %>% 
    mutate_if(is.character,~str_replace_all(.x,'"',"'"))
  
  
  # Vorstoesse
  vorstoesser <- data_list[[2]] %>% 
    filter(!if_all(c(nachname, vorname, partei), is.na))  %>% 
    rename(geschaeftsnummer = "registraturnummer") %>% 
    mutate_if(is.character,~str_replace_all(.x,'"',"'")) %>% 
    mutate(geschaeftsnummer = str_remove(geschaeftsnummer,"^20(?=[0-9])")) %>% 
    left_join(mitglieder_df %>% 
                rename(nachname = "name") %>% 
                select(nachname,vorname,partei, nr) %>% 
                distinct()) %>% 
    relocate(nr)
    

  
  # Dokumente
  
  dokumente <- data_list[[3]] %>%
    filter(!is.na(doc_link)) %>% 
    rename(geschaeftsnummer = "registraturnummer") %>% 
    mutate_if(is.character,~str_replace_all(.x,'"',"'"))
  
  
  
  old_vorstoesser <- read.csv("data/vorstoesser.csv", stringsAsFactors = FALSE) %>% 
    mutate_all(as.character) %>% 
    mutate_all(~ na_if(., "")) %>% 
    distinct() |> 
    anti_join(vorstoesser,by = "geschaeftsnummer")
  
  old_vorstoesse <- read.csv("data/geschaefte.csv", stringsAsFactors = FALSE) %>% 
    mutate_all(as.character) %>% 
    mutate_all(~ na_if(., "")) %>% 
    distinct() |> 
    anti_join(vorstoesse_wide,by = "geschaeftsnummer")
  
  old_dokumente <- read.csv("data/dokumente.csv", stringsAsFactors = FALSE) %>% 
    mutate_all(as.character) %>%
    mutate_all(~ na_if(., "")) %>%
    distinct() |> 
    anti_join(dokumente,by = "geschaeftsnummer")
  
  
  final_vorstoesser <- vorstoesser %>% 
    mutate_all(~ na_if(., "")) %>% 
    mutate_all(as.character) %>% 
    bind_rows(old_vorstoesser) %>% 
    mutate(geschaeftsnummer = str_remove(geschaeftsnummer,"^20(?=[0-9])")) %>% 
    distinct() 
  
    
  
  final_vorstoesse <- vorstoesse_wide %>% 
    mutate_all(as.character) %>% 
    mutate_all(~ na_if(., "")) %>% 
    bind_rows(old_vorstoesse) %>% 
    mutate(geschaeftsnummer = str_remove(geschaeftsnummer,"^20(?=[0-9])")) |> 
    distinct() %>%
    group_by(geschaeftsnummer) %>%
    mutate(n = n()) %>% 
    ungroup() %>% 
    filter(n==1|(n==2 & !is.na(departement))) %>% 
    select(-n)
  
  
  erst_unterzeichner <- final_vorstoesser |> 
    group_by(geschaeftsnummer) |> 
    summarise(anzahl_erstunterzeichnende = n())
  
  
  final_vorstoesse <- final_vorstoesse |> 
    left_join(erst_unterzeichner, by = "geschaeftsnummer") |> 
    mutate(anzahl_erstunterzeichnende = coalesce(as.numeric(anzahl_erstunterzeichnende.x),anzahl_erstunterzeichnende.y)) |> 
    mutate(anzahl_erstunterzeichnende = ifelse(is.na(anzahl_erstunterzeichnende),0,anzahl_erstunterzeichnende)) |> 
    mutate(total_unterzeichnende = as.numeric(anzahl_erstunterzeichnende)+as.numeric(anzahl_mitunterzeichnende)) |> 
    select(-c(anzahl_erstunterzeichnende.x,anzahl_erstunterzeichnende.y))
  
  
  final_dokumente <- dokumente %>% 
    mutate_all(~ na_if(., "")) %>% 
    mutate_all(as.character) %>% 
    bind_rows(old_dokumente) %>% 
    mutate(geschaeftsnummer = str_remove(geschaeftsnummer,"^20(?=[0-9])"))
  
  
  write.table(final_vorstoesser , file = "data/vorstoesser.csv", quote = T, sep = ",", dec = ".", 
              row.names = F, na="",fileEncoding = "utf-8")
  write.table(final_dokumente, file = "data/dokumente.csv", quote = T, sep = ",", dec = ".", 
              row.names = F, na="",fileEncoding = "utf-8")
  write.table(final_vorstoesse, file = "data/geschaefte.csv", quote = T, sep = ",", dec = ".", 
              row.names = F, na="",fileEncoding = "utf-8")
  
  write_parquet(final_vorstoesser,"parquet/vorstoesser.parquet")
  write_parquet(final_vorstoesse,"parquet/geschaefte.parquet")
  
  
  saveRDS(final_vorstoesse,"data/geschaefte.rds")
  saveRDS(final_vorstoesser,"data/vorstoesser.rds")
  saveRDS(final_dokumente,"data/dokumente.rds")
  
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
    if (nodoc_counter == 25) {
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




get_vorstossdaten <- function(current_legislatur,old_legislatur, mitglieder_df){
  ## GRGEKO Scrape
  geschafte_list <- scrape_grgeko(current_legislatur=current_legislatur,old_legislatur=old_legislatur)
  
  # Daten aufbereiten für OGD
  geschaefte_prep <- prepare_ogd_vorstoesse(geschafte_list,mitglieder_df)
  
  
  if (nrow(geschaefte_prep$vorstoesser)>0){

    
    temp <- geschaefte_prep$vorstoesser 
    
    compare_df_old <- readRDS("data/compare_df.rds")
    
    compare_df <- temp %>% 
      # filter(!is.na(nachname)) %>% 
      distinct(nr,nachname,vorname,partei) %>% 
      anti_join(mitglieder_df %>% 
                  rename(nachname = "name"))
    
    compare_df_new <- compare_df %>% 
      anti_join(compare_df_old) 
    
    existing_issues <- readRDS("data/existing_issues.rds")
    
    compare_df_new <- compare_df_new %>% 
      left_join(geschaefte_prep$vorstoesser) %>% 
      anti_join(existing_issues)
    
    if (nrow(compare_df_new)>0){
      
      
      
      gnum <- compare_df_new %>% distinct(geschaeftsnummer) %>% pull()
      
      if (length(gnum)>3){
        gnum_string <- "mehreren Geschaeften"
      } else {
        gnum_string <- paste0(gnum, collapse = ", ")
      }
      
      
      
      issue_body <- compare_df_new %>% 
        mutate(issue_body = paste0(nachname,", ",vorname," (",geschaeftsnummer,")")) %>% 
        pull(issue_body) %>% 
        paste0(.,collapse="\n")
      
      # create_gh_issue(
      #   title = paste0("GRGEKO: Unbekannte Vorstoesser bei ",gnum_string),
      #   body = paste0(
      #     "Folgende Vorstoesser konnten keinem Eintrag aus der Mitgliederliste zugeordnet werden:\n\n",
      #     issue_body
      #   )
      # )
      

      
      # message("GitHub Issue created (GRGEKO)")
      compare_df_new %>% 
        bind_rows(existing_issues) %>% 
        saveRDS("data/existing_issues.rds")
      
      
      
    } else {
      gnum_string <- ""
      issue_body <- ""
    }
    
  } else {
    gnum_string <- ""
    issue_body <- ""
  }
  saveRDS(gnum_string, "data/gh_issue_vorst_gnum_string.rds")
  saveRDS(issue_body, "data/gh_issue_vorst_issue_body.rds")
  message("GRGEKO data crawled and checked")
  return(geschaefte_prep)
}



# update_vorstossdaten <- function(...){
#   final_vorstoesse_pre <- readRDS("data/geschaefte.rds")
#   final_dokumente_pre <- readRDS("data/dokumente.rds")
#   final_vorstoesser_pre <- readRDS("data/vorstoesser.rds")
#   existing_issues_pre <- readRDS("data/existing_issues.rds")
#   compare_df_pre <- readRDS("data/compare_df.rds")
#   
#   tryCatch({
#     
#   }, error = function(cond){
#     write.table(final_vorstoesser_pre , file = "data/vorstoesser.csv", quote = T, sep = ",", dec = ".", 
#                 row.names = F, na="",fileEncoding = "utf-8")
#     write.table(final_dokumente_pre, file = "data/dokumente.csv", quote = T, sep = ",", dec = ".", 
#                 row.names = F, na="",fileEncoding = "utf-8")
#     write.table(final_vorstoesse_pre, file = "data/geschaefte.csv", quote = T, sep = ",", dec = ".", 
#                 row.names = F, na="",fileEncoding = "utf-8")
#     
#     saveRDS(gnum_string_pre, "data/gh_issue_vorst_gnum_string.rds")
#     saveRDS(issue_body_pre, "data/gh_issue_vorst_issue_body.rds")
#   })
# }
