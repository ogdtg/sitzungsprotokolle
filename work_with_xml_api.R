# Abstimmungen
eval(parse("R/load_packages.R", encoding="UTF-8"))
eval(parse("R/abstimmungen_functions.R", encoding="UTF-8"))
eval(parse("R/xml_functions.R", encoding="UTF-8"))



# Alles aus Dokumenten kann über Sitzungen abgezogen werden, Metadaten über Sitzung
# saveRDS(vorstoesser,"api_data/vorstoesser.rds")
# saveRDS(mitglieder_ogd,"api_data/mitglieder.rds")
# saveRDS(geschaeft_ogd,"api_data/geschaeft.rds")
# saveRDS(Sys.Date(),"api_data/placeholder.rds")

# Abstimmungen sind bei den Geschäften 
# Sitzungs gedöns bei Sitzungen
# Bild


# Verbindung mit alten Daten

# Geschäfte 
geschaefte_full <-readRDS("data/geschaefte.rds")
geschaefte_full <- geschaefte_full |> 
  mutate(across(c(anzahl_erstunterzeichnende,anzahl_mitunterzeichnende,total_unterzeichnende),as.numeric)) |> 
  mutate(datum_geschaeft_eingang=as.Date(datum_geschaeft_eingang))


geschaefte_full_mod <- geschaefte_full |> 
  anti_join(geschaeft_ogd, by = c("geschaeftsnummer")) |> 
  bind_rows(geschaeft_ogd)

# Daten speichern
saveRDS(geschaefte_full_mod,"data/geschaefte.rds")
write.table(geschaefte_full_mod, file = "data/geschaefte.csv", quote = T, sep = ",", dec = ".", 
            row.names = F, na="",fileEncoding = "utf-8")



# Mitglieder
saveRDS(mitglieder_ogd,"data/gr_mitglieder.rds")
write.table(mitglieder_ogd, file = "data/gr_mitglieder.csv", quote = T, sep = ",", dec = ".", 
            row.names = F, na="",fileEncoding = "utf-8")

# Vorstösser
# Geschäfte 
vorstoesser_full <-readRDS("data/vorstoesser.rds") 
vorstoesser_full_mod <- vorstoesser_full |> 
  anti_join(vorstoesser, by = c("geschaeftsnummer")) |> 
  bind_rows(vorstoesser)

# Daten speichern
saveRDS(vorstoesser_full_mod,"data/vorstoesser.rds")
write.table(vorstoesser_full_mod, file = "data/vorstoesser.csv", quote = T, sep = ",", dec = ".", 
            row.names = F, na="",fileEncoding = "utf-8")

# Dokumente
# Geschäfte 
dokumente_full <-readRDS("data/dokumente.rds") #|> 
  # rename(geschaeftstitel = titel) |> 
  # mutate(lg = as.numeric(stringr::str_extract(geschaeftsnummer,"\\d\\d"))) |> 
  # filter(lg<16)

dokumente_full_mod <- dokumente_full |> 
  anti_join(dokumente_ogd, by = c("geschaeftsnummer","doc_title")) |> 
  bind_rows(dokumente_ogd) |> 
  mutate(doc_link = case_when(
    stringr::str_detect(doc_link,"https://grgeko.tg.ch") ~ "https://archivportal.tg.ch/",
    TRUE = doc_link
  )) |> 
  select(-lg)

# Daten speichern
saveRDS(dokumente_full_mod,"data/dokumente.rds")
write.table(dokumente_full_mod, file = "data/dokumente.csv", quote = T, sep = ",", dec = ".", 
            row.names = F, na="",fileEncoding = "utf-8")




# Abstimmungen
eval(parse("R/load_packages.R", encoding="UTF-8"))
eval(parse("R/abstimmungen_functions.R", encoding="UTF-8"))

pdf_df_abst <- sitzung$dokumente |> 
  filter(str_detect(file_name,"Trakt\\.")) |> 
  select(guid,file_name,url) |> 
  left_join(sitzung$sitzung,"guid") |> 
  mutate(datum = lubridate::dmy(datum)) |> 
  select(file_name,url,datum)

get_abstimmungen(mitglieder_df=mitglieder_ogd, pdf_df = pdf_df_abst)


