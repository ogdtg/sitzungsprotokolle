# Abstimmungen
eval(parse("R/load_packages.R", encoding="UTF-8"))
eval(parse("R/abstimmungen_functions.R", encoding="UTF-8"))
eval(parse("R/xml_functions.R", encoding="UTF-8"))
packageVersion("pdftools")
pdftools::poppler_config()



sitzung <- get_sitzung()
behoerdenmandat <- get_behoerdenmandat()

# Geschäfte
gescaeft <- get_geschaeft()

geschaeft_ogd <- gescaeft$geschaefte |>
  left_join(gescaeft$zustaendigkeit,"guid") |>
  rename(datum_geschaeft_eingang = "eingangsdatum",
         datum_geschaeft_abschluss = "abschlussdatum",
         status = "geschaeftsstatus",
         geschaftstitel = "titel",
         sachbegriff_grgeko_1 = "themenbereich",
         departement = "zustaendigkeit_name",
         grg_nummer = "grg_nr",
         geschaftsart="geschaeftsart",
         anzahl_erstunterzeichnende = "anzahl_vorstoesser",
         total_unterzeichnende = "anzahl_unterzeichnende") |>
  # select(datum_geschaeft_eingang,status,datum_geschaeft_abschluss,geschaeftsnummer,grg_nummer,geschaftstitel,geschaftsart,sachbegriff_grgeko_1,departement,anzahl_erstunterzeichnende,anzahl_mitunterzeichnende,total_unterzeichnende) |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  mutate(across(c(datum_geschaeft_eingang,datum_geschaeft_abschluss,frist_beantwortung,datum_beantwortung),lubridate::dmy)) |>
  mutate(across(c(anzahl_erstunterzeichnende,anzahl_mitunterzeichnende,total_unterzeichnende),as.numeric))
#
#
# sk-stat-140 -> Departement kann über zuständigkeit gejoint werden

# Mitglieder GR
kontakt <- get_kontakt()

mitglieder_ogd <- kontakt$kontakt |>
  filter(organisation=="Grosser Rat") |>
  left_join(kontakt$adresse |>
              filter(adressart=="Privatadresse",
                     inaktiv=="false"),"guid") |>
  distinct() |>
  rename(nr = "personalnummer",
         wohnort = "ort",
         wahlbezirk = "wahlkreis") |>
  left_join(kontakt$behoerdenmandat |>
              filter(gremium_name=="Grosser Rat",
                     funktion=="Mitglied"),"guid") |>
  mutate(eintritt = as.numeric(stringr::str_extract(dauer,"\\d\\d\\d\\d"))) |>
  group_by(guid) |>
  mutate(eintritt = min(eintritt)) |>
  ungroup() |>
  select(-c(dauer,mandat_guid)) |>
  distinct() |>
  mutate(img = glue::glue("https://parlament.tg.ch/de/mitglieder/bild.php?did={guid}-1664&version=1&typ=jpg")) |>
  select(nr,name,vorname,geburtsdatum,geschlecht,beruf,wohnort,wahlbezirk,partei,fraktion,eintritt,img) |>
  mutate(geburtsdatum=lubridate::dmy(geburtsdatum))
#
# # sk-stat-138 -> alle Variablen enthalten, zusätzlich Interessenbindungen und Grmeine/Organisationen
#
# Vorstoesser aus sitzung

erstunterzeichner <- gescaeft$erstunterzeichner |>
  select(guid,benutzer_guid) |>
  left_join(kontakt$kontakt,by = c("benutzer_guid"="guid")) |>
  select(guid,nr = personalnummer,nachname = name,vorname,partei) |>
  mutate(erstunterzeichner = "ja")


mitunterzeichner <- gescaeft$mitvorstoesser |>
  select(guid,benutzer_guid) |>
  left_join(kontakt$kontakt,by = c("benutzer_guid"="guid")) |>
  select(guid,nr = personalnummer,nachname = name,vorname,partei) |>
  mutate(erstunterzeichner = "nein")


unterzeichner <- erstunterzeichner |>
  bind_rows(mitunterzeichner)

vorstoesser <- gescaeft$geschaefte |>
  select(guid,titel,geschaeftsnummer) |>
  filter(guid %in% unterzeichner$guid) |>
  left_join( unterzeichner, "guid") |>
  select(nr,nachname,vorname,partei,geschaeftsnummer,titel,erstunterzeichner)
#
#
sitzungsdokumente <- sitzung$dokumente |>
  select(doc_title = file_name,doc_link=url,guid)


dokumente_ogd <- gescaeft$dokumente |>
  select(geschaeft_guid,doc_title = titel,doc_link = url) |>
  left_join(gescaeft$geschaefte |>
              select(guid,geschaeftstitel=titel,geschaeftsnummer ),by = c("geschaeft_guid"="guid")) |>
  select(-geschaeft_guid)


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
geschaefte_full <-readRDS("data/vorstoesse_full.rds")

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
#
# Vorstösser
# Geschäfte
vorstoesser_full <-readRDS("data/vorstoesser.rds")
vorstoesser_full <-readRDS("data/vorstoesser_full.rds")

vorstoesser_full_mod <- vorstoesser_full |>
  anti_join(vorstoesser, by = c("geschaeftsnummer")) |>
  bind_rows(vorstoesser)
#
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
dokumente_full <-readRDS("data/dokumente_full.rds") 

dokumente_full_mod <- dokumente_full |>
  anti_join(dokumente_ogd, by = c("geschaeftsnummer", "doc_title")) |>
  bind_rows(dokumente_ogd) |>
  mutate(doc_link = ifelse(
    stringr::str_detect(doc_link, "https://grgeko.tg.ch"),
    "https://archivportal.tg.ch/",
    doc_link
  ))

# Daten speichern
saveRDS(dokumente_full_mod,"data/dokumente.rds")
write.table(dokumente_full_mod, file = "data/dokumente.csv", quote = T, sep = ",", dec = ".",
            row.names = F, na="",fileEncoding = "utf-8")
#
#
#
#
# # Abstimmungen
# # eval(parse("R/load_packages.R", encoding="UTF-8"))
# # eval(parse("R/abstimmungen_functions.R", encoding="UTF-8"))
# 
pdf_df_abst <- sitzung$dokumente |>
  filter(str_detect(file_name,"Trakt\\.")) |>
  select(guid,file_name,url) |>
  left_join(sitzung$sitzung,"guid") |>
  mutate(datum = lubridate::dmy(datum)) |>
  select(file_name,url,datum)


get_abstimmungen(mitglieder_df=mitglieder_ogd, pdf_df = pdf_df_abst)


# abst <- readRDS("data/abstimmungen_ogd.rds")
# 
# old_data <- odsAPI::get_dataset(dataset_id="sk-stat-136")
# 
# old_data_red <- old_data |> 
#   filter(datum<min(abst$datum))
# 
# 
# missings <- old_data |> 
#   mutate(datum = as.Date(datum)) |> 
#   filter(datum>=min(abstimmungen_ogd$datum)) |> 
#   anti_join(abstimmungen_ogd,join_by(datum)) |> 
#   mutate(nr = as.character(nr))
# 
# abst_full <- abstimmungen_ogd |> 
#   bind_rows(old_data_red |> 
#               mutate(datum = as.Date(datum),
#                      nr = NA_character_)) |> 
#   bind_rows(missings) |> 
#   arrange(desc(datum))




# abst <- saveRDS(abst_full,"data/abstimmungen_ogd.rds")




# 
# gr_mitglieder <- readRDS("data/gr_mitglieder.rds") |>
#   mutate(name_vorname = str_trim(paste0(name," ",vorname))) |>
#   mutate(fraktion = case_when(
#     fraktion == "SP und Gew." ~ "SP und Gewerkschaften",
#     .default = fraktion
#   ))
# 
# 
# 
# uni_abst <- abst |> 
#   filter(!is.na(nr)) |> 
#   distinct(fraktion,name_vorname,nr)
# 
# 
# unified_name <- uni_abst |> 
#   group_by(name_vorname,fraktion) |> 
#   summarise(nums = paste0(nr,collapse = ", "))
# 
# unique_name_nr <- abst |>
#   group_by(name_vorname,fraktion,nr) |>
#   count() |>
#   ungroup() |>
#   left_join(gr_mitglieder,join_by(nr,name_vorname)) |>
#   filter(!is.na(name) | !name_vorname %in% gr_mitglieder$name_vorname) |>
#   filter(!is.na(nr)) |>
#   filter(!(name_vorname == "Indergand Aline" & nr==360)) |>
#   filter(!(name_vorname == "Schönegger Waltraud" & nr!=428)) |>
#   filter(!(name_vorname == "Pfiffner Müller Martina" & nr==406))  |>
#   select(name_vorname,fraktion = fraktion.x,nr) |>
#   add_row(name_vorname="Forster Urs",fraktion="FDP",nr = "440")
# 
# 
# old_data_red_join <- old_data_red |> 
#     select(-nr) |>
#     left_join(unique_name_nr) |>
#     mutate(nr = case_when(
#       name_vorname=="Auer Jakob" ~ "283",
#       TRUE ~ nr
#     ))

# 
# abst_full <- abst |> 
#   select(-nr) |> 
#   left_join(unique_name_nr) |> 
#   mutate(nr = case_when(
#     name_vorname=="Auer Jakob" ~ "283",
#     TRUE ~ nr
#   )) 
# 
# abst_full |> 
#   group_by(nr) |> 
#   summarise(n = n_distinct(name_vorname)) |> 
#   arrange(desc(n))

# abst <- saveRDS(abst_full,"data/abstimmungen_ogd.rds")


# Kommissionen und Mitglieder
kom <- gescaeft$kommission

kom_ogd


