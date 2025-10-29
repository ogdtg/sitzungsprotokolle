# ---------------------------------------------------------
# Hist Geschäfte als JSON
# ---------------------------------------------------------

library(tidyverse)    # Für Datenmanipulation, String-Handling etc.
library(jsonlite)     # Zum Einlesen von JSON-Dateien
library(readxl)       # Zum Einlesen von Excel-Dateien

# Lade die JSON-Datei mit historischen Geschäften
json_data <- jsonlite::fromJSON("hist_geschaefte/test-gr-05.01.x.json")

# Lade Mapping-Datei, die Fabasoft-IDs auf DIMS-Strukturen abbildet
# (erste Zeile wird übersprungen, vermutlich Kopfzeilen)
mapping <- readxl::read_excel("hist_geschaefte/DIMS_Fabasoft_Geschäfte_Mapping_DS.xlsx", skip = 1)


# ---------------------------------------------------------
# Zugriff auf Akten-Container innerhalb der JSON-Struktur
# ---------------------------------------------------------
akten <- json_data$COO.2103.100.5.14061$akten


# ---------------------------------------------------------
# Definition der Geschäftstypen (Code → Bezeichnung)
# ---------------------------------------------------------

gesh_art <- 'code,bezeichnung
10,Parlamentarische Initiative
20,Leistungsmotion
30,Wahlgeschäft
40,Einfache Anfrage
50,Gesetz
60,Einbürgerungsgesuch
70,Interpellation
80,Motion
90,Petition
100,Beschluss
110,Begnadigungsgesuch
120,Antrag nach § 52 bzw. § 46 der Geschäftsordnung
130,Verfassung
140,Volksinitiative
150,Verordnung
160,Weitere Geschäfte
170,Fragestunde
180,Standesinitiative'


# ---------------------------------------------------------
# Definition der Sachbegriffe (Code → Themengebiet)
# getrennt durch Semikolon (;) wegen Kommata in Texten
# ---------------------------------------------------------
sachbegriffe_char <- "code;bezeichnung
10;Bundesverfassung / Menschenrechte
10.1;Kantonsverfassung und Staatsverträge
13.1;Gebiet, Organisation, Reglemente, Verwaltung
13.2;Rechnungswesen, Finanzen, Vermögen, Gebühren
13.3;Uebereinkünfte, Vereinbarungen, Zweckverbände
14.1;Bürgerrecht, Ein- und Ausbürgerungen, Taxen
14.2;Niederlassung, Aufenthalt, Asyl
14.3;Fremdenpolizei, Grenzverkehr, Ausweisschriften
16.1;Initiativen, Referenden
16.2;Abstimmungen
16.3;Majorzwahlen
16.4;Proporzwahlen
16.5;Petitionen
17;Allgemeine Aufsicht, Verwaltungsrechtspflege
17.1;Grosser Rat
17.2;Regierungsrat
17.3;Richterliche Behörden, Anwaltswesen
17.4;Kantonale Zentralverwaltung
17.6;Staatspersonal: generelle Fragen
17.7;Staatspersonal: einzelne Personalentscheide
18.2;Evangelische Landeskirche
18.3;Katholische Landeskirche
21.1;ZGB, EG/ZGB, Vormundschaft, Waisenämter
22.1;Miete, Pacht, Arbeitsvertrag, Handelsregister
22.3;Unlauterer Wettbewerb: UWG, Kartelle, Konsumentenschutz
27;Zivilrechtspflege
28;Schuldbetreibung und Konkurs
31.1;StGB, EG/StGB, Verwaltungsstrafrecht
31.2;Strafverfolgung: Staatsanwaltschaft, Jugendanwaltschaft
33;Strafregister
34.2;Anstalten, Massnahmen, Schutzaufsicht, Opferhilfe, Begnadigung
41;Unterrichtswesen, Lehrerorganisation und -fortbildung
41.1;Unterstufen: Kindergarten, Volks- und Primarschule
41.2;Oberstufen: Real- und Sekundarschulen
41.4;Berufliches Bildungswesen
41.5;Mittelschulen: Kantonsschulen und Seminare
41.6;Hochschulen: Universitäten, Technika, Höhere Fachschule
41.7;Turnen und Sport, J
41.9;Jugendpolitik
43.1;Archive, Statistik
43.2;Bibliotheken, Museen
44;Kultur, Kunst (inklusive Lotteriefonds)
45.1;Natur-, Pflanzen-, Tierschutz
45.2;Heimatschutz
51.1;Militärwesen, Beförderungen, Schiesswesen
51.2;Waffen, Munition (Sprengstoffe - 94.1)
52;inkl. ZFS, Gesamtverteidigung, Notlagen, Alarmierung
55.1;Polizeikorps / interkantonale Zusammenarbeit
55.3;Gewerbepolizei, Marktwesen, Ladenschluss, Ausverkäufe
55.4;Alkohol: Handel, Wirteprüfung, Alkoholzehntel
61;Organisation, Finanzhaushalt allgemein
61.1;Finanzhaushalt, Finanzverwaltung und -kontrolle
61.3;Finanzausgleich
63;Gebühren, Abgaben (GR, RR, SK, Verwaltung und Gerichte)
64.1;Staats- und Gemeindesteuern, allgemeine Organisation
64.2;Sondersteuern: Erbschaft, Schenkungen, Hundesteuer
64.3;Eidgenössische Abgaben
69;Salzregal
70.1;Raumplanung, Richtpläne
70.2;Baugesetz; Planungen Hoch- und Tiefbau, Baulinien
70.3;Feuerschutz, Feuerpolizei
71;Enteignung
72;Submissionswesen Bau, Beschaffungswesen öffentliche Hand
72.1;Wasserwirtschaft, Wasserbau, Öffentliche Gewässer
72.3;Strassenwesen: Gesetz, Strassenbau, Autobahnen
73.1;Allgemeines, Energiegesetz, Kernenergie, Strahlenschutz
73.2;Elektrizität inkl. Versorgung, Wasserkraft, AKW
73.3;Andere Energien: Gas, Fernwärme, Solarenergie etc.
74.1;Strassenverkehr, öffentlicher Verkehr auf Strassen
74.2;Schienenverkehr, Bahnen, NEAT, Bahn 2000, Regional
74.4;Schiffahrt, Wasserfahrzeuge, Steuern
74.5;Luftfahrt, Flugplätze
81;Allgemeines, Organisation, Gesetz, Taxen
81.1;Berufe, Ausbildung, Schulen, Spitäler, Anstalten
81.2;Heilmittel, Betäubungsmittel, Suchtprobleme
81.4;Umweltschutz: Allgemein, Boden, Wasser, Luft, Abfall
81.8;Krankheitsbekämpfung / Unfallverhütung
82.1;Arbeitsvertrag, GAV, NAV, Kollektive Arbeitsstreite
82.2;Arbeitnehmerschutz, Öffentliche Ruhe
82.3;Arbeitsmarkt, Wirtschaftsförderung
83.1;AHV, IV, EL
83.2;Kranken- und Unfallversicherung, Vereinbarungen
83.6;Familien- und Kinderzulagen
83.7;Arbeitslosenversicherung
85;Fürsorge, Massnahmen, Kinderheime, Heimaufsicht
90;Wirtschaftliche Entwicklung
91;Agrarpolitische Massnahmen, bäuerliches Bodenrecht
91.3;Flurwesen, Meliorationen, Unterhalts-Korporationen
91.4;Landw. Kredite, Beihilfen, Beiträge an Betriebsbau
91.5;Pflanzenanbau und -schutz, Rebkorporationen, Engerlingsfonds
91.6;Tierwirtschaftliche Produktion: Versicherungen
91.7;Milchproduktion
91.8;Tierseuchen und -krankheiten, Viehhandel
92.1;Forstwesen
92.2;Jagd und Vogelschutz
92.3;Fischerei
93.2;Dienstleistungen, Tourismus, Lotterien, Sport-Toto
94.2;Preise, Löhne, Gewinne
95.1;TKB, Bankenaufsicht
95.4;EKT (und NOK)
95.6;TGV, Feuerversicherung"



# Parse der CSV-Strings zu Dataframes
gesh_art_df <- read.csv(text = gesh_art, stringsAsFactors = FALSE)
sachbegriffe <- read.csv2(text = sachbegriffe_char, stringsAsFactors = FALSE)

# Lade weitere Zuordnung: Geschäftstypen → Kennungen
kennung_df <- readRDS("/r-proj/stat/pol/sitzungsprotokolle/vars/kennung.rds")


# ---------------------------------------------------------
# JSON-Parser: Extrahiert Daten aus allen Legislaturperioden
# ---------------------------------------------------------

missing_sachbegriff <- list()  # Liste für Fälle, in denen Sachbegriff nicht gefunden wird
vorstoesser_list <- list()     # Liste der Vorstösser:innen (Unterzeichner:innen)
geschaefte <- list()           # Liste der verarbeiteten Geschäfte

# Iteriere über alle Akten (Legislaturperioden)
for (elem in akten) {
  print(elem[["FSCIBIS@15.1400:title"]])  # Ausgabe der aktuellen Legislatur
  
  # Titel der Legislaturperiode (z. B. "1999–2003")
  legislatur <- elem[["FSCIBIS@15.1400:title"]]
  
  # Unterlagen enthalten die eigentlichen Geschäftseinträge
  unterlagen <- elem[["unterlagen"]]
  
  # Überspringe technische oder nicht relevante Akten
  if (legislatur == "Unterlagen GRGEKO") {
    next
  }
  
  # Kürze Legislatur auf Jahr (z. B. "1999")
  leg_short <- stringr::str_extract(legislatur, "^\\d\\d\\d\\d")
  
  # Nur alte Legislaturen vor 2005 berücksichtigen
  if (as.numeric(leg_short) > 2004) {
    next
  }
  
  # Kürze Bezeichnung auf zweistellige Jahreszahl (z. B. "99")
  if (str_starts(leg_short, "19")) {
    leg_short <- leg_short
  } else {
    leg_short <- stringr::str_extract(leg_short, "\\d\\d$")
  }
  
  # -------------------------------------------------------
  # Iteriere über alle Geschäfte innerhalb einer Legislatur
  # -------------------------------------------------------
  for (doc in unterlagen) {
    
    # Grunddaten des Geschäfts
    geschaftstitel <- doc[["FSCIBIS@15.1400:title"]] %>% paste0(collapse = " ")
    datum_geschaeft_eingang <- doc[["FSCIBIS@15.1400:entrydate"]][["objname"]]
    abschlussdatum <- doc[["FSCIBIS@15.1400:finalizedate"]][["objname"]]
    laufnummer <- doc[["FSCIBIS@15.1400:sequentialnumber"]]
    grg_nummer <- doc[["COOELAK@1.1001:ordinal"]]
    
    # Geschäftstyp (z. B. 70 → Interpellation)
    geschaftsart_num <- doc[["FSCIBIS@15.1400:businesstypegr"]]
    geschaftsart <- gesh_art_df$bezeichnung[which(gesh_art_df$code == geschaftsart_num)]
    
    # Kennung (aus externem Mapping, abhängig von Geschäftstyp)
    kennung <- kennung_df$kennung[which(kennung_df$geschaftsart == geschaftsart)]
    
    # Weitere Metadaten
    department <- doc[["FSCIBIS@15.1400:departmentandgr"]][["objname"]]
    vorstoesser <- doc[["FSCIBIS@15.1400:initiants"]]     # Erstunterzeichner:innen
    status <- doc[["FSCFOLIO@1.1001:objdocstate"]]        # Status im Workflow
    
    # Anzahl Mitunterzeichnende (numeric)
    anzahl_mitunterzeichnende <- doc[["FSCIBIS@15.1400:numberofsigners"]] %>% as.numeric()
    
    # Bemerkungen (mehrere Einträge werden zu einem String zusammengefasst)
    bemerkungen <- doc[["COOELAK@1.1001:filenotice"]] %>%
      paste0(collapse = "; ")
    
    # Erzeuge eindeutige Geschäftsnummer (z. B. "99/MO 23/123")
    geschaeftsnummer <- paste0(leg_short, "/", kennung, " ", laufnummer, "/", grg_nummer)
    print(geschaeftsnummer)
    
    # -----------------------------------------------------
    # Verarbeite Unterzeichner:innen (Vorstösser)
    # -----------------------------------------------------
    if (length(vorstoesser) == 0) {
      anzahl_erstunterzeichnende <- 0
    } else {
      # Ergänze Kontextinformationen zum Geschäft
      vorstoesser$geschaeftsnummer <- geschaeftsnummer
      vorstoesser$geschaftstitel <- geschaftstitel
      
      # Zähle Erstunterzeichner:innen
      anzahl_erstunterzeichnende <- nrow(vorstoesser)
      
      # Speichere in globaler Liste
      vorstoesser_list[[geschaeftsnummer]] <- vorstoesser
    }
    
    # Gesamtzahl der Unterzeichnenden
    total_unterzeichnende <- anzahl_mitunterzeichnende + anzahl_erstunterzeichnende
    
    # -----------------------------------------------------
    # Sachbegriff-Zuordnung (Code → Thema)
    # -----------------------------------------------------
    sachbegriff_num <- doc[["FSCIBIS@15.1400:term"]] %>%
      paste0(collapse = "; ")
    
    sachbegriff <- sachbegriffe$bezeichnung[which(sachbegriffe$code == sachbegriff_num)]

    
    # -----------------------------------------------------
    # Zusammenstellung aller Geschäftsdaten in einem Objekt
    # -----------------------------------------------------
    temp_list <- list(
      datum_geschaeft_eingang = datum_geschaeft_eingang,
      datum_geschaeft_abschluss = abschlussdatum,
      geschaeftsnummer = geschaeftsnummer,
      grg_nummer = grg_nummer,
      geschaftstitel = geschaftstitel,
      geschaftsart = geschaftsart,
      kennung = kennung,
      sachbegriff_num = sachbegriff_num,
      status_num = status,
      department = department,
      anzahl_erstunterzeichnende = anzahl_erstunterzeichnende,
      anzahl_mitunterzeichnende = anzahl_mitunterzeichnende,
      total_unterzeichnende = total_unterzeichnende,
      bemerkungen = bemerkungen
    )
    
    # Füge Geschäft zur Gesamtliste hinzu
    geschaefte[[geschaeftsnummer]] <- bind_rows(temp_list)
    
  } # Ende Schleife über Unterlagen (doc)
  
} # Ende Schleife über Akten (Legislaturperioden)

# ---------------------------------------------------------
# Datensätze final aufbereiten
# ---------------------------------------------------------

geschaefte_df <- geschaefte %>% 
  bind_rows() %>% 
  mutate(status = "abgeschlossen") %>% 
  separate(sachbegriff_num, into = c("sachbegriff_grgeko_1","sachbegriff_grgeko_2"),sep = ";") %>% 
  mutate_at(vars(datum_geschaeft_eingang,datum_geschaeft_abschluss),~str_extract(.x,"\\d\\d\\.\\d\\d\\.\\d\\d\\d\\d") %>% dmy()) %>% 
  mutate_if(is.character,~ str_replace_all(.x,'"',"'")) %>% 
  mutate_if(is.character,~ str_replace_all(.x,"',","' ,"))

sachbegriffe <- readxl::read_excel("hist_geschaefte/Sachbegriff Fabasoft.xlsx",sheet =2) %>% 
  select(-Referenzname) %>% 
  mutate_all(as.character)

geschaefte_df_join <- geschaefte_df %>% 
  left_join(sachbegriffe, join_by(sachbegriff_grgeko_1 ==Zahlenwert )) %>% 
  mutate(sachbegriff_grgeko_1=Name) %>% 
  select(-Name)

vorstoesser_df <- vorstoesser_list %>% 
  bind_rows() %>% 
  setNames(c("nachname","vorname","ort","partei","geschaeftsnummer","geschaftstitel")) %>% 
  mutate_if(is.character,~ str_replace_all(.x,'"',"'")) %>% 
  mutate_if(is.character,~ str_replace_all(.x,"',","' ,"))


odsAutomationR::write_ogd2(geschaefte_df_join,file = "hist_geschaefte/hist_geschaefte.csv", na = "",fileEncoding = "utf-8")
odsAutomationR::write_ogd2(vorstoesser_df,file = "hist_geschaefte/hist_vorstoesser.csv", na = "",fileEncoding = "utf-8")
writexl::write_xlsx(geschaefte_df,"hist_geschaefte/geschaefte_1992_2007.xlsx")



###### ZUSATZCODE #####


# vorstoesser_list <- list()     # Liste der Vorstösser:innen (Unterzeichner:innen)
# geschaefte2 <- list()           # Liste der verarbeiteten Geschäfte
# 
# # Neue 
# # Iteriere über alle Akten (Legislaturperioden)
# for (elem in akten) {
#   print(elem[["FSCIBIS@15.1400:title"]])  # Ausgabe der aktuellen Legislatur
#   
#   # Titel der Legislaturperiode (z. B. "1999–2003")
#   legislatur <- elem[["FSCIBIS@15.1400:title"]]
#   
#   # Unterlagen enthalten die eigentlichen Geschäftseinträge
#   unterlagen <- elem[["unterlagen"]]
#   
#   # Überspringe technische oder nicht relevante Akten
#   if (legislatur == "Unterlagen GRGEKO") {
#     next
#   }
#   
#   # Kürze Legislatur auf Jahr (z. B. "1999")
#   leg_short <- stringr::str_extract(legislatur, "^\\d\\d\\d\\d")
#   
#   # Nur alte Legislaturen vor 2005 berücksichtigen
#   if (as.numeric(leg_short) < 2004) {
#     next
#   }
#   
#   # Kürze Bezeichnung auf zweistellige Jahreszahl (z. B. "99")
#   if (str_starts(leg_short, "19")) {
#     leg_short <- leg_short
#   } else {
#     leg_short <- stringr::str_extract(leg_short, "\\d\\d$")
#   }
#   
#   # -------------------------------------------------------
#   # Iteriere über alle Geschäfte innerhalb einer Legislatur
#   # -------------------------------------------------------
#   for (doc in unterlagen) {
#     
#     # Grunddaten des Geschäfts
#     geschaftstitel <- doc[["FSCIBIS@15.1400:title"]] 
#     datum_geschaeft_eingang <- doc[["FSCIBIS@15.1400:entrydate"]][["objname"]]
#     abschlussdatum <- doc[["FSCIBIS@15.1400:finalizedate"]][["objname"]]
#     laufnummer <- doc[["FSCIBIS@15.1400:sequentialnumber"]]
#     grg_nummer <- doc[["COOELAK@1.1001:ordinal"]]
#     
#     # Geschäftstyp (z. B. 70 → Interpellation)
#     geschaftsart_num <- doc[["FSCIBIS@15.1400:businesstypegr"]]
#     geschaftsart <- gesh_art_df$bezeichnung[which(gesh_art_df$code == geschaftsart_num)]
#     
#     # Kennung (aus externem Mapping, abhängig von Geschäftstyp)
#     kennung <- kennung_df$kennung[which(kennung_df$geschaftsart == geschaftsart)]
#     
#     # Weitere Metadaten
#     department <- doc[["FSCIBIS@15.1400:departmentandgr"]][["objname"]]
#     vorstoesser <- doc[["FSCIBIS@15.1400:initiants"]]     # Erstunterzeichner:innen
#     status <- doc[["FSCFOLIO@1.1001:objdocstate"]]        # Status im Workflow
#     
#     # Anzahl Mitunterzeichnende (numeric)
#     anzahl_mitunterzeichnende <- doc[["FSCIBIS@15.1400:numberofsigners"]] %>% as.numeric()
#     
#     # Bemerkungen (mehrere Einträge werden zu einem String zusammengefasst)
#     bemerkungen <- doc[["COOELAK@1.1001:filenotice"]] %>%
#       paste0(collapse = "; ")
#     
#     # Erzeuge eindeutige Geschäftsnummer (z. B. "99/MO 23/123")
#     geschaeftsnummer <- paste0(leg_short, "/", kennung, " ", laufnummer, "/", grg_nummer)
#     print(geschaeftsnummer)
#     
#     # -----------------------------------------------------
#     # Verarbeite Unterzeichner:innen (Vorstösser)
#     # -----------------------------------------------------
#     if (length(vorstoesser) == 0) {
#       anzahl_erstunterzeichnende <- 0
#     } else {
#       # Ergänze Kontextinformationen zum Geschäft
#       vorstoesser$geschaeftsnummer <- geschaeftsnummer
#       vorstoesser$geschaftstitel <- geschaftstitel
#       
#       # Zähle Erstunterzeichner:innen
#       anzahl_erstunterzeichnende <- nrow(vorstoesser)
#       
#       # Speichere in globaler Liste
#       vorstoesser_list[[geschaeftsnummer]] <- vorstoesser
#     }
#     
#     # Gesamtzahl der Unterzeichnenden
#     total_unterzeichnende <- anzahl_mitunterzeichnende + anzahl_erstunterzeichnende
#     
#     # -----------------------------------------------------
#     # Sachbegriff-Zuordnung (Code → Thema)
#     # -----------------------------------------------------
#     sachbegriff_num <- doc[["FSCIBIS@15.1400:term"]] %>%
#       paste0(collapse = "; ")
#     
#     sachbegriff <- sachbegriffe$bezeichnung[which(sachbegriffe$code == sachbegriff_num)]
#     
#     
#     # -----------------------------------------------------
#     # Zusammenstellung aller Geschäftsdaten in einem Objekt
#     # -----------------------------------------------------
#     temp_list <- list(
#       datum_geschaeft_eingang = datum_geschaeft_eingang,
#       datum_geschaeft_abschluss = abschlussdatum,
#       geschaeftsnummer = geschaeftsnummer,
#       grg_nummer = grg_nummer,
#       geschaftstitel = geschaftstitel,
#       geschaftsart = geschaftsart,
#       kennung = kennung,
#       sachbegriff_num = sachbegriff_num,
#       status_num = status,
#       department = department,
#       anzahl_erstunterzeichnende = anzahl_erstunterzeichnende,
#       anzahl_mitunterzeichnende = anzahl_mitunterzeichnende,
#       total_unterzeichnende = total_unterzeichnende,
#       bemerkungen = bemerkungen
#     )
#     
#     # Füge Geschäft zur Gesamtliste hinzu
#     geschaefte2[[geschaeftsnummer]] <- bind_rows(temp_list)
#     
#   } # Ende Schleife über Unterlagen (doc)
#   
# } # Ende Schleife über Akten (Legislaturperioden)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# geschaefte2_df <- geschaefte2 %>% 
#   bind_rows() %>% 
#   mutate(status = "abgeschlossen") %>% 
#   separate(sachbegriff_num, into = c("sachbegriff_grgeko_1","sachbegriff_grgeko_2"),sep = ";") %>% 
#   mutate_at(vars(datum_geschaeft_eingang,datum_geschaeft_abschluss),~str_extract(.x,"\\d\\d\\.\\d\\d\\.\\d\\d\\d\\d") %>% dmy()) %>% 
#   mutate_if(is.character,~ str_replace_all(.x,'"',"'")) %>% 
#   mutate_if(is.character,~ str_replace_all(.x,"',","' ,"))
# 
# 
# curr_data <- odsAPI::get_dataset(dataset_id = "sk-stat-140")
# 
# 
# geschaefte2_df_add <- geschaefte2_df %>% 
#   filter(str_detect(geschaeftsnummer,"^04"))
# 
# 
# curr_data_mod <- curr_data %>% 
#   select(geschaeftsnummer,sachbegriff_grgeko_1)
# 
# 
# 
# trans <- geschaefte2_df %>% 
#   select(geschaeftsnummer,sachbegriff_grgeko_1) %>% 
#   rename(sachbegriff_num = sachbegriff_grgeko_1) %>% 
#   left_join(curr_data_mod) %>% 
#   filter(sachbegriff_num!=sachbegriff_grgeko_1) %>% 
#   distinct(sachbegriff_num,sachbegriff_grgeko_1) %>% 
#   mutate(sachbegriff_char = sachbegriff_num,
#          sachbegriff_num = as.numeric(sachbegriff_num)) %>% 
#   filter(!is.na(sachbegriff_num)) %>% 
#   select(-sachbegriff_num)
# 
# duplicates <- trans %>% 
#   group_by(sachbegriff_char) %>% 
#   mutate(n = n()) %>% 
#   ungroup() %>% 
#   filter(n>1) %>% 
#   arrange(sachbegriff_char)
# 
# 
# trans_no_dup <- trans %>% 
#   filter(!sachbegriff_char %in% duplicates$sachbegriff_char)
# 
# geschaefte_df_full <- geschaefte_df %>% 
#  left_join(trans_no_dup, join_by(sachbegriff_grgeko_1==sachbegriff_char)) %>% 
#   mutate(sachbegriff_grgeko_1 = coalesce(sachbegriff_grgeko_1.y,sachbegriff_grgeko_1)) %>% 
#   select(-sachbegriff_grgeko_1.y)
# 
# 
# missing_sachbegriffe <-geschaefte_df_full %>% 
#   filter(str_detect(sachbegriff_grgeko_1,"^\\d+$"))
# 
# writexl::write_xlsx(missing_sachbegriffe,"hist_geschaefte/geschaefte_1992_2007.xlsx")
# writexl::write_xlsx(duplicates,"hist_geschaefte/sachbegriffe_duplikate.xlsx")
# writexl::write_xlsx(trans,"hist_geschaefte/uebersetzungstabelle.xlsx")
# 
# odsAutomationR::write_ogd2(geschaefte_df_full,file = "hist_geschaefte/hist_geschaefte.csv", na = "",fileEncoding = "utf-8")
