packages <- c("xml2", "tibble", "purrr", "dplyr")

installed <- rownames(installed.packages())
missing <- packages[!packages %in% installed]

if (length(missing) > 0) install.packages(missing)

library(xml2)
library(tibble)
library(purrr)
library(dplyr)
library(httr)
auth <- function(){
  httr::authenticate(Sys.getenv("USER_GR_API"),Sys.getenv("PW_GR_API"))
}



auth2 <- function(){
  httr::authenticate("demo","demo")
}

fetch_page <- function(base_url, s, page_size = 1000) {
  url <- paste0(base_url, "?q=seq>0&l=de-CH&s=", s, "&m=", page_size)
  url |>
    httr::GET(auth()) |>
    httr::content()
}


test_connection <- function(base_url="https://tg.gemeinde.ch/api/geschaeft/searchdetails/"){
  url <- paste0(base_url, "?q=seq>0&l=de-CH&s=", 0, "&m=", 10)
  res <- url |>
    httr::GET(auth())
  print(res)
}


pw <- Sys.getenv("PW_GR_API") |>
  stringr::str_replace_all("\\\\", "\\") |>
  stringr::str_replace_all('\\"', '"')

Sys.setenv(PW_GR_API = pw)

print("PW correct length?")
print(nchar(Sys.getenv("PW_GR_API")))
print(grepl('"', Sys.getenv("PW_GR_API")))

test_connection()

get_geschaeft_base <- function(hits,ns){
  df <- map_dfr(hits, ~ {
    g <- xml_find_first(.x, "gs:Geschaeft", ns)

    get_t <- function(path) xml_text(xml_find_first(g, path, ns))

    tibble(
      guid                       = xml_attr(.x, "Guid"),
      titel                      = get_t("gs:Titel"),
      geschaeftsnummer           = get_t("gs:Geschaeftsnummer"),
      grg_nr                     = get_t("gs:GRGNr"),
      ga_laufnummer              = get_t("gs:GALaufnummer"),
      geschaeftsstatus           = get_t("gs:Geschaeftsstatus"),
      geschaeftsart              = get_t("gs:Geschaeftsart"),
      beginn                     = get_t("gs:Beginn/gs:Text"),
      anzahl_vorstoesser         = get_t("gs:AnzahlVorstoesser"),
      anzahl_mitunterzeichnende  = get_t("gs:AnzahlMitunterzeichnende"),
      anzahl_unterzeichnende     = get_t("gs:AnzahlUnterzeichnende"),
      legislatur                 = get_t("gs:Legislatur"),
      eingangsdatum              = get_t("gs:Eingangsdatum/gs:Text"),
      abschlussdatum             = get_t("gs:Abschlussdatum/gs:Text"),
      themenbereich              = get_t("gs:Themenbereich"),
      status_gr                  = get_t("gs:StatusGR"),
      frist_beantwortung         = get_t("gs:FristBeantwortung/gs:Text"),
      datum_beantwortung         = get_t("gs:DatumBeantwortung/gs:Text"),
      dringlich                  = get_t("gs:Dringlich"),
      erheblichkeitserklaerung   = get_t("gs:Erheblichkeitserklaerung"),
      teilerheblicherklaerung    = get_t("gs:Teilerheblicherklaerung"),
      diskussion                 = get_t("gs:Diskussion"),
      vorlaeutige_unterstuetzung = get_t("gs:VorlaeufigeUnterstuetzung"),
      abschreibung               = get_t("gs:Abschreibung"),
      rueckzug                   = get_t("gs:Rueckzug"),
      entlastung                 = get_t("gs:Entlastung"),
      volksabstimmung            = get_t("gs:Volksabstimmung"),
      datum_erheblicherklaerung  = get_t("gs:DatumErheblicherklaerung/gs:Text"),
      frist_bericht              = get_t("gs:FristBericht/gs:Text"),
      datum_behoerdenreferendum  = get_t("gs:DatumBehoerdenreferendum/gs:Text"),
      datum_volksabstimmung      = get_t("gs:DatumVolksabstimmung/gs:Text"),
      pendent_bei                = get_t("gs:PendentBei"),
      traktanden                 = get_t("gs:Traktanden")
    )
  })
}


get_bhmandat_base <- function(hits, ns) {
  map_dfr(hits, ~ tibble(
    guid        = xml_attr(.x, "Guid"),
    name        = xml_text(xml_find_first(.x, "bm:Behordenmandat/bm:Name", ns)),
    vorname     = xml_text(xml_find_first(.x, "bm:Behordenmandat/bm:Vorname", ns)),
    kontakt_uid        = xml_text(xml_find_first(.x, "bm:Behordenmandat/bm:KontaktGuid", ns)),
    start       = xml_text(xml_find_first(.x, "bm:Behordenmandat/bm:Dauer/bm:Start", ns)),
    end         = xml_text(xml_find_first(.x, "bm:Behordenmandat/bm:Dauer/bm:End", ns)),
    gremium     = xml_text(xml_find_first(.x, "bm:Behordenmandat/bm:Gremium", ns)),
    gremium_uid     = xml_text(xml_find_first(.x, "bm:Behordenmandat/bm:GremiumGuid", ns)),

    gremiumstyp = xml_text(xml_find_first(.x, "bm:Behordenmandat/bm:Gremiumstyp", ns)),
    funktion    = xml_text(xml_find_first(.x, "bm:Behordenmandat/bm:Funktion", ns)),
    wahlkreis   = xml_text(xml_find_first(.x, "bm:Behordenmandat/bm:Wahlkreis", ns)),
    partei      = xml_text(xml_find_first(.x, "bm:Behordenmandat/bm:Partei", ns))
  ))
}





get_erstunterzeichner <- function(hits, ns) {
  map_dfr(hits, ~ {
    g <- xml_find_first(.x, "gs:Geschaeft", ns)
    b <- xml_find_first(g, "gs:Erstunterzeichner/gs:Benutzer", ns)
    if (is.na(b)) return(NULL)

    tibble(
      guid            = xml_attr(.x, "Guid"),
      name            = xml_text(xml_find_first(b, "gs:Name", ns)),
      vorname         = xml_text(xml_find_first(b, "gs:Vorname", ns)),
      benutzer_guid   = xml_text(xml_find_first(b, "gs:BenutzerGUID", ns)),
      partei          = xml_text(xml_find_first(b, ".//gs:Partei", ns)),
      partei_kurzname = xml_text(xml_find_first(b, ".//gs:Kurzname", ns))
    )
  })
}

get_mitvorstoesser <- function(hits, ns) {
  map_dfr(hits, ~ {
    geschaeft_guid <- xml_attr(.x, "Guid")
    g <- xml_find_first(.x, "gs:Geschaeft", ns)
    benutzer <- xml_find_all(g, "gs:Mitvorstoesser/gs:Benutzer", ns)
    if (length(benutzer) == 0) return(NULL)

    map_dfr(benutzer, ~ tibble(
      guid          = geschaeft_guid,
      name          = xml_text(xml_find_first(.x, "gs:Name", ns)),
      vorname       = xml_text(xml_find_first(.x, "gs:Vorname", ns)),
      benutzer_guid = xml_text(xml_find_first(.x, "gs:BenutzerGUID", ns))
    ))
  })
}

get_kommission <- function(hits, ns) {
  map_dfr(hits, ~ {
    geschaeft_guid <- xml_attr(.x, "Guid")
    g <- xml_find_first(.x, "gs:Geschaeft", ns)
    kommissionen <- xml_find_all(g, "gs:Kommission/gs:Gremium", ns)
    if (length(kommissionen) == 0) return(NULL)

    map_dfr(kommissionen, ~ tibble(
      guid            = geschaeft_guid,
      kommission_name = xml_text(xml_find_first(.x, "gs:Name", ns)),
      kommission_guid = xml_text(xml_find_first(.x, "gs:KommissionGUID", ns))
    ))
  })
}

get_zustaendigkeit <- function(hits, ns) {
  map_dfr(hits, ~ {
    geschaeft_guid <- xml_attr(.x, "Guid")
    g <- xml_find_first(.x, "gs:Geschaeft", ns)
    zustaendigkeiten <- xml_find_all(g, "gs:Zustaendigkeit/gs:Organisationseinheit", ns)
    if (length(zustaendigkeiten) == 0) return(NULL)

    map_dfr(zustaendigkeiten, ~ tibble(
      guid                = geschaeft_guid,
      zustaendigkeit_name = xml_text(xml_find_first(.x, "gs:Name", ns)),
      zustaendigkeit_kurz = xml_text(xml_find_first(.x, "gs:Kurzname", ns)),
      zustaendigkeit_guid = xml_text(xml_find_first(.x, "gs:ZustaendigkeitGUID", ns))
    ))
  })
}

get_geschaeft <- function(size = NULL) {
  base_url <- "https://tg.gemeinde.ch/api/geschaeft/searchdetails/"

  ns <- c(
    sr = "http://www.cmiag.ch/cdws/searchDetailResponse",
    gs = "http://www.cmiag.ch/cdws/Geschaeft"
  )
  if (is.null(size) || size > 1000) {
    first_doc <- fetch_page(base_url, s = 0)
    num_hits <- if (is.null(size)) {
      as.integer(xml_attr(xml_root(first_doc), "numHits"))
    } else {
      min(size, as.integer(xml_attr(xml_root(first_doc), "numHits")))
    }

    starts <- seq(1001, num_hits, by = 1000)

    all_docs <- c(
      list(first_doc),
      map(starts, ~ fetch_page(base_url, s = .x))
    )
  } else {
    all_docs <- list(fetch_page(base_url, s = 0, page_size = size))
  }

  hits <- map(all_docs, ~ xml_find_all(.x, "//sr:Hit", ns)) |>
    purrr::reduce(c)

  list(
    geschaefte        = get_geschaeft_base(hits, ns),
    erstunterzeichner = get_erstunterzeichner(hits, ns),
    mitvorstoesser    = get_mitvorstoesser(hits, ns),
    kommission        = get_kommission(hits, ns),
    zustaendigkeit    = get_zustaendigkeit(hits, ns)
  )
}




get_behoerdenmandat <- function(size = NULL) {
  base_url <- "https://tg.gemeinde.ch/api/behoerdenmandat/searchdetails/"

  ns <- c(
    sr = "http://www.cmiag.ch/cdws/searchDetailResponse",
    bm = "http://www.cmiag.ch/cdws/Behoerdenmandat"
  )

  if (is.null(size) || size > 1000) {
    first_doc <- fetch_page(base_url, s = 0)
    num_hits <- if (is.null(size)) {
      as.integer(xml_attr(xml_root(first_doc), "numHits"))
    } else {
      min(size, as.integer(xml_attr(xml_root(first_doc), "numHits")))
    }

    starts <- seq(1001, num_hits, by = 1000)

    all_docs <- c(
      list(first_doc),
      map(starts, ~ fetch_page(base_url, s = .x))
    )
  } else {
    all_docs <- list(fetch_page(base_url, s = 0, page_size = size))
  }

  hits <- map(all_docs, ~ xml_find_all(.x, "//sr:Hit", ns)) |>
    purrr::reduce(c)

  get_bhmandat_base(hits,ns)
}




get_kontakt_base <- function(hits, ns) {
  map_dfr(hits, ~ {
    k <- xml_find_first(.x, "kt:Kontakt", ns)
    get_t <- function(path) xml_text(xml_find_first(k, path, ns))

    tibble(
      guid              = xml_attr(.x, "Guid"),
      name              = get_t("kt:Name"),
      vorname           = get_t("kt:Vorname"),
      organisation      = get_t("kt:Organisation"),
      partei            = get_t("kt:Partei"),
      fraktion          = get_t("kt:Fraktion"),
      fraktion_guid     = get_t("kt:FraktionGUID"),
      beruf             = get_t("kt:Beruf"),
      geburtsdatum      = get_t("kt:Geburtsdatum/kt:Text"),
      geschlecht        = get_t("kt:Geschlecht"),
      email_geschaeft   = get_t("kt:EmailGeschaeft"),
      email_privat      = get_t("kt:EmailPrivat"),
      telefon_geschaeft = get_t("kt:TelefonGeschaft"),
      telefon_privat    = get_t("kt:TelefonPrivat"),
      mobile            = get_t("kt:Mobile"),
      homepage          = get_t("kt:Homepage"),
      wahlkreis         = get_t("kt:Wahlkreis"),
      wahlkreis_guid    = get_t("kt:WahlkreisGUID"),
      personalnummer    = get_t("kt:Personalnummer")
    )
  })
}

get_kontakt_adresse <- function(hits, ns) {
  map_dfr(hits, ~ {
    kontakt_guid <- xml_attr(.x, "Guid")
    k <- xml_find_first(.x, "kt:Kontakt", ns)
    adressen <- xml_find_all(k, "kt:Adresse/kt:Adresse", ns)
    if (length(adressen) == 0) return(NULL)

    map_dfr(adressen, ~ tibble(
      guid       = kontakt_guid,
      strasse    = xml_text(xml_find_first(.x, "kt:Strasse", ns)),
      plz        = xml_text(xml_find_first(.x, "kt:PLZ", ns)),
      ort        = xml_text(xml_find_first(.x, "kt:Ort", ns)),
      adressart  = xml_text(xml_find_first(.x, "kt:Adressart", ns)),
      inaktiv    = xml_text(xml_find_first(.x, "kt:inaktiv", ns))
    ))
  })
}

get_kontakt_parteizugehoerigkeit <- function(hits, ns) {
  map_dfr(hits, ~ {
    kontakt_guid <- xml_attr(.x, "Guid")
    k <- xml_find_first(.x, "kt:Kontakt", ns)
    parteien <- xml_find_all(k, "kt:Parteizugehoerigkeit/kt:Parteizugehoerigkeit", ns)
    if (length(parteien) == 0) return(NULL)

    map_dfr(parteien, ~ tibble(
      guid       = kontakt_guid,
      partei     = xml_text(xml_find_first(.x, "kt:Partei", ns)),
      kurzname   = xml_text(xml_find_first(.x, "kt:Kurzname", ns)),
      funktion   = xml_text(xml_find_first(.x, "kt:Funktion", ns)),
      dauer      = xml_text(xml_find_first(.x, "kt:Dauer/kt:Text", ns)),
      partei_guid = xml_text(xml_find_first(.x, "kt:ParteiGuid", ns))
    ))
  })
}

get_kontakt_interessenbindung <- function(hits, ns) {
  map_dfr(hits, ~ {
    kontakt_guid <- xml_attr(.x, "Guid")
    k <- xml_find_first(.x, "kt:Kontakt", ns)
    bindungen <- xml_find_all(k, "kt:Interessenbindung/kt:Interessenbindung", ns)
    if (length(bindungen) == 0) return(NULL)

    map_dfr(bindungen, ~ tibble(
      guid         = kontakt_guid,
      funktion     = xml_text(xml_find_first(.x, "kt:Funktion", ns)),
      beschreibung = xml_text(xml_find_first(.x, "kt:Beschreibung", ns)),
      dauer        = xml_text(xml_find_first(.x, "kt:Dauer/kt:Text", ns))
    ))
  })
}

get_kontakt_behoerdenmandat <- function(hits, ns) {
  map_dfr(hits, ~ {
    kontakt_guid <- xml_attr(.x, "Guid")
    k <- xml_find_first(.x, "kt:Kontakt", ns)
    mandate <- xml_find_all(k, "kt:Behoerdenmandat/kt:Behoerdenmandat", ns)
    if (length(mandate) == 0) return(NULL)

    map_dfr(mandate, ~ tibble(
      guid         = kontakt_guid,
      mandat_guid  = xml_attr(.x, "OBJ_GUID"),
      gremium_name = xml_text(xml_find_first(.x, "kt:Gremium/kt:Gremium/kt:Name", ns)),
      gremium_kurz = xml_text(xml_find_first(.x, "kt:Gremium/kt:Gremium/kt:Kurzname", ns)),
      gremium_typ  = xml_text(xml_find_first(.x, "kt:Gremium/kt:Gremium/kt:GremiumTyp", ns)),
      gremium_guid = xml_text(xml_find_first(.x, "kt:Gremium/kt:Gremium/kt:GremiumGuid", ns)),
      funktion     = xml_text(xml_find_first(.x, "kt:Funktion", ns)),
      dauer        = xml_text(xml_find_first(.x, "kt:Dauer/kt:Text", ns))
    ))
  })
}

get_kontakt <- function(size = NULL) {
  base_url <- "https://tg.gemeinde.ch/api/kontakt/searchdetails/"
  ns_kt <- c(
    sr = "http://www.cmiag.ch/cdws/searchDetailResponse",
    kt = "http://www.cmiag.ch/cdws/Kontakt"
  )

  if (is.null(size) || size > 1000) {
    first_doc <- fetch_page(base_url, s =0)
    num_hits <- if (is.null(size)) {
      as.integer(xml_attr(xml_root(first_doc), "numHits"))
    } else {
      min(size, as.integer(xml_attr(xml_root(first_doc), "numHits")))
    }

    if (num_hits <= 1000) {
      all_docs <- list(first_doc)
    } else {
      starts <- seq(1001, num_hits, by = 1000)
      all_docs <- c(
        list(first_doc),
        map(starts, ~ fetch_page(base_url, s = .x))
      )
    }

  } else {
    all_docs <- list(fetch_page(base_url, s = 0, page_size = size))
  }

  hits <- map(all_docs, ~ xml_find_all(.x, "//sr:Hit", ns_kt)) |>
    purrr::reduce(c)

  list(
    kontakt              = get_kontakt_base(hits, ns_kt),
    adresse              = get_kontakt_adresse(hits, ns_kt),
    parteizugehoerigkeit = get_kontakt_parteizugehoerigkeit(hits, ns_kt),
    interessenbindung    = get_kontakt_interessenbindung(hits, ns_kt),
    behoerdenmandat      = get_kontakt_behoerdenmandat(hits, ns_kt)
  )
}


get_sitzung_base <- function(hits, ns) {
  map_dfr(hits, ~ {
    s <- xml_find_first(.x, "sz:Sitzung", ns)
    get_t <- function(path) xml_text(xml_find_first(s, path, ns))

    tibble(
      guid               = xml_attr(.x, "Guid"),
      sitzungsstatus     = get_t("sz:Sitzungsstatus"),
      freigabe           = get_t("sz:Freigabe"),
      datum              = get_t("sz:Datum/sz:Text"),
      beginn             = get_t("sz:Beginn"),
      ende               = get_t("sz:Ende"),
      sitzungsdauer      = get_t("sz:Sitzungsdauer"),
      titel              = get_t("sz:Titel"),
      sitzungsort        = get_t("sz:Sitzungsort"),
      sitzungsort_gebaeude = get_t("sz:SitzungsortGebaeude"),
      gremium_name       = get_t("sz:Gremium/sz:Gremium/sz:Name"),
      gremium_guid       = xml_attr(xml_find_first(s, "sz:Gremium/sz:Gremium", ns), "OBJ_GUID")
    )
  })
}

get_sitzung_dokumente <- function(hits, ns) {
  map_dfr(hits, ~ {
    sitzung_guid <- xml_attr(.x, "Guid")
    s <- xml_find_first(.x, "sz:Sitzung", ns)
    dokumente <- xml_find_all(s, "sz:Sitzungsdokumente/sz:Dokument", ns)
    if (length(dokumente) == 0) return(NULL)

    map_dfr(dokumente, ~ {
      dok_guid  <- xml_attr(.x, "OBJ_GUID")
      file_node <- xml_find_first(.x, "sz:File", ns)

      # Alle Versionen holen und neueste wählen
      versions  <- xml_find_all(.x, "sz:File/sz:Version", ns)
      if (length(versions) == 0) return(NULL)

      version_nrs  <- as.integer(xml_attr(versions, "Nr"))
      latest_version <- versions[[which.max(version_nrs)]]
      version_nr   <- max(version_nrs)

      renditions <- xml_find_all(latest_version, "sz:Rendition", ns)

      if (length(renditions) == 0) {
        return(tibble(
          guid       = sitzung_guid,
          dok_guid   = dok_guid,
          laufnummer = xml_text(xml_find_first(.x, "sz:Laufnummer", ns)),
          titel      = xml_text(xml_find_first(.x, "sz:Titel", ns)),
          kategorie  = xml_text(xml_find_first(.x, "sz:Kategorie", ns)),
          sortierung = xml_text(xml_find_first(.x, "sz:Sortierung", ns)),
          file_name  = xml_attr(file_node, "FileName"),
          file_id    = xml_attr(file_node, "ID"),
          version_nr = version_nr,
          extension  = NA_character_,
          ansicht    = NA_character_
        ))
      }

      map_dfr(renditions, ~ tibble(
        guid       = sitzung_guid,
        dok_guid   = dok_guid,
        laufnummer = xml_text(xml_find_first(.x, "sz:Laufnummer", ns)),
        titel      = xml_text(xml_find_first(.x, "sz:Titel", ns)),
        kategorie  = xml_text(xml_find_first(.x, "sz:Kategorie", ns)),
        sortierung = xml_text(xml_find_first(.x, "sz:Sortierung", ns)),
        file_name  = xml_attr(file_node, "FileName"),
        file_id    = xml_attr(file_node, "ID"),
        version_nr = version_nr,
        extension  = xml_attr(.x, "Extension"),
        ansicht    = xml_attr(.x, "Ansicht")
      ))
    })
  })
}

get_sitzung <- function(size = NULL) {

  ns_sz <- c(
    sr = "http://www.cmiag.ch/cdws/searchDetailResponse",
    sz = "http://www.cmiag.ch/cdws/Sitzung"
  )
  base_url <- "https://tg.gemeinde.ch/api/sitzung/searchdetails/"

  if (is.null(size) || size > 1000) {
    first_doc <- fetch_page(base_url, s = 0)
    num_hits <- if (is.null(size)) {
      as.integer(xml_attr(xml_root(first_doc), "numHits"))
    } else {
      min(size, as.integer(xml_attr(xml_root(first_doc), "numHits")))
    }

    if (num_hits <= 1000) {
      all_docs <- list(first_doc)
    } else {
      starts <- seq(1001, num_hits, by = 1000)
      all_docs <- c(
        list(first_doc),
        map(starts, ~ fetch_page(base_url, s = .x))
      )
    }
  } else {
    all_docs <- list(fetch_page(base_url, s = 0, page_size = size))
  }

  hits <- map(all_docs, ~ xml_find_all(.x, "//sr:Hit", ns_sz)) |>
    purrr::reduce(c)

  dokumente <- get_sitzung_dokumente(hits, ns_sz) |>
    mutate(url = glue::glue("https://tg.gemeinde.ch/de/politik/cdws/dok.php?did={file_id}&v={version_nr}&r={ansicht}&typ={extension}"))

  list(
    sitzung   = get_sitzung_base(hits, ns_sz),
    dokumente=dokumente
  )
}

sitzung <- get_sitzung()
behoerdenmandat <- get_behoerdenmandat()

# Geschäfte
gescaeft <- get_geschaeft()

geschaeft_ogd <- gescaeft$geschaefte |> 
  left_join(gescaeft$zustaendigkeit,join_by(guid)) |> 
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
  select(datum_geschaeft_eingang,status,datum_geschaeft_abschluss,geschaeftsnummer,grg_nummer,geschaftstitel,geschaftsart,sachbegriff_grgeko_1,departement,anzahl_erstunterzeichnende,anzahl_mitunterzeichnende,total_unterzeichnende) |> 
  mutate(across(where(is.character), ~ na_if(.x, ""))) |> 
  mutate(across(c(datum_geschaeft_eingang,datum_geschaeft_abschluss),lubridate::dmy))

# sk-stat-140 -> Departement kann über zuständigkeit gejoint werden

# Mitglieder GR
kontakt <- get_kontakt()

mitglieder_ogd <- kontakt$kontakt |> 
  filter(organisation=="Grosser Rat") |> 
  left_join(kontakt$adresse |> 
              filter(adressart=="Privatadresse"),join_by(guid)) |> 
  distinct() |> 
  rename(nr = "personalnummer",
         wohnort = "ort",
         wahlbezirk = "wahlkreis") |>
  left_join(kontakt$behoerdenmandat |> 
              filter(gremium_name=="Grosser Rat",
                     funktion=="Mitglied"),join_by(guid)) |> 
  mutate(eintritt = as.numeric(stringr::str_extract(dauer,"\\d\\d\\d\\d"))) |> 
  group_by(guid) |> 
  mutate(eintritt = min(eintritt)) |> 
  ungroup() |> 
  select(-c(dauer,mandat_guid)) |> 
  distinct() |> 
  mutate(img = glue::glue("https://tg.gemeinde.ch/de/mitglieder/bild.php?did={guid}-1664&version=1&typ=jpg")) |> 
  select(nr,name,vorname,geburtsdatum,geschlecht,beruf,wohnort,wahlbezirk,partei,fraktion,eintritt,img) |> 
  mutate(geburtsdatum=lubridate::dmy(geburtsdatum)) 
  

# sk-stat-138 -> alle Variablen enthalten, zusätzlich Interessenbindungen und Grmeine/Organisationen

# Vorstoesser aus sitzung

erstunterzeichner <- gescaeft$erstunterzeichner |> 
  select(guid,benutzer_guid) |> 
  left_join(kontakt$kontakt,join_by(benutzer_guid==guid)) |> 
  select(guid,nr = personalnummer,nachname = name,vorname,partei) |> 
  mutate(erstunterzeichner = "ja")


mitunterzeichner <- gescaeft$mitvorstoesser |> 
  select(guid,benutzer_guid) |> 
  left_join(kontakt$kontakt,join_by(benutzer_guid==guid)) |> 
  select(guid,nr = personalnummer,nachname = name,vorname,partei) |> 
  mutate(erstunterzeichner = "nein")


unterzeichner <- erstunterzeichner |> 
  bind_rows(mitunterzeichner)

vorstoesser <- gescaeft$geschaefte |> 
  select(guid,titel,geschaeftsnummer) |> 
  filter(guid %in% unterzeichner$guid) |> 
  left_join( unterzeichner, join_by(guid)) |> 
  select(nr,nachname,vorname,partei,geschaeftsnummer,geschaeftstitel = titel,erstunterzeichner)



# Alles aus Dokumenten kann über Sitzungen abgezogen werden, Metadaten über Sitzung
saveRDS(vorstoesser,"api_data/vorstoesser.rds")
saveRDS(mitglieder_ogd,"api_data/mitglieder.rds")
saveRDS(geschaeft_ogd,"api_data/geschaeft.rds")


# Abstimmungen sind bei den Geschäften 
# Sitzungs gedöns bei Sitzungen
# Bild