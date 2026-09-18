# Grosser Rat Kanton Thurgau – OGD Pipeline

Dieses Repository liest Daten des Grossen Rates des Kantons Thurgau über die
offizielle **CMI/CDWS XML-API** von `parlament.tg.ch` aus, verknüpft sie mit den
historischen Beständen und erzeugt daraus maschinenlesbare Datensätze (RDS + CSV)
für die Veröffentlichung als Open Government Data (OGD) auf data.tg.ch.

> **Hinweis:** Die Pipeline wurde von einem reinen PDF-Scraper auf die XML-API
> umgestellt. Zentrales Skript ist heute `work_with_xml_api.R`. Der frühere,
> auf `run_scrape.R` / `extract_functions.R` basierende PDF-Ansatz wird nur noch
> für die Extraktion der **Abstimmungen** aus den Traktanden-PDFs verwendet.

## Automatisierter Lauf

Der Ablauf wird per GitHub Actions (`.github/workflows/check_availability.yml`)
täglich ausgeführt (Cron `0 2 * * *`, ca. 03:00/04:00 Uhr Schweizer Zeit; zusätzlich
manuell via `workflow_dispatch`). Der Job läuft im Container
`florenz2/pdf_scrape:latest_` (enthält R, poppler/pdftools) und führt
`Rscript work_with_xml_api.R` aus. Neu erzeugte Daten unter `data/` werden am Ende
automatisch committet und gepusht.

Benötigte Secrets (als Environment-Variablen gesetzt):

| Secret | Zweck |
|--------|-------|
| `USER_GR_API`, `PW_GR_API` | Basic-Auth für die XML-API von `parlament.tg.ch` |
| `PUSHKEY` | Push-API-Key für data.tg.ch |
| `PAT` | GitHub Token (u. a. für das Eröffnen von Issues) |

## Datenquellen (XML-API)

Die API-Zugriffe sind in `R/xml_functions.R` gekapselt. Alle Endpunkte werden
über `fetch_page()` seitenweise (je 1000 Treffer) abgefragt und über
`numHits` vollständig paginiert:

| Funktion | Endpunkt | Rückgabe |
|----------|----------|----------|
| `get_geschaeft()` | `/api/geschaeft/searchdetails/` | Liste: `geschaefte`, `erstunterzeichner`, `mitvorstoesser`, `kommission`, `zustaendigkeit`, `dokumente` |
| `get_kontakt()` | `/api/kontakt/searchdetails/` | Liste: `kontakt`, `adresse`, `parteizugehoerigkeit`, `interessenbindung`, `behoerdenmandat` |
| `get_behoerdenmandat()` | `/api/behoerdenmandat/searchdetails/` | Behördenmandate (Gremien, Funktion, Dauer) |
| `get_sitzung()` | `/api/sitzung/searchdetails/` | Liste: `sitzung`, `dokumente` (inkl. Download-URLs) |

## Ablauf von `work_with_xml_api.R`

1. **Pakete & Funktionen laden** – `R/load_packages.R`, `R/abstimmungen_functions.R`,
   `R/xml_functions.R`.
2. **Rohdaten abrufen** – Sitzungen, Behördenmandate, Geschäfte und Kontakte über
   die XML-API.
3. **Geschäfte** (`geschaeft_ogd`) – Geschäfte mit Zuständigkeit joinen, Spalten
   umbenennen, Datums- und Zahlenfelder typisieren.
4. **Mitglieder** (`mitglieder_ogd`) – aktive GR-Mitglieder (laufendes Mandat)
   aus Kontakten + Behördenmandaten, angereichert um Privatadresse und Bild-URL.
5. **Vorstösser** (`vorstoesser`) – Erst- und Mitunterzeichnende je Geschäft.
6. **Dokumente** (`dokumente_ogd`) – Geschäfts- und Sitzungsdokumente mit Links;
   GRGEKO-Links werden auf das Archivportal umgeschrieben.
7. **Verknüpfung mit Historie** – die aktuellen Daten werden per `anti_join` +
   `bind_rows` in die historischen Bestände (`data/*_full.rds`) integriert.
8. **Speichern** – Ergebnisse als `.rds` **und** `.csv` (UTF-8) unter `data/`.
9. **Abstimmungen** – aus den Traktanden-PDFs der Sitzungen extrahiert
   (`get_abstimmungen()`), siehe unten.
10. **Kommissionen** (`kom`) und **Interessenbindungen** (`intver`) – aus
    Behördenmandaten bzw. Kontakten aufbereitet und gespeichert.

## Ausgabedateien (`data/`)

| Datei (rds/csv) | Inhalt |
|-----------------|--------|
| `geschaefte` | Alle Geschäfte inkl. Historie |
| `gr_mitglieder` | Aktive Mitglieder des Grossen Rates |
| `vorstoesser` | Erst-/Mitunterzeichnende der Vorstösse |
| `dokumente` | Dokumente zu Geschäften und Sitzungen (mit Links) |
| `abstimmungen_ogd` | Aus PDFs extrahierte Abstimmungsergebnisse |
| `kommission` | Kommissionsmitgliedschaften |
| `intver` | Interessenbindungen der Mitglieder |

`*_full`-Dateien halten den kumulierten Gesamtbestand; die `last_*`-Dateien in
`vars/`/`data/` merken sich den Stand des letzten Laufs.

## Abstimmungen

Die Abstimmungsergebnisse liegen nicht in der API, sondern in den
Traktanden-PDFs (`file_name` enthält `Trakt.`). `get_abstimmungen()`
(`R/abstimmungen_functions.R`) lädt diese PDFs, prüft ob es sich um
Abstimmungsprotokolle handelt, extrahiert die Stimmen und gleicht die Namen mit
der aktuellen Mitgliederliste ab. Ergebnis: `data/abstimmungen_ogd.{rds,csv}`
sowie `data/last_abstimmung.rds`.

**Bekanntes Problem:** Einzelne PDFs lassen sich in der CI-Umgebung nicht
verarbeiten, obwohl dies lokal problemlos funktioniert.

## Qualitätssicherung / Issues

Findet sich für einen Vorstösser oder ein Mitglied kein Treffer beim Abgleich mit
der Mitgliederliste (z. B. wegen abweichender Schreibweise), wird automatisch ein
GitHub-Issue eröffnet (Token `PAT`).

## Repository-Struktur

```
work_with_xml_api.R   # Hauptskript (XML-API-Pipeline)
R/
  load_packages.R          # Pakete laden/installieren
  xml_functions.R          # Zugriff auf die CMI/CDWS XML-API
  abstimmungen_functions.R # PDF-Extraktion der Abstimmungen
  grgeko_functions.R       # GRGEKO-Hilfsfunktionen
  mitglieder_functions.R   # Mitglieder-Aufbereitung
  kommisions_functions.R   # Kommissionen
  sitzungsprotokolle_functions.R
  extract_functions.R      # Legacy: PDF-Scraping der Protokolle
  general_functions.R, archive.R
data/     # Ausgabedaten (rds + csv)
vars/     # Statusdateien des letzten Laufs
.github/workflows/check_availability.yml  # Scheduler
```
