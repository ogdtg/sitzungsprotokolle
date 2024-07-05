# Parliament of the Canton Thurgau Protocol Crawler

This GitHub repository contains a set of functions that crawl protocols from the Parliament of the Canton Thurgau in Switzerland and produce a CSV file for publishing as machine-readable open government data (OGD).

## Scheduled Crawl Run
The `run_scrape.R` script will be executed regularly by using GitHub Actions. In the code, the steps below will be performed:

1. The script begins by loading several R packages: `pdftools`, `dplyr`, `tidyr`, `rvest`, `stringr`, `httr`, and `jsonlite`. These packages provide functions and tools for working with PDFs, manipulating data frames, web scraping, and handling HTTP requests.

2. By calling `eval(parse("R/extract_functions.R", encoding="UTF-8"))` the necessary functions are loaded.

3. The code checks if two files exist: "vars/last_update.rds" (date of the last protocol scraped) and "vars/last_id.rds" (number of the las t protocol).  If the files exist, the code reads the stored values into the variables `last_update` and `last_id`, respectively. If the files don't exist, it sets `last_update` to a date of "1990-01-01".

4. The code calls the `get_current_data()` function, which scrapes the website of the (parliament)[https://parlament.tg.ch/sitzungen-protokolle/ausfuehrliche-protokolle.html/4483] and retrieves information about PDF links to the newest protocol and their corresponding dates. This function uses the `rvest` package to scrape the website's HTML and extract the relevant data.

5. The code compares the date of the retrieved PDF data with the `last_update` variable. If the date of the current data is greater than `last_update`, it proceeds to process the PDF data.

6. The code calls the `prepare_pdf_data()` function, which takes the PDF link from the current data and prepares the PDF data for further processing. This function downloads the PDF file, extracts the font information, and returns a data frame containing the PDF data with font information.

7. The code calls the `extract_speaker_text()` function, which extracts speaker text from the PDF data. This function processes the PDF data and identifies speaker text based on certain patterns and font styles. It returns a data frame containing the extracted speaker text.

8. The code calls the `prepare_text_data()` function, which takes the extracted speaker text, the PDF date, and other data from the current data, and prepares it for further analysis. This function performs various transformations on the data, such as assigning group IDs, extracting speaker types, cleaning up speaker names, and reformatting the data frame.

9. The code prepares the necessary parameters and headers for making an HTTP POST request to a specified the dataset on data.tg.ch via the PUSH API. It uses the `httr` package to send the POST request and includes the prepared text data in JSON format as the request body.

10. The code saves the updated `last_id` and `last_update` values into files "vars/last_id.rds" and "vars/last_update.rds", respectively, using the `saveRDS()` function. These files will be used as the starting point for the next execution of the code.

11. If the date of the current data is not greater than `last_update`, meaning that there are no new protocls to scrape, the code sets the current time as the last run time and saves it into a file "vars/last_run.rds". It also displays a message indicating that there is no new data.


## Functions

### extract_tagesordnung

This function extracts the Tagesordnung (agenda) from a PDF document specified by the `pdf_link` parameter.

```R
extract_tagesordnung <- function(pdf_link)
```

**Parameters:**
- `pdf_link` (string): The URL or file path of the PDF document.

**Returns:**
- A dataframe containing the extracted Tagesordnung.

### prepare_pdf_data

This function prepares the PDF data with font information for further processing.

```R
prepare_pdf_data <- function(pdf_link)
```

**Parameters:**
- `pdf_link` (string): The URL or file path of the PDF document.

**Returns:**
- A dataframe containing the PDF data with font information.

### extract_sitzungsdaten

This function extracts Sitzungsdaten (session data) from PDF data.

```R
extract_sitzungsdaten <- function(pdf_data)
```

**Parameters:**
- `pdf_data` (dataframe): A dataframe containing the PDF data.

**Returns:**
- A dataframe containing the extracted Sitzungsdaten.

### extract_speaker_text

This function extracts speaker text from PDF data.

```R
extract_speaker_text <- function(pdf_data_text)
```

**Parameters:**
- `pdf_data_text` (dataframe): A dataframe containing the PDF text data.

**Returns:**
- A dataframe containing the extracted speaker text.

### get_current_data

This function retrieves the current data (PDF links and dates) from the Parliament of the Canton Thurgau website.

```R
get_current_data <- function()
```

**Returns:**
- A list containing the PDF links, PDF dates, Tagesordnung links, and Tagesordnung dates.


### prepare_text_data

This function prepares the extracted speaker text data for publishing.

```R
prepare_text_data <- function(pdf_df, date)
```

**Parameters:**
- `pdf_df` (dataframe): A dataframe containing the extracted speaker text data.
- `date` (Date): The date of the protocol.

**Returns:**
- A dataframe containing the prepared text data.


# Funktionsweise (05.07.2024)

## Mitglieder

PDF Mitgliederliste wird gescraped und als Master verwendet. Ausserdem wird die Seite der Mitglieder gescraped, um die Partei zuspielen zu können. Gibt es für einen EIntrag in der Mitgliederliste keinen Eintrag auf der Seite, weil z.B. der Name falsch geschrieben ist, so wird ein Issue eröffent.

Alle Einträge die je gescraped wurden, werden in `mitglieder_full.rds` gespeichert

## Geschaefte

Geschäfte werden aus der GRGEKO gescraped. Alle Vorstösser werden mit der aktuellen Mitgliederliste abgegelichen. Wenn es für einen Vorstösser keinen Treffer in der Mitgliederliste gibt, wird ein Issue eröffnet.

## Abstimmungen

Abstimmungen werden aus den PDFs extrahiert. Verwendet werden alle PDFs die "Trakt" im Dateinamen haben. Alle Files werden überprüft, ob es sich dabei um Abstimmungsprotokolle handelt

### Mögliche Probleme
- Manche Files können scheinbar nicht verarbeitet werden. Lokal funktioniert dies allerdings problemlos

## Sitzungsprotokolle (siehe oben)

Sitzungsprotokolle werden nach dem oben beschriebenen Muster aufbereitet und via push API an data.tg.ch gesendet

