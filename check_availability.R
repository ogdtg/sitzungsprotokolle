# check_availability.R
suppressPackageStartupMessages({
  library(curl)
  library(rvest)
  library(xml2)
  library(stringr)
})

cat("=== check_availability.R starting ===\n")
cat("Time (UTC): ", format(Sys.time(), tz = "UTC"), "\n", sep = "")
cat("R version: ", R.version.string, "\n", sep = "")
cat("Platform: ", R.version$platform, "\n", sep = "")
cat("\n--- sessionInfo() ---\n")
print(sessionInfo())
cat("\n")

host <- "parlament.tg.ch"
base_url <- "https://parlament.tg.ch/"
url_mitglieder <- "https://parlament.tg.ch/mitglieder/mitgliederliste.html/12745"

cat("Target host: ", host, "\n", sep = "")
cat("Page URL:    ", url_mitglieder, "\n\n", sep = "")

cat("=== curl version ===\n")
print(curl::curl_version())
cat("\n")

cat("=== DNS inside container (system) ===\n")
try(system(paste("getent hosts", host), intern = FALSE), silent = TRUE)
cat("\n")

ua <- "Mozilla/5.0 (compatible; GitHubActionsAvailabilityCheck/1.0)"

# ---------- helpers ----------
retry <- function(fun, tries = 5) {
  for (i in seq_len(tries)) {
    cat("\n== Try ", i, "/", tries, " ==\n", sep = "")
    out <- fun()
    if (!is.null(out)) return(out)
    Sys.sleep(min(30, 2^i))  # 2,4,8,16,30 seconds
  }
  NULL
}

fetch_url_memory_verbose <- function(url) {
  h <- curl::new_handle()
  curl::handle_setopt(
    h,
    useragent = ua,
    connecttimeout = 30,   # increase connect timeout
    timeout = 120,         # overall timeout
    followlocation = 1,
    maxredirs = 10,
    verbose = TRUE
  )
  
  raw <- tryCatch(
    curl::curl_fetch_memory(url, handle = h),
    error = function(e) e
  )
  
  if (inherits(raw, "error")) {
    cat("FETCH ERROR: ", conditionMessage(raw), "\n", sep = "")
    return(NULL)
  }
  
  cat("\n--- Response summary ---\n")
  cat("URL: ", url, "\n", sep = "")
  cat("HTTP status: ", raw$status_code, "\n", sep = "")
  cat("Headers:\n")
  print(raw$headers)
  cat("\n")
  
  raw
}

# ---------- 1) Fetch HTML ----------
cat("=== 1) Fetch HTML page (curl, verbose, retries) ===\n")

raw_html <- retry(function() fetch_url_memory_verbose(url_mitglieder), tries = 5)
if (is.null(raw_html)) {
  cat("\nStopping: couldn't fetch HTML after retries.\n")
  quit(status = 2)
}
if (raw_html$status_code >= 400) {
  cat("\nStopping: HTML returned HTTP error: ", raw_html$status_code, "\n", sep = "")
  quit(status = 3)
}

html_text <- rawToChar(raw_html$content)
cat("HTML size (chars): ", nchar(html_text), "\n\n", sep = "")

# ---------- 2) Parse HTML and extract PDF link ----------
cat("=== 2) Parse HTML and extract PDF link ===\n")

doc <- read_html(html_text)

hrefs <- doc %>%
  html_elements("a") %>%
  html_attr("href") %>%
  na.omit()

cat("Found ", length(hrefs), " hrefs total.\n", sep = "")

pdf_candidates <- hrefs %>%
  # case-insensitive match for Mitglied / mitglied / etc
  str_subset("itglied", ignore_case = TRUE) %>%
  # match .pdf with possible query string
  str_subset("\\.pdf($|\\?)", ignore_case = TRUE)

cat("PDF candidates matched: ", length(pdf_candidates), "\n", sep = "")
if (length(pdf_candidates) > 0) {
  cat("First 10 candidates:\n")
  print(head(pdf_candidates, 10))
}
cat("\n")

if (length(pdf_candidates) == 0) {
  cat("No PDF link matched your filters.\n")
  cat("Likely: page structure changed, link pattern changed, or PDF is loaded dynamically.\n")
  quit(status = 4)
}

pdf_link_raw <- pdf_candidates[[1]]
pdf_link_abs <- xml2::url_absolute(pdf_link_raw, base = base_url)

cat("Selected PDF link (raw): ", pdf_link_raw, "\n", sep = "")
cat("Selected PDF link (abs): ", pdf_link_abs, "\n\n", sep = "")

# ---------- 3) Download PDF ----------
cat("=== 3) Download PDF (curl, verbose, retries) ===\n")

destfile <- "mitglieder.pdf"
cat("Destination: ", destfile, "\n\n", sep = "")

download_pdf_once <- function() {
  h <- curl::new_handle()
  curl::handle_setopt(
    h,
    useragent = ua,
    connecttimeout = 30,
    timeout = 180,
    followlocation = 1,
    maxredirs = 10,
    verbose = TRUE
  )
  
  out <- tryCatch(
    curl::curl_download(url = pdf_link_abs, destfile = destfile, handle = h, quiet = TRUE),
    error = function(e) e
  )
  
  if (inherits(out, "error")) {
    cat("DOWNLOAD ERROR: ", conditionMessage(out), "\n", sep = "")
    return(FALSE)
  }
  
  if (!file.exists(destfile)) {
    cat("Download finished but file does not exist.\n")
    return(FALSE)
  }
  
  fs <- file.info(destfile)$size
  cat("Downloaded file size: ", fs, " bytes\n", sep = "")
  if (is.na(fs) || fs < 1000) {
    cat("Warning: file is very small; might be an HTML error page saved as .pdf.\n")
    return(FALSE)
  }
  
  TRUE
}

ok <- retry(function() if (download_pdf_once()) "OK" else NULL, tries = 5)
if (is.null(ok)) {
  cat("\nStopping: PDF download failed after retries.\n")
  quit(status = 5)
}

cat("\n=== Done. PDF downloaded successfully. ===\n")
