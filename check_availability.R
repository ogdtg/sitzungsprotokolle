# check_availability 
install.packages("curl")
library(curl)
library(rvest)
library(httr)

# check_availability.R
suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(xml2)
  library(stringr)
  library(curl)
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

cat("=== DNS inside container (system) ===\n")
# getent is available on ubuntu; if not, it's OK
try(system(paste("getent hosts", host), intern = FALSE), silent = TRUE)
cat("\n")

cat("=== 1) Fetch HTML with httr (verbose) ===\n")

ua <- "Mozilla/5.0 (compatible; GitHubActionsAvailabilityCheck/1.0)"
html_text <- NULL

# Use httr GET so we can force verbose output and set timeouts.
# This is where your job may time out if the host is down/unreachable.
res_html <- tryCatch(
  GET(
    url_mitglieder,
    user_agent(ua),
    timeout(60),
    # verbose() prints request/response details to logs
    verbose()
  ),
  error = function(e) e
)

if (inherits(res_html, "error")) {
  cat("ERROR during HTML GET:\n")
  cat(conditionMessage(res_html), "\n")
  cat("\nStopping because we couldn't fetch the HTML page.\n")
  quit(status = 2)
}

cat("\n--- HTML response summary ---\n")
cat("Status: ", status_code(res_html), "\n", sep = "")
cat("Final URL (if available): ", res_html$url %||% "(unknown)", "\n", sep = "")
cat("Headers:\n")
print(headers(res_html))
cat("\n")

if (http_error(res_html)) {
  cat("HTTP error while fetching HTML. Body snippet (first 500 chars):\n")
  body_txt <- tryCatch(content(res_html, as = "text", encoding = "UTF-8"), error = function(e) "")
  cat(substr(body_txt, 1, 500), "\n")
  cat("\nStopping because HTML fetch returned an HTTP error.\n")
  quit(status = 3)
}

html_text <- content(res_html, as = "text", encoding = "UTF-8")
cat("HTML size (chars): ", nchar(html_text), "\n\n", sep = "")

cat("=== 2) Parse HTML and extract PDF link ===\n")
doc <- read_html(html_text)

pdf_links <- doc %>%
  html_elements("a") %>%
  html_attr("href") %>%
  na.omit()

cat("Found ", length(pdf_links), " hrefs total.\n", sep = "")

# Keep your original intent, but make it robust:
# - "itglied" matches "Mitglied..." variations
# - ".pdf" matches PDF links
pdf_candidates <- pdf_links %>%
  str_subset("itglied") %>%
  str_subset("\\.pdf($|\\?)")

cat("PDF candidates matched: ", length(pdf_candidates), "\n", sep = "")
if (length(pdf_candidates) > 0) {
  cat("First 10 candidates:\n")
  print(head(pdf_candidates, 10))
}
cat("\n")

if (length(pdf_candidates) == 0) {
  cat("No PDF link matched your filters.\n")
  cat("This means the page structure or link pattern likely changed.\n")
  quit(status = 4)
}

# Take the first candidate; you can adjust this if multiple are expected
pdf_link_raw <- pdf_candidates[[1]]
pdf_link <- url_absolute(pdf_link_raw, base = base_url)

cat("Selected PDF link (raw): ", pdf_link_raw, "\n", sep = "")
cat("Selected PDF link (abs): ", pdf_link, "\n\n", sep = "")

destfile <- "mitglieder.pdf"

cat("=== 3) Download PDF with curl (very verbose, IPv6 then IPv4) ===\n")
cat("Destination: ", destfile, "\n\n", sep = "")

download_with_ipresolve <- function(ipresolve_value, label) {
  cat("\n--- Attempt download using ", label, " ---\n", sep = "")
  h <- new_handle()
  handle_setopt(
    h,
    url = pdf_link,
    useragent = ua,
    connecttimeout = 30,
    timeout = 120,
    followlocation = 1,
    maxredirs = 10,
    verbose = TRUE,          # CURL-level verbose output (key!)
    ipresolve = ipresolve_value
  )
  
  # Download
  out <- tryCatch(
    curl_download(url = pdf_link, destfile = destfile, handle = h, quiet = TRUE),
    error = function(e) e
  )
  
  if (inherits(out, "error")) {
    cat("Download ERROR (", label, "): ", conditionMessage(out), "\n", sep = "")
    return(FALSE)
  }
  
  # File size check
  if (file.exists(destfile)) {
    fs <- file.info(destfile)$size
    cat("Download OK (", label, "). File size: ", fs, " bytes\n", sep = "")
    return(TRUE)
  } else {
    cat("Download completed but file not found on disk.\n")
    return(FALSE)
  }
}

ok_v6 <- download_with_ipresolve(6, "IPv6")
if (!ok_v6) {
  cat("\nIPv6 failed; trying IPv4...\n")
  ok_v4 <- download_with_ipresolve(4, "IPv4")
  if (!ok_v4) {
    cat("\nBoth IPv6 and IPv4 downloads failed.\n")
    cat("At this point the issue is likely:\n")
    cat("- site outage / reverse proxy problem\n")
    cat("- firewall/rate-limit blocking GitHub runner IP ranges\n")
    cat("- routing problem from the CI network\n")
    quit(status = 5)
  }
}

cat("\n=== Done. PDF downloaded successfully. ===\n")
