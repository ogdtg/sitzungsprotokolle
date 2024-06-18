#' GitHub Issues erstellen
#'
#' @param token 
#' @param owner 
#' @param repo 
#' @param title 
#' @param body 
#' @param assignees 
#' @param milestone 
#' @param labels 
#'
#' @return
#' @export
#'
#' @examples
create_gh_issue <- function(token = Sys.getenv("PAT"), owner ="ogdtg", repo="sitzungsprotokolle", title, body, assignees=list("FLorenz","dkoltg"), milestone=NULL, labels=list("question")) {
  url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/issues")
  
  headers <- c(
    "Accept" = "application/json",
    "Authorization" = paste("Bearer", token),
    "X-GitHub-Api-Version" = "2022-11-28"
  )
  
  body_data <- list(
    title = title,
    body = body,
    assignees = assignees,
    milestone = milestone,
    labels = labels
  )
  response <- POST(url, body = body_data, encode = "json", add_headers(.headers = headers))
  res_list <- response$content %>% rawToChar() %>% jsonlite::fromJSON()
  
  return(res_list$url)
}



repalce_quotes <- function(df){
  df %>% 
    mutate_if(is.character,~str_replace_all(.x,'"',"'"))
}


clean_gnum <- function(df){
  df %>% 
    
}