

cro_api_key <- function(api_key = NULL){
  
  if (!is.null(api_key)) {
    cro_env$api_key <- api_key
    cli::cli_alert_success("Cronos Api Key added!")
  } else {
    return(cro_env$api_key)
  }
}

deBank <- function(address){
  httr::BROWSE(paste0("https://debank.com/profile/", address))
}
