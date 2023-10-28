
cro_tx_status <- function(txhash = NULL, api_key = cro_api_key()){
  query <- list(module = "transaction", action = "getstatus", txhash = txhash, apikey = api_key)
  out <- Api(url = cro_env$base_url, path = c("api"), query = query)
  output <- dplyr::tibble(
    date = Sys.time(), txhash = txhash, status = out$status, message = out$message, 
    isError = out$result$isError, description = out$result$errDescription
  )
  return(output)
}

cro_tx_receipt_status <- function(txhash = NULL, api_key = cro_api_key()){
  query <- list(module = "transaction", action = "gettxreceiptstatus", txhash = txhash, apikey = api_key)
  out <- Api(url = cro_env$base_url, path = c("api"), query = query)
  output <- dplyr::tibble(date = Sys.time(), txhash = txhash, status = out$status, message = out$message)
  return(output)
}