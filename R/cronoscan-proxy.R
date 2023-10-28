cro_last_block <- function(api_key = cro_api_key()){
  query <- list(module = "proxy", action = 'eth_blockNumber', apikey = api_key)
  out <- Api(url = cro_env$base_url, path = c("api"), query = query)
  as.integer(as.hexmode(out$result))
}

cro_account_tx_count <- function(address = NULL, api_key = cro_api_key()){
  query <- list(module = "proxy", action = 'eth_getTransactionCount', address = address, tag = "latest", apikey = api_key)
  out <- Api(url = cro_env$base_url, path = c("api"), query = query)
  if (!is.null(out)) {
    txcount <- as.integer(as.hexmode(out$result))
  } else {
    txcount <- NA_integer_
  }
  return(txcount)
}


cro_gas_price <- function(api_key = cro_api_key()){
  query <- list(module = "proxy", action = 'eth_gasPrice', apikey = api_key)
  out <- Api(url = cro_env$base_url, path = c("api"), query = query)
  return(out)
  if (!is.null(out)) {
    txcount <- as.integer(as.hexmode(out$result))
  } else {
    txcount <- NA_integer_
  }
  return(txcount)
}