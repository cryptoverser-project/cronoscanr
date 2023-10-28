cro_crc_supply <- function(contract, decimals = 18, api_key = cro_api_key()){
  
  query <- list(module = "stats", action = 'tokensupply', contractaddress = contract, apikey = api_key)
  out <- Api(url = cro_env$base_url, path = c("api"), query = query)
  output <- dplyr::tibble(date = Sys.time(), 
                          contract = contract, 
                          supply = rescale_decimals(out$result, decimals = decimals))
  return(output)
}

# Get CRC20-Token Account Balance for TokenContractAddress
cro_account_crc_balance <- function(account = NULL, contract = NULL, decimals = 18, api_key = cro_api_key()){
  
  query <- list(module = "account", action = 'tokenbalance', contractaddress = contract, address = account, tag = "latest", apikey = api_key)
  response <- Api(url = cro_env$base_url, path = c("api"), query = query)
  output <- dplyr::tibble(date = Sys.time(), 
                          address = account, 
                          contract = contract, 
                          balance = rescale_decimals(response$result, decimals = decimals))
  return(output)
}