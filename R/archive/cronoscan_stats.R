api_key <- "3SN1BFFHHYUNB8WT11N3IW3NN43U1PBU8D"


# Get Total Supply of CRO on the Cronos Chain Chain
get_cro_supply <- function(api_key = NULL){
  
  base_url <- "https://api.cronoscan.com/api"
  
  out <- Api(url = base_url, path = c("api"), query = list(module = "stats", action = "supply", apikey = apikey))
  
  output <- dplyr::tibble(date = Sys.time(), balance = rescale_decimals(out$result, 18))
  
  return(output)
}

#' get_cro_supply()

# Get CRC20-Token TotalSupply (aka MaxSupply) by ContractAddress
get_crc_supply <- function(contract, decimals = 18, api_key = NULL){
  
  out <- NULL
  output <- NULL
  base_url <- "https://api.cronoscan.com/api"
  
  out <- Api(url = base_url, path = c("api"), query = list(module = "stats", action = 'tokensupply', contractaddress = contract, apikey = api_key))
  output <- dplyr::tibble(date = Sys.time(), contract = contract, supply = rescale_decimals(out$result, decimals = decimals))
  
  return(output)
}

#' get_crc_supply("0x5C7F8A570d578ED84E63fdFA7b1eE72dEae1AE23", decimals = 18)

