api_key <- "3SN1BFFHHYUNB8WT11N3IW3NN43U1PBU8D"

# ------------------------------------------------------------ Accounts ------------------------------------------------------------
# https://cronoscan.com/apis#accounts
# 
# 
# Get CRO Balance for a single Address 
# https://api.cronoscan.com/api?module=account&action=balance&address=0x0000000000000000000000000000000000001004&tag=latest&apikey=YourApiKeyToken
# 
# Get CRO Balance for multiple Addresses in a single call (maximum 20)
#  
# https://api.cronoscan.com/api?module=account&action=balancemulti&address=0x0000000000000000000000000000000000001004,0xbb4cdb9cbd36b01bd1cbaebf2de08d9173bc095c&tag=latest&apikey=YourApiKeyToken
#  
#  
get_account_balance_cro <- function(account, api_key = NULL){
  
  importing_time <- Sys.time()
  base_url <- "https://api.cronoscan.com"
  
  if (length(account) == 1) {
    
    out <- Api(url = base_url, path = c("api"), query = list(module = "account", action = "balance", address = account, tag = "latest", apikey = api_key))
    output <- dplyr::tibble(date = importing_time, account = account, balance = rescale_decimals(out$result, 18))
    
  } else if (length(account) <= 20) { 
    
    multi_address <- paste0(account, collapse = ",")
    out <- Api(url = base_url, path = c("api"), query = list(module = "account", action = "balancemulti", address = multi_address, tag = "latest", apikey = api_key))
    output <- dplyr::tibble(date = importing_time, account = multi_address, balance = rescale_decimals(out$result, 18))
    
  } else if (length(account) > 20) { 
    
    multi_address <- paste0(account, collapse = ",")
    index <- unique(c(seq(1, length(account), 20), length(account)))
    output <- list()
    for(i in 2:length(index)){
      addresses <- multi_address[index[i-1]:index[i]]
      out <- Api(url = base_url, path = c("api"), query = list(module = "account", action = "balancemulti", address = addresses, tag = "latest", apikey = api_key))
      output[[i]] <- dplyr::tibble(date = importing_time, account = addresses, balance = rescale_decimals(out$result, 18))
    }
    output <- dplyr::bind_rows(output)
  }
  return(output)
}

#' get_account_balance_cro(c("0x0000000000000000000000000000000000001004", "0xD1bB5AB0613D9Dd4F659223E52d7A695Df4ec1B0"))
#' get_account_balance_cro("0xD1bB5AB0613D9Dd4F659223E52d7A695Df4ec1B0", api_key)


# Get a list of 'Normal' Transactions By Address
# [Optional Parameters] - startblock: starting blockNo to retrieve results, 
#                       - endblock: ending blockNo to retrieve results
#                       - internal: if internal == TRUE -> "internal transactions" otherwise "normal transactions"
#                       
account = "0xD1bB5AB0613D9Dd4F659223E52d7A695Df4ec1B0"
start_block = 1
end_block = 99999999
internal = FALSE 

get_account_txs <- function(account, start_block = 1, end_block = 99999999, internal = FALSE, api_key = NULL){

  base_url <- "https://api.cronoscan.com/api"
  action <- ifelse(internal, "txlistinternal", "txlist")
  query <- list(module = "account", action = action, address = account, startblock = start_block, endblock = end_block, sort = "desc", apikey = api_key)
  out <- Api(url = base_url, path = c("api"), query = query)
  output <- dplyr::as_tibble(out$result)
  output <- dplyr::mutate(output, value = rescale_decimals(value, 18))
  
  if (!internal) {
    output <- dplyr::mutate(output, 
                            txs_fee = as.numeric(gasPrice)*as.numeric(gasUsed),
                            txs_fee = rescale_decimals(txs_fee, 18))
  }
  return(output)
}


#' get_account_txs("0xD1bB5AB0613D9Dd4F659223E52d7A695Df4ec1B0", internal = FALSE, api_key = api_key) 

account = "0xbf62c67eA509E86F07c8c69d0286C0636C50270b"
contract = "0x2D03bECE6747ADC00E1a131BBA1469C15fD11e03"
# start_block = get_last_block()-1000
end_block = 99999999
nft = FALSE
# Get a list of "CRC20 - Token Transfer Events" by Address (nft = FALSE) | Get a list of "ERC721 - Token Transfer Events" by Address (nft = TRUE)
get_account_crc <- function(account, start_block = get_last_block()-10000, end_block = 99999999, contract = NULL, nft = FALSE, api_key = NULL){
  
  cro_scale_conversion <- 1e+18
  base_url <- "https://api.cronoscan.com/api"
  action <- ifelse(nft, "tokennfttx", "tokentx")
  query <-  list(module = "account", action = action, address = account, startblock = start_block, endblock = end_block, sort = "desc", apikey = api_key)
  
  out <- Api(url = base_url, path = c("api"), query = query)

  output <- dplyr::as_tibble(out$result)

  if (!nft) {
    output <-  dplyr::mutate(output, value = rescale_decimals(value, tokenDecimal))
  }  
  
  output <- dplyr::mutate(output, 
                          txs_fee = as.numeric(gasPrice)*as.numeric(gasUsed),
                          txs_fee = rescale_decimals(txs_fee, 18))
  return(output)
}

# get_account_crc("0xbf62c67eA509E86F07c8c69d0286C0636C50270b", contract="0x2D03bECE6747ADC00E1a131BBA1469C15fD11e03", nft = FALSE, api_key = api_key)




contract = "0x5C7F8A570d578ED84E63fdFA7b1eE72dEae1AE23" # wcro
contract2 = "0x2D03bECE6747ADC00E1a131BBA1469C15fD11e03" # vvs 
account = "0xbf62c67eA509E86F07c8c69d0286C0636C50270b" # LP VVS-CRO 
# Get CRC20-Token Account Balance for TokenContractAddress
get_crc_balance <- function(account = NULL, contract = NULL, decimals = 18, api_key = NULL){
  
  out <- NULL
  output <- NULL
  base_url <- "https://api.cronoscan.com/api"
  query <- list(module = "account", action = 'tokenbalance', contractaddress = contract, address = account, tag = "latest", apikey = api_key)
  out <- Api(url = base_url, path = c("api"), query = query)
  output <- dplyr::tibble(date = Sys.time(), 
                          address = account, 
                          contract = contract, 
                          balance = rescale_decimals(out$result, decimals = decimals))
  return(output)
}

#' df1 <- get_crc_balance(account = account, contract = contract, api_key = api_key)
#' df2 <- get_crc_balance(account = account, contract = contract2, api_key = api_key)
#' df2$balance / df1$balance  

