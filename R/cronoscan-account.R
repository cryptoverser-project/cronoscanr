#' Get CRO Balance
#' 
#' Get CRO Balance for a Single or Multiple Addresses in a Single Call
#' 
#' @param account wallet, single or multiple. maximum is 20 fo call.
#' 
#' @param api_key character, cronos api key
#' 
#' @return tibble
#' 
#' @examples 
#' 
#' # Get cro balance for a single wallet
#' cro_account_cro_balance(account = "0xbf62c67ea509e86f07c8c69d0286c0636c50270b", api_key = NULL)
#' 
#' # Api key is required for a multiple wallets
#' \donotrun{
#'  cro_account_cro_balance(
#'  account = c("0xbf62c67ea509e86f07c8c69d0286c0636c50270b", 
#'             "0x5C7F8A570d578ED84E63fdFA7b1eE72dEae1AE23"
#'             ), api_key = NULL)
#' }
#'
#' @export
#' 
#' @rdname 
#' @name 

cro_account_cro_balance <- function(account, api_key = cro_api_key()){
  
  # initialize query 
  query <- list(module = "account", action = "", address = "", tag = "latest", apikey = api_key)
  
  if (length(account) == 1) {
    query$action <- "balance"
    query$address <- account
  } else if (length(account) <= 20) { 
    query$action <- "balancemulti"
    query$address <- paste0(account, collapse = ",")
  } else if (length(account) > 20) { 
    warning("The maximum number of accounts for each call is 20.")
    return(NULL)
  }
  # Api response  
  response <- Api(url = cro_env$base_url, path = c("api"), query = query)
  # Output dataset 
  output <- dplyr::tibble(date = Sys.time(), account = account, balance = rep(NA_integer_, length(account)))
  if (!is.null(response) & length(account) > 1) {
    output$balance <- purrr::map_dbl(response$result$balance, ~rescale_decimals(.x, 18))
  } else if (!is.null(response) & length(account) == 1) {
    output$balance <- purrr::map_dbl(response$result, ~rescale_decimals(.x, 18))
  } 
  return(output)
}


cro_account_tx <- 
  function(account = NULL, internal = FALSE, start_block = 1, end_block = 99999999, txhash = NULL, api_key = cro_api_key()){
    
    query <- list(module = "account", 
                  action = "", 
                  address = account, 
                  startblock = start_block, 
                  endblock = end_block, 
                  txhash = txhash,
                  sort = "desc", 
                  apikey = api_key)
    
    if (internal){
      query$action <- "txlistinternal" 
    } else {
      query$action <- "txlist" 
    }
    
    if(!is.null(query$txhash)){
      query$action <- "txlistinternal"
      query$starblock <- NULL
      query$endblock <- NULL
      query$txhash <- txhash
    }
    
    response <- Api(url = cro_env$base_url, path = c("api"), query = query)
    output <- dplyr::as_tibble(response$result)
    output <- dplyr::mutate(output, value = rescale_decimals(value, 18))
    
    if (!internal) {
      output <- dplyr::mutate(output, 
                              txs_fee = as.numeric(gasPrice)*as.numeric(gasUsed),
                              txs_fee = rescale_decimals(txs_fee, 18))
    }
    return(output)
  }


cro_account_crc_tx <- 
  function(account, contract = NULL,  nft = FALSE, start_block = 1, end_block = 99999999, api_key = cro_api_key()){
    
    query <- list(module = "account", 
                  action = ifelse(nft, "tokennfttx", "tokentx"), 
                  contractaddress = contract,
                  address = account, 
                  startblock = start_block, 
                  endblock = end_block, 
                  sort = "desc", 
                  apikey = api_key)
    out <- Api(url = cro_env$base_url, path = c("api"), query = query)
    output <- dplyr::as_tibble(out$result)
    if (!nft) {
      output <- dplyr::mutate(output, value = rescale_decimals(value, tokenDecimal))
    }  else {
      output <- dplyr::mutate(output, 
                              txs_fee = as.numeric(gasPrice)*as.numeric(gasUsed),
                              txs_fee = rescale_decimals(txs_fee, 18))
    }
    return(output)
  }


cro_address_validated_blocks <- function(address = NULL, api_key = cro_api_key()){
  query <- list(module = "account", action = "getminedblocks", address = address, blocktype = "blocks", apikey = api_key)
  response <- Api(url = cro_env$base_url, path = c("api"), query = query)
  output <- dplyr::as_tibble(response$result)
  if(!is.null(response)){
    output <- dplyr::mutate(output, 
                            timeStamp = as.numeric(timeStamp),
                            timeStamp = as.POSIXct(timeStamp, origin = "1970-01-01"),
                            blockReward = rescale_decimals(blockReward, 18)) 
  }
  return(output)
}