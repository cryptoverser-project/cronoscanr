
# eth_blockNumber
# Returns the number of most recent block
# https://api.cronoscan.com/api?module=proxy&action=eth_blockNumber&apikey=YourApiKeyToken

get_last_block <- function(api_key = NULL){
  
  out <- NULL
  output <- NULL
  base_url <- "https://api.cronoscan.com"
  out <- Api(url = base_url, path = c("api"), query = list(module = "proxy", action = 'eth_blockNumber', apikey = api_key))
 
  if(is.null(api_key)){
    output <- as.integer(as.hexmode(out$result))
  } else {
    output <- get_block(as.integer(as.hexmode(out$result)), api_key = api_key)
  }
  return(output)
}

#' get_last_block(api_key = api_key)


# eth_getBlockByNumber
# Returns information about a block by block number
# https://api.cronoscan.com/api?module=proxy&action=eth_getBlockByNumber&tag=0x3d0900&boolean=true&apikey=YourApiKeyToken

get_block <- function(block_no = 1, api_key = NULL){
  
  out <- NULL
  output <- NULL
  base_url <- "https://api.cronoscan.com"
  out <- Api(url = base_url, path = c("api"), query = list(module = "proxy", action = 'eth_getBlockByNumber', tag = as.hexmode(block_no), boolean = "true", apikey = api_key))
  
  index_info <- purrr::map_lgl(out$result, ~!is.list(.x))
  
  out_info <- dplyr::bind_rows(out$result[index_info])
  
  out_info <- dplyr::mutate(out_info, 
                            nonce = as.integer(as.hexmode(nonce)), 
                            number = as.integer(as.hexmode(number)),
                            timestamp = as.integer(as.hexmode(timestamp)))
  
  out_txs <- as_tibble(out$result$transactions)
  
  if(!purrr::is_empty(out_txs)){
    out_txs <- dplyr::mutate(out_txs, 
                             blockNumber = as.integer(as.hexmode(blockNumber)), 
                             nonce = as.integer(as.hexmode(nonce)))
    
    output <- out_txs %>%
      group_by(blockHash, blockNumber) %>%
      nest() %>%
      rename(number = "blockNumber", hash = "blockHash", txs = data) %>%
      right_join(out_info, by = c("hash", "number")) 
    
  } else {
    output <- out_info
    output$txs <- c(list(NULL)) 
    output <- dplyr::select(output, hash, number, txs, dplyr::everything())
  }
  
  return(output)
  
}
