cro_block_reward <- function(block = 1, api_key = cro_api_key()){
  
  query <- list(module = "block", action = 'getblockreward', blockno = block, apikey = api_key)
  out <- Api(url = cro_env$base_url, path = c("api"), query = query)
  output <- dplyr::bind_cols(out$result)
  output <- dplyr::mutate(output, 
                          blockReward = rescale_decimals(blockReward, 18),
                          timeStamp = as.numeric(timeStamp),
                          timeStamp = as.POSIXct(timeStamp, origin = "1970-01-01"))
  return(output)
}

cro_block_number <- function(date = Sys.time(), api_key = cro_api_key()){
  
  if (!is.numeric(date)) {
    date <- format(as.numeric(as.POSIXct(date)))
  }
  query <- list(module = "block", 
                action = 'getblocknobytime', 
                timestamp = date, 
                closest = "before", 
                apikey = api_key)
  out <- Api(url = cro_env$base_url, path = c("api"), query = query)
  output <- dplyr::tibble(timeStamp = as.POSIXct(as.numeric(date), origin = "1970-01-01"), 
                          blockNumber = as.numeric(out$result))
  return(output)
}