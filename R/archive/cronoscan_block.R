api_key <- "3SN1BFFHHYUNB8WT11N3IW3NN43U1PBU8D"

# Get Block And Uncle Rewards by BlockNo
# https://api.cronoscan.com/api?module=block&action=getblockreward&blockno=2150000&apikey=YourApiKeyToken

get_block_reward <- function(block_no = NULL, api_key = NULL){
  
  out <- NULL
  output <- NULL
  base_url <- "https://api.cronoscan.com"
  
  out <- Api(url = base_url, path = c("api"), query = list(module = "block", action = 'getblockreward', blockno = block_no, apikey = api_key))
  output <- suppressWarnings(dplyr::bind_cols(out$result))
  
  return(output)
  
}

#' get_block_reward(234434, api_key = api_key)

# Get Block Number by Timestamp
# [Parameters] timestamp format: Unix timestamp (supports Unix timestamps in seconds), closest value: 'before' or 'after'
# https://api.cronoscan.com/api?module=block&action=getblocknobytime&timestamp=1601510400&closest=before&apikey=YourApiKeyToken


get_block_number <- function(timestamp = NULL, date = Sys.time(), api_key = NULL){
  
  out <- NULL
  output <- NULL
  base_url <- "https://api.cronoscan.com"
 
  if(is.null(timestamp)){
    timestamp <- as.numeric(as.POSIXct(date, origin = "1970-01-01"))
  } 
  
  out <- Api(url = base_url, path = c("api"), query = list(module = "block", action = 'getblocknobytime', timestamp = timestamp, closest = "before", apikey = api_key))
  
  output <- dplyr::tibble(date = date, block_no = suppressWarnings(as.numeric(out$result)))
  
  return(output)
  
}

# get_block_number(date = "2023-06-07 08:46:45")