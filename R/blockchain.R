blockChain <- function(){
  
  structure(
    list(
      blocks = list(),
      last = 1,
      update_time = as.POSIXct("2000-01-01")
    )
  )
}

update_blockChain <- function(object, n_blocks = 1000, api_key = NULL, quiet = FALSE){
  
  i = 5
  initial_block <- object$last
  # last_block <- get_last_block(api_key = NULL)
  last_block <- 8695596
  for(i in 1:n_blocks){
    
    if(!quiet){
      perc <- round(object$last/last_block*100, 3)
      msg <- paste0("Index (", perc, "%): ", object$last, "/", last_block, " (Update time: ", object$update_time, ") \r")
      message(msg, appendLF = FALSE)
    }
    
    new_block <- get_block(object$last, api_key = api_key)
    if(!purrr::is_empty(new_block)){
      object$blocks <- dplyr::bind_rows(object$blocks, new_block)
      object$update_time <- max(object$blocks$timestamp)
      object$update_time <- as.POSIXct(object$update_time, origin = "1970-01-01")
    }
    object$last <- object$last + 1 
    Sys.sleep(0.1)
  }
  return(object)
}
