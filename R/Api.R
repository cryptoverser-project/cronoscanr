#' Api
#' @name Api
#' @rdname Api
#' @description Generic function to execute a GET call.
#' @param url The base url, e.g. `https://url.com`
#' @param path a vector containing the url path, e.g. `c("path1", "path2)`.
#' @param query a named list containing the query parameters, e.g. `list(name_query1 = "params_query1", name_query2 = "params_query2")`.
#' @param config list
#' @param type "text"
#' @param enconding "UTF-8"
#' @param json TRUE
#' @param handle NULL
#' @param quiet TRUE
#' @param ... see
#' @example 
#' Api(url = "https://url.com", path = c("path1", "path2"),  query = list(name_query1 = "params_query1", name_query2 = "params_query2"))
#' @return Returns an `list`.
#' @export

Api <- function(url = NULL, path = NULL, query = NULL, config = list(), type = "text", encoding = "UTF-8", json = TRUE, handle = NULL, quiet = FALSE){

  # initialize 
  api_content <- NULL

  # Api Path: remove null/NA elements
  api_path <- path[!is.null(path) && !is.na(path)]

  # Api Query: remove null/NA elements
  api_query <- query[!is.null(query) && !is.na(query)]

  # Api url
  api_url <- httr::modify_url(url, path = api_path, query = api_query)

  # Api GET response 
  response <- httr::GET(api_url, config = config, handle = handle )

  # Check if response is empty 
  api_empty  <- purrr::is_empty(response)
  api_status <- httr::status_code(response)
 
  if(api_status != 200){
    # Error 1: status code is not equal to 200
    wrn <- paste0("GET error: the status code (status: ", api_status, ") is not equal to 200.", 
                  "\n \n", "Error Message is: ", httr::content(response)$msg)
    if (!quiet) {warning(wrn)}
    
  } else if (api_empty){
    # Error 2: response is empty but code was 200
    wrn <- "GET Request Error: response is empty but status code is 200!"
    if(!quiet) {warning(wrn)}
    
  } else if(api_status == 200 && !api_empty){
    # extract content from api response
    api_content <- httr::content(response, type = type, encoding = encoding)
    # if json = TRUE, conversion from JSON into an R list
    if(json){
      api_content <- jsonlite::fromJSON(api_content)
    }
  }
  
  if(!is.null(api_content)){
    # attributes for analysis of the output
    attr(api_content, "status") <- api_status
    attr(api_content, "path") <- api_path
    attr(api_content, "query") <- api_query
    attr(api_content, "url") <- api_url
    attr(api_content, "raw") <- response
  }
  return(api_content)
}
