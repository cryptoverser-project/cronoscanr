
db_token <- function(contract = NA_character_, label = NA_character_, symbol = NA_character_, decimal = 18){
  
  dplyr::tibble(contract = contract, label = label, symbol = symbol, decimal = decimal)
  
}

db_add_token <- function(db, data){
  
  new_db <- dplyr::bind_rows(db, data)
  na.omit(new_db[!duplicated(new_db$contract),])
  
}

# initialize db 
db <- db_token()


db <- db_add_token(db, db_token(contract = "0x5C7F8A570d578ED84E63fdFA7b1eE72dEae1AE23", label = "WCRO token", symbol = "WCRO", decimal = 18))  
db <- db_add_token(db, db_token(contract = "0x2D03bECE6747ADC00E1a131BBA1469C15fD11e03", label = "VVS token", symbol = "VVS", decimal = 18))
db <- db_add_token(db, db_token(contract = "0xbf62c67eA509E86F07c8c69d0286C0636C50270b", label = "VVS Finance LPs (VVS-LP)", symbol = "VVS-WCRO", decimal = 1))