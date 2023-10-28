
db_cronos_file <- read_lines("prova.R")


new_line <- 'db <- db_add_token(db, db_token(contract = \"0xbf62c67eA509E86F07c8c69d0286C0636C50270b\", label = \"VVS Finance LPs (VVS-LP)\", symbol = \"VVS-WCRO\", decimal = 1))'

db_cronos_file[length(db_cronos_file)+1] <- new_line

write_lines(db_cronos_file, file = "prova.R")



db
contract <- "0x5C7F8A570d578ED84E63fdFA7b1eE72dEae1AE23"
df <- get_account_txs(contract, api_key = api_key)
df2 <- get_account_txs(contract, api_key = api_key, internal = TRUE)
get_account_crc(contract, api_key = api_key)
df2
View(df)
