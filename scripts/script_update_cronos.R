source("__setup__.R")
load("data/cronos_chain.RData")


for(i in 1:5){
  
  cronos_chain <- update_blockChain(cronos_chain, n_blocks = 50, api_key = api_key, quiet = FALSE)
  
  save(cronos_chain, file = "data/cronos_chain.RData")
  
  Sys.sleep(5)
}

