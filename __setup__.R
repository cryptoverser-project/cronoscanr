suppressMessages(suppressWarnings(library(tidyverse))) 

R_functions <- system("ls R", intern = TRUE)

for(i in 1:length(R_functions)){
  source(paste0("R/", R_functions[i]))
}

rm(i, R_functions)

