
library(tabulizer)
library(tidyverse)

pages_with_tables<- c(13,17,23,24,25,26,27,28,29,30,34,35,37)

tables <- extract_tables("./Data/Target/blanchard.pdf", pages = pages_with_tables)
