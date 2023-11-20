library(readxl)
library(tidyverse)

count(bref_hitter_logs, vars = BOP)

bref_hitter_logs %>% group_by(BOP) %>% 
  summarise(total_ab = sum(PA))
