library(tidyverse)
library(compositions)

read.csv("input_5.txt",header = F,blank.lines.skip = F) -> passes
passes %>% as_tibble()

passes %>% separate(V1,into = paste0("V",1:11),sep = "") %>% 
  dplyr::select(-V1) %>%
  mutate(V2=-as.numeric(factor(V2))+2) %>%
  mutate(V3=-as.numeric(factor(V3))+2) %>%
  mutate(V4=-as.numeric(factor(V4))+2) %>%
  mutate(V5=-as.numeric(factor(V5))+2) %>%
  mutate(V6=-as.numeric(factor(V6))+2) %>%
  mutate(V7=-as.numeric(factor(V7))+2) %>%
  mutate(V8=-as.numeric(factor(V8))+2) %>%
  mutate(V9=as.numeric(factor(V9))-1) %>%
  mutate(V10=as.numeric(factor(V10))-1) %>%
  mutate(V11=as.numeric(factor(V11))-1) %>% as.matrix -> passes_m


passes_m[1,1:7] %>% paste(collapse = "")

passes_m[,1:7] %>% apply(1,function(x){paste(x,collapse="")}) %>% unbinary -> plane_rows
passes_m[,8:10] %>% apply(1,function(x){paste(x,collapse="")}) %>%unbinary -> plane_cols

(plane_rows*8 + plane_cols) %>% max


(!((1:858) %in% (plane_rows*8 + plane_cols)) )%>% which



