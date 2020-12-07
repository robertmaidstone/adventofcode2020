library(tidyverse)

read.csv("input_6.txt",header = F,blank.lines.skip = F) -> ans 
ans %>% as_tibble()

unique_ans<-function(str){
  str %>% str_split("") %>% unlist %>% unique %>% length
}

ans %>% as_tibble %>%
  mutate(breaker=(V1=="")) %>%
  mutate(id = cumsum(breaker)) %>% 
  group_by(id) %>%
  dplyr::mutate(com=paste0(V1,collapse = "")) %>%
  dplyr::select(id,com) %>%
  ungroup %>% unique %>%
  mutate(unique_ans=unlist(map(com,unique_ans))) %>% dplyr::select(unique_ans) %>% sum


####

all_ans<-function(str,num){
  str %>% str_split("") %>% unlist %>% table %>% as.data.frame() %>% filter(Freq==num) %>% dim %>% .[1]
}

ans %>% as_tibble %>%
  mutate(breaker=(V1=="")) %>%
  mutate(id = cumsum(breaker)) %>% 
  group_by(id) %>%
  filter(breaker==FALSE) %>%
  dplyr::mutate(numgroup=length(id)) %>%
  dplyr::mutate(com=paste0(V1,collapse = "")) %>%
  dplyr::select(id,com,numgroup) %>%
  ungroup %>% unique %>%
  mutate(all_ans=unlist(map2(com,numgroup,all_ans))) %>% dplyr::select(all_ans) %>% sum

