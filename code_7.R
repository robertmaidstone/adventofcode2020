library(tidyverse)

read.csv("input_7.txt",header = F,sep = "?") -> rules
rules %>% as_tibble()


remove <- c("bags", "bag", "[[.]]"," ","[0-9]")

rules %>% as_tibble %>% 
  mutate(V1=str_remove_all(V1,paste(remove, collapse = "|"))) %>%
  separate(V1, into=c("Parent","Children"),sep="contain") %>%
  separate(Children, into=paste0("C",1:4),sep=",") %>%
  pivot_longer(cols=C1:C4,names_to="name",values_to="Child") %>%
  dplyr::select(-name) %>%
  filter(!is.na(Child)) -> rules_p_c


child_vec="shinygold"
child_vec_m <- child_vec
parents=c()
while(length(child_vec)>0){
  rules_p_c %>% filter(Child%in%child_vec) %>% dplyr::select(Parent) %>% unlist %>% c(.,parents) %>% unique -> parents
  child_vec<-unique(parents[!(parents %in% child_vec_m)])
  child_vec_m <- unique(c(child_vec_m,parents))
}

length(parents)

#####

remove <- c("bags", "bag", "[[.]]"," ")

rules %>% as_tibble %>% 
  mutate(V1=str_remove_all(V1,paste(remove, collapse = "|"))) %>%
  separate(V1, into=c("Parent","Children"),sep="contain") %>%
  separate(Children, into=paste0("C",1:4),sep=",") %>%
  pivot_longer(cols=C1:C4,names_to="name",values_to="Child") %>%
  dplyr::select(-name) %>%
  filter(!is.na(Child)) %>%
  separate(Child,into=c("num","Child"),sep=1) %>%
  filter(num != "n") %>%
  mutate(num=as.numeric(num))-> rules_p_c

parent_vec="shinygold"
count_bags=0
parent_num_vec <- 1
while(length(parent_vec)>0){
  (parent_num_vec %>% sum) + count_bags -> count_bags
  rules_p_c %>% filter(Parent%in%parent_vec) %>%
    dplyr::mutate(parent_num = parent_num_vec[match(Parent,parent_vec)]) %>% 
    mutate(num=num*parent_num) %>%
    group_by(Child) %>%
    dplyr::mutate(num=sum(num)) %>% dplyr::select(num,Child) %>% unique-> child_tib
  child_tib$Child  -> parent_vec
  child_tib$num -> parent_num_vec
}

count_bags - 1
