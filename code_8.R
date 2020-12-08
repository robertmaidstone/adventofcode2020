library(tidyverse)

read.csv("input_8.txt",header = F,sep = " ") -> instructions
instructions %>% as_tibble()

instructions %>% as_tibble() %>%
  mutate(position=1:596) %>%
  mutate(to=ifelse(V1=="jmp",position+V2,position+1)) -> instr_1

prev_jumps <- c()
instr_2 <- instr_1[1,]
j<-unlist(instr_1[1,4])
while(!(j %in% prev_jumps)){
  instr_2<-rbind(instr_2,instr_1[j,])  
  prev_jumps <- c(prev_jumps,j)
  j <- unlist(instr_1[j,4])
}
instr_2 %>% mutate(acc=ifelse(V1=="acc",V2,0)) %>%dplyr::select(acc) %>% sum         

#######
dim_vec<-c()
sum_vec<-c()
end_vec<-c()
for(i in 1:596){

instructions %>% as_tibble() %>%
  mutate(position=1:596) %>%
  mutate(V1=ifelse(position==i,ifelse(V1=="jmp","nop",ifelse(V1=="nop","jmp",V1)),V1)) %>%
  mutate(to=ifelse(V1=="jmp",position+V2,position+1)) -> instr_1

prev_jumps <- c()
instr_2 <- instr_1[1,]
j<-unlist(instr_1[1,4])
while(!(j %in% prev_jumps)){
  instr_2<-rbind(instr_2,instr_1[j,])  
  prev_jumps <- c(prev_jumps,j)
  j <- unlist(instr_1[j,4])
}
instr_2 %>% dim %>% .[1] -> dim_vec[i]
instr_2 %>% mutate(acc=ifelse(V1=="acc",V2,0)) %>%dplyr::select(acc) %>% unlist %>%  sum(na.rm=T) -> sum_vec[i]
instr_2[dim_vec[i],4]  -> end_vec[i]
}

sum_vec[(end_vec %>% unlist %>% is.na %>% which)]
