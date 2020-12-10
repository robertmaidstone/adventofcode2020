library(tidyverse)

read.csv("input_9.txt",header = F,sep = " ") %>% unlist -> numbers


numbers %>% head(25) -> num_head
(outer(num_head,num_head,"+")) -> num_head_outer
num_head_outer[upper.tri(num_head_outer)] %>% unique -> num_head_sums

i=27
valid=TRUE
while(valid==T){
  c(num_head[-1],numbers[i-1]) -> num_head
  cbind(rbind(num_head_outer[-1,-1],(num_head+numbers[i-1])[-25]),num_head+numbers[i-1]) -> num_head_outer
  num_head_outer[upper.tri(num_head_outer)] %>% unique -> num_head_sums
  
  valid=(numbers[i]%in%num_head_sums)
  i=i+1
}
numbers[i-1]

####


for(j in 1:1000){
 (numbers[j:(j+(1000-j))] %>%  cumsum) -> cum_num
 if(numbers[i-1] %in% cum_num[-1]){
   print(j)
   print(max(numbers[j:(j+which(numbers[i-1] == cum_num[-1]))])+min(numbers[j:(j+which(numbers[i-1] == cum_num[-1]))]))
 }
}

