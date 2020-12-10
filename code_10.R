library(tidyverse)
library(rlist)

read.csv("input_10.txt",header = F,sep = " ") %>% unlist -> jolts

diff(c(0,sort(jolts),sort(jolts)[length(jolts)]+3)) %>% table %>% as.data.frame %>% dplyr::select(Freq) %>% prod

###

sort(jolts) -> s_jolts
names(s_jolts)=s_jolts


outer(c(0,s_jolts,197),c(0,s_jolts,197),"-") %>% as_tibble %>%
  mutate(N1=c(0,s_jolts,197)) %>%
  pivot_longer(cols=-N1,names_to="N2",values_to="diff") %>%
  filter((diff<0)&(diff>-4)) %>% dplyr::select(-diff) -> jumps


routes_to_n<-c()
jumps_t <- jumps %>% mutate(routes_to_n1=NA)
for(i in 0:197){
filter(jumps_t,N2==i) %>% dplyr::select(routes_to_n1) %>% unlist -> temp
routes_to_n <- ifelse(length(temp)==0,1,sum(temp))
jumps_t <- jumps_t %>% mutate(routes_to_n1=ifelse(N1==i,routes_to_n,routes_to_n1))
}
options(scipen=999)
jumps_t %>% tail(1) %>% .[3] %>% unlist
options(scipen=0)