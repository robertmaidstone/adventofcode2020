read.csv("input_3.txt",header = F,sep=" ") -> map


library(tidyverse)
#right 3 and down 1
dim(map)
head(map)

map$V1[1] %>% nchar

map %>% as_tibble %>%
  separate(V1,into=paste0("V",1:31),sep=1:30) %>% as.matrix -> map_mat

#right 3 and down 1

(seq(from=0,by=3,length.out = 323) %% (31))+1 -> potential_hits
t(map_mat)[potential_hits,] %>% diag %>% table %>% .[1] -> r3d1

#right 1 and down 1

(seq(from=0,by=1,length.out = 323) %% (31))+1 -> potential_hits
t(map_mat)[potential_hits,] %>% diag %>% table %>% .[1] -> r1d1

#right 5 and down 1

(seq(from=0,by=5,length.out = 323) %% (31))+1 -> potential_hits
t(map_mat)[potential_hits,] %>% diag %>% table %>% .[1] -> r5d1

#right 7 and down 1

(seq(from=0,by=7,length.out = 323) %% (31))+1 -> potential_hits
t(map_mat)[potential_hits,] %>% diag %>% table %>% .[1] -> r7d1

#right 1 and down 2

(seq(from=0,by=1,length.out = 162) %% (31))+1 -> potential_hits
t(map_mat[2*(0:161)+1,])[potential_hits,] %>% diag %>% table %>% .[1] -> r1d2

as.numeric(r3d1 * r1d1 * r5d1) * as.numeric(r7d1 * r1d2)

