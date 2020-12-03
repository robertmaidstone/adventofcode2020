read.csv("input_2.txt",header = F,sep=" ") -> password_data

password_data %>% head

password_data %>% as_tibble %>%
  mutate(letter = str_sub(V2,1,1)) %>%
  separate(V1,into=c("low","high"),sep="-") %>%
  mutate(low=as.numeric(low),high=as.numeric(high)) %>%
  dplyr::select(-V2) %>% dplyr::rename(password=V3) %>%
  mutate(count=str_count(password,letter)) %>%
  mutate(valid=(count >= low) & (count <= high)) %>%
  filter(valid)

########

password_data %>% as_tibble %>%
  mutate(letter = str_sub(V2,1,1)) %>%
  separate(V1,into=c("first","second"),sep="-") %>%
  mutate(first=as.numeric(first),second=as.numeric(second)) %>%
  dplyr::select(-V2) %>% dplyr::rename(password=V3) %>%
  mutate(first_val=str_sub(password,first,first)) %>%
  mutate(second_val=str_sub(password,second,second)) %>%
  mutate(valid=(first_val == letter) + (second_val == letter) ==1) %>%
  filter(valid)
