library(tidyverse)

read.csv("input_4.txt",header = F,blank.lines.skip = F) -> passports

passports %>% as_tibble %>%
  mutate(breaker=(V1=="")) %>%
  mutate(id = cumsum(breaker)) %>%
  separate(V1,into=c("V1","V2","V3","V4","V5","V6","V7","V8"),sep = " ") %>%
  pivot_longer(V1:V8) %>%
  dplyr::select(-breaker,-name) %>%
  filter(!is.na(value)) %>%
  filter(value!="") %>%
  separate(value,into=c("type","value"),sep=":") -> pass_long

pass_long %>% group_by(id) %>% mutate(valid= ("byr" %in% type)&
                                        ("iyr" %in% type)&
                                        ("eyr" %in% type)&
                                        ("hgt" %in% type)&
                                        ("hcl" %in% type)&
                                        ("ecl" %in% type)&
                                        ("pid" %in% type)) %>% filter(valid==T)
  
##part 2

pass_long %>% group_by(id) %>% mutate(valid= ("byr" %in% type)&
                                        ("iyr" %in% type)&
                                        ("eyr" %in% type)&
                                        ("hgt" %in% type)&
                                        ("hcl" %in% type)&
                                        ("ecl" %in% type)&
                                        ("pid" %in% type)) %>% filter(valid==T) %>%
  filter(type!="cid") %>%
  pivot_wider(names_from=type,values_from=value) %>%
  filter((nchar(byr)==4) & (as.numeric(byr) >= 1920) & (as.numeric(byr) <= 2002)) %>%
  filter((nchar(iyr)==4) & (as.numeric(iyr) >= 2010) & (as.numeric(iyr) <= 2020)) %>%
  filter((nchar(eyr)==4) & (as.numeric(eyr) >= 2020) & (as.numeric(eyr) <= 2030)) %>%
  separate(hgt,into=c("val","unit"),sep=-2) %>%
  mutate(hgt_logic=ifelse(unit=="cm",(as.numeric(val)>=150)&(as.numeric(val)<=193),
                          ifelse(unit=="in",(as.numeric(val)>=59)&(as.numeric(val)<=76),FALSE))) %>%
  filter(hgt_logic) %>%
  separate(hcl,into=c("srt","end"),sep=1) %>%
  filter(srt=="#") %>%
  filter(nchar(str_extract_all(end, '[0-9, a-f]+')) == 6) %>%
  filter(ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) %>%
  filter((nchar(pid)==9))









