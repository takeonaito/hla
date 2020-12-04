library(tidyverse)
library(readr)
library(data.table)
library(readxl)
library(writexl)


pop1 = fread("/mnt/share6/FOR_Takeo/phenotypdata/admix_with_jewish_nonjewish_prop_added.txt")
pop1 = pop1 %>% 
  select(FID,Jewish,nonJewish)
pop2 = fread("/mnt/share6/FOR_Takeo/phenotypdata/admixEUR70_Jewish_NonJewish.txt")
pop2 <- pop2 %>% 
  select(-IID)
pop3 <- pop1 %>% 
  full_join(pop2,by = "FID")
pop3 <- pop3 %>% 
  mutate(AJ_prop = ifelse(!is.na(Jewish),Jewish,admixJewish)) %>% 
  mutate(AJ = ifelse(AJ_prop >= 0.7,"AJ",
                     ifelse(AJ_prop < 0.7, "non_AJ",NA)))


load("/mnt/share6/SHARED_DATASETS/hla_working_group/results/hla_table.RData")

hla_table <- hla_table %>% 
  mutate(FID = as.character(FID))

hla_table1 <- hla_table %>% 
  left_join(pop3,by = "FID")

hla_table1 %>% 
  filter(AJ == "AJ") %>% 
  select(FID) %>% 
  write_tsv("/mnt/share6/FOR_Takeo/hla/hla_AJ_list")

hla_table1 %>% 
  filter(AJ == "non_AJ") %>% 
  select(FID) %>% 
  write_tsv("/mnt/share6/FOR_Takeo/hla/hla_nonAJ_list")


