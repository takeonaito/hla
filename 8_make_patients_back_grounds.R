library(tidyverse)
library(readr)
library(data.table)
library(readxl)
library(writexl)



## read sublinical phenotypes of pancreatitis subjects

pheno = fread("/mnt/share6/For_Takeo/phenotypdata/pancreas_subclinical_phenotypes.txt")
pheno <- pheno %>% 
  mutate(FID = str_replace_all(GeneticID,"-","0"))

## read HLA haplotypes 

load("/mnt/share6/SHARED_DATASETS/hla_working_group/results/hla_table.RData")

hla_table <- hla_table %>% 
  mutate(FID = as.character(FID))

## merge 2 files

hla_v2 = hla_table %>% 
  left_join(pheno,by = "FID")

relate = fread("/mnt/isilon_data/For_Takeo2/From_nick/Ichip/related_ichip")

hla_v3 <- hla_v2 %>% 
  filter(!FID %in% relate$FID) %>% 
  filter(!is.na(pancreatitis))


pc <- fread("/mnt/share6/FOR_Takeo/ICHIP/PC_ichip1to8washubbc_cauc.txt")

hla_v4 = hla_v3 %>% 
  filter(FID %in% pc$FID)

hla_v5 <- hla_v4 %>% 
  select(-starts_with("HLA")) %>% 
  rename(disease_extent = UC) %>% 
  mutate(Gender_rev = ifelse(Gender_rev == "Male","M",
                             ifelse(Gender_rev == "Female","F",Gender_rev))) %>% 
filter(Diag %in% c("CD","IBDU","UC"))

## add aj or non-aj info

AJ = fread("/mnt/share6/FOR_Takeo/hla/hla_AJ_list")
AJ = AJ %>% 
  mutate(group = "AJ")

non_AJ = fread("/mnt/share6/FOR_Takeo/hla/hla_nonAJ_list")
non_AJ = non_AJ %>% 
  mutate(group = "non_AJ")

AJ_merged = rbind(AJ,non_AJ)

hla_v5 <- hla_v5 %>% 
  left_join(AJ_merged,by = "FID")

## sumarize phenotypes
mytable = hla_v5 %>% 
  group_by(pancreatitis) %>% 
  summarise(N = n(),
            Male = sum(Gender_rev == "M",na.rm = T),
            Female = sum(Gender_rev == "F",na.rm = T),
            AJ = sum(group == "AJ",na.rm = T),
            non_AJ = sum(group == "non_AJ",na.rm = T),
            CD= sum(Diag == "CD",na.rm = T),
            UC = sum(Diag == "UC",na.rm = T),
            IBDU = sum(Diag == "IBDU",na.rm = T),
            Perianal_disease = sum(perianal_disease == "Yes" & Diag == "CD",na.rm = T),
            perianal_missing = sum(perianal_disease %in% c("missing","Unknonw") & Diag == "CD",na.rm = T),
            Age_at_Diag = mean(Diag_age,na.rm = T),
            Age_at_Diag_sd = sd(Diag_age,na.rm = T),
            Diag_age = paste0(round(Age_at_Diag,2),"+-",round(Age_at_Diag_sd,2)),
            B1 = sum(disease_behavior == "B1" & Diag == "CD",na.rm = T),
            B2 = sum(disease_behavior == "B2" & Diag == "CD",na.rm = T),
            B3 = sum(disease_behavior == "B3" & Diag == "CD",na.rm = T),
            behavior_missing = sum(disease_behavior %in% c("missing","Unknown") & Diag == "CD",na.rm = T),
            L1 = sum(Location == "L1" & Diag == "CD",na.rm = T),
            L2 = sum(Location == "L2" & Diag == "CD",na.rm = T),
            L3 = sum(Location == "L3" & Diag == "CD",na.rm = T),
            L4 = sum(L4_disease == "Yes" & Diag == "CD",na.rm = T),
            location_missing = sum(Location == "missing" & Diag == "CD",na.rm = T),
            E1 = sum(disease_extent == "E1" & Diag %in% c("UC","IBDU"),na.rm = T),
            E2 = sum(disease_extent == "E2" & Diag %in% c("UC","IBDU"),na.rm = T),
            E3 = sum(disease_extent == "E3" & Diag %in% c("UC","IBDU") ,na.rm = T),
            extend_missing = sum(disease_extent == "missing" & Diag %in% c("UC","IBDU") ,na.rm = T)) %>% 
  gather(stock,price,-pancreatitis) %>% 
  pivot_wider(names_from = pancreatitis,
              values_from = price) %>% 
  rename(controls = "1",
         cases = "2",
         vari_name = stock) %>% 
  filter(!vari_name %in% c("Age_at_Diag","Age_at_Diag_sd"))


# make P value column

pheno_all <- hla_v5 %>% 
  select(pancreatitis,Diag,Diag_age,Gender_rev,group) %>% 
  mutate(CD = ifelse(Diag == "CD", "1","0")) %>% 
  mutate(UC = ifelse(Diag == "UC", "1","0")) %>%
  mutate(IBDU = ifelse(Diag == "IBDU", "1","0")) %>% 
  mutate(Male = ifelse(Gender_rev == "M","1","0")) %>% 
  mutate(AJ = ifelse(group == "AJ","1","0"))
  
pheno_cd <- hla_v5 %>% 
  filter(Diag == "CD") %>% 
  mutate_all(list(~ ifelse(is.na(.),"missing",.))) %>% 
  select(pancreatitis,disease_behavior,perianal_disease,Location,L4_disease,B23) %>% 
  mutate(L1 = ifelse(Location == "L1",1,
                     ifelse(Location %in% c("L2","L3"),0,NA))) %>% 
           
  mutate(L2 = ifelse(Location == "L2",1,
                            ifelse(Location %in% c("L1","L3"),0,NA))) %>% 
  mutate(L3 = ifelse(Location == "L3",1,
                       ifelse(Location %in% c("L1","L2"),0,NA))) %>% 
  mutate(L4 = ifelse(L4_disease == "Yes",1,
                     ifelse(L4_disease =="No", 0,NA))) %>% 
  mutate(B1 = ifelse(disease_behavior == "B1",1,
                     ifelse(disease_behavior %in% c("B2","B3"),0,NA))) %>%
  mutate(B2 = ifelse(disease_behavior == "B2",1,
                     ifelse(disease_behavior %in% c("B1","B3"),0,NA))) %>%
  mutate(B3 = ifelse(disease_behavior == "B3",1,
                     ifelse(disease_behavior %in% c("B1","B2"),0,NA))) %>% 
    
  mutate(Perianal_disease = ifelse(perianal_disease == "Yes",1,
                                   ifelse(perianal_disease == "No",0,NA))) %>% 
  mutate(B23_1 = ifelse(B23 == "Yes",1,
                        ifelse(B23 == "No",0,NA))) %>% 
  select(pancreatitis,L1,L2,L3,L4,B1,B2,B3,B23_1,Perianal_disease)
  

pheno_uc <- hla_v5 %>% 
  filter(Diag %in% c("UC","IBDU")) %>% 
  mutate_all(list(~ ifelse(is.na(.),"missing",.))) %>% 
  select(pancreatitis,disease_extent) %>% 
  mutate(E1 = ifelse(disease_extent == "E1",1,
                     ifelse(disease_extent %in% c("E2","E3"),0,NA))) %>%
  mutate(E2 = ifelse(disease_extent == "E2",1,
                     ifelse(disease_extent %in% c("E1","E3"),0,NA))) %>% 
  mutate(E3 = ifelse(disease_extent == "E3",1,
                     ifelse(disease_extent %in% c("E2","E1"),0,NA)))
  



## get pval for all 
cand_vari_c <- c("Diag_age")
answer = data.table(vari_name = cand_vari_c, P = rep(100,length(cand_vari_c)) )

for (i in 1:length(cand_vari_c)){
  my_target <- cand_vari_c[i]
  my_vari <- pheno_all %>% 
    pull(UQ(rlang::sym(my_target)))
  
  answer$P[i] <- t.test(as.numeric(my_vari) ~ pheno_all$pancreatitis)$p.value
  
  
}


cand_vari_b <- c("CD","UC","IBDU","Male","AJ")
answer1 = data.table(vari_name = cand_vari_b, P = rep(100,length(cand_vari_b)))

for (i in 1:length(cand_vari_b)){
  my_target <- cand_vari_b[i]
  my_vari <- pheno_all %>% 
    pull(UQ(rlang::sym(my_target)))
  answer1$P[i] <- fisher.test(table(pheno_all$pancreatitis,my_vari))$p.value
  
}

answer_for_all <- rbind(answer,answer1)



# pvalue for CD
cand_vari_cd <- c("L1","L2","L3","L4","B1","B2","B3","B23_1","Perianal_disease")
answer_cd = data.table(vari_name = cand_vari_cd, P = rep(100,length(cand_vari_cd)))


for (i in 1:length(cand_vari_cd)){
  my_target <- cand_vari_cd[i]
  my_vari <- pheno_cd %>% 
    pull(UQ(rlang::sym(my_target)))
  answer_cd$P[i] <- fisher.test(table(pheno_cd$pancreatitis,my_vari))$p.value
  
}


# pvalue for UC
cand_vari_uc <- c("E1","E2","E3")
answer_uc = data.table(vari_name = cand_vari_uc, P = rep(100,length(cand_vari_uc)))


for (i in 1:length(cand_vari_uc)){
  my_target <- cand_vari_uc[i]
  my_vari <- pheno_uc %>% 
    pull(UQ(rlang::sym(my_target)))
  answer_uc$P[i] <- fisher.test(table(pheno_uc$pancreatitis,my_vari))$p.value
  
}


answer_final <- rbind(answer_for_all,answer_cd,answer_uc)



mytable1 <- mytable %>% 
  left_join(answer_final,by = "vari_name" )



## add percentage 
missing_table <- mytable1 %>% 
  select(vari_name,controls,cases) %>% 
  filter(vari_name %in% c("N","CD","UC","IBDU",
                      "perianal_missing","behavior_missing","location_missing",
                      "extend_missing")) %>% 
  gather(stock,price,-vari_name) %>% 
  pivot_wider(names_from = vari_name,
              values_from = price) %>% 
  mutate_at(.vars = vars(-"stock"),
            list(~as.numeric(.)))
mytable2 <- mytable1 %>% 
  mutate(percentage_cont = ifelse(vari_name %in% c("Male","Female","CD","IBDU","UC","AJ","non_AJ"),
                             as.numeric(controls)/as.numeric(mytable1$controls[1]),NA)) %>% 
  mutate(percentage_cont = ifelse(vari_name == "Perianal_disease",
                                  as.numeric(controls)/(missing_table$CD[1]- missing_table$perianal_missing[1]),
                                  percentage_cont)) %>% 
  mutate(percentage_cont = ifelse(vari_name %in% c("B1","B2","B3"), 
                                  as.numeric(controls)/(missing_table$CD[1]- missing_table$behavior_missing[1]),
                                  percentage_cont)) %>% 
  mutate(percentage_cont = ifelse(vari_name %in% c("L1","L2","L3","L4"),
                                  as.numeric(controls)/(missing_table$CD[1]- missing_table$location_missing[1]),
                                  percentage_cont)) %>% 
  mutate(percentage_cont = ifelse(vari_name %in% c("E1","E2","E3"),
                                  as.numeric(controls)/(missing_table$UC[1]- missing_table$extend_missing[1]),
                                  percentage_cont)) %>% 
  mutate(percentage_case = ifelse(vari_name %in% c("Male","Female","CD","IBDU","UC","AJ","non_AJ"),
                                  as.numeric(cases)/as.numeric(mytable1$cases[1]),NA)) %>% 
  mutate(percentage_case = ifelse(vari_name == "Perianal_disease",
                                  as.numeric(cases)/(missing_table$CD[2]- missing_table$perianal_missing[2]),
                                  percentage_case)) %>% 
  mutate(percentage_case = ifelse(vari_name %in% c("B1","B2","B3"), 
                                  as.numeric(cases)/(missing_table$CD[2]- missing_table$behavior_missing[2]),
                                  percentage_case)) %>% 
  mutate(percentage_case = ifelse(vari_name %in% c("L1","L2","L3","L4"),
                                  as.numeric(cases)/(missing_table$CD[2]- missing_table$location_missing[2]),
                                  percentage_case)) %>% 
  mutate(percentage_case = ifelse(vari_name %in% c("E1","E2","E3"),
                                  as.numeric(cases)/(missing_table$UC[2]- missing_table$extend_missing[2]),
                                  percentage_case)) %>% 
  mutate(percentage_cont = round(percentage_cont*100,2),
         percentage_case = round(percentage_case*100,2)) %>% 
  mutate(controls = ifelse(!vari_name %in% c("N","perianal_missing",
                                             "Diag_age",
                                             "behavior_missing",
                                             "location_missing",
                                             "extend_missing"), paste0(controls," (",percentage_cont,"%)"),controls),
         cases = ifelse(!vari_name %in% c("N","perianal_missing",
                                          "Diag_age",
                                          "behavior_missing",
                                          "location_missing",
                                          "extend_missing"),
                        paste0(cases," (",percentage_case,"%)"),
                        cases)) %>% 
  select(-starts_with("percentage")) 


mytable2 %>% 
  write_xlsx("/mnt/share6/FOR_Takeo/hla/patients_back_grounds_pancreatitis_with_hla.xlsx")
  