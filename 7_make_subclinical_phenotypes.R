library(tidyverse)
library(readr)
library(data.table)
library(readxl)
library(writexl)
## read shells file
thio <- read_xlsx('/mnt/share6/FOR_Takeo/WES2/thiopurine/thiopurine_sub_from_shell_with_WES.xlsx')


dim(thio)
thio <- thio %>% 
  filter(!is.na(Genetic.ID)) %>% 
  distinct(Genetic.ID,.keep_all =T)

## reaad original file
original <- read_xlsx('/mnt/share6/FOR_Takeo/phenotypdata/Thiopurine toxicityforDalin_v2.xlsx')
colnames(original) <- make.names(colnames(original))


original <- original %>% 
  drop_na(LAB.ID) %>% 
  distinct(LAB.ID,.keep_all = T) 



panc <-original %>% 
  select(LAB.ID,Pancreatitis)


## merge 2 files

panc1 <- panc %>% 
  full_join(thio,by = c("LAB.ID" = "Genetic.ID"))


panc1 <- panc1 %>% 
  mutate(pancreatitis = ifelse(!is.na(all_thiopurine),all_thiopurine,
                               ifelse(is.na(all_thiopurine),Pancreatitis.x,NA))) %>% 
  mutate(pancreatitis = ifelse(pancreatitis == "1","Yes",
                               ifelse(pancreatitis == "0","No",pancreatitis))) %>% 
  mutate(pancreatitis = ifelse(pancreatitis == "Yes",2,
                               ifelse(pancreatitis == "No",1,pancreatitis))) %>% 
  select(LAB.ID,pancreatitis) %>% 
  rename(GeneticID = LAB.ID)


### should be removed subjects

removed_sub = fread("/mnt/isilon_data/For_Takeo2/WES2/thiopurine/shoudl_be_removed_subjects_from_shell",
                    header = F)


panc1 = panc1 %>% 
  filter(!GeneticID %in% removed_sub$V1)


## read phenotype file from Shell

pheno = read_xlsx("/mnt/share6/FOR_Takeo/phenotypdata/Cedars_GSA_IIBGGC_corepheno_3_31_2020 (Final Final).xlsx")
colnames(pheno) = make.names(colnames(pheno))
pheno1 = read_xlsx("/mnt/share6/FOR_Takeo/phenotypdata/post2017WESGSA_corepheno_request2_originalfromTalin_final.xlsx",
                   col_types = "text",
                   sheet = 2)
colnames(pheno1) = make.names(colnames(pheno1))
cedapheno <- read_xls("/mnt/share6/FOR_Takeo/phenotypdata/Phenotype _TRICS _2019_07_26_rev.xls",
                      col_types = "text")
colnames(cedapheno) <- make.names(colnames(cedapheno))
## decide which pheno file to use.
pheno = pheno %>% 
  select(center_sample_id,sex,diag,diag_age,family_type,starts_with("dis_loc"),dis_loc_perianal,dis_behavior) %>% 
  rename(GeneticID = center_sample_id) %>% 
  distinct(GeneticID, .keep_all = T) %>% 
  mutate(group = "gsa")

pheno1 = pheno1 %>% 
  select(center_sample_id,sex,diag,diag_age,family_type,starts_with("dis_loc"),dis_loc_perianal,dis_behavior) %>% 
  rename(GeneticID = center_sample_id) %>% 
  distinct(GeneticID, .keep_all = T) %>% 
  mutate(group = "wesgsa")

cedapheno = cedapheno %>% 
  select(Genetic.ID,Gender,Current.Diagnosis,starts_with("Ulcerative"),starts_with("Crohns."),Age.at.Dx,Age.at.Onset) %>%  
  dplyr::rename(L1 = Crohns.Disease.Montreal.Classification...Disease.Location...L1...Ileal) %>% 
  dplyr::rename(L3 = Crohns.Disease.Montreal.Classification...Disease.Location...L3...Ileo.Colonic) %>%
  dplyr::rename(L2 = Crohns.Disease.Montreal.Classification...Disease.Location...L2...Colonic) %>% 
  dplyr::rename(B1 = Crohns.Disease.Montreal.Classification...Disease.Behavior...B1...Non.Stricturing.Non.Penetrating) %>% 
  dplyr::rename(B2 = Crohns.Disease.Montreal.Classification...Disease.Behavior...B2...Stricturing) %>% 
  dplyr::rename(B3 = Crohns.Disease.Montreal.Classification...Disease.Behavior...B3...Penetrating) %>% 
  dplyr::rename(B4 = Crohns.Disease.Montreal.Classification...Disease.Location...L4...Upper.GI.Disease) %>% 
  dplyr::rename(perianal = Crohns.Disease.Montreal.Classification...Disease.Behavior...P...Perianal.Disease.Modifier) %>% 
  dplyr::rename(UC = Ulcerative.Colitis) %>% 
  dplyr::rename(UC_pancolitis = Ulcerative.Colitis...Pancolitis,
                diag = Current.Diagnosis) %>% 
  rename(GeneticID = Genetic.ID) %>% 
  distinct(GeneticID, .keep_all = T) %>% 
  mutate(group = "original")


panc2 <- panc1 %>% 
  left_join(pheno,by = "GeneticID") %>% 
  left_join(pheno1,by = "GeneticID") %>% 
  left_join(cedapheno,by  ="GeneticID")

panc2 = panc2 %>% 
  mutate_at(.vars = vars(c("sex.x","sex.y","Gender","diag.x","diag.y","diag","dis_loc_ilealc.L1.",
                           "dis_loc_ilealc_L1_","dis_loc_colorectal..L2.",
                           "dis_loc_colorectal._L2_","dis_loc_jejunal.x",
                           "dis_loc_jejunal.y","dis_loc_perianal.x","dis_loc_perianal.y",
                           "dis_loc_gi..L4.","dis_loc_gi._L4_","dis_loc_proctitis..E1.",
                           "dis_loc_proctitis._E1_","dis_loc_left..E2.","dis_loc_left._E2_",
                          "dis_loc_extensive..E3.", "dis_loc_extensive._E3_","dis_behavior.x","dis_behavior.y",
                          "diag_age.x","diag_age.y","Age.at.Dx","B1","B2","B3","UC","L1","L2","L3")),
            list(~ifelse(is.na(.),"missing",.)))

panc3 <- panc2 %>%
  mutate(Gender_rev = ifelse(sex.x != "missing",sex.x,
                         ifelse(sex.y != "missing",sex.y,Gender))) %>% 
  mutate(Diag = ifelse(diag.x != "missing",diag.x,
                       ifelse(diag.y != "missing",diag.y,diag))) %>% 
  mutate(ileo_disease = ifelse(dis_loc_ilealc.L1. != "missing",dis_loc_ilealc.L1.,
                               ifelse(dis_loc_ilealc_L1_ != "missing",dis_loc_ilealc_L1_,"missing"))) %>%
  mutate(colonic_disease = ifelse(dis_loc_colorectal..L2. != "missing",dis_loc_colorectal..L2.,
                                  ifelse(dis_loc_colorectal._L2_ != "missing",dis_loc_colorectal._L2_,"missing"))) %>% 
  mutate(jujunum_disease = ifelse(dis_loc_jejunal.x != "missing",dis_loc_jejunal.x,
                                  ifelse(dis_loc_jejunal.y != "missing",dis_loc_jejunal.y,"missing"))) %>% 
  mutate(perianal_disease = ifelse(dis_loc_perianal.x != "missing",dis_loc_perianal.x,
                                   ifelse(dis_loc_perianal.y != "missing",dis_loc_perianal.y,"missing"))) %>% 
  mutate(L4_disease = ifelse(dis_loc_gi..L4. !=  "missing",dis_loc_gi..L4.,
                             ifelse(dis_loc_gi._L4_ != "missing",dis_loc_gi._L4_,"missing"))) %>% 
  mutate(E1_disease = ifelse(dis_loc_proctitis..E1. != "missing",dis_loc_proctitis..E1.,
                             ifelse(dis_loc_proctitis._E1_ != "missing",dis_loc_proctitis._E1_,"missing"))) %>% 
  mutate(E2_disease = ifelse(dis_loc_left..E2. != "missing",dis_loc_left..E2.,
                                    ifelse(dis_loc_left._E2_ != "missing",dis_loc_left._E2_,"missing"))) %>% 
  mutate(E3_disease = ifelse(dis_loc_extensive..E3. != "missing",dis_loc_extensive..E3.,
                             ifelse(dis_loc_extensive._E3_ != "missing",dis_loc_extensive._E3_,"missing"))) %>% 
  mutate(disease_behavior = ifelse(dis_behavior.x != "missing",dis_behavior.x,
                                   ifelse(dis_behavior.y != "missing",dis_behavior.y,"missing"))) %>% 
  mutate(Diag_age = ifelse(diag_age.x != "missing",diag_age.x,
                           ifelse(diag_age.y != "missing",diag_age.y,
                                  ifelse(Age.at.Dx != "missing",Age.at.Dx,"missing")))) %>% 
  select(GeneticID,Gender_rev,pancreatitis,Diag,ends_with("disease"),disease_behavior,UC,B3,B2,B1,B4,L1,L2,L3,Diag_age)



panc4 = panc3 %>% 
  mutate(L1_disease = ifelse((ileo_disease == "Yes" | jujunum_disease == "Yes") & colonic_disease == "No", "Y",
                             ifelse(L1 != "missing",L1,"missing")),
         L2_disease = ifelse((ileo_disease == "No" & jujunum_disease == "No") & colonic_disease == "Yes", "Y",
                             ifelse(L2 != "missing",L2,"missing")),
         L3_disease = ifelse((ileo_disease == "Yes" | jujunum_disease == "Yes") & colonic_disease == "Yes", "Y",
                             ifelse(L3 != "missing",L3,"missing"))) %>% 
  mutate(B2 = ifelse(disease_behavior == "B2","Y",B2),
         B3 = ifelse(disease_behavior == "B3","Y",B3),
         B1 = ifelse(disease_behavior == "B1","Y",B1)) %>%
  mutate(disease_behavior = ifelse(disease_behavior != "missing",disease_behavior,
                                   ifelse(disease_behavior == "missing" & B1 == "Y" & B2 %in% c("missing","N") &
                                            B3 %in% c("missing","N"),"B1",
                                          ifelse(disease_behavior == "missing" & B2 == "Y" & B1 %in% c("missing","N") &
                                                   B3 %in% c("missing","N"),"B2",
                                                 ifelse(disease_behavior == "missing" & B3 == "Y" & B2 %in% c("missing","N") &
                                                          B1 %in% c("missing","N"),"B3",disease_behavior))))) %>% 
  mutate(B23 = ifelse(disease_behavior %in% c("B2","B3"),"Yes",
                      ifelse(disease_behavior == "B1","No","missing"))) %>% 
  mutate(UC = ifelse(E1_disease == "Yes" & E2_disease != "Yes" & E3_disease != "Yes","E1",
                     ifelse(E2_disease == "Yes" & E1_disease != "Yes" & E3_disease != "Yes","E2",
                            ifelse(E3_disease == "Yes" & E2_disease != "Yes" & E1_disease != "Yes","E3",UC)))) %>% 
  mutate(UC = ifelse(UC  == "(E1) Proctitis","E1",
                     ifelse(UC == "(E2) Left Side","E2",
                            ifelse(UC == "(E3) Extensive Disease","E3",
                                   ifelse(UC == "(E0) Insufficient Data","missing",UC))))) %>% 
  mutate(Location = ifelse(L3_disease == "Y","L3",
                           ifelse(L2_disease == "Y" & L1_disease %in% c("missing","N") & 
                                    L3_disease %in% c("missing","N"),"L2",
                                  ifelse(L1_disease == "Y" & 
                                           L2_disease %in% c("missing","N") &
                                           L3_disease %in% c("missing","N"),"L1","missing")))) %>% 
  mutate(Diag = ifelse(Diag %in% c("Crohn's Colitis","Crohn's Disease"),"CD",
                       ifelse(Diag %in% c("Ulcerative Colitis"),"UC",
                              ifelse(Diag %in% c("Colitis Unclear Type/IBDU","Indeterminate"),"IBDU",
                                     ifelse(Diag %in% c("Non-IBD"),"HC",Diag)))))


panc4 %>% 
  select(GeneticID,Gender_rev,pancreatitis,Diag,perianal_disease,disease_behavior,UC,B23,Location,L4_disease,Diag_age) %>% 
  mutate(Diag_age = as.numeric(Diag_age)) %>% 
  write_tsv("/mnt/share6/For_Takeo/phenotypdata/pancreas_subclinical_phenotypes.txt")



