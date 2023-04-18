library(tidyverse)
library(phenotools)

source("N:/durable/projects/scq_efa/rename_items.r") #function to rename from item codes


npr <- curate_npr(diagnoses = "F84") #NPR data for all of MoBa

data_dx <- readRDS("N:/durable/projects/scq_efa/CFA/data_dx_NPRupdate.rds") #data used in analyses 

barn <- as.vector(paste(data_dx$pregid, data_dx$barnNR, sep=  "_")) #list of ids for children in analyses



# % completed the eight year questionnaire by dx  

table(data_dx$autismDX_new)[1] / table(npr$received_dx_F84_npr)[1] #no dx
table(data_dx$autismDX_new)[2] / table(npr$received_dx_F84_npr)[2] #dx

# difference in this missingness 
(table(data_dx$autismDX_new)[1] / table(npr$received_dx_F84_npr)[1] - table(data_dx$autismDX)[2] / table(npr$received_dx_F84_npr)[2])*100 
#about 5.3%


####  item summary tables####
#Getting endorsement rates for individual items in the full sample and by dx and sex

sum1 <- data_dx %>%
  select(starts_with("NN")) %>%
  summarise_at(vars(3:40),list(
    ~ sum(!is.na(.)),
    ~ round(sum(is.na(.))/(n()),3),
    ~ table(.)[1],
    ~ table(.)[2])) %>%
  gather(key, value) %>%
  separate(key, c("Item", "Function"), sep = "_") %>%
  spread(key = Function, value = value) %>%
  rename("Not Endorsed" = "[..3", 
         Endorsed = "[..4",
         "Percent NA" = "round",
         n = "sum") %>%
  mutate("Endorsement Rate" = Endorsed / n) %>%
  rename_items()

 sum1_dx <- data_dx %>%
  select(starts_with("NN"), autismDX_new) %>%
  group_by(autismDX_new)%>%
  summarise_at(vars(3:40),list(
    ~ sum(!is.na(.)),
    ~ round(sum(is.na(.))/(n()),3),
    ~ table(.)[1],
    ~ table(.)[2])) %>%
  gather(key, value, -autismDX_new) %>%
  separate(key, c("Item", "Function"), sep = "_") %>%
  spread(key = Function, value = value) %>%
   mutate(autismDX_new = case_when(autismDX_new == 0 ~ "noDx",
                                   autismDX_new == 1 ~ "Dx",
                                   is.na(autismDX_new) ~ "NA")) %>%
   rename("Not Endorsed" = "[..3", 
          Endorsed = "[..4",
          "Percent NA" = "round",
          n = "sum", 
         "Autism Diagnosis Status" = autismDX_new) %>%
   mutate("Endorsement Rate" = Endorsed / n) %>%
   rename_items() 

sum1_sex <- data_dx %>%
  select(starts_with("NN"), sex) %>%
  group_by(sex)%>%
  summarise_at(vars(3:40),list(
    ~ sum(!is.na(.)),
    ~ round(sum(is.na(.))/(n()),3),
    ~ table(.)[1],
    ~ table(.)[2])) %>%
  gather(key, value, -sex) %>%
  separate(key, c("Item", "Function"), sep = "_") %>%
  spread(key = Function, value = value) %>%
  rename("Not Endorsed" = "[..3", 
         Endorsed = "[..4",
         "Percent NA" = "round",
         n = "sum") %>%
  mutate("Endorsement Rate" = Endorsed / n) %>%
  rename_items() 





##avg % missing across all items within the scale by autism diagnosis status 

(sum1_dx  %>%
  filter(`Autism Diagnosis Status` == "noDx") %>%
  select(`Percent NA`) %>%
  sum()) / nrow(sum1_dx)

(sum1_dx  %>%
    filter(`Autism Diagnosis Status` == "Dx") %>%
    select(`Percent NA` ) %>%
    sum()) / nrow(sum1_dx )

#difference ~1%
((sum1_dx  %>%
    filter(`Autism Diagnosis Status` == "Dx") %>%
    select(`Percent NA` )  %>%
    sum()) / nrow(sum1_dx))  - 
  ((sum1_dx %>%
     filter(`Autism Diagnosis Status` == "noDx") %>%
     select(`Percent NA`)  %>%
    sum()) / nrow(sum1_dx ))

#total scores 

scq_tot <- c("NN159" , 
             "NN160" , "NN161" , "NN162" , "NN163" , "NN164" , 
             "NN165" , "NN166" , "NN167" , "NN168" , "NN151" ,
             "NN169" , "NN170" , "NN171" , "NN172" , "NN173" , 
             "NN175" , "NN176" , "NN177" , "NN178" , "NN152" ,
             "NN179" , "NN180", "NN181" , "NN182" , "NN183" , 
             "NN184" , "NN185" , "NN186" , "NN187" , "NN188" , 
             "NN153" , "NN189" , "NN154" , "NN155" , "NN156" , 
             "NN157" , "NN158")

scq_rrb <- c("NN160" , "NN161" , "NN162" , "NN163" , "NN164" , 
             "NN165" , "NN167", "NN156" , "NN157")

scq_com <- c("NN151" , "NN152" , "NN153" , "NN154", 
             "NN155" ,  "NN169", "NN170", "NN171",
             "NN172", "NN173", "NN183" , "NN184")

scq_soc <- c("NN158","NN159","NN168", "NN175" , "NN176",  "NN177" , "NN178" ,
             "NN179" , "NN180", "NN181" , "NN182","NN185" , 
             "NN186" , "NN188", "NN189")


dat_tot <- data_dx %>%
  mutate(tot = rowSums(.[,scq_tot], na.rm = TRUE), 
         answ = rowSums(!is.na(.[,scq_tot])),
         tot = round((tot / answ)*38,0), 
         tot = case_when(tot <= 37 ~ NA_real_, 
                         tot >= 38 ~ tot - 38)) %>%
  mutate(rrb = rowSums(.[,scq_rrb], na.rm = TRUE), 
         answ = rowSums(!is.na(.[,scq_rrb])),
         rrb = round((rrb / answ)*9,0), 
         rrb = case_when(rrb <= 8 ~ NA_real_, 
                         rrb >= 9 ~ rrb - 9))  %>%
  mutate(com = rowSums(.[,scq_com], na.rm = TRUE), 
         answ = rowSums(!is.na(.[,scq_com])),
         com = round((com / answ)*12,0), 
         com = case_when(com <= 11 ~ NA_real_, 
                         com >= 12 ~ com - 12))  %>%
  mutate(soc = rowSums(.[,scq_soc], na.rm = TRUE), 
         answ = rowSums(!is.na(.[,scq_soc])),
         soc = round((soc / answ)*15,0), 
         soc = case_when(soc <= 14 ~ NA_real_, 
                         soc >= 15 ~ soc - 15))

dat_tot %>%
  #group_by(autismDX_new, sex) %>%  # changed depending on what to group of interest 
  summarise(mean = mean(com,na.rm = TRUE), 
            sd = sd(com, na.rm = TRUE), 
            range_low = range(com, na.rm = TRUE)[[1]],
            range_high = range(com, na.rm = TRUE)[[2]],
            n = n())



            
  

