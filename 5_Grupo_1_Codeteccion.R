#An?lisis Grupo Presentaci?n Cl?nica: 
#C?digo por: Gabriela Friedrich 
#Fecha ?ltima modificaci?n: 22/07/2022


#Cargar librer?as
#------------
library(factoextra)
library(FactoMineR)
library(gtsummary)
library(tidyverse) #Manipulacion y visualizaci?n de data
library(magrittr) #Manipulaci?n de data
library(lubridate) #Manejo de fechas
library(Hmisc)
library(plotly)
library(summarytools)
library(miceadds)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(data.table)
library(fmsb)
library(ggpubr)
library(readxl)
library(openxlsx)
#------------


#Cargar base de datos
#------------
rm(list=ls())
#data=read.csv('~/EPICO/Datos/datos_todos.csv')
data=read.csv('~/EPICO/Datos/datos_todos_analisis_sinneonatos_9MAY2023.csv')
#------------


##Arreglar formato de variables fecha y edad:
#-------------------------------------------------------------------------------
# fechas <- data %>% select(contains("date"))
data$admission_date %<>% ymd()
data$patient_birth_date %<>% ymd()
data$fever_start_date %<>%ymd()
data$pcr_cov2_1_date %<>% ymd()
data$pcr_cov2_2_date %<>% ymd()
data$pcr_cov2_3_date %<>% ymd()
data$pcr_cov2_4_date %<>% ymd()
data$first_sr_sc2_date %<>% ymd()
data$second_sr_sc2_date %<>% ymd()
data$date_ser_1 %<>% ymd()
data$date_ser_2 %<>% ymd()
data$fever_end_date %<>% ymd()
data$oxigen_start_date %<>% ymd()
data$oxigen_end_date %<>% ymd()
data$hft_start_date %<>% ymd()
data$hft_end_date %<>% ymd()
data$cpap_start_date %<>% ymd()
data$cpap_end_date %<>% ymd()
data$picu_admission_date %<>% ymd()
data$picu_discharge_date %<>% ymd()
data$nimv_start_date %<>% ymd()
data$nimv_end_date %<>% ymd()
data$imv_start_date %<>% ymd()
data$imv_end_date %<>% ymd()
data$discharge_date %<>% ymd()
data$x_ray_1_date %<>% ymd()
data$x_ray_2_date %<>% ymd()
data$x_ray_3_date %<>% ymd()
data$lung_ct_date %<>% ymd()
data$iv_ab_start_date %<>% ymd()
data$iv_ab_end_date %<>% ymd()
data$o_ab_start_date %<>% ymd()
data$o_ab_end_date %<>% ymd()
data$qc_date %<>% ymd()


data %<>% mutate(edad_anios = year(data$admission_date) - year(data$patient_birth_date))
data %<>% mutate(edad_cat = ifelse(edad_anios <2, "Lactante", ifelse(edad_anios >=2 & edad_anios < 6, "Preescolar", ifelse(edad_anios>=6 & edad_anios <12, "Escolar", "Adolescente"))))
freq(data$edad_cat)
data %<>% filter(is.na(edad_cat)==F)
str(data$edad_cat)
data$edad_cat <- factor(data$edad_cat, ordered = TRUE, 
                        levels = c("Lactante", "Preescolar", "Escolar", "Adolescente"))
#-------------------------------------------------------------------------------


#Codetection
freq(data$codetecion_type)
freq(data$virus_obtained_pcr_16)
#Recode <NA> to "No"
data[is.na(data$virus_obtained_pcr_16)==T,"virus_obtained_pcr_16"]<-"No"
freq(data$virus_obtained_pcr_16)
freq(data$pcr_types_virus_sr___1)
freq(data$pcr_types_virus_sr___2)
freq(data$pcr_types_virus_sr___3)
freq(data$pcr_types_virus_sr___4)
freq(data$pcr_types_virus_sr___5)
freq(data$pcr_types_virus_sr___6)
freq(data$pcr_types_virus_sr___7)
freq(data$pcr_types_virus_sr___8)
freq(data$pcr_types_virus_sr___9)
freq(data$pcr_types_virus_sr___10)
freq(data$pcr_types_virus_sr___11)
freq(data$pcr_types_virus_sr___12)
freq(data$pcr_types_virus_sr___13)
freq(data$pcr_types_virus_sr___14)
freq(data$pcr_types_virus_sr___15)
freq(data$pcr_types_virus_sr___16)
freq(data$anti_influenza)
data[is.na(data$anti_influenza)==T,"anti_influenza"]<-"Not performed"
freq(data$anti_influenza)
freq(data$anti_rsv)
data[is.na(data$anti_rsv)==T,"anti_rsv"]<-"Not performed"
freq(data$anti_rsv)

freq(data$blood_culture)
data[is.na(data$blood_culture)==T,"blood_culture"]<-"No"
freq(data$blood_culture)
freq(data$probable_bacterial_etiology_hemoculture)
freq(data$probable_text)

#Negativos
#"negat" "none"
#E. coli
#"coli"
#K. pneumoniae
#"pneumoniae"
#S. epidermidis
#"epider"
#S. Hominis
#"homini"
#S. Mitis
#"mitis"
library(stringr)
vector <- tolower(data$probable_text)
Negativos<-str_detect(vector, "negat")
E_coli<-str_detect(vector, "coli")
K_pneumoniae<-str_detect(vector, "pneumoniae")
S_epidermidis<-str_detect(vector, "epider")
S_Hominis<-str_detect(vector, "homini")
S_Mitis<-str_detect(vector, "mitis")


data %<>% mutate(Bacteria_Etiology = ifelse(Negativos==T, "Negativo",
                                                      ifelse(E_coli==T, "E. coli",
                                                             ifelse(K_pneumoniae==T, "K. pneumoniae",
                                                                    ifelse(S_epidermidis==T,"S. epidermidis",
                                                                           ifelse(S_Hominis==T, "S. Hominis",
                                                                                  ifelse(S_Mitis==T, "S. Mitis",
                                                                                         ifelse(probable_bacterial_etiology_hemoculture != "Other (specify)",probable_bacterial_etiology_hemoculture,"Other"))))))))
                                                
freq(data$Bacteria_Etiology)
freq(data$first_sr_atipical)
data[is.na(data$first_sr_atipical)==T,"first_sr_atipical"]<-"No"
freq(data$first_sr_atipical)
freq(data$second_sr_atipical)
data[is.na(data$second_sr_atipical)==T,"second_sr_atipical"]<-"No"
freq(data$second_sr_atipical)

codeteccion <- data %>% select(codetecion_type,
                               virus_obtained_pcr_16,
                               pcr_types_virus_sr___1,
                               pcr_types_virus_sr___2,
                               pcr_types_virus_sr___3,
                               pcr_types_virus_sr___4,
                               pcr_types_virus_sr___5,
                               pcr_types_virus_sr___6,
                               pcr_types_virus_sr___7,
                               pcr_types_virus_sr___8,
                               pcr_types_virus_sr___9,
                               pcr_types_virus_sr___10,
                               pcr_types_virus_sr___11,
                               pcr_types_virus_sr___12,
                               pcr_types_virus_sr___13,
                               pcr_types_virus_sr___14,
                               pcr_types_virus_sr___15,
                               pcr_types_virus_sr___16,
                               anti_influenza,
                               anti_rsv,
                               blood_culture,
                               Bacteria_Etiology,
                               first_sr_atipical,
                               second_sr_atipical,
                               edad_cat,
                               Country)
label <- c("Codetection type",
           "PCR obtained for 16 viruses in respiratory secretion",
           "VRS",
           "hMPV",
           "PIV 1",
           "PIV 2",
           "PIV 3",
           "PIV 4",
           "Influenza A",
           "Influenza B",
           "hBoV",
           "ADV",
           "EV-hRV",
           "Rhinovirus",
           "CoV 229E",
           "CoV OC43",
           "CoV NL63",
           "CoV HKU12",
           "Antigen Influenza",
           "Antigen RSV",
           "Blood culture for significant bacteria",
           "Most likely bacterial aetiology in blood culture",
           "First atypical serology performed",
           "Second atypical serology performed","age_group","Country")
colnames(codeteccion) <- label

freq(codeteccion$`Codetection type`)
codeteccion[is.na(codeteccion$`Codetection type`)==T,"Codetection type"]<-"Unknown"
freq(codeteccion$`Codetection type`)
freq(codeteccion$`Most likely bacterial aetiology in blood culture`)
codeteccion[is.na(codeteccion$`Most likely bacterial aetiology in blood culture`)==T,"Most likely bacterial aetiology in blood culture"]<-"Unknown"
freq(codeteccion$`Most likely bacterial aetiology in blood culture`)

tbl_summary(
  codeteccion %>% select(-c("Country")), 
  by = "age_group", 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  #add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels()

tbl_summary(
  codeteccion %>% filter(Country=="Colombia"), 
  by = "age_group", 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  #add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels()

tbl_summary(
  codeteccion %>% filter(Country=="España"), 
  by = "age_group", 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  #add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels()


