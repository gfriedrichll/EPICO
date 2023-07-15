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
library(haven)
#------------


#Cargar base de datos
#------------
rm(list=ls())
#data=read.csv('~/EPICO/Datos/datos_todos_01272023.csv')
data=read.csv('~/EPICO/Datos/datos_todos_analisis_sinneonatos_9MAY2023.csv')
percentiles=read_dta("~/EPICO/Datos/PercentilesFCFR.dta")
percentiles_TA=read_dta("~/EPICO/Datos/Percentiles TA2.dta")
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

#Eliminar datos con edad missing
data %<>% filter(is.na(age_months)==F)
#Eliminar datos con edad no consistente
data %<>% filter(age_months<250)
data %<>% mutate(edad_anios = year(data$admission_date) - year(data$patient_birth_date))
data %<>% mutate(edad_cat = ifelse(edad_anios <2, "Lactante", ifelse(edad_anios >=2 & edad_anios < 6, "Preescolar", ifelse(edad_anios>=6 & edad_anios <12, "Escolar", "Adolescente"))))
freq(data$edad_cat)
data %<>% filter(is.na(edad_cat)==F)
str(data$edad_cat)
data$edad_cat <- factor(data$edad_cat, ordered = TRUE, 
                        levels = c("Lactante", "Preescolar", "Escolar", "Adolescente"))
freq(data$edad_cat)
#-------------------------------------------------------------------------------

#Resumen signos vitales y laboratorios categorizados por grupo de edad

##Categorizar signos vitales: 
data %<>% mutate (age_months_round = round(age_months))
summary(data$age_months_round)

data <- merge(percentiles, data, by.x="mes", by.y="age_months_round", all.y=T)
data <- merge(percentiles_TA, data, by="record", all.y=T)
freq(data$catTAdiastolica)
freq(data$catTAsistolica)

#Crear grupo TA sistolica
data %<>% mutate(
  cat_sysbp_vsorres = ifelse(catTAsistolica==0, "Baja (<p5)", ifelse(catTAsistolica==1, "Normal","Alta (>p95)"))
)
freq(data$cat_sysbp_vsorres)

#Crear grupo TA diastolica
data %<>% mutate(
  cat_admission_diabp_vsorres = ifelse(catTAdiastolica==0, "Baja (<p5)", ifelse(catTAdiastolica==1, "Normal","Alta (>p95)"))
)
freq(data$cat_admission_diabp_vsorres)

#Crea grupo bracardia y taquicardia (FCcat)
data %<>% mutate(
  hr_vsorres_cat = ifelse(hr_vsorres > FCp95, "Taquicardia", ifelse(hr_vsorres < FCp5, "Bradicardia", "Normal"))
)
#Crea grupo bradipnea y taquipnea (FCcat)
data %<>% mutate(
  rr_vsorres_cat = ifelse(rr_vsorres > FRp95, "Taquipnea", ifelse(rr_vsorres < FRp5, "Bradipnea","Normal"))
)
#Temperatura (febril no febril)
data %<>%
  mutate(cat_temp_vsorres = ifelse(temp_vsorres<=38,"No febril", "Febril"))
#Laboratorios:
#Leucocitos
data %<>%
  mutate(cat_leukocytes = ifelse( (leukocytes<=5000 & edad_cat=="Lactante") | (leukocytes<=6000 & edad_cat=="Preescolares") | (leukocytes<=5000 & edad_cat=="Escolares") | (leukocytes<=4500 & edad_cat=="Adolescentes"), "Baja", ifelse( (leukocytes>=19500 & edad_cat=="Lactante") | (leukocytes>= 17500 & edad_cat=="Preescolares") | (leukocytes>= 14500 & edad_cat=="Escolares") | (leukocytes>=13500 & edad_cat=="Adolescentes"), "Alta", "Normal")))
#Neutrofilos
data %<>%
  mutate(cat_neutrophils = ifelse( (neutrophils<=1000 & edad_cat=="Lactante") | (neutrophils<=1000 & edad_cat=="Preescolares") | (neutrophils<=1500 & edad_cat=="Escolares") | (neutrophils<=1800 & edad_cat=="Adolescentes"), "Baja", ifelse( (neutrophils>=9000 & edad_cat=="Lactante") | (neutrophils>= 8500 & edad_cat=="Preescolares") | (neutrophils>= 8000 & edad_cat=="Escolares") | (neutrophils>=8000 & edad_cat=="Adolescentes"), "Alta", "Normal")))
#Linfocitos
data %<>%
  mutate(cat_lymphocytes = ifelse( (lymphocytes<=1400 & edad_cat=="Lactante") | (lymphocytes<=2000 & edad_cat=="Preescolares") | (lymphocytes<=850 & edad_cat=="Escolares") | (lymphocytes<=680 & edad_cat=="Adolescentes"), "Baja", ifelse( (lymphocytes>=16500 & edad_cat=="Lactante") | (lymphocytes>= 15400 & edad_cat=="Preescolares") | (lymphocytes>= 9700 & edad_cat=="Escolares") | (lymphocytes>=7400 & edad_cat=="Adolescentes"), "Alta", "Normal")))
#Hemoglobina (anemia o no anemia)
data %<>%
  mutate(cat_hb = ifelse( (hb<=10.5 & edad_cat=="Lactante") | (hb<=11.5 & edad_cat=="Preescolares") | (hb<=11.5 & edad_cat=="Escolares") | (hb<=12.0 & edad_cat=="Adolescentes"), "Anemia", "No anemia"))
data %>% group_by(as.factor(edad_cat)) %>% summarise(mean_sys = mean(sysbp_vsorres.x,na.rm = T), sd_sys = sd(sysbp_vsorres.x,na.rm = T))
data %>% group_by(as.factor(edad_cat)) %>% summarise(mean_sys = mean(sysbp_vsorres.y,na.rm = T), sd_sys = sd(sysbp_vsorres.y,na.rm = T))

sig_cat_lab <- data %>% select(Country,edad_cat,cat_sysbp_vsorres,cat_admission_diabp_vsorres,hr_vsorres_cat,rr_vsorres_cat,cat_temp_vsorres,cat_leukocytes,cat_neutrophils,cat_lymphocytes,cat_hb)
tbl_summary(
  sig_cat_lab %>% select(-c("Country")), 
  by = "edad_cat", 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels()

tbl_summary(
  sig_cat_lab %>% filter(Country=="Colombia"), 
  by = "edad_cat", 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels()

tbl_summary(
  sig_cat_lab %>% filter(Country=="España"), 
  by = "edad_cat", 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels()



#Oxygentherapy
#-------------------------------------------------------------------------------

data %<>% mutate(
  Oxygen_therapy_order = ifelse(imv == "Yes", "imv",
                                ifelse(imv=="No" & nimv=="Yes", "nimv", 
                                       ifelse(hft=="No" & cpap == "Yes", "cpap",
                                              ifelse(cpap == "No" & hft=="Yes" & is.na(imv)==T & is.na(nimv)==T,"hft"," "))))) #& hft=="No" & (is.na(imv)==T & is.na(nimv)==T |imv == "No" & nimv== "No")

data %<>% mutate(
  Oxygen_therapy_order = ifelse(cpap == "No" & hft=="Yes" & (is.na(imv)==T & is.na(nimv)==T),"hft", 
                                ifelse(hft=="No" & cpap == "Yes" & (is.na(imv)==T & is.na(nimv)==T), "cpap", 
                                       ifelse(imv=="No" & nimv=="Yes", "nimv",
                                              ifelse(imv == "Yes", "imv", NA))))
)

data %>% select(Oxygen_therapy_order,cpap,hft,nimv,imv)
table(data$Oxygen_therapy_order)
table(data$oxygen_therapy)
table(data$cpap) #28
table(data$hft) #200
table(data$nimv) #38
table(data$imv) #91

oxygentherapy<-data %>% select(Country, Oxygen_therapy_order,cpap,hft,nimv,imv) %>% filter(imv=="Yes")

table(data$hft, data$nimv) 
table(data$hft, data$imv)

#Graph
library(scales)


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


oxygentherapy<-data %>% select(Country,edad_cat,oxygen_therapy,Oxygen_therapy_order)

oxygentherapy_n <- oxygentherapy %>% group_by(Country,edad_cat,Oxygen_therapy_order) %>% summarise(n=n())
oxygentherapy_N <- oxygentherapy_n %>% group_by(Country,edad_cat) %>% summarise(N=sum(n))
percentage <- merge(oxygentherapy_n, oxygentherapy_N, by=c("Country", "edad_cat"))
percentage %<>% mutate(
  percentage = n/N*100
)

t_oxygentherapy_all<- tbl_summary(
  oxygentherapy, 
  by = edad_cat, 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels() 

t_oxygentherapy_col<- tbl_summary(
  oxygentherapy %>% filter(Country=="Colombia"), 
  by = edad_cat, 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels() 

t_oxygentherapy_esp<- tbl_summary(
  oxygentherapy %>% filter(Country=="España"), 
  by = edad_cat, 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels() 

t_oxygentherapy_all
t_oxygentherapy_col
t_oxygentherapy_esp


percentage %<>% filter(is.na(Oxygen_therapy_order)==F)
percentage %>% group_by(Country, Oxygen_therapy_order) %>% summarise(perc_sum = sum(percentage))

ggplot(percentage, aes(x = Country, y = n, fill=edad_cat)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ Oxygen_therapy_order) +
  theme_bw()+
  #scale_y_continuous(labels = function(x) paste0(x, "%"))+ # Add percent sign
  labs(y="Number of patients", fill="Age group")+ 
  scale_fill_manual(values = c("#00628F", "#097EFF", "#00A7F5","#5CCBFF"))


oxygentherapy_colombia <- oxygentherapy %>% filter(Country=="Colombia")
oxygentherapy_spain<- oxygentherapy %>% filter(Country=="España")

table(oxygentherapy_colombia$edad_cat)
table(oxygentherapy_spain$edad_cat)
table(oxygentherapy$edad_cat)
table(oxygentherapy_colombia$Oxygen_therapy_order)
table(oxygentherapy_colombia$Oxygen_therapy_order, oxygentherapy_colombia$edad_cat)
table(oxygentherapy_spain$Oxygen_therapy_order)
table(oxygentherapy_spain$Oxygen_therapy_order, oxygentherapy_spain$edad_cat)
table(oxygentherapy$Oxygen_therapy_order, oxygentherapy$edad_cat)
table(oxygentherapy$Oxygen_therapy_order)


table(oxygentherapy_colombia$oxygen_therapy)
table(oxygentherapy_colombia$oxygen_therapy, oxygentherapy_colombia$edad_cat)


table(oxygentherapy$oxygen_therapy)
table(oxygentherapy$oxygen_therapy, oxygentherapy$edad_cat)

table(oxygentherapy_spain$oxygen_therapy)
table(oxygentherapy_spain$oxygen_therapy, oxygentherapy_spain$edad_cat)


#-------------------------------------------------------------------------------
