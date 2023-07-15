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
#data2=read.csv('~/EPICO/Datos/datos_todos_01272023.csv')
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
#setdiff(data$ID, data2$record)
#-------------------------------------------------------------------------------

#Comorbilidades

#Colombia - borrar other risk text
data %>% select(otherrisktext, record) %>% filter(record=="30392-524")
data[data$record=="30392-524","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30392-498")
data[data$record=="30392-498","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30392-481")
data[data$record=="30392-481","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30392-228")
data[data$record=="30392-228","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30392-198")
data[data$record=="30392-198","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30392-106")
data[data$record=="30392-106","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30392-20")
data[data$record=="30392-20","otherrisktext"] <- ""

#España - borrar other risk text
data %>% select(otherrisktext, record) %>% filter(record=="30002-11")
data[data$record=="30002-11","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30002-17")
data[data$record=="30002-17","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30003-58")
data[data$record=="30003-58","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30003-194")
data[data$record=="30003-194","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30006-6")
data[data$record=="30006-6","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30006-6")
data[data$record=="30006-6","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30011-36")
data[data$record=="30011-36","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30011-87")
data[data$record=="30011-87","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30011-110")
data[data$record=="30011-110","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30011-132")
data[data$record=="30011-132","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30011-135")
data[data$record=="30011-135","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30011-145")
data[data$record=="30011-145","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30011-289")
data[data$record=="30011-289","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30013-10")
data[data$record=="30013-10","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30034-13")
data[data$record=="30034-13","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30040-10")
data[data$record=="30040-10","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30040-10")
data[data$record=="30040-10","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30040-15")
data[data$record=="30040-15","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30040-17")
data[data$record=="30040-17","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30040-22")
data[data$record=="30040-22","otherrisktext"] <- ""
data %>% select(otherrisktext, record) %>% filter(record=="30052-59")
data[data$record=="30052-59","otherrisktext"] <- ""

#record españa reclasificar en inmunosupresion
inmunosipresion <- c("30001-36","30001-52","30001-67","30003-29","30003-97","30003-126","30003-129","30003-131","30003-166","30003-177","30003-177","30011-48","30011-49","30013-49","30024-12","30027-2","30040-57","30040-62","30045-4",
  "30052-75","30070-25","30095-9")
data[data$record %in% inmunosipresion, c("otherrisktext","immunosupressors")] <- "Yes"
data[data$record %in% inmunosipresion, c("otherrisktext")] <- ""
#record españa reclasificar en metabolicas
metabolicas <- c("30002-37","30011-26","30040-44","30058-1")
data[data$record %in% metabolicas, c("otherrisktext")] <- ""
#recirds españa reclasificar en cardiovasculares
cardiovascular <- c("30002-44","30003-23","30003-34","30003-37","30043-1","30048-20")
data[data$record %in% cardiovascular, c("otherrisktext")] <- ""
#records españa reclasificar en enfermedades hepáticas
hepaticas <- c("30003-133")
data[data$record %in% hepaticas, c("otherrisktext")] <- ""
#records españa reclasificar en enfermedades neurologicas
neurologicas <- c("30003-145","30003-211","30011-25","30011-27","30011-28","30011-67","30033-8","30048-22","30048-24","30048-26","30056-10")
data[data$record %in% neurologicas, c("otherrisktext")] <- ""
#records españa reclasificar en enfermedades renales
renales <- c("30003-155")
data[data$record %in% renales, c("otherrisktext")] <- ""
#records españa reclasificar en enfermedades inflamatorias
inflamatorias <- c("30003-161","30004-6","30006-1","30007-23","30007-24","30011-88","30022-24","30022-2","30086-3")
data[data$record %in% inflamatorias, c("otherrisktext")] <- ""
#records españa reclasificar en enfermedades oncohematologicas
oncohematologicas <- c("30003-163","30013-43","30040-8","30040-23","30040-29","30044-6","30045-2","30045-3","30045-5")
data[data$record %in% oncohematologicas, c("otherrisktext")] <- ""
#records españa reclasificar en enfermedades oncohematologicas
oncohematologicas <- c("30003-163","30013-43","30040-8","30040-23","30040-29","30044-6","30045-2","30045-3","30045-5")
data[data$record %in% oncohematologicas, c("otherrisktext")] <- ""


#Create comorbidities dataset
which(colnames(data)=="chronicneu_mhyn")
pos1 <- which(colnames(data)=="chroniccard_mhyn")
pos2 <- which(colnames(data)=="malnutrition_mhyn")
pos3 <- which(colnames(data)=="otherrisktext")
pos4 <- which(colnames(data)=="immunosupressors")
comorbilidades<-data[,c(pos1:pos2,pos3,pos4)]
comorbilidades$age_group <- data[,"edad_cat"]
comorbilidades$Country <- data[,"Country"]
comorbilidades$record <- data[,"record"]
#Recode missing values with "No"
comorbilidades[is.na(comorbilidades)] <- "No"
comorbilidades <- na.omit(comorbilidades)
comorbilidades[,1:22]<-lapply(comorbilidades[,1:22],as.factor)
comorbilidades[,1:22][comorbilidades[,1:22]=="Unknown"] <- "No"
comorbilidades[,1:22]<-lapply(comorbilidades[,1:22],as.numeric)
comorbilidades %<>% as.data.frame()
comorbilidades[,1:22][comorbilidades[,1:22]=="1"] <-0
comorbilidades[,1:22][comorbilidades[,1:22]=="3"] <-1
comorbilidades$otherrisktext %<>% tolower()
# Loading library
library(stringr)
# Creating vector
vector <- comorbilidades$otherrisktext
r1<-str_detect(vector, "prem")
r2<-str_detect(vector, "bajo")
r3<-str_detect(vector, "low")
comorbilidades %<>% mutate(Prem_lbw = ifelse(r1==T | r2==T | r3==T, 1,0))
r1 <- str_detect(vector, "rinitis")
r2 <- str_detect(vector, "dermatitis")
comorbilidades %<>% mutate(Atopic_March = ifelse(comorbilidades$asthma_mhyn==1 |r1==T | r2==T,1,0))
r1 <- str_detect(vector, "down")
r2 <- str_detect(vector, "robin")
r3 <- str_detect(vector, "síndrome (no especificado)")
r4 <- str_detect(vector, "síndrome malformativo")
r5 <- str_detect(vector, "alexander")
r6 <- str_detect(vector, "stat1")
r7 <- str_detect(vector, "bipulmonar")
r8 <- str_detect(vector, "cromosomopatía")
comorbilidades %<>% mutate(Chromosome_abnormalities = ifelse(r1==T | r2==T | r3==T | r4==T | r5==T | r6==T | r7==T | r8==T,1,0))
comorbilidades %<>% mutate(Other = ifelse(otherrisktext!="" & Prem_lbw==0 & Atopic_March==0 & Chromosome_abnormalities==0,1,0))
comorbilidades %<>% select(-"otherrisktext")





label <- c("Chronic cardiac disease, including congenital heart disease (not hypertesnion)",
           "Hypertension","Chronic pulmonary disease (not asthma)","Asthma (or recurrent wheezing)",
           "Tuberculosis","History of respiratory infection in the previous 4 weeks prior to current illness?",
           "Chronic kidney disease","Moderate or severe liver disease","Mild Liver disease","Chronic neurological disorder",
           "Malignant neoplasm","Chronic hematologic disease","AIDS/HIV","Obesity (as defined by clinical staff)",
           "Diabetes with complications","Diabetes without complications","Inflammatory disorder","Rheumatologic disorder",
           "Past history of Kawasaki disease","Family history of Kawasaki disease","Dementia","Malnutrition","On immunosuppressive medication","age_group","Country","record","Premature/Low birth weight","Atopic March","Chromosome_abnormalities","Other")
colnames(comorbilidades) <-label

comorbilidades %<>% mutate(
  Respiratory = ifelse(comorbilidades$`Chronic pulmonary disease (not asthma)`==1 | comorbilidades$`History of respiratory infection in the previous 4 weeks prior to current illness?`==1 | comorbilidades$Tuberculosis==1, 1,0),
  Cardiovascular = ifelse(comorbilidades$`Chronic cardiac disease, including congenital heart disease (not hypertesnion)`==1 | comorbilidades$Hypertension==1 | record %in% cardiovascular, 1, 0),
  Kidney_disease = ifelse(comorbilidades$`Chronic kidney disease`==1 | record %in% renales,1,0),
  Liver_disease = ifelse(comorbilidades$`Mild Liver disease`==1 | comorbilidades$`Moderate or severe liver disease`==1 | record %in% hepaticas, 1, 0),
  Nutritional_Metabolic = ifelse(comorbilidades$Malnutrition==1 | comorbilidades$`Obesity (as defined by clinical staff)`==1 | comorbilidades$`Diabetes with complications`==1 | comorbilidades$`Diabetes without complications`==1 | record %in% metabolicas, 1, 0),
  Inflammatory = ifelse(comorbilidades$`Inflammatory disorder`==1 | comorbilidades$`Rheumatologic disorder`==1 | comorbilidades$`Past history of Kawasaki disease`==1 | comorbilidades$`Family history of Kawasaki disease`==1 | record %in% inflamatorias, 1, 0),
  Neurological = ifelse(comorbilidades$`Chronic neurological disorder`==1 | comorbilidades$Dementia==1 | record %in% neurologicas, 1, 0),
  Oncohematological = ifelse(comorbilidades$`Malignant neoplasm`==1 | comorbilidades$`Chronic hematologic disease`==1 | record %in% oncohematologicas, 1,0),
  Immunosuppressive_medication = ifelse(comorbilidades$`On immunosuppressive medication`=="Yes" ,1,0),
  Premature_Lowbirth_weight = ifelse(comorbilidades$`Premature/Low birth weight`==1, 1, 0),
  Atopic_March = comorbilidades$`Atopic March`,
  Chromosome_abnormalities = Chromosome_abnormalities,
  Other_comorbidity = Other
)
long <- comorbilidades[,c(24,25,27:39)]

tbl_summary(
  long %>% select(-c("Country")), 
  by = "age_group", 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels()


#P values Lactantes colombia vs spain

tbl_summary(
  long %>% filter(age_group=="Lactante") %>% select(-c("age_group")), 
  by = "Country", 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels()

#P values Preescolar colombia vs spain

tbl_summary(
  long %>% filter(age_group=="Preescolar") %>% select(-c("age_group")), 
  by = "Country", 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels()

#P values Escolar colombia vs spain

tbl_summary(
  long %>% filter(age_group=="Escolar") %>% select(-c("age_group")), 
  by = "Country", 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels()

#P values Adolescente colombia vs spain

tbl_summary(
  long %>% filter(age_group=="Adolescente") %>% select(-c("age_group")), 
  by = "Country", 
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
  long %>% filter(Country=="Colombia"), 
  by = "age_group", 
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
  long %>% filter(Country=="España"), 
  by = "age_group", 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels()

comorbilidades_age <- long %>%
  group_by(age_group, Country) %>%
  summarise(across(`Premature/Low birth weight`:Immunosuppressive_medication, mean))

comorbilidades_age_n <- long %>%
  group_by(age_group, Country) %>%
  summarise(across(`Premature/Low birth weight`:Immunosuppressive_medication, sum))

long <- melt(setDT(comorbilidades_age), id.vars = c("age_group","Country"), variable.name = "Comorbilidad")
long_n <- melt(setDT(comorbilidades_age_n), id.vars = c("age_group","Country"), variable.name = "Comorbilidad")
long$n <- long_n$value
long %<>% mutate(perc = round(value * 100, 1),
                 res = str_c(n, "(", perc, "%)"))

#----------------------------------------------------------------------------------------------------


ggplot(long %>% filter(Comorbilidad != "Other"), aes(x = reorder(Comorbilidad,-n), y = perc, fill = Comorbilidad)) +
  geom_hline(yintercept = seq(0, 20, by = 5), colour = "grey90", size = 0.3) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set3") +
  facet_wrap(~Country+age_group,ncol = 4) + 
  theme_minimal() +
  labs(x = "Comorbidities", size = 12) +
  theme(
    text=element_text(family="Times New Roman", size = 12),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size=12, hjust = 0),
    axis.title.y = element_blank(),
    panel.grid  = element_blank(),
    legend.position="right",
    legend.text = element_text(size=9),
    legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 1)) +
  coord_polar(start = 0) +
  scale_x_discrete(position = "top") +
  theme(plot.title = element_text(face = "bold", hjust = 0)) +
  geom_text(x = 0.5, y = 4, label = "2%",
            aes(family="Times New Roman"), size=3, colour = "grey60") + 
  geom_text(x = 0.5, y = 8, label = "8%",
            aes(family="Times New Roman"), size=3, colour = "grey60") +
  geom_text(x = 0.5, y = 12, label = "12%",
            aes(family="Times New Roman"), size=3, colour = "grey60") + 
  geom_text(x = 0.5, y = 16, label = "16%",
            aes(family="Times New Roman"), size=3, colour = "grey60") +
  geom_text(x = 0.5, y = 20, label = "20%",
            aes(family="Times New Roman"), size=3, colour = "grey60") 


t_comp_all<- tbl_summary(
  comorbilidades %>% select(-c("Country")), 
  by = "age_group", 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels() 

t_comp_col<- tbl_summary(
  comorbilidades %>% filter(Country=="Colombia"), 
  by = age_group, 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels() 

t_comp_esp<- tbl_summary(
  comorbilidades %>% filter(Country=="España"), 
  by = age_group, 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels() 

t_comp_all
t_comp_col
t_comp_esp

