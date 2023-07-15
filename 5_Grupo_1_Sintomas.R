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
#data=read.csv('~/EPICO/Datos/datos_todos_01272023.csv')
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
data %<>% mutate(edad_cat = ifelse(edad_anios <2, "Infants", ifelse(edad_anios >=2 & edad_anios < 6, "Preschool", ifelse(edad_anios>=6 & edad_anios <12, "School aged", "Adolescents"))))
freq(data$edad_cat)
data %<>% filter(is.na(edad_cat)==F)
str(data$edad_cat)
data$edad_cat <- factor(data$edad_cat, ordered = TRUE, 
                        levels = c("Infants", "Preschool", "School aged", "Adolescents"))

data %<>% mutate(Country_eng = ifelse(Country=="España", "Spain","Colombia"))
data$Country <- data$Country_eng
#-------------------------------------------------------------------------------

#Sintomas
pos1 <- which(colnames(data)=="fever_ceoccur_v2")
pos2 <- which(colnames(data)=="sens_alt")
sig_sint<-data[,c(pos1:pos2)]
sig_sint$age_group <- data[,"edad_cat"]
sig_sint$Country <- data[,"Country"]
#Recode missing values with "No"
sig_sint[is.na(sig_sint)] <- "No"
sig_sint <- na.omit(sig_sint)
sig_sint[,1:33]<-lapply(sig_sint[,1:33],as.factor)
sig_sint[,1:33][sig_sint[,1:33]=="Unknown"] <- "No"
sig_sint[,1:33]<-lapply(sig_sint[,1:33],as.numeric)

sig_sint %<>% as.data.frame()
sig_sint[,1:33][sig_sint[,1:33]=="1"] <-0
sig_sint[,1:33][sig_sint[,1:33]=="3"] <-1
label=c("History of fever","Cough","Sore throat","Runny nose (Rhinorrhoea)","Wheezing","Chest pain","Muscle aches (Myalgia)","Joint pain (Arthralgia)","Fatigue / Malaise","Shortness of breath or work of breathing (Dyspnea)","Lower chest wall indrawing","Headache","Altered consciousness / confusion","Seizures","Abdominal pain","Vomiting / Nausea","Diarrhoea","Conjunctivitis","Oral mucosal inflammation signs","Pale/mottled skin","Skin rash","Skin ulcers","Peripheral cutaneous inflammation signs (hands or feet)","Lymphadenopathy","Bleeding (Haemorrhage)","Cold hands/feet","Capillary refill time > 2 seconds ?","Swollen joints","Stiff neck","Hypotonia / floppiness","Paralysis","Shock signs","Alteration in taste or smell","age_group","Country")
colnames(sig_sint)<-label

sig_sint %<>% mutate(
  Respiratory = ifelse(Cough==1 | sig_sint$`Sore throat`==1 | sig_sint$`Runny nose (Rhinorrhoea)`==1 | sig_sint$`Shortness of breath or work of breathing (Dyspnea)`==1 | sig_sint$Wheezing | sig_sint$`Lower chest wall indrawing`, 1,0),
  Hemodynamic = ifelse(sig_sint$`Chest pain`==1 | sig_sint$`Cold hands/feet`==1 | sig_sint$`Capillary refill time > 2 seconds ?`| sig_sint$`Pale/mottled skin` | sig_sint$`Shock signs`==1, 1,0),
  Skin_mucosal = ifelse(sig_sint$`Oral mucosal inflammation signs`==1 | sig_sint$Conjunctivitis==1 | sig_sint$`Skin rash`==1 | sig_sint$`Peripheral cutaneous inflammation signs (hands or feet)`==1, 1,0), 
  Osteomuscular = ifelse(sig_sint$`Joint pain (Arthralgia)`==1 | sig_sint$`Muscle aches (Myalgia)`==1, 1,0), 
  Gastrointestinal = ifelse(sig_sint$`Abdominal pain`==1 | sig_sint$`Vomiting / Nausea`==1 | sig_sint$Diarrhoea==1 | sig_sint$`Alteration in taste or smell`==1, 1,0),
  Hematolymphoid = ifelse(sig_sint$Lymphadenopathy==1, 1,0),
  Systemic = ifelse(sig_sint$`History of fever`==1 | sig_sint$`Fatigue / Malaise`==1, 1, 0), 
  Neurologic = ifelse(sig_sint$Headache==1 | sig_sint$Seizures==1 | sig_sint$Paralysis ==1 | sig_sint$`Hypotonia / floppiness`==1 | sig_sint$`Altered consciousness / confusion`==1, 1,0),
  Uncommon = ifelse(sig_sint$`Skin ulcers`==1 | sig_sint$`Bleeding (Haemorrhage)`==1 | sig_sint$`Swollen joints`==1 | sig_sint$`Stiff neck`==1,1,0)
)

long <- sig_sint %>% select(age_group:Uncommon)
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
  long %>% filter(Country=="Spain"), 
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

symptoms_age <- long %>%
  group_by(age_group, Country) %>%
  summarise(across(Respiratory:Uncommon, mean))

symptoms_age_n <- long %>%
  group_by(age_group, Country) %>%
  summarise(across(Respiratory:Uncommon, sum))


long <- melt(setDT(symptoms_age), id.vars = c("age_group","Country"), variable.name = "Sintoma")
long_n <- melt(setDT(symptoms_age_n), id.vars = c("age_group","Country"), variable.name = "Sintoma")
long$n <- long_n$value
long %<>% mutate(perc = round(value * 100, 1),
                 res = str_c(n, "(", perc, "%)"))

#library(openxlsx)
#write.xlsx(long, '~/EPICO/Datos/clasificacion_signossintomas.xlsx')
#----------------------------------------------------------------------------------------------------

tiff(file="symptoms.tiff",width=1400, height=600, res=100)
ggplot(long%>% filter(Sintoma != "uncommon"), aes(x = reorder(Sintoma,-n), y = perc, fill = Sintoma)) +
  geom_hline(yintercept = seq(0, 80, by = 20), colour = "grey90", size = 0.3) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Spectral") +
  facet_wrap(~Country+age_group,ncol = 4) + 
  theme_minimal() +
  labs(x = "Symptoms", size = 12) +
  theme(
    text=element_text(family="Times New Roman", size = 12),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size=12, hjust = 0),
    axis.title.y = element_blank(),
    panel.grid  = element_blank(),
    legend.position="right",
    legend.text = element_text(size=8),
    legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 1)) +
  coord_polar(start = 0) +
  scale_x_discrete(position = "top") +
  theme(plot.title = element_text(face = "bold", hjust = 0)) +
  geom_text(x = 0.5, y = 20, label = "20%",
            aes(family="Times New Roman"), size=3, colour = "grey60") + 
  geom_text(x = 0.5, y = 40, label = "40%",
            aes(family="Times New Roman"), size=3, colour = "grey60") +
  geom_text(x = 0.5, y = 60, label = "60%",
            aes(family="Times New Roman"), size=3, colour = "grey60") + 
  geom_text(x = 0.5, y = 80, label = "80%",
            aes(family="Times New Roman"), size=3, colour = "grey60") 
dev.off()

t_comp_all<- tbl_summary(
  sig_sint %>% select(-c("Country")), 
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
  sig_sint %>% filter(Country=="Colombia"), 
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
  sig_sint %>% filter(Country=="España"), 
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

