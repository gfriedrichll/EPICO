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
lista_misc=read_excel('~/EPICO/Datos/lista_misc.xlsx')
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

#Duracion promedio de fiebre en dias
summary(data$fever_days)
summary(data$fever_days_total)
sd(data$fever_days_total, na.rm = T)

#% vacunacion
data_colombia <- data %>% filter(Country=="Colombia")
data_spain <- data %>% filter(Country=="España")

freq(data$vaccines_complete)
freq(data_colombia$vaccines_complete)
freq(data_spain$vaccines_complete)
freq(data$influenza_full_regimen)
freq(data_colombia$influenza_full_regimen)
freq(data_spain$influenza_full_regimen)

#Paraclinicos
paraclinicos <- data %>% select(
  edad_cat,
  Country,
  hb,
  leukocytes,
  neutrophils,
  lymphocytes,
  platelets,
  alt_gpt,
  ast_got,
  ldh,
  trig,
  bilirubin,
  lactate,
  ph,
  po2,
  pco2,
  c_reactive_protein,
  procalcitonin,
  inr,
  creatinin,
  albumin,
  sodium,
  d_dimer,
  il_6)

tbl_summary(
  paraclinicos, 
  by = edad_cat, 
  percent="col", 
  missing="always" #no sacar missing como categoria 
) %>%
  add_n() %>% #n?mero total de observaciones 
  #add_p() %>% #diferencia entre grupos (chi2)
  modify_header(label = "**Variable**") %>% 
  bold_labels() 
tbl_summary(
  paraclinicos %>% filter(Country=="Colombia"), 
  by = edad_cat, 
  percent="col", 
  missing="always" #no sacar missing como categoria 
) %>%
  add_n() %>% #n?mero total de observaciones 
  #add_p() %>% #diferencia entre grupos (chi2)
  modify_header(label = "**Variable**") %>% 
  bold_labels() 
tbl_summary(
  paraclinicos %>% filter(Country=="España"), 
  by = edad_cat, 
  percent="col", 
  missing="always" #no sacar missing como categoria 
) %>%
  add_n() %>% #n?mero total de observaciones 
  #add_p() %>% #diferencia entre grupos (chi2)
  modify_header(label = "**Variable**") %>% 
  bold_labels() 

#Oxygen therapy
oxygen_therapies <- data %>% select(oxygen_therapy,hft,cpap,nimv,imv, edad_cat, Country)
tbl_summary(
  oxygen_therapies, 
  by = edad_cat, 
  percent="col", 
  missing="always" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #n?mero total de observaciones 
  #add_p() %>% #diferencia entre grupos (chi2)
  modify_header(label = "**Variable**") %>% 
  bold_labels() 
tbl_summary(
  oxygen_therapies %>% filter(Country=="Colombia"), 
  by = edad_cat, 
  percent="col", 
  missing="always" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #n?mero total de observaciones 
  #add_p() %>% #diferencia entre grupos (chi2)
  modify_header(label = "**Variable**") %>% 
  bold_labels() 
tbl_summary(
  oxygen_therapies %>% filter(Country=="España"), 
  by = edad_cat, 
  percent="col", 
  missing="always" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #n?mero total de observaciones 
  #add_p() %>% #diferencia entre grupos (chi2)
  modify_header(label = "**Variable**") %>% 
  bold_labels()

#	% ingreso uci%, media de estancia uci.
freq(data$picu_admission)
summary(data$picu_time)
sd(data$picu_time, na.rm = T)
#Estancia global hospitalizacion. 
summary(data$hosp_time)
sd(data$hosp_time, na.rm = T)

summary(data_colombia$hosp_time)
sd(data_colombia$hosp_time, na.rm = T)

summary(data_spain$hosp_time)
sd(data_spain$hosp_time, na.rm = T)

#Radiografias
freq(data$x_ray_1)
freq(data$x_ray_2)
freq(data$x_ray_3)


data$x_ray_1[data$x_ray_1=="[document]"] <- 1
data$x_ray_2[data$x_ray_2=="[document]"] <- 1
data$x_ray_3[data$x_ray_3=="[document]"] <- 1

data$x_ray_1[data$x_ray_1==""] <- 0
data$x_ray_2[data$x_ray_2==""] <- 0
data$x_ray_3[data$x_ray_3==""] <- 0
data %<>%
  mutate(num_xrays = as.numeric(x_ray_1)+as.numeric(x_ray_2)+as.numeric(x_ray_3))

freq(data$num_xrays)
rx_data <- data %>% filter(x_ray_1 == 1 | x_ray_2 == 1 | x_ray_3 == 1)
freq(rx_data$Country) 

#Hallazgos cardiologicos mas frecuentes
freq(data$cardio_comp)
data %>% filter(cardio_comp=="Yes") %>% select(cardio_comp_myoc, cardio_comp_peri, cardio_comp_valve, cardio_comp_arry, cardio_comp_cor, cardio_comp_ane) %>% freq()

#% recibió reposicion hidrica oral, endovenosa, 
freq(data$oro_fluids)
freq(data$iv_fluids)

#•	%sims tac
freq(data$kawa)

#% terapia antiviral, tipo de terapia, duradion
freq(data$antiviral_cmyn)
summary(data$antiviral_days)
sd(data$antiviral_days, na.rm = T)
table(data$antiviral_cmyn,data$Country)
freq(data$antiviral_cmtrt___1)
freq(data$antiviral_cmtrt___2)
freq(data$antiviral_cmtrt___3)
freq(data$antiviral_cmtrt___4)
freq(data$antiviral_cmtrt___5)
freq(data$antiviral_cmtrt___6)
freq(data$antiviral_cmtrt___7)
freq(data$antiviral_cmtrt___8)
freq(data$antiviral_cmtrt___10)
freq(data$antiviral_cmtrt___11)
freq(data$antiviral_cmtrt___9)
freq(data$antiviral_cmtype)

#•	% antibiótico, iv, via oral. Cuales antibióticos
freq(data$antibiotic_cmyn)
table(data$antibiotic_cmyn,data$Country)
freq(data$iv_ab)
freq(data$type_iv_ab___0)
freq(data$type_iv_ab___1)
freq(data$type_iv_ab___2)
freq(data$type_iv_ab___3)
freq(data$type_iv_ab___4)
freq(data$type_iv_ab___5)
freq(data$type_iv_ab___6)
freq(data$type_iv_ab___9)
freq(data$type_iv_ab___8)
freq(data$type_iv_ab___7)
freq(data$other_iv_ab)


freq(data$o_ab)
freq(data$type_o_ab___1)
freq(data$type_o_ab___2)
freq(data$type_o_ab___3)
freq(data$type_o_ab___4)
freq(data$type_o_ab___5)
freq(data$type_o_ab___6)
freq(data$type_o_ab___7)
freq(data$type_o_ab___8)
freq(data$other_o_ab)

#•	% corticoide, duracion, tipo corticoide, 
freq(data$corticost_cmyn)
freq(data$corticost_cmtrt)

# •	% uso antiinflamatorios,
freq(data$anti_inflammatory___1)
freq(data$anti_inflammatory___2)

#•	% uso inmunoglobulina, dosis, duración
freq(data$iv_imglob)
summary(data$iv_imglob_days)
sd(data$iv_imglob_days, na.rm = T)
freq(data$iv_imglob_dose)

#•	% uso inotrópico, cual, 
freq(data$vasopressors)
freq(data$vasopressors_type___1)
freq(data$vasopressors_type___2)
freq(data$vasopressors_type___3)
freq(data$vasopressors_type___4)
freq(data$vasopressors_type___5)
freq(data$vasopressors_type___6)

#Ecmo
freq(data$ecmo)

#Trasnfusiones
freq(data$blood_trans)

#Diagnosticos
freq(data$final_dx_1)

#•	% punción lumbar, hallazgos citoquimicos.
freq(data$lumbar_punc)

#•	% urocultivo 
freq(data$urine_culture)

# Mortalidad
freq(data$dsterm)
 %>% group_by(Country,edad_cat) %>% select(dsterm) %>% summarise_all()

tbl_summary(
  data%>% select(dsterm,edad_cat), 
  by = edad_cat, 
  percent="col",
  digits = list(all_categorical() ~ c(0, 0))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  #add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels()

tbl_summary(
  data %>% filter(Country=="Colombia")%>% select(dsterm,edad_cat), 
  by = edad_cat, 
  percent="col",
  digits = list(all_categorical() ~ c(0, 0))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  #add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels()
#freq(data$dsterm)
#library("writexl")
#write_xlsx(data,"~/EPICO/Datos/datos_todos_excel.xlsx")
#data %>% filter(Country=="Colombia" & cardio_comp=="Yes" & (cardio_comp_ane=="Yes" | cardio_comp_cor=="Yes")) %>% select(record, hospital, cardio_comp, cardio_comp_ane, cardio_comp_cor)

##Demgraphics: 
demographics <- data %>% select(Country, sex, edad_cat)

t_demographics<- tbl_summary(
  demographics, 
  by = Country, 
  percent="col",
  digits = list(all_categorical() ~ c(0, 0))
  #missing="no" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels() 

t_demographics

##Signs and symptoms
#----------------------------------------------------------------------------------------------------
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

long <- sig_sint[,age_group:Uncommon]
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
    legend.text = element_text(size=9),
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






##Comorbilidades
#----------------------------------------------------------------------------------------------------

which(colnames(data)=="chronicneu_mhyn")
pos1 <- which(colnames(data)=="chroniccard_mhyn")
pos2 <- which(colnames(data)=="malnutrition_mhyn")
pos3 <- which(colnames(data)=="otherrisktext")
pos4 <- which(colnames(data)=="immunosupressors")
comorbilidades<-data[,c(pos1:pos2,pos3,pos4)]
comorbilidades$age_group <- data[,"edad_cat"]
comorbilidades$Country <- data[,"Country"]
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
comorbilidades %<>% select(-"otherrisktext")

label <- c("Chronic cardiac disease, including congenital heart disease (not hypertesnion)",
           "Hypertension","Chronic pulmonary disease (not asthma)","Asthma (or recurrent wheezing)",
           "Tuberculosis","History of respiratory infection in the previous 4 weeks prior to current illness?",
           "Chronic kidney disease","Moderate or severe liver disease","Mild Liver disease","Chronic neurological disorder",
           "Malignant neoplasm","Chronic hematologic disease","AIDS/HIV","Obesity (as defined by clinical staff)",
           "Diabetes with complications","Diabetes without complications","Inflammatory disorder","Rheumatologic disorder",
           "Past history of Kawasaki disease","Family history of Kawasaki disease","Dementia","Malnutrition","On immunosuppressive medication","age_group","Country","Premature/Low birth weight")
colnames(comorbilidades) <-label

comorbilidades %<>% mutate(
  Respiratory = ifelse(comorbilidades$`Chronic pulmonary disease (not asthma)`==1 | comorbilidades$`History of respiratory infection in the previous 4 weeks prior to current illness?`==1 | comorbilidades$Tuberculosis==1 | comorbilidades$`Asthma (or recurrent wheezing)`==1, 1,0),
  Cardiovascular = ifelse(comorbilidades$`Chronic cardiac disease, including congenital heart disease (not hypertesnion)`==1 | comorbilidades$Hypertension==1, 1, 0),
  Kidney_disease = ifelse(comorbilidades$`Chronic kidney disease`==1,1,0),
  Liver_disease = ifelse(comorbilidades$`Mild Liver disease`==1 | comorbilidades$`Moderate or severe liver disease`==1, 1, 0),
  Nutritional_Metabolic = ifelse(comorbilidades$Malnutrition==1 | comorbilidades$`Obesity (as defined by clinical staff)`==1 | comorbilidades$`Diabetes with complications`==1 | comorbilidades$`Diabetes without complications`==1, 1, 0),
  Inflammatory = ifelse(comorbilidades$`Inflammatory disorder`==1 | comorbilidades$`Rheumatologic disorder`==1 | comorbilidades$`Past history of Kawasaki disease`==1 | comorbilidades$`Family history of Kawasaki disease`==1, 1, 0),
  Neurological = ifelse(comorbilidades$`Chronic neurological disorder`==1 | comorbilidades$Dementia==1, 1, 0),
  Oncohematological = ifelse(comorbilidades$`Malignant neoplasm`==1 | comorbilidades$`Chronic hematologic disease`==1, 1,0),
  Immunosuppressive_medication = ifelse(comorbilidades$`On immunosuppressive medication`=="Yes",1,0),
  Premature_Lowbirth_weight = ifelse(comorbilidades$`Premature/Low birth weight`==1, 1, 0)
)
long <- comorbilidades[,24:35]

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


ggplot(long, aes(x = reorder(Comorbilidad,-n), y = perc, fill = Comorbilidad)) +
  geom_hline(yintercept = seq(0, 25, by = 5), colour = "grey90", size = 0.3) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Spectral") +
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
  geom_text(x = 0.5, y = 5, label = "5%",
            aes(family="Times New Roman"), size=3, colour = "grey60") + 
  geom_text(x = 0.5, y = 10, label = "10%",
            aes(family="Times New Roman"), size=3, colour = "grey60") +
  geom_text(x = 0.5, y = 15, label = "15%",
            aes(family="Times New Roman"), size=3, colour = "grey60") + 
  geom_text(x = 0.5, y = 20, label = "20%",
            aes(family="Times New Roman"), size=3, colour = "grey60") +
  geom_text(x = 0.5, y = 25, label = "25%",
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

#Complications
#----------------------------------------------------------------------------------------------------

complicaciones <- data[,c("complications","pleural_effusion","pneumatocele_abscess","pneumothorax","cardio_comp","sepsis","renal_failure","kawa","other_complications")]
#complicaciones$other_complications[data$other_complications != ""] <- "Yes"
#complicaciones[is.na(complicaciones)==T] <- "No"
complicaciones$other_complications[data$other_complications == ""] <- "No"
# Creating vector
vector <- data$other_complications %>% tolower()
# Loading library
library(stringr)
r1<-str_detect(vector, "encephalitis")
r2<-str_detect(vector, "encefal ")
r3<-str_detect(vector, "ictal")
r4<-str_detect(vector, "convul")
r5<-str_detect(vector, "edema")
complicaciones %<>% mutate(Neurological = ifelse(r1==T | r2==T | r3==T| r4==T | r5==T, 1,0))
#complicaciones %<>% mutate(Other = ifelse( (r1!=T | r2!=T | r3!=T| r4!=T | r5!=T) & other_complications=="Yes", 1,0))
#freq(complicaciones$other_complications)
#freq(complicaciones$Neurological)
#freq(complicaciones$Other)
complicaciones$age_group <- data[,"edad_cat"]
complicaciones$Country <- data[,"Country"]
complicaciones[is.na(complicaciones)] <-"No"
complicaciones <- na.omit(complicaciones)
complicaciones[,1:9]<-lapply(complicaciones[,1:9],as.factor)
complicaciones[,1:9]<-lapply(complicaciones[,1:9],as.numeric)
complicaciones %<>% as.data.frame()
complicaciones[,1:9][complicaciones[,1:9]=="1"] <-0
complicaciones[,1:9][complicaciones[,1:9]=="2"] <-1
complicaciones %<>% select(-complications)
complicaciones %<>% mutate(Respiratory = ifelse(pleural_effusion==1 | pneumatocele_abscess==1 | pneumothorax==1,1,0))
label <- c("Pleural effusion",
           "Necrotizing pneumonia / abscess / pneumatocele",
           "Pneumothorax",
           "Cardiological complications",
           "Sepsis",
           "Renal failure",
           "Kawasaki-like / Inflammatory Syndrome symptoms",
           "Other complications","Neurological",
           "age_group","Country","Respiratory")
colnames(complicaciones) <-label
long <- complicaciones[,c(4:7,12,9:11)]
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



complicaciones_age <- long %>%
  group_by(age_group,Country) %>%
  summarise(across(`Cardiological complications`:Neurological, mean))

complicaciones_age_n <- long %>%
  group_by(age_group,Country) %>%
  summarise(across(`Cardiological complications`:Neurological, sum))

long <- melt(setDT(complicaciones_age), id.vars = c("age_group","Country"), variable.name = "Complicaciones")
long_n <- melt(setDT(complicaciones_age_n), id.vars = c("age_group","Country"), variable.name = "Complicaciones")
long$n <- long_n$value
long %<>% mutate(perc = round(value * 100, 1),
                 res = str_c(n, "(", perc, "%)"))


long %<>% mutate(OrganSystem=ifelse(
  Complicaciones=="Pleural effusion"
  | Complicaciones=="Necrotizing pneumonia / abscess / pneumatocele"
  | Complicaciones=="Pneumothorax","Respiratory", ifelse(
    Complicaciones=="Sepsis", "Sepsis", ifelse(
      Complicaciones=="Cardiological complications","Cardiovascular", ifelse(
        Complicaciones=="Renal failure", "Renal", ifelse(Complicaciones=="Kawasaki-like / Inflammatory Syndrome symptoms","MISC",
                                                         ifelse(Complicaciones=="Neurological","Neurological","Other")))))))

#----------------------------------------------------------------------------------------------------


t_comp_all<- tbl_summary(
  complicaciones %>% select(-c("Country","Other complications","Pleural effusion",
                               "Necrotizing pneumonia / abscess / pneumatocele",
                               "Pneumothorax")), 
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
  complicaciones %>% select(-c("Other complications","Pleural effusion",
                               "Necrotizing pneumonia / abscess / pneumatocele",
                               "Pneumothorax")) %>% filter(Country=="Colombia"), 
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
  complicaciones %>% select(-c("Other complications","Pleural effusion",
                               "Necrotizing pneumonia / abscess / pneumatocele",
                               "Pneumothorax")) %>% filter(Country=="España"), 
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

ggplot(long, aes(x = reorder(Complicaciones,-n), y = perc, fill = Complicaciones)) +
  geom_hline(yintercept = seq(0, 20, by = 5), colour = "grey90", size = 0.3) + 
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
    legend.text = element_text(size=9),
    legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 1)) +
  coord_polar(start = 0) +
  scale_x_discrete(position = "top") +
  theme(plot.title = element_text(face = "bold", hjust = 0)) +
  geom_text(x = 0.5, y = 5, label = "5%",
            aes(family="Times New Roman"), size=3, colour = "grey60") + 
  geom_text(x = 0.5, y = 10, label = "10%",
            aes(family="Times New Roman"), size=3, colour = "grey60") +
  geom_text(x = 0.5, y = 15, label = "15%",
            aes(family="Times New Roman"), size=3, colour = "grey60") + 
  geom_text(x = 0.5, y = 20, label = "20%",
            aes(family="Times New Roman"), size=3, colour = "grey60")



## Codetection
codetection <-  data %>% select(
  edad_cat,
  Country,
  codetection,
  codetecion_type)


t_codetection_all<- tbl_summary(
  codetection %>% select(-Country), 
  by = edad_cat, 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2)),
  missing="always" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels() 

t_codetection_col<- tbl_summary(
  codetection %>% filter(Country=="Colombia"), 
  by = edad_cat, 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2)),
  missing="always" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels() 

t_codetection_esp<- tbl_summary(
  codetection %>% filter(Country=="España"), 
  by = edad_cat, 
  percent="col",
  digits = list(all_categorical() ~ c(0, 2)),
  missing="always" #no sacar missing como categoria 
) %>%
  add_overall() %>%
  add_n() %>% #number of observations
  add_p() %>% #difference between groups
  modify_header(label = "**Variable**") %>% 
  bold_labels() 

t_codetection_all
t_codetection_col
t_codetection_esp



## Oxigenoterapia
oxygen_therapies <- data %>% select(oxygen_therapy,hft,cpap,nimv,imv, edad_cat, Country)
oxygen_therapies[is.na(oxygen_therapies)] <-"No"

oxygen_therapies %<>%
  mutate(ot_tyoe = ifelse(imv=="Yes", "imv", 
                          ifelse(imv=="No" & nimv=="Yes", "nimv",
                                 ifelse(imv=="No" & nimv=="No" & hft=="Yes","hft",
                                        ifelse(imv=="No" & nimv=="No" & hft=="No"& cpap=="Yes", "cpap", 
                                               ifelse(imv=="No" & nimv=="No" & hft=="No"& cpap=="No" & oxygen_therapy=="Yes","Oxygen","No oxygen therapy"))))))


oxygen_therapies$ot_tyoe %<>% as.factor()
freq(oxygen_therapies$ot_tyoe)

oxygen_therapies$ot_tyoe <- factor(oxygen_therapies$ot_tyoe, ordered = TRUE, 
                                   levels = c("Oxygen", "cpap", "hft", "nimv","imv","No oxygen therapy"))

t_oxygentherapy_all<- tbl_summary(
  oxygen_therapies %>% select(-c(oxygen_therapy,hft,cpap,nimv,imv,Country)), 
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
  oxygen_therapies %>% select(-c(oxygen_therapy,hft,cpap,nimv,imv)) %>% filter(Country=="Colombia"), 
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
  oxygen_therapies %>% select(-c(oxygen_therapy,hft,cpap,nimv,imv)) %>% filter(Country=="España"), 
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



#Epidemic curves: https://epirhandbook.com/en/epidemic-curves.html#epidemic-curves
pacman::p_load(
  rio,          # file import/export
  here,         # relative filepaths 
  lubridate,    # working with dates/epiweeks
  aweek,        # alternative package for working with dates/epiweeks
  incidence2,   # epicurves of linelist data
  i2extras,     # supplement to incidence2
  stringr,      # search and manipulate character strings
  forcats,      # working with factors
  RColorBrewer, # Color palettes from colorbrewer2.org
  tidyverse     # data management + ggplot2 graphics
) 

#Importar set de datos
curva <- data %>% select(Country, pcr_cov2_1_date, edad_cat)
curva$Date <- format(as.Date(curva$pcr_cov2_1_date), "%Y-%m")
curva %<>% filter(Date < "2021-12")
curva %<>% filter(Date > "2020-03")

#Count data
count_curva <- curva %>%
  group_by(Date,edad_cat, Country) %>%
  summarise(n=n())

#Report date
data_date <-Sys.Date()

# check range of onset dates
ggplot(data = curva)+
  geom_histogram(aes(x = pcr_cov2_1_date))

# create the incidence object, aggregating cases by day
epi_colombia<- incidence(       # create incidence object
  x = curva %>% filter(Country=="Colombia"),             # dataset
  date_index = pcr_cov2_1_date,  # date column
  interval = "month",# date grouping interval
  groups = edad_cat
)
summary(epi_colombia)

epi_spain<- incidence(       # create incidence object
  x = curva %>% filter(Country=="España"),             # dataset
  date_index = pcr_cov2_1_date,  # date column
  interval = "month",# date grouping interval
  groups = edad_cat
)
summary(epi_spain)

library("RColorBrewer")
display.brewer.all()

#Personalize x axis: 

plot(
  epi_colombia,             # incidence object with age_cat as group
  fill = edad_cat, title="Casos COVID 19 EPICO Colombia")+          # age_cat is used for bar fill color (must have been set as a groups column above)
  labs(fill = "Age Category")+ 
  scale_fill_manual(values = c("#00628F", "#097EFF", "#00A7F5","#5CCBFF")) # change legend title from default "age_cat" (this is a ggplot2 modification)

plot(
  epi_spain,             # incidence object with age_cat as group
  fill = edad_cat, title="Casos COVID 19 EPICO España")+          # age_cat is used for bar fill color (must have been set as a groups column above)
  labs(fill = "Age Category")+ # change legend title from default "age_cat" (this is a ggplot2 modification)
  scale_fill_manual(values = c("#00628F", "#097EFF", "#00A7F5","#5CCBFF")) # change legend title from default "age_cat" (this is a ggplot2 modification)





