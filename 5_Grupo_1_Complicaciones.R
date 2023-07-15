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
data %<>% mutate(edad_cat = ifelse(edad_anios <2, "Lactante", ifelse(edad_anios >=2 & edad_anios < 6, "Preescolar", ifelse(edad_anios>=6 & edad_anios <12, "Escolar", "Adolescente"))))
freq(data$edad_cat)
data %<>% filter(is.na(edad_cat)==F)
str(data$edad_cat)
data$edad_cat <- factor(data$edad_cat, ordered = TRUE, 
                        levels = c("Lactante", "Preescolar", "Escolar", "Adolescente"))
#-------------------------------------------------------------------------------

#Complications
complicaciones <- data[,c("complications","pleural_effusion","pneumatocele_abscess","pneumothorax","cardio_comp","sepsis","renal_failure","kawa","other_complications","record")]

#Recategorizar other complications en España: respiratorias
respiratorias <- c("30001-6","30003-22","30003-89","30013-9","30013-45","30014-27","30014-30","30040-11","30040-109","30048-20","30052-18","30052-27","30052-28","30052-37",
                   "30058-18","30070-25","")
complicaciones[complicaciones$record %in% respiratorias, c("other_complications")] <- ""
#Recategorizas other complicaciones en España: cardiovasculares
cardiovascular <- c("30001-8","30001-10","30001-11","30001-12","30001-13","30001-57","30003-34","30003-48","30003-50","30003-51","30003-52","30003-120","30011-48","30011-65",
                    "30011-66","30013-11","30013-13","30013-14","30014-3","30027-7","30033-14","30040-3","30040-23","30040-62","30042-9","30044-9","30048-12","30048-17","30048-18","30052-7"
                    ,"30070-19")
complicaciones[complicaciones$record %in% cardiovascular, c("other_complications")] <- ""
#Recategorizas other complicaciones en España: neurologicas
neurologicas <- c("30001-46","30001-54","30003-31","30003-182","30003-184","30006-6","30013-48","30022-1","30027-5","30041-2","30042-7")
complicaciones[complicaciones$record %in% neurologicas, c("other_complications")] <- ""
#Recategorizas other complicaciones en España: misc 
misc <- c("30001-59","30003-44","30003-45","30003-47","30003-92","30011-13","30015-1","30022-7","30039-1","30040-13","30042-33","30055-17","30085-6")
complicaciones[complicaciones$record %in% misc, c("other_complications")] <- ""
#Recategorizas other complicaciones en España: infecciones 
infecciones <- c("30001-147","30003-8","30003-35","30003-36","30003-102","30003-129","30003-137","30004-3","30008-3","30011-35","30022-5","30032-6","30033-14","30029-20",
                 "30034-4","30034-21","30036-3","30042-6","30044-2","30048-8","30049-1","30052-9","30055-11","30058-22","30070-21","30071-19","30071-20","30085-5","30092-5",
                 "30003-6","30003-11","30003-49")
complicaciones[complicaciones$record %in% infecciones, c("other_complications")] <- ""
#Recategorizas other complicaciones en España: renal
renal <- c("30003-43","30008-1","30027-21")
complicaciones[complicaciones$record %in% renal, c("other_complications")] <- ""
#Recategorizas other complicaciones en España: eliminar campo
eliminar <- c("30092-3","30092-2","30092-1","30048-40","30048-7","30013-43","30013-12","30007-13","30003-25","30003-15","30001-16")
complicaciones[complicaciones$record %in% eliminar, c("other_complications")] <- ""

# Creating vector
vector <- data$other_complications %>% tolower()
# Loading library
library(stringr)
r1<-str_detect(vector, "encephalitis")
r2<-str_detect(vector, "encefal ")
r3<-str_detect(vector, "ictal")
r4<-str_detect(vector, "convul")
r5<-str_detect(vector, "edema")
complicaciones %<>% mutate(Neurological = ifelse(r1==T | r2==T | r3==T| r4==T | r5==T | record %in% neurologicas, 1,0))
complicaciones %<>% mutate(Other = ifelse(other_complications !="" & Neurological!=1, 1,0))
complicaciones$age_group <- data[,"edad_cat"]
complicaciones$Country <- data[,"Country"]
complicaciones$record <- data[,"record"]
complicaciones[is.na(complicaciones)] <-"No"
complicaciones <- na.omit(complicaciones)
complicaciones[,1:9]<-lapply(complicaciones[,1:9],as.factor)
complicaciones[,1:9]<-lapply(complicaciones[,1:9],as.numeric)
complicaciones %<>% as.data.frame()
complicaciones[,1:9][complicaciones[,1:9]=="1"] <-0
complicaciones[,1:9][complicaciones[,1:9]=="2"] <-1
complicaciones %<>% mutate(Respiratory = ifelse(pleural_effusion==1 | pneumatocele_abscess==1 | pneumothorax==1 | record %in% respiratorias,1,0),
                           Cardiovascular = ifelse(cardio_comp=="Yes" | record %in% cardiovascular,1,0),
                           MISC = ifelse(kawa=="Yes" | record %in% misc, 1,0), 
                           Infecctions = ifelse(sepsis=="Yes"| record %in% infecciones,1,0),
                           Renal = ifelse(renal_failure=="Yes" |record %in% renal,1,0),
                           Neurological_comp = Neurological,
                           Other_comp = Other)

long <- complicaciones[,c(13:21)]

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

complicaciones_age <- long %>%
  group_by(age_group,Country) %>%
  summarise(across(Respiratory:Other_comp, mean))

complicaciones_age_n <- long %>%
  group_by(age_group,Country) %>%
  summarise(across(Respiratory:Other_comp, sum))

long <- melt(setDT(complicaciones_age), id.vars = c("age_group","Country"), variable.name = "Complicaciones")
long_n <- melt(setDT(complicaciones_age_n), id.vars = c("age_group","Country"), variable.name = "Complicaciones")
long$n <- long_n$value
long %<>% mutate(perc = round(value * 100, 1),
                 res = str_c(n, "(", perc, "%)"))

ggplot(long, aes(x = reorder(Complicaciones,-n), y = perc, fill = Complicaciones)) +
  geom_hline(yintercept = seq(0, 10, by = 2.5), colour = "grey90", size = 0.3) + 
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
  geom_text(x = 0.5, y = 2.5, label = "2.5%",
            aes(family="Times New Roman"), size=3, colour = "grey60") + 
  geom_text(x = 0.5, y = 5, label = "5%",
            aes(family="Times New Roman"), size=3, colour = "grey60") +
  geom_text(x = 0.5, y = 7.5, label = "7.5%",
            aes(family="Times New Roman"), size=3, colour = "grey60") + 
  geom_text(x = 0.5, y = 10, label = "10%",
            aes(family="Times New Roman"), size=3, colour = "grey60")



#-------------------------------------------------------------------------------

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

rm(list=ls())
base1 <- read_excel("C:/Users/gfrie/Downloads/complicaciones_cambios.xlsx", sheet="changes")
#data=read.csv('~/EPICO/Datos/datos_todos_01272023.csv')
data=read.csv('~/EPICO/Datos/datos_todos_analisis_sinneonatos_9MAY2023.csv')


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
data$age_group <- factor(data$edad_cat, ordered = TRUE, 
                         levels = c("Lactante", "Preescolar", "Escolar", "Adolescente"))
#-------------------------------------------------------------------------------

data_complicaciones <- data %>% select(c("record","Country","age_group","complications":"other_complications"))
merge<-merge(data_complicaciones,base1,by="record", all.x = T)

table(merge$change)

merge %<>% mutate(
  other_changes = ifelse(change=="Dejar en categoría de cardiovascular" | change =="Dejar en categoría de cardiovasculares", "Cardiovascular",
                         ifelse(change=="Dejar en categoría de de inflamación sistémica" | change=="Dejar en categoría de inflamación sistémica", "Inflamación Sistémica",
                                ifelse(change=="Dejar en categoría de gastrointestinal"|change=="Dejar en categoría de gastrointestinales","Gastrointestinales",
                                       ifelse(change=="Dejar en categoría de infrecuentes","Infrecuentes",
                                              ifelse(change=="Dejar en categoria de neurológicas" | change=="Dejar en categoría de neurológicas" | change=="Dejar en categoría de neurológico" | change=="Dejar en categoría de neurológicas\r\nDejar en categoría de gastrointestinal", "Neurológicas",
                                                     ifelse(change=="Dejar en categoría de renales", "Renales", ifelse(change=="Dejar en categoría de respiratorias","Respiratorias",NA)))))))
)

table(merge$other_changes)


merge %<>% mutate(Cardiovascular = ifelse(cardio_comp=="Yes" | other_changes=="Cardiovascular",1,0),
                  Respiratorias = ifelse(pleural_effusion==1 | pneumatocele_abscess==1 | pneumothorax==1 | other_changes=="Respiratorias",1,0),
                  Gastrointestinales = ifelse(other_changes=="Gastrointestinales",1,0),
                  Renales = ifelse(renal_failure=="Yes" |other_changes == "Renales",1,0),
                  Neurologicas = ifelse(other_changes=="Neurológicas",1,0),
                  Inflamacion_sistemica = ifelse(kawa=="Yes" | sepsis=="Yes" | other_changes=="Inflamación Sistémica", 1,0), 
                  Infrecuentes = ifelse(other_changes=="Infrecuentes",1,0))

merge[is.na(merge)] <-0
long <- merge[,c(2,3,30:36)]

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


complicaciones_age <- long %>%
  group_by(age_group,Country) %>%
  summarise(across(Cardiovascular:Infrecuentes, mean))

complicaciones_age_n <- long %>%
  group_by(age_group,Country) %>%
  summarise(across(Cardiovascular:Infrecuentes, sum))

long <- melt(setDT(complicaciones_age), id.vars = c("age_group","Country"), variable.name = "Complicaciones")
long_n <- melt(setDT(complicaciones_age_n), id.vars = c("age_group","Country"), variable.name = "Complicaciones")
long$n <- long_n$value
long %<>% mutate(perc = round(value * 100, 1),
                 res = str_c(n, "(", perc, "%)"))

ggplot(long, aes(x = reorder(Complicaciones,-n), y = perc, fill = Complicaciones)) +
  geom_hline(yintercept = seq(0, 20, by = 5), colour = "grey90", size = 0.3) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Spectral") +
  facet_wrap(~Country+age_group,ncol = 4) + 
  theme_minimal() +
  labs(x = "Complications", size = 12) +
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


