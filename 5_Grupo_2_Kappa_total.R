#Kappa coefficient- general

library(readxl)
library(irr)
library(openxlsx)
library("vcd")
library(tidyverse)
library(magrittr)

rm(list=ls())
#grupo_1_rater_1 <- read_excel("~/EPICO/Datos/Kappa/Semana 0 Grupo 2/Semana 0_Grupo 2 Meli.xlsx", sheet="Lector 1")
#grupo_1_rater_2 <- read_excel("~/EPICO/Datos/Kappa/Semana 0 Grupo 2/Semana 0_Grupo 2 Meli.xlsx", sheet="Lector 2")
todos <-read_excel("~/EPICO/Datos/Kappa/Unidos.xlsx", sheet="Grupo 2")
grupo_1_rater_1 <- todos %>% filter(LECTOR==1)
grupo_1_rater_2 <- todos %>% filter(LECTOR==2)


#Crear una variable nueva
grupo_1_rater_1 %<>% mutate(
  Localizacion = ifelse(`Engrosamiento peribronquial`=="SI" | `Opacidades Intersticiales` == "SI", "SI", "NO")
)
grupo_1_rater_2 %<>% mutate(
  Localizacion = ifelse(`Engrosamiento peribronquial`=="SI" | `Opacidades Intersticiales` == "SI", "SI", "NO")
)
grupo_1_rater_1 %<>% mutate(
  Alveolar = ifelse(`Opacidades Alveolares`=="SI" | Consolidación == "SI", "SI", "NO")
)
grupo_1_rater_2 %<>% mutate(
  Alveolar = ifelse(`Opacidades Alveolares`=="SI" | Consolidación == "SI", "SI", "NO")
)


merge_grupo_1 <- merge(grupo_1_rater_1,grupo_1_rater_2, by="REDCAP")
#write.xlsx(merge_grupo_1, '~/EPICO/Datos/Kappa/semana0_grupo1.xlsx')

#Kappa variables nuevas
#Localizacion
kappa_1 <- cbind(as.factor(merge_grupo_1$Localizacion.x), as.factor(merge_grupo_1$Localizacion.y))
kappa2(kappa_1)
t_kappa1<-table(as.data.frame(kappa_1))
t_kappa1
Kappa(t_kappa1) #Intervalos de confianz

#Alveolar
kappa_1 <- cbind(as.factor(merge_grupo_1$Alveolar.x), as.factor(merge_grupo_1$Alveolar.y))
kappa2(kappa_1)
t_kappa1<-table(as.data.frame(kappa_1))
t_kappa1
Kappa(t_kappa1) #Intervalos de confianz

#Engrosamiento peribronquial
kappa_1 <- cbind(as.factor(merge_grupo_1$`Engrosamiento peribronquial.x`), as.factor(merge_grupo_1$`Engrosamiento peribronquial.y`))
kappa2(kappa_1)
t_kappa1<-table(as.data.frame(kappa_1))
t_kappa1
Kappa(t_kappa1) #Intervalos de confianza

#Patron principal
kappa_patronprincipal <- cbind(as.factor(merge_grupo_1$`Patron principal.x`),as.factor(merge_grupo_1$`Patron principal.y`))
kappa2(kappa_patronprincipal)
t_kappa9<-table(as.data.frame(kappa_patronprincipal))
t_kappa9
Kappa(t_kappa9) #Intervalos de confianza