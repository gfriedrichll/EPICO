#Kappa coefficient 

library(readxl)
library(irr)
library(openxlsx)
library("vcd")
library(tidyverse)
library(magrittr)

rm(list=ls())
#grupo_1_rater_1 <- read_excel("~/EPICO/Datos/Kappa/Semana 0 Grupo 2/Semana 0_Grupo 2 Meli.xlsx", sheet="Lector 1")
#grupo_1_rater_2 <- read_excel("~/EPICO/Datos/Kappa/Semana 0 Grupo 2/Semana 0_Grupo 2 Meli.xlsx", sheet="Lector 2")

grupo_1_rater_1 <- read_excel("~/EPICO/Datos/Kappa/Semana 5 Grupo 2/Grupo 2 Lector 1.xlsx")
grupo_1_rater_2 <- read_excel("~/EPICO/Datos/Kappa/Semana 5 Grupo 2/Grupo 2 Lector 2.xlsx")

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

str(grupo_1_rater_1)


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


kappa_1 <- cbind(as.factor(merge_grupo_1$`Engrosamiento peribronquial.x`), as.factor(merge_grupo_1$`Engrosamiento peribronquial.y`))
#Engrosamiento peribronquial
kappa_1 <- cbind(as.factor(merge_grupo_1$`Engrosamiento peribronquial.x`), as.factor(merge_grupo_1$`Engrosamiento peribronquial.y`))
kappa2(kappa_1)
t_kappa1<-table(as.data.frame(kappa_1))
t_kappa1
Kappa(t_kappa1) #Intervalos de confianza

#Opacidad en vidrio esmerilado
kappa_2 <- cbind(as.factor(merge_grupo_1$`Opacidad en vidrio esmerilado.x`), as.factor(merge_grupo_1$`Opacidad en vidrio esmerilado.y`))
kappa2(kappa_2)
t_kappa2<-table(as.data.frame(kappa_2))
t_kappa2
Kappa(t_kappa2) #Intervalos de confianza

#Opacidades Intersticiales 
kappa_3 <- cbind(as.factor(merge_grupo_1$`Opacidades Intersticiales.x`), as.factor(merge_grupo_1$`Opacidades Intersticiales.y`))
kappa2(kappa_3)
t_kappa3<-table(as.data.frame(kappa_3))
t_kappa3 
Kappa(t_kappa3) #Intervalos de confianza

#Opacidades Alveolares 
kappa_4 <- cbind(as.factor(merge_grupo_1$`Opacidades Alveolares.x`), as.factor(merge_grupo_1$`Opacidades Alveolares.y`))
kappa2(kappa_4)
t_kappa4<-table(as.data.frame(kappa_4))
t_kappa4
Kappa(t_kappa4) #Intervalos de confianza



#Consolidación
kappa_5 <- cbind(as.factor(merge_grupo_1$Consolidación.x),as.factor(merge_grupo_1$Consolidación.y))
kappa2(kappa_5)
t_kappa5<-table(as.data.frame(kappa_5))
t_kappa5
Kappa(t_kappa5) #Intervalos de confianza

#Localicación
kappa_6 <- cbind(as.factor(merge_grupo_1$Localicación.x),as.factor(merge_grupo_1$Localicación.y))
kappa2(kappa_6)
t_kappa6<-table(as.data.frame(kappa_6))
t_kappa6
Kappa(t_kappa6) #Intervalos de confianza

#Distribución
kappa_7 <- cbind(as.factor(merge_grupo_1$Distribución.x),as.factor(merge_grupo_1$Distribución.y))
kappa2(kappa_7)
t_kappa7<-table(as.data.frame(kappa_7))
t_kappa7
Kappa(t_kappa7) #Intervalos de confianza

#Derrame pleural
kappa_8 <- cbind(as.factor(merge_grupo_1$`Derrame pleural.x`),as.factor(merge_grupo_1$`Derrame pleural.y`))
kappa2(kappa_8)
t_kappa8<-table(as.data.frame(kappa_8))
t_kappa8
Kappa(t_kappa8) #Intervalos de confianza

#Patron principal
kappa_patronprincipal <- cbind(as.factor(merge_grupo_1$`Patron principal.x`),as.factor(merge_grupo_1$`Patron principal.y`))
kappa2(kappa_patronprincipal)
t_kappa9<-table(as.data.frame(kappa_patronprincipal))
t_kappa9
Kappa(t_kappa9) #Intervalos de confianza





# modificados <- read_excel("~/EPICO/Datos/Kappa/Modificados/semana1_grupo2 (1).xlsx")
# kappa_patronprincipal <- cbind(as.factor(modificados$`Patron principal.x`),as.factor(modificados$`Patron principal.y`))
# kappa2(kappa_patronprincipal)
# t_kappa9<-table(as.data.frame(kappa_patronprincipal))
# t_kappa9




