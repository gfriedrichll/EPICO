#An?lisis Grupo Clusters: Base de datos sensibiliad (incluye dsterm NA) sin neonatos
#C?digo por: Gabriela Friedrich 
#Fecha ?ltima modificaci?n: 06/05/2023

rm(list=ls())
data=read.csv('~/EPICO/Datos/datos_todos_analisis_sinneonatos_9MAY2023.csv')

#Cargar librer?as
#----------------------------
library(factoextra)
library(FactoMineR)
library(gtsummary)
library(knitr) #Para mejorar calidad de reportes
library(tidyverse) #Manipulacion y visualizaci?n de data
library(magrittr) #Manipulaci?n de data
library(kableExtra) #Mejorar calidad de reportes
library(dlookr) #Si no funciona reinstalar "rmarkdown". Para diagnosticos generales
library(lubridate) #Manejo de fechas
library(Hmisc)
library(plotly)
library(summarytools)
library(miceadds)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(reshape2)
library(purrr)
library(dplyr)
library(factoextra)
library(cluster)
library(fpc)
library(sets)
library(dendextend)
#----------------------------

##Arreglar formato de variables fecha:
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
#-------------------------------------------------------------------------------

#Variables: 
#---------------------------------

#Recodificar variable edad
data$admission_date %<>% as.Date()
data$discharge_date %<>% as.Date()
data$patient_birth_date %<>% as.Date()
#Eliminar datos con edad missing
data %<>% filter(is.na(age_months)==F)
#Grupo etario
data %<>% mutate(edad_anios = year(data$admission_date) - year(data$patient_birth_date))
data %<>% mutate(edad_dias = data$admission_date - data$patient_birth_date)
data %>% select(record,Country, edad_dias, edad_anios, age_months, admission_date, patient_birth_date) %>% filter(edad_dias < 0)

data %<>% mutate(edad_cat = ifelse(edad_anios <2, "Lactante", ifelse(edad_anios >=2 & edad_anios < 6, "Preescolar", ifelse(edad_anios>=6 & edad_anios <12, "Escolar", "Adolescente"))))
data$edad_cat <- factor(data$edad_cat, ordered = TRUE, 
                        levels = c("Lactante", "Preescolar", "Escolar", "Adolescente"))

freq(data$edad_cat)

## Recodificar paraclinicos
#Valor en 0 es NA
data$lymphocytes[data$lymphocytes<=1] <- NA
data$hb[data$hb <= 1] <- NA
data$platelets[data$platelets<=1] <- NA
#Linfocitos
data %<>%
  mutate(cat_lymphocytes = ifelse( (lymphocytes<=1400 & edad_cat=="Lactante") | (lymphocytes<=2000 & edad_cat=="Preescolares") | (lymphocytes<=850 & edad_cat=="Escolares") | (lymphocytes<=680 & edad_cat=="Adolescentes"), "Baja", ifelse( (lymphocytes>=16500 & edad_cat=="Lactante") | (lymphocytes>= 15400 & edad_cat=="Preescolares") | (lymphocytes>= 9700 & edad_cat=="Escolares") | (lymphocytes>=7400 & edad_cat=="Adolescentes"), "Alta", "Normal")))
data$cat_lymphocytes %<>% as.factor()
#Hemoglobina (anemia o no anemia)
data %<>%
  mutate(cat_hb = ifelse( (hb<=10.5 & edad_cat=="Lactante") | (hb<=11.5 & edad_cat=="Preescolares") | (hb<=11.5 & edad_cat=="Escolares") | (hb<=12.0 & edad_cat=="Adolescentes"), "Anemia", "No anemia"))
data$cat_hb %<>% as.factor()
#Plaquetas: Revisar como categorizar por grupo de edad
data %<>%
  mutate(cat_platelets = ifelse(platelets < 50000, "Baja", ifelse(platelets > 100000, "Alto", "Normal")))
data$cat_platelets %<>% as.factor()
#Death
data %<>%
  mutate(death = ifelse(dsterm=="Death","Death","Discharged alive"))
data %>% filter(is.na(dsterm)==T) %>% select(Country)
#Recodificar comorbilidades en respiratorias vs no respiratorias vs no comorbilidad
pos1 <- which(colnames(data)=="chroniccard_mhyn")
pos2 <- which(colnames(data)=="other_mhyn")
colSums(is.na(data[,pos1:pos2]))
data[,pos1:pos2][is.na(data[,pos1:pos2])==T] <- "No" 
colSums(is.na(data[,pos1:pos2]))

#Check accurate comorbidities
comorbilidades_check <-data[,c(2,(pos1-1):pos2)]
comorbilidades_check[,3:25][comorbilidades_check[,3:25]=="No"] <- 0 
comorbilidades_check[,3:25][comorbilidades_check[,3:25]=="Unknown"] <- 0 
comorbilidades_check[,3:25][comorbilidades_check[,3:25]=="Yes"] <- 1
freq(comorbilidades_check$chroniccard_mhyn)
comorbilidades_check[,3:25] <- lapply(comorbilidades_check[,3:25],as.numeric)
freq(comorbilidades_check$chroniccard_mhyn)
comorbilidades_check$check <- rowSums(comorbilidades_check[,3:25])
bien_yes <-comorbilidades_check %>% filter(comorb=="Yes" & check > 0)
bien_no <-comorbilidades_check %>% filter(comorb=="No" & check == 0)
mal_yes <-comorbilidades_check %>% filter(comorb=="Yes" & check == 0) 
mal_no <-comorbilidades_check %>% filter(comorb=="No" & check > 0) #Est es el que tengo que cambiar para que funcione la condicion
summary(comorbilidades_check$check)
comorbilidades_check %>% filter(comorb=="No" & check > 0) #Change to yes for analysis
comorbilidades_check <- comorbilidades_check%>%select(c("record", "check"))
#Arreglar comorbilidades missing:
data <- merge(data,comorbilidades_check, by="record")
data %>% filter(is.na(comorb)==T & check > 0) %>% select(Country, comorb, check) # 74
data %>% filter(is.na(comorb)==T & check == 0) %>% select(Country, comorb, check) # 96
freq(data$comorb)
data %<>% mutate(comorb_rev = ifelse(is.na(comorb)==T & check > 0, "Yes", 
                                     ifelse(is.na(comorb)==T & check == 0, "No", comorb)))
freq(data$comorb_rev)

data %<>%
  mutate(cat_comorb = ifelse(chronicpul_mhyn=="Yes" | asthma_mhyn=="Yes" | tb_mhyn=="Yes" | resp_inf_mhyn=="Yes", "Respiratory comorb", 
                             ifelse(comorb_rev == "Yes" & (chronicpul_mhyn!="Yes" | asthma_mhyn!="Yes" | tb_mhyn!="Yes" | resp_inf_mhyn!="Yes"),"Comorb, not respiratory","No comorb")))
data$cat_comorb %<>% as.factor()
summary(data$cat_comorb)

#Seleccionar las variables: S?ntomas, comorbilidades, examenes de laboratorio
data_cluster <- data %>% select(Country,edad_cat,sex,codetection, #Demogr?ficas
                                #chroniccard_mhyn,#Comorbilidades
                                #asthma_mhyn,
                                #malignantneo_mhyn,
                                #chronicneu_mhyn,
                                #immunosupressors,
                                cat_comorb, 
                                corticost_cmyn,#Tratamientos
                                antibiotic_cmyn,
                                fever_ceoccur_v2, #S?notmas
                                cough_ceoccur_v2, 
                                runnynose_ceoccur_v2, 
                                wheeze_ceoccur_v2,
                                confusion_ceoccur_v2,
                                abdopain_ceoccur_v2, 
                                vomit_ceoccur_v2, 
                                diarrhoea_ceoccur_v2, 
                                pale_skin, 
                                rash_ceoccur_v2, 
                                lymp_ceoccur_v2, 
                                cap_refill, 
                                shock,
                                #cat_hb,#Paraclinicos
                                #cat_lymphocytes,
                                #cat_platelets, 
                                hosp_time, #Desenlaces cl?nicos
                                picu_admission, 
                                oxygen_therapy,
                                hft,
                                cpap,
                                mechanical_ventilation,
                                nimv,
                                imv,
                                kawa, 
                                death, 
                                final_dx_1, 
                                related_1)


colSums(is.na(data_cluster)) #Revisar missings

#Recodificar: Reemplazar Unknown con No en s?ntomas al ingreso 
data_cluster[,5:20][data_cluster[,5:20]=="Unknown"] <- "No"
colSums(is.na(data_cluster[,5:20])) #Todos menos 1 son de españa
data_cluster[,5:20][is.na(data_cluster[,5:20])==T] <- "No"
colSums(is.na(data_cluster[,5:20]))
data_cluster[,5:20] <- droplevels(data_cluster[,5:20])
#Recodificar: Reemplazar Pending/Unknown con No  
data_cluster[,4][data_cluster[,4]=="Pending"] <- "No"
data_cluster[,4][data_cluster[,4]=="Unknown due to lack of microbiological data"] <- "No"
data_cluster[,4][is.na(data_cluster[,4])==T] <- "No"
data_cluster$codetection%<>% as.factor()
data_cluster$codetection<-droplevels(data_cluster$codetection)
data_cluster$sex %<>% as.factor()
data_cluster$sex<-droplevels(data_cluster$sex)



#Estad?sticas descriptivas b?sicas de cada variable.  
summary(data_cluster)
data_cluster.active <-  na.omit(data_cluster[,1:20])
colSums(is.na(data_cluster.active)) #Revisar missings

#---------------------------------

#Descriptive
#---------------------------------

resumen2 <- tbl_summary(
  data_cluster.active,
  percent="col",
  missing="ifany", #no sacar missing como categoria
  sort = all_categorical() ~ "frequency"
) %>%
  add_n() %>% #n?mero total de observaciones 
  #add_p() %>% #diferencia entre grupos (chi2)
  modify_header(label = "**Variable**") %>% 
  bold_labels() 
resumen2

data_cluster.active %<>% select(-Country)

#---------------------------------


#Dissimilarity Matrix - Gower distance
#---------------------------------

data_cluster.active[sapply(data_cluster.active, is.character)] <- lapply(data_cluster.active[sapply(data_cluster.active, is.character)], 
                                                                         as.factor)
str(data_cluster.active)
gower.dist <- daisy(data_cluster.active, metric = c("gower"))

#---------------------------------


#Clustering jer?rquico - aglomerativo
#---------------------------------

aggl.clust.c <- hclust(gower.dist, method = "ward.D")
plot(aggl.clust.c,
     main = "Agglomerative, Complete linkage")

#---------------------------------

#Heatmap
#---------------------------------
library(heatmaply)
library(fastDummies)
data_heatmap <- data_cluster.active
# data_heatmap_dummy <- dummy_cols(data_heatmap, select_columns = c("edad_cat","sex","cat_comorb"))
# data_heatmap$Lactante <- data_heatmap_dummy$edad_cat_Lactante
# data_heatmap$Preescolar <- data_heatmap_dummy$edad_cat_Preescolar
# data_heatmap$Escolar <- data_heatmap_dummy$edad_cat_Escolar
# data_heatmap$Adolescente <- data_heatmap_dummy$edad_cat_Adolescente
# data_heatmap$Female<-data_heatmap_dummy$sex_Female
# data_heatmap$Male<-data_heatmap_dummy$sex_Male
# data_heatmap$Notspecified<-data_heatmap_dummy$`sex_Not specified`
# data_heatmap$Comorb_norespiratory<-data_heatmap_dummy$`cat_comorb_Comorb, not respiratory`
# data_heatmap$NoComorb<-data_heatmap_dummy$`cat_comorb_No comorb`
# data_heatmap$Comorb_respiratory<-data_heatmap_dummy$`cat_comorb_Respiratory comorb`
data_heatmap %<>% select(-c("edad_cat","sex","cat_comorb","codetection","corticost_cmyn","antibiotic_cmyn"))

data_heatmap<-ifelse(data_heatmap == "Yes", 1, 0)
colnames(data_heatmap) <- c("History of fever","Cough","Rhinorrhoea","Wheezing","Altered consciousness","Abdominal pain","Vomiting / Nausea",
                            "Diarrhoea","Pale skin", "Skin rash","Lymphadenopathy","Capillary refi ll time > 2 seconds ?","Shock")
heatmaply(data_heatmap)
gplots::heatmap.2(
  as.matrix(data_heatmap),
  trace = "none",
  col = viridis(100),
  key = FALSE
)

heatmaply_cor(x = cor(data_heatmap))
#---------------------------------


# Explorar estabilidad de clusters
#---------------------------------
cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}
# Cap max clusters: 10

#stats.df.aggl <-cstats.table(gower.dist, aggl.clust.c, 10) #Ward approach
#stats.df.aggl
#---------------------------------


# Escoger numero de clusters (Codo y silhueta)
#---------------------------------

# Elbow
ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  geom_vline(xintercept = 5, linetype="dotted", 
             color = "blue", size=1.5)+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))


# Silhouette
ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  geom_vline(xintercept = 5, linetype="dotted", 
             color = "blue", size=1.5)+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))


#---------------------------------


# Visualizar dendograma
#---------------------------------

dendro <- as.dendrogram(aggl.clust.c)
dendro.col <- dendro %>%
  set("branches_k_color", k = 5, value =  c("darkslategray", "darkslategray4", "darkslategray3","gold3","blue")) %>% #"gold3","blue"
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)
ggd1 <- as.ggdend(dendro.col, labels = FALSE)
ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 5")

dendro <- as.dendrogram(aggl.clust.c)
dendro.col <- dendro %>%
  set("branches_k_color", k = 3, value =  c("darkslategray3","gold3","blue")) %>% #"gold3","blue"
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)
ggd1 <- as.ggdend(dendro.col, labels = FALSE)
ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 3")

#---------------------------------


# Distribuci?n de las caracter?sticas por cluster
#---------------------------------
#k = 5
clusters <- data.frame(cutree(aggl.clust.c,5))
freq(clusters$cutree.aggl.clust.c..5.)

#An?lisis de clusters
data_subset<- data_cluster %>% select(c(Country,hosp_time, #Desenlaces cl?nicos
                                        picu_admission, 
                                        oxygen_therapy,
                                        hft,
                                        cpap,
                                        mechanical_ventilation,
                                        nimv,
                                        imv,
                                        kawa, 
                                        death, final_dx_1, related_1))

data_analysis <- data_cluster.active # Omit NAs by columns
data_analysis$cluster <- clusters$cutree.aggl.clust.c..5.
data_analysis <- merge(data_analysis, data_subset, by="row.names")

resumen2 <- tbl_summary(
  data_analysis %>% select(-c(Row.names, final_dx_1,related_1)), 
  by = cluster,
  percent="col",
  missing="ifany", #no sacar missing como categoria
  sort = all_categorical() ~ "frequency"
) %>%
  add_n() %>% #n?mero total de observaciones 
  add_p() %>% #diferencia entre grupos (chi2)
  modify_header(label = "**Variable**") %>% 
  bold_labels() 
resumen2


resumen2 <- tbl_summary(
  data_analysis %>% select(-Row.names), 
  by = cluster,
  percent="col",
  missing="ifany", #no sacar missing como categoria
  sort = all_categorical() ~ "frequency"
) %>%
  add_n() %>% #n?mero total de observaciones 
  add_p() %>% #diferencia entre grupos (chi2)
  modify_header(label = "**Variable**") %>% 
  bold_labels() 
resumen2



resumen2 <- tbl_summary(
  data_analysis %>% select(-c(Row.names, final_dx_1,related_1)) %>% filter(Country=="Colombia"), 
  by = cluster,
  percent="col",
  missing="ifany", #no sacar missing como categoria
  sort = all_categorical() ~ "frequency"
) %>%
  add_n() %>% #n?mero total de observaciones 
  add_p() %>% #diferencia entre grupos (chi2)
  modify_header(label = "**Variable**") %>% 
  bold_labels()
resumen2


resumen2 <- tbl_summary(
  data_analysis %>% select(-c(Row.names, final_dx_1,related_1)) %>% filter(Country=="España"), 
  by = cluster,
  percent="col",
  missing="ifany", #no sacar missing como categoria
  sort = all_categorical() ~ "frequency"
) %>%
  add_n() %>% #n?mero total de observaciones 
  add_p() %>% #diferencia entre grupos (chi2)
  modify_header(label = "**Variable**") %>% 
  bold_labels()
resumen2
#---------------------------------



# Distribuci?n de las caracter?sticas por cluster
#---------------------------------
#k = 3
clusters <- data.frame(cutree(aggl.clust.c,3))
freq(clusters$cutree.aggl.clust.c..3.)

#An?lisis de clusters
data_subset<- data_cluster %>% select(c(Country,hosp_time, #Desenlaces cl?nicos
                                        picu_admission, 
                                        oxygen_therapy,
                                        hft,
                                        cpap,
                                        mechanical_ventilation,
                                        nimv,
                                        imv,
                                        kawa, 
                                        death))

data_analysis <- data_cluster.active # Omit NAs by columns
data_analysis$cluster <- clusters$cutree.aggl.clust.c..3.
data_analysis <- merge(data_analysis, data_subset, by="row.names")

resumen2 <- tbl_summary(
  data_analysis %>% select(-Row.names), 
  by = cluster,
  percent="col",
  missing="ifany", #no sacar missing como categoria
  sort = all_categorical() ~ "frequency"
) %>%
  add_n() %>% #n?mero total de observaciones 
  add_p() %>% #diferencia entre grupos (chi2)
  modify_header(label = "**Variable**") %>% 
  bold_labels() 
resumen2


resumen2 <- tbl_summary(
  data_analysis  %>% filter(Country=="Colombia")%>% select(-c(Row.names,Country)), 
  by = cluster,
  percent="col",
  missing="ifany", #no sacar missing como categoria
  sort = all_categorical() ~ "frequency"
) %>%
  add_n() %>% #n?mero total de observaciones 
  add_p() %>% #diferencia entre grupos (chi2)
  modify_header(label = "**Variable**") %>% 
  bold_labels()
resumen2


resumen2 <- tbl_summary(
  data_analysis %>% filter(Country=="España")%>% select(-c(Row.names,Country)), 
  by = cluster,
  percent="col",
  missing="ifany", #no sacar missing como categoria
  sort = all_categorical() ~ "frequency"
) %>%
  add_n() %>% #n?mero total de observaciones 
  add_p() %>% #diferencia entre grupos (chi2)
  modify_header(label = "**Variable**") %>% 
  bold_labels()
resumen2
#---------------------------------


# Distribuci?n de las caracter?sticas por cluster
#---------------------------------
#k = 4
clusters <- data.frame(cutree(aggl.clust.c,4))
freq(clusters$cutree.aggl.clust.c..4.)

#An?lisis de clusters
data_subset<- data_cluster %>% select(c(hosp_time, #Desenlaces cl?nicos
                                        picu_admission, 
                                        oxygen_therapy,
                                        hft,
                                        cpap,
                                        mechanical_ventilation,
                                        nimv,
                                        imv,
                                        kawa, 
                                        death))

data_analysis <- data_cluster.active # Omit NAs by columns
data_analysis$cluster <- clusters$cutree.aggl.clust.c..4.
data_analysis <- merge(data_analysis, data_subset, by="row.names")

resumen2 <- tbl_summary(
  data_analysis %>% select(-Row.names), 
  by = cluster,
  percent="col",
  missing="ifany", #no sacar missing como categoria
  sort = all_categorical() ~ "frequency"
) %>%
  add_n() %>% #n?mero total de observaciones 
  add_p() %>% #diferencia entre grupos (chi2)
  modify_header(label = "**Variable**") %>% 
  bold_labels() 
resumen2
#---------------------------------








