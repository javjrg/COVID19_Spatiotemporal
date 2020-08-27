#setwd("~/Documents/Covid19_Chile_Age")

## Cargaremos los paquetes necesarios?contact_matrix

library(tidyverse)
library(readxl)


##Leemos los estimadores de Chile

Estimadores <- read_excel("Bases_de_datos/contact_matrices_152_countries/MUestimates_all_locations_1.xlsx", 
                                          sheet = "Chile")


##Agregamos como nombres de columa las edades correspondientes

colnames(Estimadores) <- paste0("Age_",paste(c(0:15)*5, c(1:16)*5, sep = "_")) 
rownames(Estimadores) <- paste0("Age_",paste(c(0:15)*5, c(1:16)*5, sep = "_")) 

## Leemos la base de datos de cantidad de personas por sexo y edad
Cantidad_Edad <- read_excel("Bases_de_datos/Cantidad-de-Personas-por-Sexo-y-Edad.xlsx",sheet = "COMUNAS")
colnames(Cantidad_Edad) <- make.names(colnames(Cantidad_Edad))

## Ponemos los nombres de grupo de edad entre cada grupo etareo 
Grupo1 <- c("Age_0_5", "Age_5_10", "Age_10_15", "Age_15_20", "Age_20_25")
Grupo2 <- c("Age_25_30", "Age_30_35", "Age_35_40", "Age_40_45", "Age_45_50","Age_50_55", "Age_55_60", "Age_60_65")
Grupo3 <- c("Age_65_70", "Age_70_75","Age_75_80")


## Nos quedamos con solo lo de todo el paÃ­s
Pais <- Cantidad_Edad %>%  dplyr::filter(NOMBRE.REGIÃN == "PAÃS", Edad != "Total PaÃ­s") %>% 
  ## Nos quedamos solo con los grupos de edad y los Totales poblacionales
  dplyr::select(Edad, TOTAL) %>% 
  ## Reagrupamos segun los grupos etareos que nos interesan
  dplyr::mutate(Grupo = case_when(Edad %in% c("0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24") ~ "0_24",
                                  Edad %in% c("25 a 29","30 a 34", "35 a 39", "40 a 44", "45 a 49", "50 a 54", "55 a 59", "60 a 64") ~ "25_65",
                                  Edad %in% c("65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 a 89","90 a 94", "95 a 99", "100 o mÃ¡s") ~ "65+")) %>% 
  # sumamos la poblacion  y calculamos la proporcion de cada grupo
  mutate(Poblacion = sum(TOTAL), Prop = TOTAL/Poblacion)

Antofagasta <- Cantidad_Edad %>%  dplyr::filter(NOMBRE.REGIÃN == "ANTOFAGASTA", Edad != "Total Comunal") %>% 
  ## Nos quedamos solo con los grupos de edad y los Totales poblacionales
  dplyr::select(Edad, TOTAL,NOMBRE.COMUNA) %>% 
  ## Reagrupamos segun los grupos etareos que nos interesan
  dplyr::mutate(Grupo = case_when(Edad %in% c("0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24") ~ "0_24",
                                  Edad %in% c("25 a 29","30 a 34", "35 a 39", "40 a 44", "45 a 49", "50 a 54", "55 a 59", "60 a 64") ~ "25_65",
                                  Edad %in% c("65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 a 89","90 a 94", "95 a 99", "100 o mÃ¡s") ~ "65+")) %>% 
  # sumamos la poblacion  y calculamos la proporcion de cada grupo
  mutate(Poblacion = sum(TOTAL), Prop = TOTAL/Poblacion)

## EstimaciÃ³n de numero de contactos por grupo etareo

Contactos_Grupo_1 <- weighted.mean(colSums(Estimadores[Grupo1]), w = Pais$TOTAL[1:5])
Contactos_Grupo_2 <- weighted.mean(colSums(Estimadores[Grupo2]), w = Pais$TOTAL[6:13])
Contactos_Grupo_3 <- weighted.mean(colSums(Estimadores[Grupo3]), w = Pais$TOTAL[14:16])


Ant_0_25 = Antofagasta$TOTAL[1:5]+Antofagasta$TOTAL[22:26]+Antofagasta$TOTAL[43:47]+Antofagasta$TOTAL[64:68]+Antofagasta$TOTAL[85:89]+Antofagasta$TOTAL[106:110]+Antofagasta$TOTAL[127:131]+Antofagasta$TOTAL[148:152]+Antofagasta$TOTAL[169:173]
Ant_25_65 = Antofagasta$TOTAL[6:13]+Antofagasta$TOTAL[27:34]+Antofagasta$TOTAL[48:55]+Antofagasta$TOTAL[69:76]+Antofagasta$TOTAL[90:97]+Antofagasta$TOTAL[111:118]+Antofagasta$TOTAL[132:139]+Antofagasta$TOTAL[153:160]+Antofagasta$TOTAL[174:181]
Ant_65_ = Antofagasta$TOTAL[14:16]+Antofagasta$TOTAL[35:37]+Antofagasta$TOTAL[56:58]+Antofagasta$TOTAL[77:79]+Antofagasta$TOTAL[98:100]+Antofagasta$TOTAL[119:121]+Antofagasta$TOTAL[140:142]+Antofagasta$TOTAL[161:163]+Antofagasta$TOTAL[182:184]


Contactos_Grupo_1_A <- weighted.mean(colSums(Estimadores[Grupo1]), w = Ant_0_25)
Contactos_Grupo_2_A <- weighted.mean(colSums(Estimadores[Grupo2]), w = Ant_25_65)
Contactos_Grupo_3_A <- weighted.mean(colSums(Estimadores[Grupo3]), w = Ant_65_)

Contactos <- c(Contactos_Grupo_1, Contactos_Grupo_2, Contactos_Grupo_3)
Contactos_A <- c(Contactos_Grupo_1_A, Contactos_Grupo_2_A, Contactos_Grupo_3_A)

saveRDS(Contactos, "Bases_de_datos/Contactos.rds")
saveRDS(Contactos_A, "Bases_de_datos/Contactos_Antogagasta.rds")
  
Data_Final <- data.frame(Age_0_25 = rep(NA, 3), Age_25_65 = rep(NA, 3), Age_65_ = rep(NA, 3))
Data_Final_A <- data.frame(Age_0_25 = rep(NA, 3), Age_25_65 = rep(NA, 3), Age_65_ = rep(NA, 3))

Data_Final$Age_0_25[1] <- weighted.mean(colSums(Estimadores[1:5,Grupo1]), Pais$TOTAL[1:5])/Contactos[1] 
Data_Final$Age_25_65[1] <- weighted.mean(colSums(Estimadores[6:13,Grupo1]), Pais$TOTAL[1:5])/Contactos[1]
Data_Final$Age_65_[1] <- weighted.mean(colSums(Estimadores[14:16,Grupo1]), Pais$TOTAL[1:5])/Contactos[1]

Data_Final$Age_0_25[2] <- weighted.mean(colSums(Estimadores[1:5,Grupo2]), Pais$TOTAL[6:13])/Contactos[2] 
Data_Final$Age_25_65[2] <- weighted.mean(colSums(Estimadores[6:13,Grupo2]), Pais$TOTAL[6:13])/Contactos[2]
Data_Final$Age_65_[2] <- weighted.mean(colSums(Estimadores[14:16,Grupo2]), Pais$TOTAL[6:13])/Contactos[2]

Data_Final$Age_0_25[3] <- weighted.mean(colSums(Estimadores[1:5,Grupo3]), Pais$TOTAL[14:16])/Contactos[3] 
Data_Final$Age_25_65[3] <- weighted.mean(colSums(Estimadores[6:13,Grupo3]), Pais$TOTAL[14:16])/Contactos[3]
Data_Final$Age_65_[3] <- weighted.mean(colSums(Estimadores[14:16,Grupo3]), Pais$TOTAL[14:16])/Contactos[3]



Data_Final_A$Age_0_25[1] <- weighted.mean(colSums(Estimadores[1:5,Grupo1]), Ant_0_25)/Contactos_A[1]
Data_Final_A$Age_25_65[1] <- weighted.mean(colSums(Estimadores[6:13,Grupo1]), Ant_0_25)/Contactos_A[1]
Data_Final_A$Age_65_[1] <- weighted.mean(colSums(Estimadores[14:16,Grupo1]), Ant_0_25)/Contactos_A[1]

Data_Final_A$Age_0_25[2] <- weighted.mean(colSums(Estimadores[1:5,Grupo2]), Ant_25_65)/Contactos_A[2] 
Data_Final_A$Age_25_65[2] <- weighted.mean(colSums(Estimadores[6:13,Grupo2]), Ant_25_65)/Contactos_A[2]
Data_Final_A$Age_65_[2] <- weighted.mean(colSums(Estimadores[14:16,Grupo2]), Ant_25_65)/Contactos_A[2]

Data_Final_A$Age_0_25[3] <- weighted.mean(colSums(Estimadores[1:5,Grupo3]), Ant_65_)/Contactos_A[3] 
Data_Final_A$Age_25_65[3] <- weighted.mean(colSums(Estimadores[6:13,Grupo3]), Ant_65_)/Contactos_A[3]
Data_Final_A$Age_65_[3] <- weighted.mean(colSums(Estimadores[14:16,Grupo3]), Ant_65_)/Contactos_A[3]



Data_Final <- as.matrix(Data_Final)
rownames(Data_Final) <- colnames(Data_Final)

saveRDS(Data_Final, "Bases_de_datos/Age_Matrix.rds")

Data_Final_A <- as.matrix(Data_Final_A)
rownames(Data_Final_A) <- colnames(Data_Final_A)

saveRDS(Data_Final_A, "Bases_de_datos/Age_Matrix_Antofagasta.rds")

