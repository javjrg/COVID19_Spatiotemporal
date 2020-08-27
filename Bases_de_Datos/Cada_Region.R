library(tidyverse)
library(lubridate)
library(readxl)

#Para actualizar base de datos



#Regiones <- c("Arica y Parinacota", "TarapacÃ¡", "Antofagasta", "Atacama", 
#"Coquimbo", "ValparaÃ­so", "Metropolitana", "OâHiggins", "Maule", 
# "Ãuble", "BiobÃ­o", "AraucanÃ­a", "Los RÃ­os", "Los Lagos", 
#  "AysÃ©n", "Magallanes")

Regiones <- c("Antofagasta")

for(x in 1:length(Regiones)){

Region_Seleccionada <- Regiones[x]

githubURL <- ("https://raw.githubusercontent.com/jorgeperezrojas/covid19-data/master/csv/confirmados_comunas.csv")
download.file(githubURL,"COVID_tasaXcomunas.csv", method="curl")


Pais <- read_csv("COVID_tasaXcomunas.csv")


Pais <- Pais %>% mutate_at(as.numeric, .vars = vars(5:ncol(Pais))) %>% dplyr::select(-codigo_region, -codigo_comuna) %>% pivot_longer(cols = c(-region, -comuna), names_to = "Fecha", values_to = "Acumulados") %>% mutate(Fecha = mdy(Fecha), Acumulados = ifelse(is.na(Acumulados), 0, Acumulados)) %>% rename(Comuna = comuna)  %>% mutate(Comuna = str_to_lower(Comuna)) %>% mutate(Comuna = str_remove_all(Comuna, " \\*")) %>%
  mutate(Comuna = str_replace_all(Comuna, "Ã¡", "a"), Comuna = str_replace_all(Comuna, "Ã©", "e"), Comuna = str_replace_all(Comuna, "Ã­", "i"), Comuna = str_replace_all(Comuna, "Ã³", "o"), Comuna = str_replace_all(Comuna, "Ãº", "u")) %>% dplyr::filter(Comuna != "total") %>% mutate(Comuna = str_replace_all(Comuna, "aisen", "aysen")) %>% 
  mutate(Comuna = case_when(region != Region_Seleccionada ~ "pais", region == Region_Seleccionada ~ Comuna)) %>% group_by(Comuna, Fecha) %>% summarise(Acumulados = sum(Acumulados)) %>% ungroup()


Pais_2 <- Pais[Pais$Comuna !="pais", ] 


saveRDS(Pais, paste0("Bases_de_datos/", Region_Seleccionada, ".rds"))
saveRDS(Pais_2, paste0("Bases_de_datos/", Region_Seleccionada, "_2.rds"))

file.remove("COVID_tasaXcomunas.csv")

## Si no hay que actualizar la base de datos empezamos desde acÃ¡

### Esta parte del cÃ³digo genera la base de datos para Infectados acumulados, Infectados y Recuperados 

Dias_Hasta_Recuperado = 14

Pais <- read_rds(paste0("Bases_de_datos/", Region_Seleccionada, ".rds")) %>% mutate(Fecha_Reciente = max(Fecha), Diferencia = as.numeric(Fecha_Reciente - Fecha), Para_Recuperados = abs(Dias_Hasta_Recuperado - Diferencia))
Pais_2 <- read_rds(paste0("Bases_de_datos/", Region_Seleccionada, "_2.rds")) %>% mutate(Fecha_Reciente = max(Fecha), Diferencia = as.numeric(Fecha_Reciente - Fecha), Para_Recuperados = abs(Dias_Hasta_Recuperado - Diferencia))


Fecha_Reciente <- unique(Pais$Fecha_Reciente)

Actual <- Pais %>% dplyr::filter(Fecha == max(Fecha)) %>% dplyr::select
Actual_2 <- Pais_2 %>% dplyr::filter(Fecha == max(Fecha)) %>% dplyr::select(Comuna, Fecha, Acumulados)

Pasado <- Pais %>% dplyr::filter(Para_Recuperados == min(Para_Recuperados)) %>% dplyr::select(Comuna, Acumulados) %>% rename(Acumulados_old = Acumulados)
Pasado_2 <- Pais_2 %>% dplyr::filter(Para_Recuperados == min(Para_Recuperados)) %>% dplyr::select(Comuna, Acumulados) %>% rename(Acumulados_old = Acumulados)

Pais <- full_join(Actual, Pasado) %>% mutate(Infectados = Acumulados - Acumulados_old, Recuperados = Acumulados - Infectados) %>% dplyr::select(-Acumulados_old)
Pais_2 <- full_join(Actual_2, Pasado_2) %>% mutate(Infectados = Acumulados - Acumulados_old, Recuperados = Acumulados - Infectados) %>% dplyr::select(-Acumulados_old)

rm(Actual)
rm(Pasado)
rm(Actual_2)
rm(Pasado_2)

### Ahora unimos esto con las poblaciones y separamos por grupo etareo, ademÃ¡s agregamos los asintomÃ¡ticos y los expuestos


Cantidad_Edad <- read_excel("Bases_de_datos/Cantidad-de-Personas-por-Sexo-y-Edad.xlsx",sheet = "COMUNAS")


colnames(Cantidad_Edad) <- make.names(colnames(Cantidad_Edad))

Cantidad_Edad <- Cantidad_Edad %>% dplyr::select(NOMBRE.REGIÃN, NOMBRE.COMUNA, Edad, TOTAL) %>% dplyr::filter(NOMBRE.COMUNA != "PAÃS", Edad != "Total PaÃ­s") %>% rename(Region = NOMBRE.REGIÃN, Comuna = NOMBRE.COMUNA)  %>% mutate(Comuna = str_to_lower(Comuna))%>% mutate(Comuna = str_remove_all(Comuna, " \\*")) %>%
  mutate(Comuna = str_replace_all(Comuna, "Ã¡", "a"), Comuna = str_replace_all(Comuna, "Ã©", "e"), Comuna = str_replace_all(Comuna, "Ã­", "i"), Comuna = str_replace_all(Comuna, "Ã³", "o"), Comuna = str_replace_all(Comuna, "Ãº", "u")) %>% 
  mutate(Comuna = case_when(Comuna %in% unique(Pais$Comuna) ~ Comuna, !(Comuna %in% unique(Pais$Comuna))~ "pais")) %>% 
  mutate(Edad = case_when(Edad %in% c("0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24")~ "Under_25", Edad %in% c("25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 a 49", "50 a 54", "55 a 59", "60 a 64") ~ "Adult", Edad %in% c("65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 a 89", "90 a 94", "95 a 99", "100 o mÃ¡s") ~ "Over_65", Edad == "Total Comunal" ~ "Total")) %>% 
  group_by(Comuna, Edad) %>% summarise(Poblacion = sum(TOTAL)) %>% ungroup() %>% pivot_wider(values_from = Poblacion, names_from = Edad) %>% mutate(Under_25 = Under_25/Total, Adult = Adult/Total, Over_65 = Over_65/Total)  %>% ungroup()

Cantidad_Edad_2 <- Cantidad_Edad[Cantidad_Edad$Comuna !="pais", ] 

### Unimos ambas bases de datos

Pais <- full_join(Pais, Cantidad_Edad)
Pais_2 <- full_join(Pais_2, Cantidad_Edad_2)

# Agregamos una estimaciÃ³n de AsintomÃ¡ticos y de Expuestos y dividimos el resto de los grupos por edad


Pais <- Pais  %>%  mutate(Asintomaticos = Infectados*2.56, Expuestos = Infectados*2.79, UCI = Infectados*0.02, Recuperados = Recuperados - UCI, Suceptibles = Total - (Infectados + Asintomaticos + Expuestos + UCI + Recuperados), Recuperados = ifelse(Recuperados < 0, 0, Recuperados)) %>% 
  mutate(Infectados_Adult = Infectados*Adult, Infectados_Over_65 = Infectados*Over_65, Infectados_Under_25 = Infectados*Under_25) %>% 
  mutate(Suceptibles_Adult = Suceptibles*Adult, Suceptibles_Under_25 = Suceptibles*Under_25, Suceptibles_Over_65 = Suceptibles*Over_65) %>% 
  mutate(Expuestos_Adult = Expuestos*Adult, Expuestos_Under_25 = Expuestos*Under_25, Expuestos_Over_65 = Expuestos*Over_65) %>% 
  mutate(Asintomaticos_Adult = Asintomaticos*Adult, Asintomaticos_Under_25 = Asintomaticos*Under_25, Asintomaticos_Over_65 = Asintomaticos*Over_65) %>%
  mutate(UCI_Adult = UCI*Adult, UCI_Under_25 = UCI*Under_25, UCI_Over_65 = UCI*Over_65) %>% 
  mutate(Muerto_Adult = 0, Muerto_Under_25 = 0, Muerto_Over_65 = 0) %>% 
  mutate(Recuperados_Adult = Recuperados*Adult, Recuperados_Under_25 = Recuperados*Under_25, Recuperados_Over_65 = Recuperados*Over_65) %>% 
  mutate(Cuarentenados_Adult = 0, Cuarentenados_Under_25 = 0, Cuarentenados_Over_65 = 0, Time = 1, Adult = Adult*Total, Over_65 = Over_65*Total, Under_25 = Under_25*Total) %>% 
  dplyr::select("Comuna", "Total", "Adult", "Over_65", "Under_25", "Infectados", 
                "Infectados_Adult", "Infectados_Over_65", "Infectados_Under_25", 
                "Suceptibles_Adult", "Suceptibles_Under_25", "Suceptibles_Over_65", 
                "Expuestos_Adult", "Expuestos_Under_25", "Expuestos_Over_65", 
                "Asintomaticos_Adult", "Asintomaticos_Under_25", "Asintomaticos_Over_65", 
                "UCI_Adult", "UCI_Under_25", "UCI_Over_65", "Muerto_Adult", "Muerto_Under_25", 
                "Muerto_Over_65", "Recuperados_Adult", "Recuperados_Under_25", 
                "Recuperados_Over_65", "Cuarentenados_Adult", "Cuarentenados_Under_25", 
                "Cuarentenados_Over_65", "Time") %>% rename(Poblacion = Total) 

Pais_2 <- Pais_2  %>%  mutate(Asintomaticos = Infectados*2.56, Expuestos = Infectados*2.79, UCI = Infectados*0.02, Recuperados = Recuperados - UCI, Suceptibles = Total - (Infectados + Asintomaticos + Expuestos + UCI + Recuperados), Recuperados = ifelse(Recuperados < 0, 0, Recuperados)) %>% 
  mutate(Infectados_Adult = Infectados*Adult, Infectados_Over_65 = Infectados*Over_65, Infectados_Under_25 = Infectados*Under_25) %>% 
  mutate(Suceptibles_Adult = Suceptibles*Adult, Suceptibles_Under_25 = Suceptibles*Under_25, Suceptibles_Over_65 = Suceptibles*Over_65) %>% 
  mutate(Expuestos_Adult = Expuestos*Adult, Expuestos_Under_25 = Expuestos*Under_25, Expuestos_Over_65 = Expuestos*Over_65) %>% 
  mutate(Asintomaticos_Adult = Asintomaticos*Adult, Asintomaticos_Under_25 = Asintomaticos*Under_25, Asintomaticos_Over_65 = Asintomaticos*Over_65) %>%
  mutate(UCI_Adult = UCI*Adult, UCI_Under_25 = UCI*Under_25, UCI_Over_65 = UCI*Over_65) %>% 
  mutate(Muerto_Adult = 0, Muerto_Under_25 = 0, Muerto_Over_65 = 0) %>% 
  mutate(Recuperados_Adult = Recuperados*Adult, Recuperados_Under_25 = Recuperados*Under_25, Recuperados_Over_65 = Recuperados*Over_65) %>% 
  mutate(Cuarentenados_Adult = 0, Cuarentenados_Under_25 = 0, Cuarentenados_Over_65 = 0, Time = 1, Adult = Adult*Total, Over_65 = Over_65*Total, Under_25 = Under_25*Total) %>% 
  dplyr::select("Comuna", "Total", "Adult", "Over_65", "Under_25", "Infectados", 
                "Infectados_Adult", "Infectados_Over_65", "Infectados_Under_25", 
                "Suceptibles_Adult", "Suceptibles_Under_25", "Suceptibles_Over_65", 
                "Expuestos_Adult", "Expuestos_Under_25", "Expuestos_Over_65", 
                "Asintomaticos_Adult", "Asintomaticos_Under_25", "Asintomaticos_Over_65", 
                "UCI_Adult", "UCI_Under_25", "UCI_Over_65", "Muerto_Adult", "Muerto_Under_25", 
                "Muerto_Over_65", "Recuperados_Adult", "Recuperados_Under_25", 
                "Recuperados_Over_65", "Cuarentenados_Adult", "Cuarentenados_Under_25", 
                "Cuarentenados_Over_65", "Time") %>% rename(Poblacion = Total) 

Under_25 <- Pais %>% dplyr::select(Comuna, Poblacion, contains("Under_25"), Time) %>% rename(Generacion = Under_25)

Adult <- Pais %>% dplyr::select(Comuna, Poblacion, contains("Adult"), Time) %>% rename(Generacion = Adult)

Over_65 <- Pais %>% dplyr::select(Comuna, Poblacion, contains("Over_65"), Time) %>% rename(Generacion = Over_65)


Under_25_2 <- Pais_2 %>% dplyr::select(Comuna, Poblacion, contains("Under_25"), Time) %>% rename(Generacion = Under_25)

Adult_2 <- Pais_2 %>% dplyr::select(Comuna, Poblacion, contains("Adult"), Time) %>% rename(Generacion = Adult)

Over_65_2 <- Pais_2 %>% dplyr::select(Comuna, Poblacion, contains("Over_65"), Time) %>% rename(Generacion = Over_65)

#Cambiamos los nombres de las columnas para que todos tengas cosas iguales

colnames(Under_25) <- str_remove_all(string = colnames(Under_25), pattern = "_Under_25")

colnames(Adult) <- str_remove_all(string = colnames(Adult), pattern = "_Adult")

colnames(Over_65) <- str_remove_all(string = colnames(Over_65), pattern = "_Over_65")


colnames(Under_25_2) <- str_remove_all(string = colnames(Under_25), pattern = "_Under_25")

colnames(Adult_2) <- str_remove_all(string = colnames(Adult), pattern = "_Adult")

colnames(Over_65_2) <- str_remove_all(string = colnames(Over_65), pattern = "_Over_65")

### Lo agregamos todo a una lista llamada df_out con los nombres de las edades


df_out <- list(Under_25, Adult, Over_65)

names(df_out) <- c("Under_25", "Adult", "Over_65")

dir.create(paste0("Bases_de_datos/Datos_", Region_Seleccionada))

df_out_2 <- list(Under_25_2, Adult_2, Over_65_2)

names(df_out_2) <- c("Under_25", "Adult", "Over_65")




##### Matriz de migraciÃ³n

Viajes_Comunas <- read_rds("Bases_de_datos/Viajes_comunas.rds")


Viajes_Comunas <- Viajes_Comunas  %>% mutate(origen = str_to_lower(origen)) %>% mutate(origen = str_remove_all(origen, " \\*")) %>%
  mutate(origen = str_replace_all(origen, "Ã¡", "a"), origen = str_replace_all(origen, "Ã©", "e"), origen = str_replace_all(origen, "Ã­", "i"), origen = str_replace_all(origen, "Ã³", "o"), origen = str_replace_all(origen, "Ãº", "u")) %>% dplyr::filter(origen != "total") %>% 
  mutate(destino = str_to_lower(destino)) %>% mutate(destino = str_remove_all(destino, " \\*")) %>%
  mutate(destino = str_replace_all(destino, "Ã¡", "a"), destino = str_replace_all(destino, "Ã©", "e"), destino = str_replace_all(destino, "Ã­", "i"), destino = str_replace_all(destino, "Ã³", "o"), destino = str_replace_all(destino, "Ãº", "u")) %>% dplyr::filter(destino != "total") %>% mutate(destino  = str_replace_all(destino, "marchigÃ¼e", "marchihue"), origen  = str_replace_all(origen, "marchigÃ¼e", "marchihue"))%>% mutate(destino  = str_replace_all(destino, "paihuano", "paiguano"), origen  = str_replace_all(origen, "paihuano", "paiguano")) %>% 
  mutate(destino = ifelse(destino %in% unique(Pais$Comuna), destino, "pais"), origen = ifelse(origen %in% unique(Pais$Comuna), origen, "pais")) %>% group_by(origen, destino) %>% summarise_if(is.numeric, sum) %>% ungroup()


Viajes_Comunas_2 <- Viajes_Comunas[Viajes_Comunas$origen!="pais", ] 

Viajes_Comunas_2 <- Viajes_Comunas_2[Viajes_Comunas_2$destino!="pais", ] 

Areas <- read_rds("Bases_de_datos/Areas.rds") %>% mutate(Comuna = ifelse(Comuna %in% unique(Viajes_Comunas$origen), Comuna, "pais")) %>% group_by(Comuna) %>% summarise_if(is.numeric, sum) %>% ungroup()
Areas_2 <- Areas[Areas$Comuna!="pais", ]

df_out <- df_out %>% purrr::map(~dplyr::filter(.x, Comuna %in% unique(Viajes_Comunas$origen))) %>% purrr::map(~left_join(.x, Areas))
df_out_2 <- df_out %>% purrr::map(~dplyr::filter(.x, Comuna %in% unique(Viajes_Comunas_2$origen))) %>% purrr::map(~left_join(.x, Areas_2))

saveRDS(df_out, paste0("Bases_de_datos/Datos_",Region_Seleccionada, "/df_out_", Fecha_Reciente, ".rds")) 
saveRDS(df_out_2, paste0("Bases_de_datos/Datos_",Region_Seleccionada, "/df_out_", Fecha_Reciente, "_2.rds")) 


Nombres <- df_out[[2]]$Comuna
Nombres_2 <- df_out_2[[2]]$Comuna


#Generamos una lista de Probabilidades
Probs <-list()
Probs_2 <-list()

#Hacemos el proceso que tenÃ­amos arriba para todas las regiones


for(i in 1:length(Nombres)){
  Probs[[i]] <- Viajes_Comunas %>% dplyr::filter(origen ==	Nombres[i]) %>% mutate(p = n_personas/sum(n_personas)) %>% dplyr::select(destino, p)
  colnames(Probs[[i]])[2] <- Nombres[i]
}

for(i in 1:length(Nombres_2)){
  Probs_2[[i]] <- Viajes_Comunas_2 %>% dplyr::filter(origen ==	Nombres_2[i]) %>% mutate(p = n_personas/sum(n_personas)) %>% dplyr::select(destino, p)
  colnames(Probs_2[[i]])[2] <- Nombres_2[i]
}

#Luego unimos todos estos dataframes con un full_join y remplazamos los na por 0 en caso de = origen destino
Probs <- Probs %>% reduce(full_join) %>% mutate_if(is.numeric, list(~replace_na(., 0)))
Probs_2 <- Probs_2 %>% reduce(full_join) %>% mutate_if(is.numeric, list(~replace_na(., 0)))

saveRDS(Probs, paste0("Bases_de_datos/Datos_", Region_Seleccionada,"/Probs_", Fecha_Reciente, ".rds"))
saveRDS(Probs_2, paste0("Bases_de_datos/Datos_", Region_Seleccionada,"/Probs_", Fecha_Reciente, "_2.rds"))

message(paste("Region", Regiones[x], "lista!"))
}
