library(tidyverse)
library(lubridate)
library(readxl)
library(doParallel)
library(ggplot2)

## Cargamos las funciones

source("Modelos/Funcion.R")

Start <- Sys.time()
### Elegir fecha inicial de simulacion

Inicio = dmy("14-08-2020")

### Leer probabilidades de migracion

Probs <- read_rds("Bases_de_datos/Datos_Antofagasta/Probs_2020-08-14.rds")

### Leer condiciones iniciales

df_out_temp <- read_rds("Bases_de_datos/Datos_Antofagasta/df_out_2020-08-14.rds")
temp1 <- mutate(df_out_temp$Under_25, Hospitalizados=UCI*3.5)
temp2 <- mutate(df_out_temp$Adult, Hospitalizados=UCI*3.5)
temp3 <- mutate(df_out_temp$Over_65, Hospitalizados=UCI*3.5)

df_out <- list(Under_25=temp1,Adult=temp2,Over_65=temp3)

#Infectividad de expuestos
betaE = 0.0045#0.0045#0.0563
#Infectividad de Infectados leve
betaIm = 0.009#0.009#0.1125
#Infectividad de Infectados
betaI = 0.06#0.06#0.75

#numero de contactos promedio
K_g = read_rds("Bases_de_datos/Contactos_Antogagasta.rds")

## Matriz de contactos entre generaciones
C_G_H <- read_rds("Bases_de_datos/Age_Matrix_Antofagasta.rds")

#Factor de movilidad
p_G = c(0.5, 1, 0.5)

#Gammas
alpha = 1.0
gammaE = 0.1701#(1-alpha)*1/6 + alpha*1/4#0.1701
gammaIm = 0.08#(1-alpha)*1/14 + alpha*1/7#0.08
gammaI = 0.07#(1-alpha)*1/14 + alpha*1/7#0.07
gammaH = 0.1204 #(1-alpha)*1/10 + alpha*1/2#0.1204
gammaHc = 0.0476 #(1-alpha)*1/15 + alpha*1/10#0.0476

#Phis
phiEI = 0.5
phiIR = 0.85
phiHR = 0.6789
phiHD = 0.05
phiHcD = 0.2

# Factor de densidad
Epsilon = 0.01

###Tamaño promedio de hogar
Sigma = 3.2 #3.2 Antofagasta,   3.1 pais










