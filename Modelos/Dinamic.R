## Cuarentena dinamica extención 7 dias por 50 días cuarentena media

Resultados3 <- Modelo_Edad(Inicio = Inicio, df_out = df_out, Probs = Probs, betaE=betaE, betaIm=betaIm, betaI=betaI, K_g=K_g, C_G_H=C_G_H, p_G=p_G, gammaE=gammaE, gammaH=gammaH, gammaHc=gammaHc, gammaI=gammaI, gammaIm=gammaIm, phiEI=phiEI, phiIR=phiIR, phiHR=phiHR, phiHD=phiHD, phiHcD=phiHcD, Epsilon=Epsilon, Sigma=Sigma, Dias = 150, K_Cuar = 1.0, Umbral = 40, ncores = 40, Min_Days = 7)
                          

saveRDS(Resultados3, "Results_Antofagasta_Umbral40_Dias7_KCuar1_PRUEBA2.rds")



# Resultados3_2 <- Modelo_Edad(Inicio = Inicio+101, df_out = Resultados3$Starting, Probs = Probs, betaE=betaE, betaIm=betaIm, betaI=betaI, K_g=K_g, C_G_H=C_G_H, p_G=p_G, gammaE=gammaE, gammaH=gammaH, gammaHc=gammaHc, gammaI=gammaI, gammaIm=gammaIm, phiEI=phiEI, phiIR=phiIR, phiHR=phiHR, phiHD=phiHD, phiHcD=phiHcD, Epsilon=Epsilon, Sigma=Sigma, Dias = 3, K_Cuar = 1.0, Umbral = 40, ncores = 40, Min_Days = 7)
# 
# Resultados3_Total <- list(Resultados3$Results, Resultados3_2$Results) %>% bind_rows()
# 
# Resultados3_Total <- list(Results =Resultados3_Total, starting = Resultados3_2$Starting)


#saveRDS(Resultados3_Total, "Results_Antofagasta_Umbral40_Dias7_KCuar1_PRUEBA.rds")




#ggplot(Results_Antofagasta_Umbral40_Dias7_KCuar1$Results, aes(x = Fecha, y = Infectados)) + geom_point(aes(color = as.factor(K_0),size=2)) + ylab("Cuarentena") + facet_wrap(~Comuna, scale="free") 

#ggplot(Results_Antofagasta_Umbral40_Dias7_KCuar1$Results, aes(x = Fecha, y = Infectados)) + geom_path(aes(group = Comuna)) + geom_point(aes(color = as.factor(K_0),size=2)) + ylab("Cuarentena") + facet_wrap(~Comuna, scale="free")

#ggplot(Results$Results, aes(x = Fecha, y = 100.000*(Infectados/Poblacion))) + geom_line(aes(color = Comuna)) + ylab("Infectados por cada 100.000 habitantes") + facet_wrap(~Comuna, scale="free")facet_wrap(~Comuna)