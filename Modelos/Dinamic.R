Resultados <- Modelo_Edad(Inicio = Inicio, df_out = df_out, Probs = Probs, betaE=betaE, betaIm=betaIm, betaI=betaI, K_g=K_g, C_G_H=C_G_H, p_G=p_G, gammaE=gammaE, gammaH=gammaH, gammaHc=gammaHc, gammaI=gammaI, gammaIm=gammaIm, phiEI=phiEI, phiIR=phiIR, phiHR=phiHR, phiHD=phiHD, phiHcD=phiHcD, Epsilon=Epsilon, Sigma=Sigma, Dias = 150, K_Cuar = 1.0, Umbral = 40, ncores = 40, Min_Days = 7)
                          
saveRDS(Resultados, "Results_Antofagasta_Umbral40_Dias7_KCuar1.rds")
