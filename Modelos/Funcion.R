Modelo_Edad <- function(Inicio, df_out, Probs, betaE, betaIm, betaI, K_g, C_G_H, p_G, gammaE, gammaH, gammaHc, gammaI, gammaIm, phiEI, phiIR, phiHR, phiHD, phiHcD, Epsilon, Sigma, Dias, K_Cuar, Umbral, ncores, Min_Days ){
  prevalencias <- df_out %>% bind_rows() %>% group_by(Comuna, Poblacion) %>% summarise(Infectados = sum(Infectados)) %>% mutate(Prevalencia = (Infectados/Poblacion)*100000) %>% ungroup()  %>%  dplyr::pull(Prevalencia) 
  for(i in 1:length(df_out)){
    df_out[[i]]$Prevalencia <- prevalencias
  }
  
  print(paste("Min_Days", Min_Days, "Umbral ", Umbral, "K_Cuar ", K_Cuar))
  
  Nombres <- df_out$Under_25$Comuna
  
  df_out <- df_out %>% purrr::map2(.y = K_g, ~mutate(.x, K_g = .y, Count = 0))
  
  df_out <- df_out %>% purrr::map(~mutate(.x, K_0 = case_when(Count > 0 & Count != Min_Days ~ K_Cuar,
                                                              Count == 0 & Prevalencia >= Umbral ~ K_Cuar,
                                                              Count == 0 & Prevalencia < Umbral ~ 0,
                                                              Count == Min_Days & Prevalencia >= Umbral ~ K_Cuar,
                                                              Count == Min_Days & Prevalencia < Umbral ~ 0),
                                          Count = case_when(Count > 0 & Count != Min_Days ~ Count + 1,
                                                            Count == 0 & Prevalencia >= Umbral ~ Count + 1,
                                                            Count == 0 & Prevalencia < Umbral ~ 0,
                                                            Count == Min_Days & Prevalencia >= Umbral ~ 1,
                                                            Count == Min_Days & Prevalencia < Umbral ~ 0)
  ))
  
  df_out <- df_out %>% purrr::map2(.y = p_G, ~mutate(.x, p_G = .y))
  
  df_out  <- df_out %>% purrr::map(~mutate(.x, p_G_c = (1 - K_0)*p_G))
  
  Mat  <- matrix(rep(0,(length(Nombres)*length(Nombres))), nrow = length(Nombres), ncol = length(Nombres))
  
  colnames(Mat) <- Nombres
  rownames(Mat) <- Nombres
  
  Func <- function(x, epsilon = 0.01){
    y <- 1 + (1- exp(-epsilon*x))
    return(y)
  }
  
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  for(x in 1:3){
    #creamos la columna a llenar 
    df_out[[x]]$n_i_g_eff <- 0
    #Para cada Comuna
    for(R in 1:nrow(df_out[[x]])){
      n_i_g_eff <- foreach(i = 1:length(Nombres), .combine=c, .inorder = T, .packages = c("dplyr")) %dopar%{
        ((1 - df_out[[x]]$p_G_c[i])*ifelse(Nombres[i] == df_out[[x]][R,]$Comuna, 1, 0) + df_out[[x]]$p_G_c[i]*Probs %>% dplyr::filter(destino == df_out[[x]][R,]$Comuna) %>% pull(Nombres[i]))*(df_out[[x]] %>% dplyr::filter(Comuna == Nombres[i]) %>% pull(Generacion))
      }
      df_out[[x]][R,]$n_i_g_eff <- sum(n_i_g_eff) 
    }
  }
  
  n_i_eff <- data.frame(Comuna = df_out$Adult$Comuna, n_i_eff = df_out[[1]]$n_i_g_eff + df_out[[2]]$n_i_g_eff+ df_out[[3]]$n_i_g_eff)
  
  z_g <- vector()
  
  for(x in 1:length(df_out)){
    z_g[x] <- (df_out[[x]] %>% summarise(N = sum(Generacion)) %>% pull(N))/sum(Func(n_i_eff$n_i_eff/(df_out[[x]]$Area))*df_out[[x]]$n_i_g_eff)
  }
  
  
  df_out <- df_out %>% purrr::map(~mutate(.x, K_g_c = (1 - K_0)*K_g + K_0*(Sigma - 1)))
  
  Div3 <- function(x){
    return(x/3)
  }
  
  Results <- list()
  
  Results[[1]] <- df_out %>% bind_rows() %>% dplyr::select(-Poblacion, -n_i_g_eff, -Count) %>% rename(Poblacion = Generacion) %>% group_by(Comuna) %>% summarise_if(is.numeric, sum) %>% mutate_at(vars(Time:K_0), Div3)
  
  for(d in 2:Dias){
    
    Num <- list()
    Denom <- list()
    for(g in 1:3){
      Num[[g]] <- ((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion) + (df_out[[g]]$Recuperados/df_out[[g]]$Generacion))*df_out[[g]]$Generacion
      Denom [[g]]<- sum(df_out[[g]]$Generacion)
    }
    CH_i_tc <- purrr::reduce(Num, `+`)/purrr::reduce(Denom, `+`)^Sigma
    
    df_out  <- df_out %>% purrr::map(~mutate(.x, p_G_c = (1 - K_0)*p_G)) 
    
    df_out <- df_out %>% purrr::map(~mutate(.x, K_g_c = (1 - K_0)*K_g + K_0*(Sigma - 1)))
    
    
    for(x in 1:3){
      #creamos la columna a llenar 
      df_out[[x]]$n_i_g_eff <- 0
      #Para cada Comuna
      for(R in 1:nrow(df_out[[x]])){
        n_i_g_eff <- foreach(i = 1:length(Nombres), .combine=c, .inorder = T, .packages = c("dplyr")) %dopar%{
          ((1 - df_out[[x]]$p_G_c[i])*ifelse(Nombres[i] == df_out[[x]][R,]$Comuna, 1, 0) + df_out[[x]]$p_G_c[i]*Probs %>% dplyr::filter(destino == df_out[[x]][R,]$Comuna) %>% pull(Nombres[i]))*(df_out[[x]] %>% dplyr::filter(Comuna == Nombres[i]) %>% pull(Generacion))
        }
        df_out[[x]][R,]$n_i_g_eff <- sum(n_i_g_eff) 
      }
    }
    
    
    
    n_i_eff <- data.frame(Comuna = df_out$Adult$Comuna, n_i_eff = df_out[[1]]$n_i_g_eff + df_out[[2]]$n_i_g_eff+ df_out[[3]]$n_i_g_eff)
    
    z_g <- vector()
    
    for(x in 1:length(df_out)){
      z_g[x] <- (df_out[[x]] %>% summarise(N = sum(Generacion)) %>% pull(N))/sum(Func(n_i_eff$n_i_eff/(df_out[[x]]$Area))*df_out[[x]]$n_i_g_eff)
    }
    
    N_I_h_j_i <- list(Mat, Mat, Mat)
    names(N_I_h_j_i) <- c("Under_25", "Adult", "Over_65")
    
    N_A_h_j_i <- list(Mat, Mat, Mat)
    names(N_A_h_j_i) <- c("Under_25", "Adult", "Over_65")
    

    N_E_h_j_i <- list(Mat, Mat, Mat)
    names(N_E_h_j_i) <- c("Under_25", "Adult", "Over_65")    
    
    for(x in 1:length(N_I_h_j_i)){
      for(i in 1:length(Nombres)){
        Temp <- foreach(j = 1:length(Nombres), .combine=c, .inorder = T, .packages = c("dplyr")) %dopar%{
          (df_out[[x]] %>% dplyr::filter(Comuna == Nombres[j]) %>% pull(Generacion))*((df_out[[x]] %>% dplyr::filter(Comuna == Nombres[j]) %>% pull(Infectados))/(df_out[[x]] %>% dplyr::filter(Comuna == Nombres[j]) %>% pull(Generacion)))*((1-df_out[[x]]$p_G_c[i])*ifelse(Nombres[j] == Nombres[i], 1, 0) + df_out[[x]]$p_G_c[i]*Probs %>% dplyr::filter(destino == Nombres[i]) %>% pull(Nombres[j]))
        }
        N_I_h_j_i[[x]][,i] <- Temp
      }
    }
    
    
    
    
    for(x in 1:length(N_A_h_j_i)){
      for(i in 1:length(Nombres)){
        Temp <- foreach(j = 1:length(Nombres), .combine=c, .inorder = T, .packages = c("dplyr")) %dopar%{
          (df_out[[x]] %>% dplyr::filter(Comuna == Nombres[j]) %>% pull(Generacion))*((df_out[[x]] %>% dplyr::filter(Comuna == Nombres[j]) %>% pull(Asintomaticos))/(df_out[[x]] %>% dplyr::filter(Comuna == Nombres[j]) %>% pull(Generacion)))*((1-df_out[[x]]$p_G_c[j])*ifelse(Nombres[j] == Nombres[i], 1, 0) + df_out[[x]]$p_G_c[j]*Probs %>% dplyr::filter(destino == Nombres[i]) %>% pull(Nombres[j]))
        }
        N_A_h_j_i[[x]][,i] <- Temp 
      }
    }
  
    for(x in 1:length(N_E_h_j_i)){
      for(i in 1:length(Nombres)){
        Temp <- foreach(j = 1:length(Nombres), .combine=c, .inorder = T, .packages = c("dplyr")) %dopar%{
          (df_out[[x]] %>% dplyr::filter(Comuna == Nombres[j]) %>% pull(Generacion))*((df_out[[x]] %>% dplyr::filter(Comuna == Nombres[j]) %>% pull(Expuestos))/(df_out[[x]] %>% dplyr::filter(Comuna == Nombres[j]) %>% pull(Generacion)))*((1-df_out[[x]]$p_G_c[j])*ifelse(Nombres[j] == Nombres[i], 1, 0) + df_out[[x]]$p_G_c[j]*Probs %>% dplyr::filter(destino == Nombres[i]) %>% pull(Nombres[j]))
        }
        N_E_h_j_i[[x]][,i] <- Temp 
      }
    }  
    
    
    for(g in 1:3){
      df_out[[g]]$P_G <- NA
      Temp2 <- list()
      for(h in 1:3){
        temp <- foreach(j = 1:nrow(df_out[[g]]), .inorder = T, .packages = c("dplyr")) %dopar% {
          (1 - betaE)^(z_g[g]*df_out[[g]]$K_g_c[j]*Func(x = n_i_eff$n_i_eff/(df_out[[x]]$Area))*C_G_H[g,h]*(N_E_h_j_i[[h]][j,]/df_out[[h]]$n_i_g_eff))*(1 - betaIm)^(z_g[g]*df_out[[g]]$K_g_c[j]*Func(x = n_i_eff$n_i_eff/(df_out[[x]]$Area))*C_G_H[g,h]*(N_A_h_j_i[[h]][j,]/df_out[[h]]$n_i_g_eff))*(1 - betaI)^(z_g[g]*df_out[[g]]$K_g_c[j]*Func(x = n_i_eff$n_i_eff/(df_out[[x]]$Area))*C_G_H[g,h]*(N_I_h_j_i[[h]][j,]/df_out[[h]]$n_i_g_eff))
        }
        Temp2[[h]] <- purrr::reduce(temp, `*`)
      }
      
      df_out[[g]]$P_G <- 1 - purrr::reduce(Temp2, `*`)
    }
    
    
    
    for(g in 1:3){
      df_out[[g]]$PI = NA
      df_out[[g]]$PI <-  foreach(i = 1:nrow(df_out[[g]]), .combine=c, .inorder = T, .packages = c("dplyr", "purrr")) %dopar%{
        df_out[[g]]$p_G_c[i]*(df_out[[g]] %>% dplyr::filter(Comuna == Nombres[i]) %>% pull(P_G)) + df_out[[g]]$p_G_c[i]*sum((Probs %>% dplyr::filter(destino == Nombres[i]) %>% select_if(is.numeric) %>% reduce(c))*df_out[[g]]$P_G)
      }
    }
    
    
    
    Temp1 <- df_out
 
     for(g in 1:3){
     
       Temp1[[g]]$Suceptibles <- ifelse((df_out[[g]]$Generacion*((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion)*(1-df_out[[g]]$PI)*(1 - df_out[[g]]$K_0*CH_i_tc)))>0,df_out[[g]]$Generacion*((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion)*(1-df_out[[g]]$PI)*(1 - df_out[[g]]$K_0*CH_i_tc)),0)
       Temp1[[g]]$Expuestos <- ifelse((df_out[[g]]$Generacion*(((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion)*(1 - df_out[[g]]$K_0*CH_i_tc)*df_out[[g]]$PI) + (1 - gammaE)*(df_out[[g]]$Expuestos/df_out[[g]]$Generacion)))>0, df_out[[g]]$Generacion*(((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion)*(1 - df_out[[g]]$K_0*CH_i_tc)*df_out[[g]]$PI) + (1 - gammaE)*(df_out[[g]]$Expuestos/df_out[[g]]$Generacion)),0)
       Temp1[[g]]$Cuarentenados <- ifelse((df_out[[g]]$Generacion*((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion)*df_out[[g]]$K_0*CH_i_tc))>0,df_out[[g]]$Generacion*((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion)*df_out[[g]]$K_0*CH_i_tc),0)
       Temp1[[g]]$Asintomaticos <- ifelse((df_out[[g]]$Generacion*(((1-phiEI)*gammaE*(df_out[[g]]$Expuestos/df_out[[g]]$Generacion)) + (1 - gammaIm)*(df_out[[g]]$Asintomaticos/df_out[[g]]$Generacion)))>0,df_out[[g]]$Generacion*(((1-phiEI)*gammaE*(df_out[[g]]$Expuestos/df_out[[g]]$Generacion)) + (1 - gammaIm)*(df_out[[g]]$Asintomaticos/df_out[[g]]$Generacion)),0)
       Temp1[[g]]$Infectados <- ifelse((df_out[[g]]$Generacion*((phiEI*gammaE*(df_out[[g]]$Expuestos/df_out[[g]]$Generacion)) + (1 - gammaI)*(df_out[[g]]$Infectados/df_out[[g]]$Generacion)))>0,df_out[[g]]$Generacion*((phiEI*gammaE*(df_out[[g]]$Expuestos/df_out[[g]]$Generacion)) + (1 - gammaI)*(df_out[[g]]$Infectados/df_out[[g]]$Generacion)),0)
       Temp1[[g]]$Recuperados <-  ifelse((df_out[[g]]$Generacion*(phiIR*gammaI*(df_out[[g]]$Infectados/df_out[[g]]$Generacion) + phiHR*gammaH*(df_out[[g]]$Hospitalizados/df_out[[g]]$Generacion) + gammaIm*(df_out[[g]]$Asintomaticos/df_out[[g]]$Generacion) + (1 - phiHcD)*gammaHc*(df_out[[g]]$UCI/df_out[[g]]$Generacion) + (df_out[[g]]$Recuperados/df_out[[g]]$Generacion)))>0,df_out[[g]]$Generacion*(phiIR*gammaI*(df_out[[g]]$Infectados/df_out[[g]]$Generacion) + phiHR*gammaH*(df_out[[g]]$Hospitalizados/df_out[[g]]$Generacion) + gammaIm*(df_out[[g]]$Asintomaticos/df_out[[g]]$Generacion) + (1 - phiHcD)*gammaHc*(df_out[[g]]$UCI/df_out[[g]]$Generacion) + (df_out[[g]]$Recuperados/df_out[[g]]$Generacion)),0)
       Temp1[[g]]$Hospitalizados <- ifelse((df_out[[g]]$Generacion*((1-phiIR)*gammaI*(df_out[[g]]$Infectados/df_out[[g]]$Generacion) + (1-gammaH)*(df_out[[g]]$Hospitalizados/df_out[[g]]$Generacion)))>0,df_out[[g]]$Generacion*((1-phiIR)*gammaI*(df_out[[g]]$Infectados/df_out[[g]]$Generacion) + (1-gammaH)*(df_out[[g]]$Hospitalizados/df_out[[g]]$Generacion)),0)
       Temp1[[g]]$UCI <-  ifelse((df_out[[g]]$Generacion*((1-phiHR-phiHD)*gammaH*(df_out[[g]]$Hospitalizados/df_out[[g]]$Generacion) + (1 - gammaHc)*(df_out[[g]]$UCI/df_out[[g]]$Generacion)))>0,df_out[[g]]$Generacion*((1-phiHR-phiHD)*gammaH*(df_out[[g]]$Hospitalizados/df_out[[g]]$Generacion) + (1 - gammaHc)*(df_out[[g]]$UCI/df_out[[g]]$Generacion)),0)
       Temp1[[g]]$Muerto <-  ifelse((df_out[[g]]$Generacion*(phiHD*gammaH*(df_out[[g]]$Hospitalizados/df_out[[g]]$Generacion) + phiHcD*gammaHc*(df_out[[g]]$UCI/df_out[[g]]$Generacion) + (df_out[[g]]$Muerto/df_out[[g]]$Generacion)))>0,df_out[[g]]$Generacion*(phiHD*gammaH*(df_out[[g]]$Hospitalizados/df_out[[g]]$Generacion) + phiHcD*gammaHc*(df_out[[g]]$UCI/df_out[[g]]$Generacion) + (df_out[[g]]$Muerto/df_out[[g]]$Generacion)),0)
     }
    
    
    df_out <- Temp1
    
    
    
    K <- df_out %>% bind_rows() %>% select(Comuna, Generacion, Infectados, Count) %>% group_by(Comuna) %>% summarise_if(is.numeric, sum) %>% 
      mutate(Prev = (Infectados/Generacion)*100000, Count = Count/length(df_out), K_0 = case_when(Count > 0 & Count != Min_Days ~ K_Cuar,
                                                                    Count == 0 & Prev >= Umbral ~ K_Cuar,
                                                                    Count == 0 & Prev < Umbral ~ 0,
                                                                    Count == Min_Days & Prev >= Umbral ~ K_Cuar,
                                                                    Count == Min_Days & Prev < Umbral ~ 0),
             Count = case_when(Count > 0 & Count != Min_Days ~ Count + 1,
                               Count == 0 & Prev >= Umbral ~ Count + 1,
                               Count == 0 & Prev < Umbral ~ 0,
                               Count == Min_Days & Prev >= Umbral ~ 1,
                               Count == Min_Days & Prev < Umbral ~ 0)) %>%
      dplyr::select(K_0, Count)

    
    
    for(i in 1:length(df_out)){
      df_out[[i]]$K_0 <- K$K_0
      df_out[[i]]$Count <- K$Count
    }
    
    
    Results[[d]] <- df_out %>% bind_rows() %>% dplyr::select(-Poblacion, -n_i_g_eff, -P_G, -PI) %>% rename(Poblacion = Generacion) %>% group_by(Comuna) %>% summarise_if(is.numeric, sum)%>% mutate_at(vars(Time:K_0), Div3) %>% mutate(Time = d)
    
   
    
    print(paste("Día", d, "de", Dias, "Listos!", Sys.time()))
    
  }
  
  stopCluster(cl)
  
  Results <- bind_rows(Results) %>% mutate(Fecha = Inicio + (Time-1))   %>% mutate(Cuarentena = case_when(K_0 > 0~ "Cuarentena", K_0 == 0~ " Sin cuarentena"))
  return(list(Results = Results, Starting = df_out))
}
