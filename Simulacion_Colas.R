##################################################################################

# SIMULACION TIEMPOS DE ESPERA EN COLA
# Ana Paula Rojas - 29/05/2021

##################################################################################

library(readxl)
library(tidyverse)
library(lubridate)
library(RColorBrewer)

# Working Directory donde se encuentra este archivo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df_admision <- read_xlsx("DataTiempos.xlsx", sheet = 1)
df_egresos <- read_xlsx("DataTiempos.xlsx", sheet = 2)
df_centros <- read_xlsx("DataPersonal.xlsx", sheet = 1)

df_centros <- df_centros %>%
  select("SEDE", "Tipo centro", "PPTO 2021 ADM") %>%
  rename(sede = "SEDE", tipo_centro = "Tipo centro", num_adm = "PPTO 2021 ADM")

# Num de adm por tipo de centro
df_centros %>% group_by(tipo_centro, num_adm) %>% summarize(n = n())

n_distinct(df_admision$Centro) #57
n_distinct(df_egresos$Centro) #39

#Centros que quedan fuera del analisis
df_admision %>%
  filter(!(Centro %in% (df_centros %>% filter(tipo_centro!="Tipo 3"))$sede)) %>% 
  distinct(Centro)

# Nos quedamos solo con los centros que sean Tipo 1 y Tipo 2
df_admision <- df_admision %>% left_join(df_centros, by = c("Centro" = "sede"))
df_egresos <- df_egresos %>% left_join(df_centros, by = c("Centro" = "sede"))

df_admision <- df_admision %>% filter(tipo_centro != "Tipo 3")
df_egresos <- df_egresos %>% filter(tipo_centro != "Tipo 3")

# Verificar que para cada centro haya los 7 dias y 24 horas
df_admision %>% group_by(Centro) %>% summarize(N = n()) %>% summarize(min(N), max(N))
df_egresos %>% group_by(Centro) %>% summarize(N = n()) %>% summarize(min(N), max(N))

######################################################################################
################################# PARTE A ############################################
######################################################################################

# Simulacion de num de pacientes que llegan, a que hora llegan, y cuanto demoran 

centros <- df_admision %>% select(Centro) %>% distinct() %>% pull(Centro)
dias <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo")

simular_pacientes <- function(){
  
  df_simulacion_caso <- NULL
  
  for(centro in centros){
    
    for(d in dias){
      
      df_adm_filt <- df_admision %>% filter(Centro == centro, DiaSem == d)
      df_egr_filt <- df_egresos %>% filter(Centro == centro, DiaSem == d)
      
      for(i in 1:24){
        
        # Ingresos
        pacientes_ing <- rpois(1, lambda = df_adm_filt$TotalIngreso[i]) #Ingresos
        pacientes_ces <- rpois(1, lambda = df_adm_filt$ControlEspontaneo[i]) #Control Espontaneo
        n_adm <- pacientes_ing + pacientes_ces
        t_adm <- rnorm(n_adm, mean = 8, sd = 2) * 60
    
        # Egresos
        pacientes_egrporing <- rpois(1, lambda = df_egr_filt$IngresoEgresoAVG[i])
        pacientes_egrporctr <- rpois(1, lambda = df_egr_filt$ControlEgresoAVG[i])
        n_egr <- pacientes_egrporing + pacientes_egrporctr
        
        t_egr <- sample(c(315,20), n_egr, prob = c(0.67,0.33), replace = TRUE) #315 seg en 67% casos + 20 seg en 33% casos
        t_egr <- sort(t_egr)
        t_egr <- t_egr + c(rnorm(sum(t_egr == 20), mean = 0, sd = 5), rnorm(sum(t_egr == 315), mean = 0, sd = 30))
        
        t_tot <- c(t_adm, t_egr) #Tiempos
        tipo <- c(rep("I", length(t_adm)), rep("E", length(t_egr)))
        
        seg <- sample(0:3599, length(t_tot), replace = TRUE) #En que momento de la hora llegan
      
        # Convertimos a Data Frame
        df_res <- data.frame(Centro = rep(centro, length(t_tot)), Dia = rep(d, length(t_tot)),
                             Hora = rep(i-1, length(t_tot)), Tiempo = t_tot, Seg = seg, Tipo = tipo)
    
        df_simulacion_caso <- bind_rows(df_simulacion_caso, df_res)
        
      }
      
    }
  }
  
  df_simulacion_caso <- df_simulacion_caso %>%
  mutate(Hora_Inicio = hms::as_hms(Hora*3600 + Seg)) %>% 
  mutate(Hora_Fin = hms::as_hms(Hora_Inicio + Tiempo)) %>% 
  mutate(Dia_Num = case_when(Dia == "Lunes" ~ 1, Dia == "Martes" ~ 2, Dia == "Miercoles" ~ 3,
                             Dia == "Jueves" ~ 4, Dia == "Viernes" ~ 5, Dia == "Sabado" ~ 6,
                             Dia == "Domingo" ~ 7)) %>% 
  arrange(Centro,Dia_Num,Hora,Hora_Inicio) %>%
  mutate(Hora_Llegada = Hora_Inicio)
  
  return(df_simulacion_caso)

}

######################################################################################
################################# PARTE B ############################################
######################################################################################

# Creamos data frame con tantas filas como admisionistas por centro

centros <- df_admision %>% select(Centro) %>% distinct() %>% pull(Centro)
df_centros <- df_centros %>% filter(tipo_centro!="Tipo 3")

df_centros_expand <- NULL

for(centro in centros){
  df_filt <- df_centros %>% filter(sede == centro)
  num_adm <- df_filt %>% pull(num_adm)
  vec_adms <- paste0("Adm_N", 1:num_adm)
  df_aux <- data.frame(Admisionista = vec_adms) %>%
    mutate(Sede = df_filt$sede, tipo_centro = df_filt$tipo_centro) %>%
    mutate(Disponible = 0, 
           Ocupado_Hasta = hms::as_hms(0)) %>% 
    select(Sede, tipo_centro, Admisionista, Disponible, Ocupado_Hasta)
  df_centros_expand <- bind_rows(df_centros_expand, df_aux)
}

######################################################################################

# Algoritmo 1 : Toda la admision es presencial (el num de adm x centro puede variar)

algoritmo_1 <- function(df){
  
  df_resultados_algo1 <- NULL
  
  for(d in dias){
    
    df_cent_filt <- df_centros_expand %>% mutate(ID = paste0(Sede,"-",Admisionista))
    df_filt <- df %>% filter(Dia == d) %>% arrange(Hora_Llegada) %>% mutate(Adm_Ocupadas = 0, Adm_Atencion = "")
    
    if(nrow(df_filt)>1){
      
      for(i in 1:(nrow(df_filt))){
        
        # Actualizacion de disponibilidad de admisionistas
        df_cent_filt <- df_cent_filt %>%
          mutate(Disponible = ifelse(Ocupado_Hasta > df_filt[i,]$Hora_Llegada, 1, 0))
        
        # Si la admisionista de ese centro esta disponible
        if(min((df_cent_filt %>% filter(Sede == df_filt[i,]$Centro))$Disponible) == 0){
          adm_selec <- (df_cent_filt %>% filter(Sede == df_filt[i,]$Centro, Disponible < 1))[1,] %>% pull(ID)
          df_cent_filt[df_cent_filt$ID == adm_selec, ]$Disponible = 1
          df_cent_filt[df_cent_filt$ID == adm_selec, ]$Ocupado_Hasta = df_filt[i,]$Hora_Fin
        }
        else{
          # La que se libere primero de ese centro
          adm_selec <- (df_cent_filt %>% filter(Sede == df_filt[i,]$Centro) %>% arrange(Ocupado_Hasta))[1,] %>% pull(ID)
          df_filt[i,]$Hora_Inicio <- (df_cent_filt %>% filter(Sede == df_filt[i,]$Centro) %>% arrange(Ocupado_Hasta))[1,]$Ocupado_Hasta
          df_filt[i,]$Hora_Fin <- hms::as_hms(df_filt[i,]$Hora_Inicio + df_filt[i,]$Tiempo)
          df_cent_filt[df_cent_filt$ID == adm_selec, ]$Ocupado_Hasta = df_filt[i,]$Hora_Fin
        }
        
        df_filt[i,]$Adm_Ocupadas <- sum(df_cent_filt$Disponible)
        df_filt[i,]$Adm_Atencion <- adm_selec
        
      }
    }
    
    df_resultados_algo1 <- bind_rows(df_resultados_algo1, df_filt)
    
  }
  
  df_resultados_algo1 <- df_resultados_algo1 %>% mutate(Cola = Hora_Inicio - Hora_Llegada)
  
  return(df_resultados_algo1)
  
}

######################################################################################
################################# PARTE C ############################################
######################################################################################

##############################################################################################

# Algoritmo 2 : Todos los egresos son presencial y los ingresos remoto (el num de adm x centro puede variar)

algoritmo_2 <- function(df){
  
  df_resultados_algo2 <- NULL
  
  for(d in dias){
    
    df_cent_filt <- df_centros_expand %>% mutate(ID = paste0(Sede,"-",Admisionista))
    df_filt <- df %>% filter(Dia == d) %>% arrange(Hora_Llegada) %>% mutate(Adm_Ocupadas = 0, Adm_Atencion = "")
    #%>% mutate(Tipo_Atencion = "")
    
    if(nrow(df_filt)>1){ #TODO >=1
      
      for(i in 1:(nrow(df_filt))){
        
        # Actualizacion de disponibilidad de admisionistas
        df_cent_filt <- df_cent_filt %>%
          mutate(Disponible = ifelse(Ocupado_Hasta > df_filt[i,]$Hora_Llegada, 1, 0))
        
        # Si es un Egreso es 100% presencial
        if(df_filt[i,]$Tipo == "E"){
          
          # Si hay alguna admisionista disponible en ese Centro
          if(min((df_cent_filt %>% filter(Sede == df_filt[i,]$Centro))$Disponible) == 0){
            adm_selec <- (df_cent_filt %>% filter(Sede == df_filt[i,]$Centro, Disponible < 1))[1,] %>% pull(ID)
            df_cent_filt[df_cent_filt$ID == adm_selec, ]$Disponible = 1
            df_cent_filt[df_cent_filt$ID == adm_selec, ]$Ocupado_Hasta = df_filt[i,]$Hora_Fin
          }
          else{
            # La que se libere primero de ese Centro
            adm_selec <- (df_cent_filt %>% filter(Sede == df_filt[i,]$Centro) %>% arrange(Ocupado_Hasta))[1,] %>% pull(ID)
            df_filt[i,]$Hora_Inicio <- (df_cent_filt %>% filter(Sede == df_filt[i,]$Centro) %>% arrange(Ocupado_Hasta))[1,]$Ocupado_Hasta
            df_filt[i,]$Hora_Fin <- hms::as_hms(df_filt[i,]$Hora_Inicio + df_filt[i,]$Tiempo)
            df_cent_filt[df_cent_filt$ID == adm_selec, ]$Ocupado_Hasta = df_filt[i,]$Hora_Fin
          }
          # Si es un Ingreso es 100% remoto  
        }else{
          
          # Si hay alguna admisionista disponible
          if(min(df_cent_filt$Disponible) == 0){
            # La admisionista que se libero primero de cualquier centro
            adm_selec <- (df_cent_filt %>% filter(Disponible == 0) %>% arrange(Ocupado_Hasta))[1,] %>% pull(ID)
            df_cent_filt[df_cent_filt$ID == adm_selec, ]$Disponible = 1
            df_cent_filt[df_cent_filt$ID == adm_selec, ]$Ocupado_Hasta = df_filt[i,]$Hora_Fin
          }else{
            # La admisionista que se libere primero de cualquier centro
            adm_selec <- (df_cent_filt %>% filter(Disponible == 1) %>% arrange(Ocupado_Hasta))[1,] %>% pull(ID)
            df_filt[i,]$Hora_Inicio <- (df_cent_filt %>% filter(Disponible == 1) %>% arrange(Ocupado_Hasta))[1,]$Ocupado_Hasta
            df_filt[i,]$Hora_Fin <- hms::as_hms(df_filt[i,]$Hora_Inicio + df_filt[i,]$Tiempo)
            df_cent_filt[df_cent_filt$ID == adm_selec, ]$Ocupado_Hasta = df_filt[i,]$Hora_Fin
          }
        }
        
        df_filt[i,]$Adm_Ocupadas <- sum(df_cent_filt$Disponible)
        df_filt[i,]$Adm_Atencion <- adm_selec
        
      }
    }
    
    df_resultados_algo2 <- bind_rows(df_resultados_algo2, df_filt)
    
  }
  
  df_resultados_algo2 <- df_resultados_algo2 %>% mutate(Cola = Hora_Inicio - Hora_Llegada)
  
  return(df_resultados_algo2)
  
}

##############################################################################################

# Algoritmo 3 : Todos los egresos son presencial y los ingresos presencial
#               si disponible, sino por remoto (el num de adm x centro puede variar)

algoritmo_3 <- function(df){
  
  df_resultados_algo3 <- NULL
  
  for(d in dias){
    
    df_cent_filt <- df_centros_expand %>% mutate(ID = paste0(Sede,"-",Admisionista))
    df_filt <- df %>% filter(Dia == d) %>% arrange(Hora_Llegada) %>% mutate(Adm_Ocupadas = 0, Adm_Atencion = "")
    
    if(nrow(df_filt)>1){ #TODO >=1
      
      for(i in 1:(nrow(df_filt))){
        
        # Actualizacion de disponibilidad de admisionistas
        df_cent_filt <- df_cent_filt %>%
          mutate(Disponible = ifelse(Ocupado_Hasta > df_filt[i,]$Hora_Llegada, 1, 0))
        
        # Si es un Egreso es 100% presencial
        if(df_filt[i,]$Tipo == "E"){
          
          # Si hay alguna admisionista disponible en ese Centro
          if(min((df_cent_filt %>% filter(Sede == df_filt[i,]$Centro))$Disponible) == 0){
            adm_selec <- (df_cent_filt %>% filter(Sede == df_filt[i,]$Centro, Disponible < 1))[1,] %>% pull(ID)
            df_cent_filt[df_cent_filt$ID == adm_selec, ]$Disponible = 1
            df_cent_filt[df_cent_filt$ID == adm_selec, ]$Ocupado_Hasta = df_filt[i,]$Hora_Fin
          }
          else{
            # La que se libere primero de ese Centro
            adm_selec <- (df_cent_filt %>% filter(Sede == df_filt[i,]$Centro) %>% arrange(Ocupado_Hasta))[1,] %>% pull(ID)
            df_filt[i,]$Hora_Inicio <- (df_cent_filt %>% filter(Sede == df_filt[i,]$Centro) %>% arrange(Ocupado_Hasta))[1,]$Ocupado_Hasta
            df_filt[i,]$Hora_Fin <- hms::as_hms(df_filt[i,]$Hora_Inicio + df_filt[i,]$Tiempo)
            df_cent_filt[df_cent_filt$ID == adm_selec, ]$Ocupado_Hasta = df_filt[i,]$Hora_Fin
          }
          # Si es un Ingreso es presencial si libre, sino remoto 
        }else{
          # Si hay alguna admisionista libre en ese centro
          if(min((df_cent_filt %>% filter(Sede == df_filt[i,]$Centro))$Disponible) == 0){
            adm_selec <- (df_cent_filt %>% filter(Sede == df_filt[i,]$Centro, Disponible < 1))[1,] %>% pull(ID)
            df_cent_filt[df_cent_filt$ID == adm_selec, ]$Disponible = 1
            df_cent_filt[df_cent_filt$ID == adm_selec, ]$Ocupado_Hasta = df_filt[i,]$Hora_Fin
          }
          else{
            # Atencion Remota
            # Si hay alguna otra admisionista disponible
            if(min(df_cent_filt$Disponible) == 0){
              # La admisionista que se libero primero de cualquier centro
              adm_selec <- (df_cent_filt %>% filter(Disponible == 0) %>% arrange(Ocupado_Hasta))[1,] %>% pull(ID)
              df_cent_filt[df_cent_filt$ID == adm_selec, ]$Disponible = 1
              df_cent_filt[df_cent_filt$ID == adm_selec, ]$Ocupado_Hasta = df_filt[i,]$Hora_Fin
            }else{
              # La admisionista que se libere primero de cualquier centro
              adm_selec <- (df_cent_filt %>% filter(Disponible == 1) %>% arrange(Ocupado_Hasta))[1,] %>% pull(ID)
              df_filt[i,]$Hora_Inicio <- (df_cent_filt %>% filter(Disponible == 1) %>% arrange(Ocupado_Hasta))[1,]$Ocupado_Hasta
              df_filt[i,]$Hora_Fin <- hms::as_hms(df_filt[i,]$Hora_Inicio + df_filt[i,]$Tiempo)
              df_cent_filt[df_cent_filt$ID == adm_selec, ]$Ocupado_Hasta = df_filt[i,]$Hora_Fin
            }
            
          }
          
        }
        
        df_filt[i,]$Adm_Ocupadas <- sum(df_cent_filt$Disponible)
        df_filt[i,]$Adm_Atencion <- adm_selec
        
      }
    }
    
    df_resultados_algo3 <- bind_rows(df_resultados_algo3, df_filt)
    
  }
  
  df_resultados_algo3 <- df_resultados_algo3 %>%
    mutate(Cola = Hora_Inicio - Hora_Llegada,
           Tipo_Atencion = case_when(Tipo == "E" ~ "P",
                                     word(Adm_Atencion, sep = "-") == Centro ~ "P",
                                     TRUE ~ "R"))
  
  return(df_resultados_algo3)
  
}

######################################################################################
################################# PARTE D ############################################
######################################################################################

# Corremos N simulaciones y aplicamos los algoritmos

df_res_simu_algo1 <- NULL
df_res_simu_algo2 <- NULL
df_res_simu_algo3 <- NULL

t0 <- Sys.time()

for(i in 1:50){
  
  print(paste0("Num Simulacion ", i))
  
  df_simulacion <- simular_pacientes()
  df_simulacion <- df_simulacion %>% mutate(Simulacion = i)
  print("Termino de crear data")
  
  df_algo1 <- algoritmo_1(df_simulacion)
  print("Termino algoritmo 1")
  df_algo2 <- algoritmo_2(df_simulacion)
  print("Termino algoritmo 2")
  df_algo3 <- algoritmo_3(df_simulacion)
  print("Termino algoritmo 3")
  
  df_res_simu_algo1 <- bind_rows(df_res_simu_algo1, df_algo1)
  df_res_simu_algo2 <- bind_rows(df_res_simu_algo2, df_algo2)
  df_res_simu_algo3 <- bind_rows(df_res_simu_algo3, df_algo3)
  
}

t1 <- Sys.time()

t1-t0

# Porsiacaso
write_csv(df_res_simu_algo1, "df_res_simu_algo1.csv")
write_csv(df_res_simu_algo2, "df_res_simu_algo2.csv")
write_csv(df_res_simu_algo3, "df_res_simu_algo3.csv")

###############################################################################

# ANALISIS DE LOS RESULTADOS

# Numero total de pacientes semanales por Centro

df_resultados <- df_res_simu_algo1 %>% group_by(Simulacion, Centro) %>%
  summarize(n = n(), Por_Egr = sum(Tipo == "E")/n()) %>% 
  left_join(df_centros, by = c("Centro" = "sede"))

# Boxplot total pacientes semanales por simulacion - coloreado por num admisionistas
ggplot(df_resultados, aes(x = reorder(Centro, -n, FUN = median), y = n, fill = as.factor(num_adm))) +
  geom_boxplot(aes(reorder(Centro, -n, FUN = median)), col = "#555454", size = 0.3, outlier.size = 0.2)+
  scale_fill_manual(values=c("#F2BE54", "#799F4D", "#153E5C")) +
  scale_y_continuous(breaks = seq(0,800,100)) +
  labs(title="", x = "Sede", y = "Numero total de pacientes por semana \n", fill = "Numero de admisionistas") +
  theme_classic() +
  theme(legend.position= c(0.8,0.95),
        legend.direction = "horizontal",
        axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust=1))

ggsave("G1.png", width = 12, height = 5)

# Boxplot total pacientes semanales por simulacion - coloreado por tipo de centro
ggplot(df_resultados, aes(x = reorder(Centro, -n, FUN = median), y = n, fill = tipo_centro)) +
  geom_boxplot(aes(reorder(Centro, -n, FUN = median)), col = "#555454", size = 0.3, outlier.size = 0.2)+
  scale_fill_manual(values=c("#F2BE54", "#799F4D")) +
  scale_y_continuous(breaks = seq(0,800,100)) +
  labs(title="", x = "Sede", y = "Numero total de pacientes por semana \n", fill = "Tipo Centro") +
  theme_classic() +
  theme(legend.position= c(0.8,0.95),
        legend.direction = "horizontal",
        axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust=1))

ggsave("G2.png", width = 12, height = 5)

# Boxplot de Porcentaje de Egresos por Centro por simulacion - coloreado por tipo de centro 
ggplot(df_resultados, aes(x = reorder(Centro, -n, FUN = median), y = Por_Egr, fill = tipo_centro)) +
  geom_boxplot(aes(reorder(Centro, -n, FUN = median)), col = "#555454", size = 0.3, outlier.size = 0.2)+
  scale_fill_manual(values=c("#153E5C", "#007F46")) +
  scale_y_continuous(breaks = seq(0,1,0.1), labels = paste0(seq(0,1,0.1)*100,"%")) +
  labs(title="", x = "Sede", y = "Distribucion del Porcentaje de Egreso \n", fill = "Tipo Centro") +
  theme_classic() +
  theme(legend.position= c(0.8,0.95),
        legend.direction = "horizontal",
        axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust=1))

ggsave("G3.png", width = 12, height = 5)

# Histograma de Tiempo de Ingresos y Egresos

ggplot(df_res_simu_algo1, aes(x = Tiempo/60, fill = Tipo, color = Tipo, y=stat(count)/sum(stat(count)))) +
  geom_histogram(position="identity", alpha = 0.7) +
  labs(title="", x = "Tiempo de Atencion (minutos)", y = "Densidad \n", color = "", fill = "Tipo de Atencion") +
  scale_fill_manual(values = c("#007F46","#F2BE54"), labels = c("Egreso","Ingreso"))+
  scale_color_manual(values = c("#007F46","#F2BE54"), guide = 'none')+
  scale_x_continuous(breaks = seq(0,16,1)) +
  scale_y_continuous(breaks = seq(0,0.2,0.05), labels = paste0(seq(0,0.2,0.05)*100,"%")) +
  theme_classic() +
  theme(legend.position= c(0.8,0.8),
        legend.direction = "vertical")

ggsave("G4.png", width = 5, height = 5)

# Descriptivos
summary((df_res_simu_algo1 %>% filter(Tipo == "E"))$Tiempo/60)
summary((df_res_simu_algo1 %>% filter(Tipo == "I"))$Tiempo/60)

n_1 = df_res_simu_algo1 %>% filter(Tipo == "E") %>% mutate(Tiempo = Tiempo) %>% filter(Tiempo < 100)
n_2 = df_res_simu_algo1 %>% filter(Tipo == "E") %>% mutate(Tiempo = Tiempo) %>% filter(Tiempo >= 100)

nrow(n_1)/(nrow(n_1)+nrow(n_2))

n_1 %>% pull(Tiempo) %>% summary()
n_2 %>% pull(Tiempo) %>% summary()

# Heatmap de dias y rangos horarios

df_heatmap <- df_res_simu_algo1 %>%
  filter(Hora >= 7) %>% 
  group_by(Dia, Hora, Simulacion) %>% summarize(Pacientes = n()) %>% 
  group_by(Dia, Hora) %>% summarize(Paciente_Promedio = mean(Pacientes)) %>% 
  mutate(Dia = factor(Dia, ordered = TRUE, levels = rev(dias)))

ggplot(df_heatmap, aes(x=Hora, y=Dia, fill=Paciente_Promedio)) + 
  geom_tile(color = "white", size = 1.5) +
  scale_fill_gradient2(low = "#b3cde0", high = "#011f4b", mid = "#005b96", 
                       midpoint = 250, limit = c(0,500), space = "Lab", name = "Num Pacientes") +
  scale_x_continuous(name ="Hora", breaks=7:22) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  theme_classic()

ggsave("G5.png", width = 8, height = 4)

# Descriptivo
df_res_simu_algo1 %>% group_by(Dia) %>% summarize(n=n()) %>% mutate(freq = n * 100/ sum(n))
#Sabado + Domingo 0.00591 + 0.266

df_res_simu_algo1 %>% group_by(Hora) %>% summarize(n=n()) %>% mutate(freq = n * 100/ sum(n))

#(10.2+12.6+12.7+11.4)

# Resultados de las Colas

df_res_simu_algo1 %>% summarize(n = n(), n_cola = sum(Cola > 0)) %>% mutate(por_no_cola = 100 - n_cola*100/n)
df_res_simu_algo2 %>% summarize(n = n(), n_cola = sum(Cola > 0)) %>% mutate(por_no_cola = 100 - n_cola*100/n)
df_res_simu_algo3 %>% summarize(n = n(), n_cola = sum(Cola > 0)) %>% mutate(por_no_cola = 100 - n_cola*100/n)

df_res_simu_algo1 %>% filter(Cola > 0) %>% mutate(Cola = Cola/60) %>% pull(Cola) %>% as.numeric() %>% summary()
df_res_simu_algo2 %>% filter(Cola > 0) %>% mutate(Cola = Cola/60) %>% pull(Cola) %>% as.numeric() %>% summary()
df_res_simu_algo3 %>% filter(Cola > 0) %>% mutate(Cola = Cola/60) %>% pull(Cola) %>% as.numeric() %>% summary()

df_res_simu_algo1 %>% filter(Cola > 0) %>% summarize(n=n(), N_30 = sum(Cola/60 > 50)) %>% mutate(por_N_30 = N_30/n)

#Boxplot de Colas
ggplot(df_res_simu_algo1 %>% filter(Cola > 0), aes(x = as.numeric(Cola)/60)) +
  geom_boxplot(fill = "white", col = "#153E5C") +
  labs(title="", y = "", x = "Tiempo de Cola (minutos)") +
  scale_x_continuous(limits = c(0,50), breaks = seq(0,50,5)) +
  theme_classic() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank())

ggsave("G6.png", width = 4, height = 2)

ggplot(df_res_simu_algo2 %>% filter(Cola > 0), aes(x = as.numeric(Cola)/60)) +
  geom_boxplot(fill = "white", col = "#153E5C") +
  labs(title="", y = "", x = "Tiempo de Cola (minutos)") +
  scale_x_continuous(limits = c(0,50), breaks = seq(0,50,5)) +
  theme_classic() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank())

ggsave("G7.png", width = 4, height = 2)

ggplot(df_res_simu_algo3 %>% filter(Cola > 0), aes(x = as.numeric(Cola)/60)) +
  geom_boxplot(fill = "white", col = "#153E5C") +
  labs(title="", y = "", x = "Tiempo de Cola (minutos)") +
  scale_x_continuous(limits = c(0,50), breaks = seq(0,50,5)) +
  theme_classic() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank())

ggsave("G8.png", width = 4, height = 2)

# Porcentaje de ocupacion de la administradora

df_por_ocupacion1 <- df_res_simu_algo1 %>%
  filter(Adm_Atencion != "", Dia != "Domingo", Dia != "Sabado") %>%
  group_by(Centro, Adm_Atencion, Simulacion) %>%
  summarize(Por_Ocupacion = sum(Tiempo)/234000, n = n()) %>% 
  left_join(df_centros, by = c("Centro" = "sede"))

df_por_ocupacion2 <- df_res_simu_algo2 %>%
  filter(Adm_Atencion != "", Dia != "Domingo", Dia != "Sabado") %>%
  mutate(Centro_Admisionista = word(Adm_Atencion, sep = "-")) %>% 
  group_by(Centro_Admisionista, Adm_Atencion, Simulacion) %>%
  summarize(Por_Ocupacion = sum(Tiempo)/234000, n = n()) %>% 
  left_join(df_centros, by = c("Centro_Admisionista" = "sede"))

df_por_ocupacion3 <- df_res_simu_algo3 %>%
  filter(Adm_Atencion != "", Dia != "Domingo", Dia != "Sabado") %>%
  mutate(Centro_Admisionista = word(Adm_Atencion, sep = "-")) %>% 
  group_by(Centro_Admisionista, Adm_Atencion, Simulacion) %>%
  summarize(Por_Ocupacion = sum(Tiempo)/234000, n = n()) %>% 
  left_join(df_centros, by = c("Centro_Admisionista" = "sede"))

# Boxplots Porcentaje Ocupacion
ggplot(df_por_ocupacion1, aes(x = reorder(Centro, -n, FUN = sum), y = Por_Ocupacion, fill = as.factor(num_adm))) +
  geom_boxplot(aes(reorder(Centro, -n, FUN = sum)), col = "#555454", size = 0.3, outlier.size = 0.2)+
  scale_fill_manual(values=c("#F2BE54", "#799F4D", "#153E5C")) +
  scale_y_continuous(breaks = seq(0,1,0.05), labels = paste0(seq(0,1,0.05)*100,"%"), limits = c(0,0.8)) +
  labs(title="", x = "Sede", y = "Porcentaje de tiempo ocupado \n", fill = "Numero de admisionistas") +
  theme_classic() +
  theme(legend.position= c(0.8,0.95),
        legend.direction = "horizontal",
        axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust=1))

ggsave("G9.png", width = 12, height = 5)

ggplot(df_por_ocupacion2, aes(x = reorder(Centro_Admisionista, -n, FUN = sum), y = Por_Ocupacion, fill = as.factor(num_adm))) +
  geom_boxplot(aes(reorder(Centro_Admisionista, -n, FUN = sum)), col = "#555454", size = 0.3, outlier.size = 0.2)+
  scale_fill_manual(values=c("#F2BE54", "#799F4D", "#153E5C")) +
  scale_y_continuous(breaks = seq(0,1,0.05), labels = paste0(seq(0,1,0.05)*100,"%"), limits = c(0,0.8)) +
  labs(title="", x = "Sede", y = "Numero total de pacientes por semana \n", fill = "Numero de admisionistas") +
  theme_classic() +
  theme(legend.position= c(0.8,0.95),
        legend.direction = "horizontal",
        axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust=1))

ggsave("G10.png", width = 12, height = 5)

ggplot(df_por_ocupacion3, aes(x = reorder(Centro_Admisionista, -n, FUN = sum), y = Por_Ocupacion, fill = as.factor(num_adm))) +
  geom_boxplot(aes(reorder(Centro_Admisionista, -n, FUN = sum)), col = "#555454", size = 0.3, outlier.size = 0.2)+
  scale_fill_manual(values=c("#F2BE54", "#799F4D", "#153E5C")) +
  scale_y_continuous(breaks = seq(0,1,0.05), labels = paste0(seq(0,1,0.05)*100,"%"), limits = c(0,0.8)) +
  labs(title="", x = "Sede", y = "Numero total de pacientes por semana \n", fill = "Numero de admisionistas") +
  theme_classic() +
  theme(legend.position= c(0.8,0.95),
        legend.direction = "horizontal",
        axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust=1))

ggsave("G11.png", width = 12, height = 5)

###############################################################################################


