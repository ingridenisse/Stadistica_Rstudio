#---------------------------------CASE STUDY ----------------------------------

#Cargar paquetes --------------------------------------------------------------
library(haven)
library(dplyr)

#Cargar datos -----------------------------------------------------------------
envigmu <- read_dta("Data/201911_EnvigmuBDD_casadas_unidas.dta")

#Limpieza de datos ------------------------------------------------------------

#Para el presente caso de estudio se creará una nueva variable que indique: 
#1 si la mujer ha sufrido algún tipo de violencia (física, psicológica, sexual,
# económica/patrimonial) y 0 en caso contrario, para esto se utiliza la SECCIÓN 
#7A: VIDA EN PAREJA RELACIÓN ACTUAL - TIPOS DE VIOLENCIA

data_study <- envigmu %>% select(id_per:persona,starts_with("f2_s7a_14")) %>% 
  mutate(violencia=case_when(
      f2_s7a_14_1 %in% c(1, 2, 3) | 
      f2_s7a_14_2 %in% c(1, 2, 3) | 
      f2_s7a_14_3 %in% c(1, 2, 3) | 
      f2_s7a_14_4 %in% c(1, 2, 3) | 
      f2_s7a_14_5 %in% c(1, 2, 3) | 
      f2_s7a_14_6 %in% c(1, 2, 3) | 
      f2_s7a_14_7 %in% c(1, 2, 3) | 
      f2_s7a_14_8 %in% c(1, 2, 3) | 
      f2_s7a_14_9 %in% c(1, 2, 3) | 
      f2_s7a_14_10 %in% c(1, 2, 3) | 
      f2_s7a_14_11 %in% c(1, 2, 3) | 
      f2_s7a_14_12 %in% c(1, 2, 3) | 
      f2_s7a_14_13 %in% c(1, 2, 3) | 
      f2_s7a_14_14 %in% c(1, 2, 3) | 
      f2_s7a_14_15 %in% c(1, 2, 3) | 
      f2_s7a_14_16 %in% c(1, 2, 3) | 
      f2_s7a_14_17 %in% c(1, 2, 3) | 
      f2_s7a_14_18 %in% c(1, 2, 3) | 
      f2_s7a_14_19 %in% c(1, 2, 3) | 
      f2_s7a_14_20 %in% c(1, 2, 3) | 
      f2_s7a_14_21 %in% c(1, 2, 3) | 
      f2_s7a_14_22 %in% c(1, 2, 3) | 
      f2_s7a_14_23 %in% c(1, 2, 3)  ~ 1,
    TRUE ~ 0
  ))
  
