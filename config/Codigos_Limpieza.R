
# Este documento corresponde al archivo .R que contiene los códigos para la 
# limpieza de la base de datos

# command + shift + C

# Librerías:

# install.packages("ProjectTemplate")
# install.packages("devtools")
# install.packages("tidyverse")
# install.packages("cowplot")
# install.packages("openintro")

library("digest")
library("tibble")
library(ProjectTemplate)
library(tidyverse)
library(cowplot)
  # Estos son necesarios para cargar openintro
  library(airports)
  library(cherryblossom)
  library(usdata)
library(openintro)
library(haven) # Para cargar el .sav
               # Para usar "as_factor()"

##############################################

# Algunas funciones descubiertas

# sum(!is.na(variable)) -- Permite saber cuantas variables no son NA --

##############################################


# Cargamos la base de datos 

datos <- read_sav("data/GPES-ELAB-GEBD-ENAHO-2023_BdBasePublica.sav")
datos_con_tipo_de_varibles <- datos

####################################

# Variables Relacionadas con educación

####################################

datos_con_tipo_de_varibles$A13 <- as_factor(datos_con_tipo_de_varibles$A13)
# Asistencia a educación formal 
# Tasa de respuesta: 100%
sum(!is.na(datos_con_tipo_de_varibles$A13))/30540

datos_con_tipo_de_varibles$NivInst <- as_factor(datos_con_tipo_de_varibles$NivInst)
# Nivel de instrucción
# Tasa de respuesta: 100%
sum(!is.na(datos_con_tipo_de_varibles$NivInst))/30540

datos_con_tipo_de_varibles$A14 <- as_factor(datos_con_tipo_de_varibles$A14)
# Último año aprovado 
# Tasa de respuesta: 100%
sum(!is.na(datos_con_tipo_de_varibles$A14))/30540

datos_con_tipo_de_varibles$Escolari <- as.integer(datos_con_tipo_de_varibles$Escolari)
datos_con_tipo_de_varibles$Escolari[datos_con_tipo_de_varibles$Escolari == 99] <- NA

# Años de escolaridad
# Tasa de respuesta: 100%
sum(!is.na(datos_con_tipo_de_varibles$Escolari))/30540

datos_con_tipo_de_varibles$A4 <- as_factor(datos_con_tipo_de_varibles$A4)
datos_con_tipo_de_varibles$LugNac <- as_factor(datos_con_tipo_de_varibles$LugNac)
datos_con_tipo_de_varibles$A15A <- as_factor(datos_con_tipo_de_varibles$A15A)
datos_con_tipo_de_varibles$A15B <- as_factor(datos_con_tipo_de_varibles$A15B)
datos_con_tipo_de_varibles$A17 <- as_factor(datos_con_tipo_de_varibles$A17)
datos_con_tipo_de_varibles$A18A <- as_factor(datos_con_tipo_de_varibles$A18A)
datos_con_tipo_de_varibles$A16B <- as_factor(datos_con_tipo_de_varibles$A16B)
datos_con_tipo_de_varibles$REZ_ESC <- as_factor(datos_con_tipo_de_varibles$REZ_ESC)
datos_con_tipo_de_varibles$ForReg <- as_factor(datos_con_tipo_de_varibles$ForReg)
datos_con_tipo_de_varibles$A18A <- as_factor(datos_con_tipo_de_varibles$A18A)
datos_con_tipo_de_varibles$A18B <- as_factor(datos_con_tipo_de_varibles$A18B)
datos_con_tipo_de_varibles$A18C <- as_factor(datos_con_tipo_de_varibles$A18C)
datos_con_tipo_de_varibles$A18C <- as_factor(datos_con_tipo_de_varibles$A18C)
datos_con_tipo_de_varibles$A20A <- as_factor(datos_con_tipo_de_varibles$A20A)
datos_con_tipo_de_varibles$A20A <- as_factor(datos_con_tipo_de_varibles$A20A)
datos_con_tipo_de_varibles$ForNoReg <- as_factor(datos_con_tipo_de_varibles$ForNoReg)
datos_con_tipo_de_varibles$A21 <- as_factor(datos_con_tipo_de_varibles$A21)
datos_con_tipo_de_varibles$A22A <- as_factor(datos_con_tipo_de_varibles$A22A)
datos_con_tipo_de_varibles$A22B <- as_factor(datos_con_tipo_de_varibles$A22B)

#variables que creo que puedan ser utiles
datos_con_tipo_de_varibles$REGION <- as_factor(datos_con_tipo_de_varibles$REGION)
datos_con_tipo_de_varibles$ZONA <- as_factor(datos_con_tipo_de_varibles$ZONA)
datos_con_tipo_de_varibles$EFI <- as_factor(datos_con_tipo_de_varibles$EFI)
datos_con_tipo_de_varibles$CalViv <- as_factor(datos_con_tipo_de_varibles$CalViv)
datos_con_tipo_de_varibles$ZONA <- as_factor(datos_con_tipo_de_varibles$ZONA)
datos_con_tipo_de_varibles$V19 <- as_factor(datos_con_tipo_de_varibles$V19)
datos_con_tipo_de_varibles$ZONA <- as_factor(datos_con_tipo_de_varibles$ZONA)
datos_con_tipo_de_varibles$A3 <- as_factor(datos_con_tipo_de_varibles$A3)
datos_con_tipo_de_varibles$A6 <- as_factor(datos_con_tipo_de_varibles$A6)
datos_con_tipo_de_varibles$CondMig <- as_factor(datos_con_tipo_de_varibles$CondMig)
datos_con_tipo_de_varibles$A3 <- as_factor(datos_con_tipo_de_varibles$A3)







####################################

# Variables Relacionadas con ingreso o indicadores de condición socioeconómica

####################################

datos_con_tipo_de_varibles$A23 <- as_factor(datos_con_tipo_de_varibles$A23)
datos_con_tipo_de_varibles$RamaEmpPri <- as_factor(datos_con_tipo_de_varibles$RamaEmpPri)
datos_con_tipo_de_varibles$np <- as_factor(datos_con_tipo_de_varibles$np)
datos_con_tipo_de_varibles$E1 <- as_factor(datos_con_tipo_de_varibles$E1)
datos_con_tipo_de_varibles$IPM_Pobreza <- as_factor(datos_con_tipo_de_varibles$IPM_Pobreza)
datos_con_tipo_de_varibles$IPM_Intensidad <- as_factor(datos_con_tipo_de_varibles$IPM_Intensidad)
datos_con_tipo_de_varibles$ClasPubPrivPri <- as_factor(datos_con_tipo_de_varibles$ClasPubPrivPri)
datos_con_tipo_de_varibles$G3 <- as_factor(datos_con_tipo_de_varibles$G3)
datos_con_tipo_de_varibles$IPM_PS2 <- as_factor(datos_con_tipo_de_varibles$IPM_PS2)
datos_con_tipo_de_varibles$IPM_PS4 <- as_factor(datos_con_tipo_de_varibles$IPM_PS4)
datos_con_tipo_de_varibles$Q_ZON_IPCN <- as_factor(datos_con_tipo_de_varibles$Q_ZON_IPCN)
datos_con_tipo_de_varibles$Q_REG_IPCN <- as_factor(datos_con_tipo_de_varibles$Q_REG_IPCN)
datos_con_tipo_de_varibles$Q_IPCN <- as_factor(datos_con_tipo_de_varibles$Q_IPCN)


variables_utiles <- datos_con_tipo_de_varibles %>% select(A4,        A5,
                                                          LugNac,    A13,
                                                          A14,       A15A,
                                                          A15B,      A16B,
                                                          A17,       A18A,
                                                          NivInst,   Escolari,
                                                          REZ_ESC,   ForReg,
                                                          A18A,      A18B,
                                                          A18C,      A20A,
                                                          ForNoReg,  A21,
                                                          A22A,      A22B,
                                                          np, # Socioeconomicas
                                                          A23,
                                                          E1,
                                                          IPM_Pobreza,
                                                          IPM_Intensidad,
                                                          ClasPubPrivPri,
                                                          G3,
                                                          IPM_PS2, #Revisar sitio INEC respecto a esta variable
                                                          IPM_PS4,
                                                          Q_ZON_IPCN,
                                                          Q_REG_IPCN,
                                                          Q_IPCN,
                                                          spmn, # Cuantitativas
                                                          ipbh, 
                                                          ipnh,
                                                          ithb,
                                                          ithn,
                                                          ipcb,
                                                          ipcn,
                                                          ipsnt,
                                                          itpn,
                                                          isanh,
                                                          ts,
                                                          ot,
                                                          ttmh,
                                                          tnmh,
                                                          cba,
                                                          lp,
                                                          )

# Formatear los valores en la columna Q_IPCN
variables_utiles$Q_IPCN <- as.character(variables_utiles$Q_IPCN)
variables_utiles$Q_IPCN <- variables_utiles$Q_IPCN %>% str_replace_all("\\d+", function(x) {
  formatC(as.numeric(x), format = "f", big.mark = ".", digits = 0)
})
variables_utiles$Q_IPCN <- gsub("\\s([0-9]{3}\\.+)", " ₡\\1", variables_utiles$Q_IPCN)
variables_utiles$Q_IPCN <- as_factor(variables_utiles$Q_IPCN)
# Reordenar los niveles de Q_IPCN
variables_utiles$Q_IPCN <- factor(variables_utiles$Q_IPCN, 
                                  levels = c("Q1: ₡110.683 ó menos", 
                                             "Q2: Más de ₡110.683 a ₡195.000", 
                                             "Q3: Más de ₡195.000 a ₡321.523", 
                                             "Q4: Más de ₡321.523 a ₡574.085", 
                                             "Q5: Más de ₡574.085", 
                                             "NA"))


# escogencia de datos JOSE
datos_jc <- datos_con_tipo_de_varibles %>% select(REGION, ZONA,
                                                  EFI, CalViv,
                                                  V19,TamViv,
                                                  A3, A4,
                                                  A5, A6,
                                                  CondMig, A15A,
                                                  A15B,NivInst,
                                                  Escolari, ForReg,
                                                  A16B, A17,
                                                  A20A, ForNoReg,
                                                  A22A,itpn,
                                                  np, IPM_Pobreza, 
                                                  Q_IPCN, ipcn)

datos_jc <- datos_jc %>% filter(A5 >= 18 & A5 <= 60)  

datos_jc <- datos_jc %>% 
  mutate(Tiene_prim_completa = case_when(NivInst == "Sin nivel de instrucción" | 
                                        NivInst == "Primaria incompleta" ~ "Sin primaria completa",
                                        TRUE ~ "Con primaria completa"))
datos_jc <- datos_jc %>% 
  mutate(Tiene_sec_completa = case_when(NivInst == "Secundaria técnica completa" |
                                           NivInst == "Secundaria académica completa" |
                                           NivInst == "Educación superior de pregrado y grado" |
                                           NivInst == "Educación superior de posgrado" ~ "Con secundaria completa",
                                          TRUE ~ "Sin secundaria completa"))
datos_jc <- datos_jc %>% 
  mutate(Tiene_est_postsec = case_when(NivInst == "Educación superior de pregrado y grado" | 
                                           NivInst == "Educación superior de posgrado" ~ "Con estudios postsecundarios",
                                         TRUE ~ "Sin estudios postsecundarios"))

datos_jc <- datos_jc %>% 
  mutate(colegio_zona = str_c("Colegio ", A15A, " de zona ", ZONA))

cuadro_primaria_completa <- datos_jc %>% 
  filter(!is.na(np)) %>%
  group_by(Tiene_prim_completa, np) %>% 
  summarise(conteo = n(), 
            .groups = 'drop') %>%
  group_by(Tiene_prim_completa) %>%  # Agrupar por Tiene_prim_completa para calcular el porcentaje correcto
  mutate(porcentaje = (conteo / sum(conteo)) * 100)  # Actualizar el porcentaje

cuadro_secundaria_completa <- datos_jc %>% 
  filter(!is.na(np)) %>%
  group_by(Tiene_sec_completa, np) %>% 
  summarise(conteo = n(), 
            .groups = 'drop') %>%
  group_by(Tiene_sec_completa) %>%  # Agrupar por Tiene_prim_completa para calcular el porcentaje correcto
  mutate(porcentaje = (conteo / sum(conteo)) * 100)  # Actualizar el porcentaje

cuadro_estudios_postsecundarios <- datos_jc %>% 
  filter(!is.na(np)) %>%
  group_by(Tiene_est_postsec, np) %>% 
  summarise(conteo = n(), 
            .groups = 'drop') %>%
  group_by(Tiene_est_postsec) %>%  # Agrupar por Tiene_prim_completa para calcular el porcentaje correcto
  mutate(porcentaje = (conteo / sum(conteo)) * 100)  # Actualizar el porcentaje

# Formatear los valores en la columna Q_IPCN
datos_jc$Q_IPCN <- as.character(datos_jc$Q_IPCN)
datos_jc$Q_IPCN <- datos_jc$Q_IPCN %>% str_replace_all("\\d+", function(x) {
  formatC(as.numeric(x), format = "f", big.mark = ".", digits = 0)
})
datos_jc$Q_IPCN <- gsub("\\s([0-9]{3}\\.+)", " ₡\\1", datos_jc$Q_IPCN)
datos_jc$Q_IPCN <- as_factor(datos_jc$Q_IPCN)
# Reordenar los niveles de Q_IPCN
datos_jc$Q_IPCN <- factor(datos_jc$Q_IPCN, 
                                  levels = c("Q1: ₡110.683 ó menos", 
                                             "Q2: Más de ₡110.683 a ₡195.000", 
                                             "Q3: Más de ₡195.000 a ₡321.523", 
                                             "Q4: Más de ₡321.523 a ₡574.085", 
                                             "Q5: Más de ₡574.085", 
                                             "NA"))

datos_jc$np <- factor(datos_jc$np, 
                          levels = c("No pobre",
                                     "Pobreza no extrema",
                                     "Pobreza extrema"))

###############################################

# Formatear los valores en la columna Q_IPCN
variables_comunes$Q_IPCN <- as.character(variables_comunes$Q_IPCN)
variables_comunes$Q_IPCN <- variables_comunes$Q_IPCN %>% str_replace_all("\\d+", function(x) {
  formatC(as.numeric(x), format = "f", big.mark = ".", digits = 0)
})
variables_comunes$Q_IPCN <- gsub("\\s([0-9]{3}\\.+)", " ₡\\1", variables_comunes$Q_IPCN)
variables_comunes$Q_IPCN <- as_factor(variables_comunes$Q_IPCN)
# Reordenar los niveles de Q_IPCN 
variables_comunes$Q_IPCN <- factor(variables_comunes$Q_IPCN, 
                          levels = c("Q1: ₡110.683 ó menos", 
                                     "Q2: Más de ₡110.683 a ₡195.000", 
                                     "Q3: Más de ₡195.000 a ₡321.523", 
                                     "Q4: Más de ₡321.523 a ₡574.085", 
                                     "Q5: Más de ₡574.085", 
                                     "NA"))

columnas_jc <- colnames(datos_jc)
columnas_dv <- colnames(variables_utiles)
columnas_comunes <- intersect(columnas_jc, columnas_dv)

variables_comunes <- datos_con_tipo_de_varibles %>% select(all_of(columnas_comunes))

tabla1 <- variables_comunes %>% filter(!is.na(A15B) & A15B != 'Ignorado') %>% select(A15B,np)
tabla1 <- tabla1 %>%
  count(A15B, np) %>%
  pivot_wider(names_from = np, 
              values_from = n, 
              values_fill = 0)
tabla1 <- tabla1 %>% mutate( Total = `Pobreza extrema`+`Pobreza no extrema`+ `No pobre`)
tabla1 <- bind_rows( tabla1, tabla1 %>% summarise( A15B = "Total",
                                                   `Pobreza extrema` = sum(`Pobreza extrema`),
                                                   `Pobreza no extrema` = sum(`Pobreza no extrema`),
                                                   `No pobre` = sum(`No pobre`),
                                                   Total = sum(Total)))
tabla1 <- tabla1 %>%
  mutate(across(where(is.numeric), 
                ~ percent(. /sum(!is.na(variables_comunes$A15B) & variables_comunes$A15B != 'Ignorado'))))



tabla2 <- variables_comunes %>% filter(!is.na(Q_IPCN) & !is.na(A22A)) %>% select(Q_IPCN,A22A)
tabla2 <- tabla2 %>%
  count(Q_IPCN, A22A) %>%
  pivot_wider(names_from = A22A, 
              values_from = n, 
              values_fill = 0)
tabla2 <- tabla2 %>% mutate( Total = `Sí`+`No` )
tabla2 <- bind_rows( tabla2, tabla2 %>% summarise( Q_IPCN = "Total",
                                                   `Sí` = sum(`Sí`),
                                                   `No` = sum(`No`),
                                                   Total = sum(Total)))
tabla2 <- tabla2 %>%
  mutate(across(where(is.numeric), 
                ~ percent(. /sum(!is.na(variables_comunes$Q_IPCN) & !is.na(variables_comunes$A22A)))))
