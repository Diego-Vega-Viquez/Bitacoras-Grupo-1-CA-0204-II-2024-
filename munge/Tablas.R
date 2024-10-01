
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
library(scales)
library(knitr)

columnas_jc <- colnames(datos_jc)
columnas_dv <- colnames(variables_utiles)
columnas_comunes <- intersect(columnas_jc, columnas_dv)

variables_comunes <- datos_con_tipo_de_varibles %>% select(all_of(columnas_comunes))

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

###############
#   TABLA 1   #
###############

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
tabla1 <- tabla1 %>% rename(Universidad = A15B)


###############
#   TABLA 2   #
###############

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
tabla2 <- tabla2 %>% rename(Quintil = Q_IPCN,
                            'Domina un segundo idioma' = Sí,
                            'No domina un segundo idioma' = No
                            )

###############
#   TABLA 3   #
###############

tabla3 <- variables_comunes %>% filter(NivInst != 'Ignorado' & !is.na(itpn) & itpn != 0) %>% 
                                select(NivInst,itpn) %>% 
                                group_by(NivInst) %>% 
                                summarize(Media = median(itpn, na.rm = TRUE),
                                          Promedio = mean(itpn, na.rm = TRUE),
                                          Desviación = sd(itpn, na.rm = TRUE),
                                          Min = min(itpn, na.rm = TRUE),
                                          Max = max(itpn, na.rm = TRUE)) %>% 
                                rename('Nivel de instrucción' = NivInst)
tabla3 <- tabla3 %>%
  mutate(across(where(is.numeric), 
                ~ number(., big.mark = ".", decimal.mark = ",", accuracy = 0.01, prefix = "₡")))

###############
#   TABLA 4   #
###############


tabla4 <- variables_comunes %>% filter(!is.na(Q_IPCN) & !is.na(ipcn) & ipcn != 0) %>% 
  select(Q_IPCN,ipcn) %>% 
  group_by(Q_IPCN) %>% 
  summarize(Media = median(ipcn, na.rm = TRUE),
            Promedio = mean(ipcn, na.rm = TRUE),
            Desviación = sd(ipcn, na.rm = TRUE),
            Min = min(ipcn, na.rm = TRUE),
            Max = max(ipcn, na.rm = TRUE)) %>% 
  rename('Quintil' = Q_IPCN)
tabla4 <- tabla4 %>%
  mutate(across(where(is.numeric), 
                ~ number(., big.mark = ".", decimal.mark = ",", accuracy = 0.01, prefix = "₡")))

