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
library(haven) 
library(scales)
library(ggridges) # Para usar geom_density_ridges
library(sf) 
library(RColorBrewer)
library(see)
library(GGally)
library(kableExtra)

#Codigo para la lectura de la base de datos
ENAHO <- read_sav("data/GPES-ELAB-GEBD-ENAHO-2023_BdBasePublica.sav")

#Codigo para convertir las columnas categóricas a factor
ENAHO$REGION <- as_factor(ENAHO$REGION)
ENAHO$ZONA <- as_factor(ENAHO$ZONA)
ENAHO$A3 <- as_factor(ENAHO$A3) #parentesco
ENAHO$A4 <- as_factor(ENAHO$A4) #Sexo
ENAHO$CondMig <- as_factor(ENAHO$CondMig) #condicion de migrante
ENAHO$A15A <- as_factor(ENAHO$A15A) #tipo centro educativo
ENAHO$A15B <- as_factor(ENAHO$A15B) #universidad publica
ENAHO$NivInst <- as_factor(ENAHO$NivInst) #Nivel de instrucción
ENAHO$Escolari <- as.integer(ENAHO$Escolari)
ENAHO$Escolari[ENAHO$Escolari == 99] <- NA #años de escolaridad
ENAHO$ForReg <- as_factor(ENAHO$ForReg) #formacion educativa formal
ENAHO$A16B <- as_factor(ENAHO$A16B) #titulo
ENAHO$ForNoReg <- as_factor(ENAHO$ForNoReg) #formacion educativa no formal
ENAHO$A22A <- as_factor(ENAHO$A22A) #Dominio de segundo idioma
ENAHO$Estabili <- as_factor(ENAHO$Estabili) #Estabilidad laboral
ENAHO$InsLab <- as_factor(ENAHO$InsLab) #Satisfaccion laboral
ENAHO$np <- as_factor(ENAHO$np) #Nivel de pobreza
ENAHO$IPM_Pobreza <- as_factor(ENAHO$IPM_Pobreza) #nivel de pobreza multidimensional
ENAHO$Q_IPCN <- as_factor(ENAHO$Q_IPCN) #Quintil de ingreso per capita neto

#filtrar variables de interés
ENAHO <- ENAHO %>% select(REGION, ZONA,
                          A3, A4, 
                          A5, CondMig,
                          A15A, A15B,
                          NivInst, Escolari,
                          ForReg, A16B,
                          ForNoReg, A22A,
                          Estabili, InsLab,
                          itpn, lp, 
                          np, IPM_Pobreza,
                          IPM_Intensidad, Q_IPCN,
                          ipcn) %>% 
  filter(A5 >= 18 & A5 <= 60) #filtrar personas entre 18 y 60 años


