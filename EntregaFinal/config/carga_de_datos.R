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

# Convertir columnas categóricas a factores y ajustar años de escolaridad
cols_to_factor <- c("ZONA", "A15A", "A15B", 
                    "NivInst", "ForReg", "A16B", "A22A", 
                    "Estabili", "InsLab", "np", "IPM_Pobreza", "Q_IPCN")
ENAHO[cols_to_factor] <- lapply(ENAHO[cols_to_factor], as_factor)
ENAHO$Escolari <- ifelse(ENAHO$Escolari == 99, NA, as.integer(ENAHO$Escolari))

#filtrar variables de interés
ENAHO <- ENAHO %>% select(ZONA,
                          A5,
                          A15A, A15B,
                          NivInst, Escolari,
                          ForReg, A16B,
                          A22A,
                          Estabili, InsLab,
                          itpn, lp, 
                          np, IPM_Pobreza,
                          IPM_Intensidad, Q_IPCN,
                          ipcn) %>% 
  filter(A5 >= 18 & A5 <= 60) #filtrar personas entre 18 y 60 años

saveRDS(ENAHO, file = "./data/ENAHO.rds")


