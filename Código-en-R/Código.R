library(tidyverse)
library(haven)
library(ggridges)
library(ggrepel)
library(ggpp)



#Codigo para la lectura de la base de datos
ENAHO <- read_sav("Código-en-R/ENAHO2023.sav")

# Convertir columnas categóricas a factores y ajustar años de escolaridad
cols_to_factor <- c("ZONA", "A15A", 
                    "NivInst", "A16B", "A22A", 
                    "np", "Q_IPCN")
ENAHO[cols_to_factor] <- lapply(ENAHO[cols_to_factor], as_factor)
ENAHO$Escolari <- ifelse(ENAHO$Escolari == 99, NA, as.integer(ENAHO$Escolari))

#filtrar variables de interés
ENAHO <- ENAHO %>% select(ZONA,
                          A5,
                          A15A,
                          NivInst,
                          A16B,
                          itpn,
                          np,
                          IPM_Intensidad, 
                          Q_IPCN
) %>% 
  filter(A5 >= 18 & A5 <= 60) #filtrar personas entre 18 y 60 años

#Graficos utilizados

grafico1 <- as.data.frame(table(ENAHO$np, ENAHO$A15A)) %>% 
  filter(Var2 != "Ignorado") %>% 
  ggplot(aes(x = Var2, y = Freq, fill = Var1)) +
  geom_col(position = "fill", alpha = 0.8, color = "white", size = 0.3) +  
  labs(
    x = NULL,
    y = NULL,
    fill = "Nivel de pobreza",
  ) +
  scale_fill_viridis_d(option = "F") +  # Paleta de colores viridis
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.justification = "left",  # Alinear la leyenda a la izquierda
    legend.box.margin = margin(l = -200, r = 0, t = 0, b = 0),  # Ajustar margen izquierdo para mover la caja
    legend.title = element_text(size = 40, face = "bold", hjust = 0),
    legend.text = element_text(size = 40),
    axis.title.y = element_text(size = 40, margin = margin(t = 10)),
    axis.text.y = element_text(size = 40, color = "gray20", face = "bold"),
    axis.text.x = element_text(size = 40, color = "gray20"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  guides(
    fill = guide_legend(ncol = 2)  # Dividir la leyenda en 2 columnas
  )

ggsave("EntregaFinal/graphs/Grafico1.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#####################################################

grafico6 <- as.data.frame(table(ENAHO$np, fct_collapse(ENAHO$NivInst, 
                                                       "Primaria completa" = c("Primaria completa", "Secundaria académica incompleta", "Secundaria técnica incompleta"),
                                                       "Secundaria completa" = c("Secundaria académica completa", "Secundaria técnica completa"),
                                                       "Sin o poco nivel de instrucción" = c("Sin nivel de instrucción", "Primaria incompleta")))) %>% 
  rename(Nivel_de_pobreza = Var1) %>% 
  filter(Var2 != "Ignorado") %>% 
  rename(NivInst = Var2) %>% 
  ggplot(aes(x = NivInst, y = Freq, fill = Nivel_de_pobreza)) + 
  geom_col(position = "fill", alpha = 0.8, color = "white", size = 0.3) +  # Ajustes visuales
  labs(
    x = NULL,
    y = NULL,
    fill = "Nivel de pobreza",
  ) +
  scale_fill_viridis_d(option = "F") +  # Paleta de colores viridis
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.justification = "left",  # Alinear la leyenda a la izquierda
    legend.box.margin = margin(l = -420, r = 0, t = 0, b = 0),  # Ajustar margen izquierdo para mover la caja
    legend.title = element_text(size = 40, face = "bold", hjust = -5),
    legend.text = element_text(size = 40),
    axis.title.y = element_text(size = 40, margin = margin(t = 10)),
    axis.text.y = element_text(size = 40, color = "gray20", face = "bold"),
    axis.text.x = element_text(size = 40, color = "gray20"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )+
  guides(
    fill = guide_legend(ncol = 2)  # Dividir la leyenda en 2 columnas
  )

ggsave("EntregaFinal/graphs/Grafico6.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#####################################################

grafico12 <- as.data.frame(table(ENAHO$np, ENAHO$A16B)) %>% 
  rename(Nivel_de_pobreza = Var1) %>% 
  filter(Var2 != "Ignorado", Var2 != "Profesorado o diplomado universitario", Var2 != "Técnico medio, perito o diplomado no universitario") %>% 
  rename(titulo = Var2) %>% 
  ggplot(aes(x = titulo, y = Freq, fill = Nivel_de_pobreza)) + 
  geom_col(position = "fill", alpha = 0.8, color = "white", size = 0.3) +  # Transparencia y bordes
  labs(x = NULL,
       y = NULL,
       fill = "Nivel de pobreza") +
  scale_fill_viridis_d(option = "F") +  # Paleta de colores viridis
  coord_flip() +
  theme_minimal(base_size = 14) +  # Estilo minimalista
  theme(
    legend.position = "bottom",
    legend.justification = "left",  # Alinear la leyenda a la izquierda
    legend.title = element_text(size = 40, face = "bold", hjust = -5),
    legend.text = element_text(size = 40),
    axis.title.y = element_text(size = 40, margin = margin(t = 10)),
    axis.text.y = element_text(size = 40, color = "gray20", face = "bold"),
    axis.text.x = element_text(size = 40, color = "gray20"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )+
  guides(
    fill = guide_legend(ncol = 2)  # Dividir la leyenda en 2 columnas
  )

ggsave("EntregaFinal/graphs/Grafico12.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#####################################################

tabla_media_ingreso_por_nivinst <- ENAHO %>% 
  filter(NivInst != "Ignorado", itpn != 0) %>% 
  mutate(NivInst = fct_collapse(NivInst,
                                "Primaria completa" = c("Primaria completa", "Secundaria académica incompleta", "Secundaria técnica incompleta"),
                                "Secundaria completa" = c("Secundaria académica completa", "Secundaria técnica completa"),
                                "Sin o poco nivel de instrucción" = c("Sin nivel de instrucción", "Primaria incompleta"))) %>%
  group_by(NivInst) %>%
  dplyr::summarize(
    media_itpn = mean(itpn, na.rm = TRUE)
  )
tabla_media_ingreso_por_nivinst

grafico7 <- ENAHO %>% 
  filter(NivInst != "Ignorado" & itpn > 0) %>%
  mutate(NivInst = fct_collapse(NivInst,
                                "Primaria completa" = c("Primaria completa", "Secundaria académica incompleta", "Secundaria técnica incompleta"),
                                "Secundaria completa" = c("Secundaria académica completa", "Secundaria técnica completa"),
                                "Sin o poco nivel de instrucción" = c("Sin nivel de instrucción", "Primaria incompleta"))) %>%
  ggplot(aes(x = itpn, y = fct_reorder(NivInst, itpn, median), fill = NivInst)) +
  # Mejorar la visualización con densidades y lineas más detalladas
  geom_density_ridges(rel_min_height = 0.01, scale = 1.2, alpha = 0.8, color = "white") + 
  # Etiquetas y títulos mejorados
  geom_label_repel(
    data = tabla_media_ingreso_por_nivinst,
    mapping = aes(
      x = media_itpn,
      y = NivInst,
      label = scales::label_number(accuracy = 0.1)(media_itpn),
    ), position = ggpp::position_nudge_to(x = 7.1), color = "white", size = 12
  ) +
  labs(
    x = "Ingreso Personal Neto (log10)",
    y = NULL
    ) +
  # Ajuste de escala con formato de coma en el eje x
  scale_x_log10(labels = scales::label_number(scale = 0.000001, suffix = "M", big.mark = ","), limits = c(10000, NA)) + 
  # Mejora de la paleta de colores con gradientes suaves
  scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.7) +  # Paleta de colores viridis
  # Temas mejorados con mayor atención a los detalles visuales
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",  # Ocultar leyenda
    axis.title.x = element_text(size = 40, margin = margin(t = 10)),
    axis.text.x = element_text(size = 40, color = "gray20"),
    axis.text.y = element_text(size = 40, color = "gray20", face = "bold"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )

ggsave("EntregaFinal/graphs/Grafico7.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#####################################################
tabla_media_ingreso_por_centro_educativo <- ENAHO %>% 
  filter(A15A != "Ignorado", itpn != 0) %>% 
  group_by(A15A) %>%
  dplyr::summarize(
    media_itpn = mean(itpn, na.rm = TRUE)
  )
tabla_media_ingreso_por_centro_educativo

grafico2 <- ENAHO %>% 
  filter(A15A != "Ignorado" & itpn > 0) %>% 
  ggplot(aes(x = itpn, y = fct_reorder(A15A, itpn, median), fill = A15A)) +
  geom_density_ridges(alpha = 0.8, color = "white", size = 0.3) +  # Ajuste en las curvas de densidad
  geom_label_repel(
    data = tabla_media_ingreso_por_centro_educativo,
    mapping = aes(
      x = media_itpn,
      y = A15A,
      label = scales::label_number(accuracy = 0.1)(media_itpn),
    ), position = ggpp::position_nudge_to(x = 7), color = "white", size = 12
  ) +
  labs(
    x = "Ingreso Personal Neto (log10)",
    y = NULL
  ) +
  scale_x_log10(labels = scales::label_number(scale = 0.000001, suffix = "M", big.mark = ","), limits = c(10000, NA)) + 
  scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.7) +  # Paleta de colores viridis
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    legend.position = "none",  # Ocultar leyenda
    axis.title.x = element_text(size = 40, margin = margin(t = 10)),
    axis.text.x = element_text(size = 40, color = "gray20"),
    axis.text.y = element_text(size = 40, color = "gray20", face = "bold"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )

ggsave("EntregaFinal/graphs/Grafico2.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#####################################################
tabla_media_ingreso_por_titulo <- ENAHO %>% 
  filter(A16B != "Ignorado", itpn != 0, A16B != "Profesorado o diplomado universitario", A16B != "Técnico medio, perito o diplomado no universitario") %>% 
  group_by(A16B) %>%
  dplyr::summarize(
    media_itpn = mean(itpn, na.rm = TRUE)
  )
tabla_media_ingreso_por_titulo

grafico13 <- ENAHO %>% 
  filter(A16B != "Ignorado", itpn != 0, A16B != "Profesorado o diplomado universitario", A16B != "Técnico medio, perito o diplomado no universitario") %>%
  ggplot(aes(x = itpn, y = fct_reorder(A16B, itpn, median), fill = A16B)) +
  # Mejorar la visualización con densidades y lineas más detalladas
  geom_density_ridges(rel_min_height = 0.01, scale = 1.2, alpha = 0.8, color = "white") + 
  # Etiquetas y títulos mejorados
  geom_label_repel(
    data = tabla_media_ingreso_por_titulo,
    mapping = aes(
      x = media_itpn,
      y = A16B,
      label = scales::label_number(accuracy = 0.1)(media_itpn),
    ), position = ggpp::position_nudge_to(x = 7.1), color = "white", size = 12
  ) +
  labs(
    x = "Ingreso Personal Neto (log10)",
    y = NULL
  ) +
  # Ajuste de escala con formato de coma en el eje x
  scale_x_log10(labels = scales::label_number(scale = 0.000001, suffix = "M", big.mark = ","), limits = c(10000, NA)) + 
  scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.7) +  # Paleta de colores viridis
  # Temas mejorados con mayor atención a los detalles visuales
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",  # Ocultar leyenda
    axis.title.x = element_text(size = 40, margin = margin(t = 10)),
    axis.text.x = element_text(size = 40, color = "gray20"),
    axis.text.y = element_text(size = 40, color = "gray20", face = "bold"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )

ggsave("EntregaFinal/graphs/Grafico13.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#####################################################

grafico8 <- as.data.frame(prop.table(table(fct_collapse(ENAHO$NivInst, 
                                                        "Primaria completa" = c("Primaria completa", "Secundaria académica incompleta", "Secundaria técnica incompleta"),
                                                        "Secundaria completa" = c("Secundaria académica completa", "Secundaria técnica completa"),
                                                        "Sin o poco nivel de\ninstrucción" = c("Sin nivel de instrucción", "Primaria incompleta"), 
                                                        "Educación superior de\npregrado y grado" = c("Educación superior de pregrado y grado"),
                                                        "Educación superior de\nposgrado" = c("Educación superior de posgrado")
                                                        ),
                                           ENAHO$Q_IPCN), margin = 2)) %>% 
  filter(Var1 != "Ignorado") %>% 
  mutate(Var2 = str_extract(Var2, "\\d")) %>% 
  mutate(Var2 = parse_number(Var2)) %>% 
  ggplot(aes(x = Var2, y = Freq, fill = fct_relevel(Var1, c("Educación superior de\nposgrado", "Educación superior de\npregrado y grado", "Secundaria completa", "Primaria completa", "Sin o poco nivel de\ninstrucción"
)))) +
  geom_area(alpha = 0.7) +  # Agregar color de borde y ajustar opacidad
  labs(
    x = NULL,
    y = NULL, 
    fill = "Nivel\nde instrucción"
  ) +
  scale_fill_viridis_d(option = "H") +  # Paleta de colores viridis
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    legend.position = "bottom",  # Mover la leyenda a la derecha
    legend.title = element_text(size = 40),
    legend.text = element_text(size = 40),
    axis.text.x = element_text(size = 40, color = "gray20", face = "bold"),
    axis.text.y = element_text(size = 40, color = "gray20"),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )+
  guides(
    fill = guide_legend(ncol = 2)  # Dividir la leyenda en 2 columnas
  )

ggsave("EntregaFinal/graphs/Grafico8.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18, # Tamaño: 11.5 pulgadas de ancho
       height = 10, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

#####################################################
datos_filtrados <- ENAHO %>%
  filter(A16B != "Ignorado",
         A16B != "Técnico medio, perito o diplomado no universitario",
         A16B != "Profesorado o diplomado universitario")

grafico14 <- as.data.frame(prop.table(table(datos_filtrados$A16B, datos_filtrados$Q_IPCN), margin = 2)) %>% 
  filter(Var1 != "Ignorado", Var1 != "Técnico medio, perito o diplomado no universitario", Var1 != "Profesorado o diplomado universitario") %>% 
  mutate(Var2 = str_extract(Var2, "\\d")) %>% 
  mutate(Var2 = parse_number(Var2)) %>% 
  ggplot(aes(x = Var2, y = Freq, fill = fct_reorder(Var1, Freq, median))) +
  geom_area(alpha = 0.8) +  # Transparencia para el área
  labs(
       x = NULL,
       y = NULL,
       fill = "Título"
  ) +
  # Mejora de la paleta de colores con gradientes suaves
  scale_fill_viridis_d(option = "H") +  
  theme_minimal(base_size = 14) +  # Estilo minimalista
  theme(
    legend.position = "bottom",  # Mover la leyenda a la derecha
    legend.title = element_text(size = 40),
    legend.text = element_text(size = 40),
    axis.text.x = element_text(size = 40, color = "gray20", face = "bold"),
    axis.text.y = element_text(size = 40, color = "gray20"),
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
    panel.grid.minor = element_blank()
  )

ggsave("EntregaFinal/graphs/Grafico14.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 14, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
