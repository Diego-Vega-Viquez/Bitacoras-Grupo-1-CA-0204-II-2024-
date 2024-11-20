# Librerías
library(shiny)
library(tidyverse)
library(haven)

# Cargar y procesar datos
ENAHO <- read_sav("base_enaho.sav")
cols_to_factor <- c("ZONA", "A15A", "A15B", "NivInst", "A16B", "A22A", "np", "Q_IPCN", "IPM_Pobreza")
ENAHO[cols_to_factor] <- lapply(ENAHO[cols_to_factor], as_factor)
ENAHO$Escolari <- ifelse(ENAHO$Escolari == 99, NA, as.integer(ENAHO$Escolari))

ENAHO <- ENAHO %>% 
  select(ZONA, A5, A15A, A15B, NivInst, A16B, A22A, itpn, Q_IPCN, np) %>%
  filter(A5 >= 18 & A5 <= 60) %>%
  rename(
    centro_educativo = A15A,
    zona = ZONA,
    edad = A5,
    universidad = A15B,
    nivel_de_instruccion = NivInst,
    titulacion = A16B,
    dominio_de_un_segundo_idioma = A22A,
    nivel_de_pobreza = np
  ) %>%
  mutate(nivel_de_instruccion = fct_collapse(
    nivel_de_instruccion,
    "Primaria completa" = c("Primaria completa", "Secundaria académica incompleta", "Secundaria técnica incompleta"),
    "Secundaria completa" = c("Secundaria académica completa", "Secundaria técnica completa"),
    "Sin o poco nivel de instrucción" = c("Sin nivel de instrucción", "Primaria incompleta")
  ))

ENAHO$Q_IPCN <- factor(ENAHO$Q_IPCN, 
                       levels = c("Q1: 110683 ó menos", 
                                  "Q2: Más de 110683 a 195000", 
                                  "Q3: Más de 195000 a 321523", 
                                  "Q4: Más de 321523 a 574085", 
                                  "Q5: Más de 574085"), 
                       ordered = TRUE)

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Impacto de la educación en el ingreso"),
  
  # Primera fila: Selección de variables y primer gráfico
  fluidRow(
    column(4,
           selectInput("variable", "Variable", setdiff(names(ENAHO), c("zona", "itpn", "Q_IPCN", "edad", "nivel_de_pobreza"))),
           uiOutput("categorias_ui"),
           checkboxInput("facet", "Facetas por zona", value = FALSE)
    ),
    column(8, align = "center", plotOutput("plot"))
  ),
  
  # Segunda fila: Tabla de datos (mostrada condicionalmente)
  fluidRow(
    column(12, align = "center",
           checkboxInput("show_table", "Mostrar tabla de datos", value = FALSE),
           conditionalPanel(
             condition = "input.show_table == true",
             tableOutput("data")
           )
    )
  ),
  
  # Tercera fila: Selección de quintiles y gráfico de barras
  fluidRow(
    column(4,
           checkboxGroupInput("quintiles", 
                              "Seleccione los quintiles de ingreso per cápita",
                              choices = levels(ENAHO$Q_IPCN),
                              selected = levels(ENAHO$Q_IPCN))
    ),
    column(8, align = "center", plotOutput("bar_plot"))
  ),
  
  # Cuarta fila: Selección de variable y gráfico de barras de nivel de pobreza
  fluidRow(
    column(4,
           selectInput("variable_pobreza", "Variable para nivel de pobreza", 
                       choices = setdiff(names(ENAHO), c("zona", "itpn", "Q_IPCN", "edad", "nivel_de_pobreza"))),
           checkboxInput("flip_coord", "Cambiar orientación", value = FALSE),
           selectInput("color_palette", "Seleccionar paleta de colores", 
                       choices = c("C", "D", "A", "B", "E"), selected = "C")
    ),
    column(8, align = "center", plotOutput("pobreza_plot"))
  )
)

# Lógica del servidor
server <- function(input, output) {
  output$categorias_ui <- renderUI({
    req(input$variable)
    selected_variable <- ENAHO[[input$variable]]
    categories <- setdiff(levels(selected_variable), "Ignorado")
    
    checkboxGroupInput("categorias", 
                       "Elija categorías", 
                       choices = categories, 
                       selected = categories)
  })
  
  output$plot <- renderPlot({
    req(input$variable, input$categorias)
    filtered_data <- ENAHO %>% 
      filter(get(input$variable) %in% input$categorias, get(input$variable) != "Ignorado")
    
    p <- ggplot(filtered_data, aes_string(x = "itpn", fill = input$variable)) +
      geom_density(alpha = 0.7, size = 0, color = "white") +
      scale_x_log10(labels = scales::comma) +
      labs(
        title = paste("Distribución del ingreso por ", input$variable),
        x = "Ingreso Personal Neto (log10)",
        y = "Densidad",
        caption = "Fuente: Encuesta Nacional de Hogares 2023"
      ) +
      scale_fill_viridis_d(option = "C") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, color = "gray30"),
        plot.caption = element_text(size = 10, color = "gray60", hjust = 1),
        legend.position = "bottom",
        panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
        panel.grid.minor = element_blank()
      )
    
    if (input$facet) {
      p <- p + facet_wrap(~zona)
    }
    
    p
  })
  
  output$data <- renderTable({
    req(input$variable, input$categorias)
    filtered_data <- ENAHO %>% 
      filter(get(input$variable) %in% input$categorias, get(input$variable) != "Ignorado")
    
    summary_stats <- filtered_data %>%
      group_by_at(input$variable) %>%
      summarise(
        Media = mean(itpn, na.rm = TRUE),
        Desviacion = sd(itpn, na.rm = TRUE),
        Cuantil_25 = quantile(itpn, 0.25, na.rm = TRUE),
        Mediana = median(itpn, na.rm = TRUE),
        Cuantil_75 = quantile(itpn, 0.75, na.rm = TRUE),
        Minimo = min(itpn, na.rm = TRUE),
        Maximo = max(itpn, na.rm = TRUE)
      )
    
    summary_stats
  })
  
  output$bar_plot <- renderPlot({
    req(input$quintiles, input$variable)
    filtered_data <- ENAHO %>% 
      filter(Q_IPCN %in% input$quintiles, 
             get(input$variable) %in% input$categorias,
             get(input$variable) != "Ignorado")
    
    ggplot(filtered_data, aes(x = factor(Q_IPCN), fill = get(input$variable))) +
      geom_bar(position = "fill", stat = "count", width = 1, alpha = 0.8) +
      labs(
        title = paste("Distribución de", input$variable, "por Quintil de Ingreso"),
        x = "Quintil de Ingreso Per Cápita",
        y = "Frecuencia",
        fill = input$variable,
        caption = "Fuente: Encuesta Nacional de Hogares 2023"
      ) +
      scale_fill_viridis_d(option = "C", direction = -1) +
      theme_minimal(base_size = 14) + 
      theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, color = "gray30"),
        plot.caption = element_text(size = 10, color = "gray60", hjust = 1),
        legend.position = "top",
        panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
        panel.grid.minor = element_blank()
      )
  })
  
  # Gráfico de barras para nivel de pobreza en la cuarta fila
  output$pobreza_plot <- renderPlot({
    req(input$variable_pobreza)
    
    filtered_data_pobreza <- ENAHO %>%
      filter(!is.na(nivel_de_pobreza), 
             !is.na(get(input$variable_pobreza)),
             get(input$variable_pobreza) != "Ignorado")
    
    p <- ggplot(filtered_data_pobreza, aes(fill = factor(nivel_de_pobreza), x = get(input$variable_pobreza))) +
      geom_bar(position = "fill", alpha = 0.8) +
      labs(
        title = paste("Distribución del Nivel de Pobreza según", input$variable_pobreza),
        x = "Nivel de Pobreza",
        y = "Frecuencia Relativa",
        fill = input$variable_pobreza,
        caption = "Fuente: Encuesta Nacional de Hogares 2023"
      ) +
      scale_fill_viridis_d(option = input$color_palette, direction = -1) +
      theme_minimal(base_size = 14) + 
      theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, color = "gray30"),
        plot.caption = element_text(size = 10, color = "gray60", hjust = 1),
        legend.position = "top",
        panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray80"),
        panel.grid.minor = element_blank()
      )
    
    # Aplicar coord_flip si se selecciona
    if (input$flip_coord) {
      p <- p + coord_flip()
    }
    
    p
  })
}

# Correr la aplicación
shinyApp(ui = ui, server = server)
