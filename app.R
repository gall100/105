# ============================================================
# DASHBOARD: MÁXIMOS Y MÍNIMOS - VERSIÓN CORREGIDA
# ============================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(tidyverse)

# Cargar funciones
source("funciones_corregida.R")

# ============================================================
# UI
# ============================================================

ui <- dashboardPage(
  dashboardHeader(title = "Máximos y Mínimos"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Análisis", tabName = "analisis", icon = icon("calculator")),
      menuItem("Gráficas", tabName = "graficas", icon = icon("chart-line")),
      menuItem("Tabla", tabName = "tabla", icon = icon("table")),
      menuItem("Ayuda", tabName = "ayuda", icon = icon("question"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # TAB 1: ANÁLISIS
      tabItem(tabName = "analisis",
        h2("Ingrese una función f(x,y)"),
        
        fluidRow(
          column(6,
            box(
              title = "Entrada de Función",
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              
              textInput("formula",
                label = "f(x,y) = ",
                value = "x^2 + y^2 - 2*x - 4*y + 5",
                width = "100%"
              ),
              
              p("Ejemplos:"),
              p("• x^2 + y^2"),
              p("• x*y"),
              p("• x^3 + y^3 - 3*x*y"),
              
              actionButton("analizar", "Analizar Función", 
                          class = "btn btn-primary btn-lg",
                          style = "width: 100%; margin-top: 15px;")
            )
          ),
          
          column(6,
            box(
              title = "Rango de Gráficas",
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              
              numericInput("x_min", "x mínimo:", value = -3, step = 0.5),
              numericInput("x_max", "x máximo:", value = 3, step = 0.5),
              numericInput("y_min", "y mínimo:", value = -3, step = 0.5),
              numericInput("y_max", "y máximo:", value = 3, step = 0.5)
            )
          )
        ),
        
        br(),
        
        # Derivadas
        fluidRow(
          column(6,
            box(
              title = "Derivadas Parciales",
              solidHeader = TRUE,
              status = "info",
              width = 12,
              
              h4("∂f/∂x ="),
              verbatimTextOutput("output_fx"),
              br(),
              h4("∂f/∂y ="),
              verbatimTextOutput("output_fy")
            )
          ),
          
          column(6,
            box(
              title = "Puntos Críticos",
              solidHeader = TRUE,
              status = "success",
              width = 12,
              
              tableOutput("tabla_puntos_criticos")
            )
          )
        ),
        
        br(),
        
        # Hessiano
        fluidRow(
          column(12,
            box(
              title = "Análisis del Hessiano",
              solidHeader = TRUE,
              status = "warning",
              width = 12,
              
              tableOutput("tabla_hessiano")
            )
          )
        ),
        
        br(),
        
        # Conclusiones
        fluidRow(
          column(12,
            box(
              title = "Conclusiones",
              solidHeader = TRUE,
              status = "success",
              width = 12,
              
              htmlOutput("resumen_conclusiones")
            )
          )
        )
      ),
      
      # TAB 2: GRÁFICAS
      tabItem(tabName = "graficas",
        h2("Visualizaciones"),
        
        fluidRow(
          column(12,
            box(
              title = "Superficie 3D",
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              height = 750,
              
              plotlyOutput("grafica_3d", height = "700px")
            )
          )
        ),
        
        br(),
        
        fluidRow(
          column(12,
            box(
              title = "Curvas de Nivel",
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              height = 750,
              
              plotlyOutput("grafica_contornos", height = "700px")
            )
          )
        )
      ),
      
      # TAB 3: TABLA
      tabItem(tabName = "tabla",
        h2("Tabla de Valores"),
        
        fluidRow(
          column(12,
            box(
              title = "Valores de f(x,y)",
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              
              DTOutput("tabla_valores")
            )
          )
        )
      ),
      
      # TAB 4: AYUDA
      tabItem(tabName = "ayuda",
        h2("Cómo Usar"),
        
        box(
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          title = "Instrucciones",
          
          h4("1. Ingresa la función"),
          p("Escribe una función en dos variables x e y. Usa operadores: +, -, *, /, ^"),
          
          h4("2. Presiona 'Analizar Función'"),
          p("El sistema calculará:"),
          tags$ul(
            tags$li("Derivadas parciales"),
            tags$li("Puntos críticos"),
            tags$li("Matriz Hessiana"),
            tags$li("Clasificación (mínimo, máximo, punto silla)")
          ),
          
          h4("3. Interpreta resultados"),
          p("det(H) > 0 y fxx > 0 → Mínimo local"),
          p("det(H) > 0 y fxx < 0 → Máximo local"),
          p("det(H) < 0 → Punto silla"),
          p("det(H) = 0 → No concluyente")
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # Valores reactivos
  resultados <- reactiveValues(
    formula = NULL,
    derivadas = NULL,
    puntos_criticos = NULL,
    datos_3d = NULL,
    analizado = FALSE
  )
  
  # Evento: Botón Analizar
  observeEvent(input$analizar, {
    
    formula_input <- input$formula
    
    if (formula_input == "") {
      showNotification("Por favor ingrese una función", type = "error")
      return()
    }
    
    tryCatch({
      # Calcular derivadas
      resultados$derivadas <- calcular_derivadas(formula_input)
      
      if (!resultados$derivadas$exito) {
        showNotification(paste("Error:", resultados$derivadas$error), type = "error")
        return()
      }
      
      # Encontrar puntos críticos
      resultados$puntos_criticos <- encontrar_puntos_criticos(formula_input)
      
      # Generar datos 3D
      resultados$datos_3d <- generar_datos_3d(
        formula_input,
        x_min = input$x_min,
        x_max = input$x_max,
        y_min = input$y_min,
        y_max = input$y_max,
        n = 40
      )
      
      resultados$formula <- formula_input
      resultados$analizado <- TRUE
      
      showNotification("Análisis completado", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # OUTPUT: Derivada fx
  output$output_fx <- renderText({
    if (!resultados$analizado) {
      return("(Ingresa una función y presiona Analizar)")
    }
    
    if (!resultados$derivadas$exito) {
      return(paste("Error:", resultados$derivadas$error))
    }
    
    resultados$derivadas$fx
  })
  
  # OUTPUT: Derivada fy
  output$output_fy <- renderText({
    if (!resultados$analizado) {
      return("(Ingresa una función y presiona Analizar)")
    }
    
    if (!resultados$derivadas$exito) {
      return(paste("Error:", resultados$derivadas$error))
    }
    
    resultados$derivadas$fy
  })
  
  # OUTPUT: Tabla puntos críticos
  output$tabla_puntos_criticos <- renderTable({
    if (!resultados$analizado) {
      return(data.frame())
    }
    
    if (!resultados$puntos_criticos$exito) {
      return(data.frame(Mensaje = "No se encontraron puntos críticos"))
    }
    
    puntos <- resultados$puntos_criticos$puntos
    
    if (nrow(puntos) == 0) {
      return(data.frame(Mensaje = "No hay puntos"))
    }
    
    # Evaluar función
    puntos$f_xy <- sapply(1:nrow(puntos), function(i) {
      evaluar_funcion(resultados$formula, puntos$x[i], puntos$y[i])
    })
    
    data.frame(
      x = round(puntos$x, 4),
      y = round(puntos$y, 4),
      f_xy = round(puntos$f_xy, 4)
    )
  })
  
  # OUTPUT: Tabla Hessiano
  output$tabla_hessiano <- renderTable({
    if (!resultados$analizado) {
      return(data.frame())
    }
    
    puntos <- resultados$puntos_criticos$puntos
    
    if (nrow(puntos) == 0) {
      return(data.frame())
    }
    
    tabla_h <- data.frame()
    
    for (i in 1:nrow(puntos)) {
      x_val <- puntos$x[i]
      y_val <- puntos$y[i]
      
      hess <- calcular_hessiano(resultados$formula, x_val, y_val)
      
      if (hess$exito) {
        clasificacion <- clasificar_punto(hess$det_H, hess$fxx)
        
        fila <- data.frame(
          Punto = paste("(", round(x_val, 3), ",", round(y_val, 3), ")"),
          fxx = round(hess$fxx, 4),
          fyy = round(hess$fyy, 4),
          fxy = round(hess$fxy, 4),
          det_H = round(hess$det_H, 4),
          Tipo = clasificacion
        )
        
        tabla_h <- rbind(tabla_h, fila)
      }
    }
    
    tabla_h
  })
  
  # OUTPUT: Conclusiones
  output$resumen_conclusiones <- renderUI({
    if (!resultados$analizado) {
      return(h4("Ingresa una función para ver conclusiones"))
    }
    
    puntos <- resultados$puntos_criticos$puntos
    
    if (nrow(puntos) == 0) {
      return(h4("No se encontraron puntos críticos"))
    }
    
    html_text <- "<p><strong>Puntos encontrados:</strong></p>"
    
    for (i in 1:nrow(puntos)) {
      x_val <- puntos$x[i]
      y_val <- puntos$y[i]
      f_val <- evaluar_funcion(resultados$formula, x_val, y_val)
      hess <- calcular_hessiano(resultados$formula, x_val, y_val)
      
      if (hess$exito) {
        clasificacion <- clasificar_punto(hess$det_H, hess$fxx)
        html_text <- paste(html_text, 
          "<p style='color: green; font-weight: bold;'>",
          clasificacion, 
          " en (", round(x_val, 3), ",", round(y_val, 3), ") = ",
          round(f_val, 3),
          "</p>", sep = "")
      }
    }
    
    HTML(html_text)
  })
  
  # OUTPUT: Gráfica 3D
  output$grafica_3d <- renderPlotly({
    if (!resultados$analizado || !resultados$datos_3d$exito) {
      return(plotly_empty())
    }
    
    datos <- resultados$datos_3d$datos
    
    x_unique <- sort(unique(datos$x))
    y_unique <- sort(unique(datos$y))
    
    z_matrix <- matrix(datos$z, nrow = length(x_unique), byrow = FALSE)
    
    p <- plot_ly(x = x_unique, y = y_unique, z = z_matrix,
                 type = "surface", colorscale = "Viridis") %>%
      layout(title = "Superficie 3D",
             scene = list(
               xaxis = list(title = "x"),
               yaxis = list(title = "y"),
               zaxis = list(title = "f(x,y)")
             ))
    
    # Añadir puntos críticos
    if (resultados$puntos_criticos$exito) {
      puntos <- resultados$puntos_criticos$puntos
      for (i in 1:nrow(puntos)) {
        z_p <- evaluar_funcion(resultados$formula, puntos$x[i], puntos$y[i])
        p <- p %>% add_trace(x = puntos$x[i], y = puntos$y[i], z = z_p,
                             type = "scatter3d", mode = "markers",
                             marker = list(size = 8, color = "red"),
                             showlegend = (i == 1), name = "Crítico")
      }
    }
    
    p
  })
  
  # OUTPUT: Contornos
  output$grafica_contornos <- renderPlotly({
    if (!resultados$analizado || !resultados$datos_3d$exito) {
      return(plotly_empty())
    }
    
    datos <- resultados$datos_3d$datos
    
    x_unique <- sort(unique(datos$x))
    y_unique <- sort(unique(datos$y))
    
    z_matrix <- matrix(datos$z, nrow = length(x_unique), byrow = FALSE)
    
    p <- plot_ly(x = x_unique, y = y_unique, z = z_matrix,
                 type = "contour", colorscale = "Viridis") %>%
      layout(title = "Curvas de Nivel",
             xaxis = list(title = "x"),
             yaxis = list(title = "y"))
    
    # Añadir puntos críticos
    if (resultados$puntos_criticos$exito) {
      puntos <- resultados$puntos_criticos$puntos
      p <- p %>% add_trace(x = puntos$x, y = puntos$y, type = "scatter",
                           mode = "markers",
                           marker = list(size = 10, color = "red"),
                           name = "Críticos")
    }
    
    p
  })
  
  # OUTPUT: Tabla valores
  output$tabla_valores <- renderDT({
    if (!resultados$analizado) {
      return(datatable(data.frame()))
    }
    
    tabla <- generar_tabla_valores(
      resultados$formula,
      x_min = input$x_min,
      x_max = input$x_max,
      y_min = input$y_min,
      y_max = input$y_max,
      n = 15
    )
    
    datatable(tabla, options = list(pageLength = 20))
  })
}

# ============================================================
# EJECUTAR
# ============================================================

shinyApp(ui, server)
