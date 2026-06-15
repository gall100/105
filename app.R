# ============================================================
# DASHBOARD: MÁXIMOS Y MÍNIMOS DE FUNCIONES DE VARIAS VARIABLES
# ============================================================
# Autor: Sistema Educativo
# Descripción: Aplicación Shiny para análisis interactivo
#              de máximos, mínimos y puntos silla
# ============================================================

library(shiny)
library(plotly)
library(DT)
library(tidyverse)
library(latex2exp)
library(Ryacas)

# Cargar funciones auxiliares
source("funciones.R")

# ============================================================
# INTERFAZ DE USUARIO (UI)
# ============================================================

ui <- fluidPage(
  # CSS personalizado
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #f5f5f5;
      }
      .title-main {
        color: #0275D8;
        text-align: center;
        margin-bottom: 30px;
        font-weight: bold;
        font-size: 36px;
      }
      .sidebar-section {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
        border-left: 4px solid #0275D8;
      }
      .info-box {
        padding: 10px;
        background-color: #e8f4f8;
        border-left: 4px solid #0275D8;
        margin: 10px 0;
        border-radius: 4px;
      }
      .btn-primary {
        background-color: #0275D8;
        border-color: #0275D8;
      }
      .btn-primary:hover {
        background-color: #0263bc;
        border-color: #0263bc;
      }
      .box {
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }
      .result-box {
        padding: 20px;
        background-color: white;
        border-radius: 8px;
        margin: 15px 0;
      }
      .minimo {
        color: #28a745;
        font-weight: bold;
        font-size: 16px;
      }
      .maximo {
        color: #fd7e14;
        font-weight: bold;
        font-size: 16px;
      }
      .silla {
        color: #9b59b6;
        font-weight: bold;
        font-size: 16px;
      }
    "))
  ),
  
  # Título principal
  h1("📊 Dashboard: Máximos y Mínimos", class = "title-main"),
  
  # Layout principal
  sidebarLayout(
    # ============================================================
    # PANEL LATERAL (SIDEBAR)
    # ============================================================
    sidebarPanel(
      width = 3,
      
      # Sección 1: Entrada de función
      div(class = "sidebar-section",
        h3("📝 ENTRADA DE DATOS", style = "color: #0275D8; margin-top: 0;"),
        
        textInput(
          "formula",
          label = "Ingrese la función f(x,y):",
          value = "x^2 + y^2 - 2*x - 4*y + 5",
          placeholder = "Ej: x^2 + y^2 - 2*x - 4*y"
        ),
        
        div(class = "info-box",
          h5("📋 Ejemplos:", style = "margin-top: 0;"),
          p("• x^2 + y^2", style = "font-size: 12px; margin: 5px 0;"),
          p("• x*y - x - y", style = "font-size: 12px; margin: 5px 0;"),
          p("• x^3 + y^3 - 3*x*y", style = "font-size: 12px; margin: 5px 0;"),
          p("• sin(x) + cos(y)", style = "font-size: 12px; margin: 5px 0;"),
          p("• x*y", style = "font-size: 12px; margin: 5px 0;")
        ),
        
        actionButton(
          "analizar",
          "🔍 Analizar Función",
          class = "btn-primary btn-lg",
          style = "width: 100%; margin: 20px 0; font-size: 16px;"
        )
      ),
      
      # Sección 2: Rango de gráficas
      div(class = "sidebar-section",
        h4("📈 Rango de Gráficas", style = "margin-top: 0;"),
        
        fluidRow(
          column(6,
            numericInput("x_min", "x mín:", value = -3, step = 0.5)
          ),
          column(6,
            numericInput("x_max", "x máx:", value = 3, step = 0.5)
          )
        ),
        
        fluidRow(
          column(6,
            numericInput("y_min", "y mín:", value = -3, step = 0.5)
          ),
          column(6,
            numericInput("y_max", "y máx:", value = 3, step = 0.5)
          )
        ),
        
        p("Ajusta el rango para enfocarte en diferentes áreas",
          style = "font-size: 11px; color: gray; margin-top: 10px;")
      ),
      
      # Sección 3: Información
      div(class = "info-box",
        h5("ℹ️ Información", style = "margin-top: 0;"),
        p("Este dashboard calcula automáticamente:",
          style = "font-size: 12px; margin: 0 0 5px 0;"),
        p("✓ Derivadas parciales",
          style = "font-size: 11px; margin: 3px 0;"),
        p("✓ Puntos críticos",
          style = "font-size: 11px; margin: 3px 0;"),
        p("✓ Matriz Hessiana",
          style = "font-size: 11px; margin: 3px 0;"),
        p("✓ Clasificación de puntos",
          style = "font-size: 11px; margin: 3px 0;")
      )
    ),
    
    # ============================================================
    # PANEL PRINCIPAL
    # ============================================================
    mainPanel(
      width = 9,
      
      # Tabs principales
      tabsetPanel(
        
        # TAB 1: ANÁLISIS MATEMÁTICO
        tabPanel(
          "📐 Análisis Matemático",
          icon = icon("calculator"),
          
          br(),
          
          # Fila 1: Derivadas y Puntos Críticos
          fluidRow(
            column(6,
              box(
                title = "Derivadas Parciales",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                
                h5("∂f/∂x =", style = "color: #0275D8; margin-bottom: 10px;"),
                verbatimTextOutput("output_fx"),
                br(),
                h5("∂f/∂y =", style = "color: #0275D8; margin-bottom: 10px;"),
                verbatimTextOutput("output_fy")
              )
            ),
            
            column(6,
              box(
                title = "Puntos Críticos",
                status = "success",
                solidHeader = TRUE,
                width = 12,
                
                tableOutput("tabla_puntos_criticos")
              )
            )
          ),
          
          br(),
          
          # Fila 2: Hessiano
          fluidRow(
            column(12,
              box(
                title = "Análisis del Hessiano (Prueba de la Segunda Derivada)",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                
                tableOutput("tabla_hessiano")
              )
            )
          ),
          
          br(),
          
          # Fila 3: Conclusiones
          fluidRow(
            column(12,
              box(
                title = "✅ Conclusiones y Clasificación",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                
                htmlOutput("resumen_conclusiones"),
                style = "font-size: 16px;"
              )
            )
          )
        ),
        
        # TAB 2: SUPERFICIE 3D
        tabPanel(
          "🎨 Superficie 3D",
          icon = icon("cube"),
          
          br(),
          
          box(
            title = "Visualización de la Función en Tres Dimensiones",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            plotlyOutput("grafica_3d", height = "700px"),
            
            p(
              "💡 Interactividad: Usa el ratón para rotar, rueda del mouse para zoom, clic derecho para desplazar. Los puntos rojos marcan los puntos críticos.",
              style = "color: #0275D8; font-size: 13px; margin-top: 15px; font-style: italic;"
            )
          )
        ),
        
        # TAB 3: CURVAS DE NIVEL
        tabPanel(
          "🗺️ Curvas de Nivel",
          icon = icon("mountain"),
          
          br(),
          
          box(
            title = "Contornos de la Función (Líneas de Nivel)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            plotlyOutput("grafica_contornos", height = "700px"),
            
            p(
              "Las líneas representan puntos donde la función tiene el mismo valor. Los puntos rojos marcan los puntos críticos.",
              style = "color: #0275D8; font-size: 13px; margin-top: 15px; font-style: italic;"
            )
          )
        ),
        
        # TAB 4: TABLA DE VALORES
        tabPanel(
          "📊 Tabla de Valores",
          icon = icon("table"),
          
          br(),
          
          box(
            title = "Valores de la Función (Ordenados de Menor a Mayor)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            DTOutput("tabla_valores")
          ),
          
          br(),
          
          box(
            title = "Información de la Tabla",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            p(
              "La tabla muestra una muestra de valores de la función en diferentes puntos del dominio especificado.",
              style = "font-size: 13px; margin: 0;"
            ),
            p(
              "El color de fondo refleja la magnitud del valor (azul = bajo, rojo = alto).",
              style = "font-size: 13px;"
            )
          )
        ),
        
        # TAB 5: AYUDA
        tabPanel(
          "❓ Ayuda",
          icon = icon("question-circle"),
          
          br(),
          
          box(
            title = "Cómo Usar Este Dashboard",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            h4("1. Ingresa la Función"),
            p("Escribe tu función en el campo 'Ingrese la función f(x,y)'.
              Usa variables 'x' e 'y', y operadores como +, -, *, /, ^."),
            
            h4("2. Presiona 'Analizar Función'"),
            p("El sistema calculará automáticamente:"),
            tags$ul(
              tags$li("Derivadas parciales ∂f/∂x y ∂f/∂y"),
              tags$li("Puntos críticos (donde ∂f/∂x = 0 y ∂f/∂y = 0)"),
              tags$li("Matriz Hessiana (segundas derivadas)"),
              tags$li("Determinante del Hessiano"),
              tags$li("Clasificación de cada punto")
            ),
            
            h4("3. Interpreta los Resultados"),
            tags$ul(
              tags$li("MÍNIMO LOCAL: det(H) > 0 y fₓₓ > 0"),
              tags$li("MÁXIMO LOCAL: det(H) > 0 y fₓₓ < 0"),
              tags$li("PUNTO SILLA: det(H) < 0"),
              tags$li("NO CONCLUYENTE: det(H) = 0")
            ),
            
            h4("4. Visualiza con las Gráficas"),
            tags$ul(
              tags$li("Superficie 3D: Visualiza la función en tres dimensiones"),
              tags$li("Curvas de Nivel: Ve las líneas de contorno"),
              tags$li("Tabla de Valores: Explora valores numéricos")
            )
          )
        )
      )
    )
  )
)

# ============================================================
# LÓGICA DEL SERVIDOR
# ============================================================

server <- function(input, output, session) {
  
  # Valores reactivos para almacenar resultados
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
    
    # Validar entrada
    if (formula_input == "") {
      showNotification("⚠️ Por favor ingrese una función", type = "error", duration = 3)
      return()
    }
    
    # Mostrar notificación de carga
    showNotification("⏳ Analizando función...", type = "message", duration = 2)
    
    # Calcular derivadas
    resultados$derivadas <- calcular_derivadas(formula_input)
    
    if (!resultados$derivadas$exito) {
      showNotification(paste("❌ Error:", resultados$derivadas$error), type = "error", duration = 5)
      return()
    }
    
    # Encontrar puntos críticos
    resultados$puntos_criticos <- encontrar_puntos_criticos(formula_input)
    
    # Generar datos para gráfica 3D
    resultados$datos_3d <- generar_datos_3d(
      formula_input,
      x_min = input$x_min,
      x_max = input$x_max,
      y_min = input$y_min,
      y_max = input$y_max,
      n = 50
    )
    
    resultados$formula <- formula_input
    resultados$analizado <- TRUE
    
    showNotification("✅ Análisis completado exitosamente", type = "message", duration = 3)
  })
  
  # ============================================================
  # OUTPUT 1: Derivada parcial ∂f/∂x
  # ============================================================
  output$output_fx <- renderText({
    if (!resultados$analizado) {
      return("(Presiona '🔍 Analizar Función' para ver resultados)")
    }
    
    if (!resultados$derivadas$exito) {
      return(paste("Error:", resultados$derivadas$error))
    }
    
    resultados$derivadas$fx
  })
  
  # ============================================================
  # OUTPUT 2: Derivada parcial ∂f/∂y
  # ============================================================
  output$output_fy <- renderText({
    if (!resultados$analizado) {
      return("(Presiona '🔍 Analizar Función' para ver resultados)")
    }
    
    if (!resultados$derivadas$exito) {
      return(paste("Error:", resultados$derivadas$error))
    }
    
    resultados$derivadas$fy
  })
  
  # ============================================================
  # OUTPUT 3: Tabla de Puntos Críticos
  # ============================================================
  output$tabla_puntos_criticos <- renderTable({
    if (!resultados$analizado) {
      return(data.frame(Mensaje = "No hay datos"))
    }
    
    if (!resultados$puntos_criticos$exito) {
      return(data.frame(Error = resultados$puntos_criticos$error))
    }
    
    puntos <- resultados$puntos_criticos$puntos
    
    if (nrow(puntos) == 0) {
      return(data.frame(Mensaje = "No se encontraron puntos críticos"))
    }
    
    # Evaluar función en cada punto crítico
    puntos$f_xy <- mapply(function(x, y) {
      evaluar_funcion(resultados$formula, x, y)
    }, puntos$x, puntos$y)
    
    puntos_tabla <- puntos %>% 
      mutate(
        x = round(x, 4),
        y = round(y, 4),
        f_xy = round(f_xy, 4)
      ) %>%
      rename(
        "x" = x,
        "y" = y,
        "f(x,y)" = f_xy
      )
    
    puntos_tabla
  }, rownames = TRUE)
  
  # ============================================================
  # OUTPUT 4: Tabla del Hessiano
  # ============================================================
  output$tabla_hessiano <- renderTable({
    if (!resultados$analizado) {
      return(data.frame(Mensaje = "No hay datos"))
    }
    
    puntos <- resultados$puntos_criticos$puntos
    
    if (nrow(puntos) == 0) {
      return(data.frame(Mensaje = "No hay puntos críticos para analizar"))
    }
    
    tabla_h <- data.frame()
    
    for (i in 1:nrow(puntos)) {
      x_val <- puntos$x[i]
      y_val <- puntos$y[i]
      
      hess <- calcular_hessiano(resultados$formula, x_val, y_val)
      
      if (hess$exito) {
        clasificacion <- clasificar_punto(hess$det_H, hess$fxx)
        
        fila <- data.frame(
          "Punto" = paste("(", round(x_val, 3), ",", round(y_val, 3), ")"),
          "fₓₓ" = round(hess$fxx, 4),
          "f_yy" = round(hess$fyy, 4),
          "fₓᵧ" = round(hess$fxy, 4),
          "det(H)" = round(hess$det_H, 4),
          "Clasificación" = clasificacion,
          check.names = FALSE
        )
        
        tabla_h <- rbind(tabla_h, fila)
      }
    }
    
    tabla_h
  }, rownames = TRUE)
  
  # ============================================================
  # OUTPUT 5: Conclusiones
  # ============================================================
  output$resumen_conclusiones <- renderUI({
    if (!resultados$analizado) {
      return(h4("Presiona '🔍 Analizar Función' para ver conclusiones"))
    }
    
    puntos <- resultados$puntos_criticos$puntos
    
    if (nrow(puntos) == 0) {
      return(h4("No se encontraron puntos críticos para esta función"))
    }
    
    # Clasificar puntos
    minimos <- data.frame()
    maximos <- data.frame()
    sillas <- data.frame()
    
    for (i in 1:nrow(puntos)) {
      x_val <- puntos$x[i]
      y_val <- puntos$y[i]
      f_val <- evaluar_funcion(resultados$formula, x_val, y_val)
      
      hess <- calcular_hessiano(resultados$formula, x_val, y_val)
      
      if (hess$exito) {
        clasificacion <- clasificar_punto(hess$det_H, hess$fxx)
        
        punto_info <- data.frame(
          x = round(x_val, 3),
          y = round(y_val, 3),
          f = round(f_val, 3)
        )
        
        if (clasificacion == "MÍNIMO LOCAL") {
          minimos <- rbind(minimos, punto_info)
        } else if (clasificacion == "MÁXIMO LOCAL") {
          maximos <- rbind(maximos, punto_info)
        } else if (clasificacion == "PUNTO SILLA") {
          sillas <- rbind(sillas, punto_info)
        }
      }
    }
    
    # Construir HTML de resultados
    html_content <- ""
    
    if (nrow(minimos) > 0) {
      html_content <- paste(html_content, 
        "<p class='minimo'>✓ MÍNIMOS LOCALES:</p>",
        paste(sapply(1:nrow(minimos), function(i) {
          paste("<p style='margin-left: 20px;'>
                 Punto (", minimos$x[i], ",", minimos$y[i], ") 
                 con valor f(x,y) = <strong>", minimos$f[i], "</strong>
                 </p>")
        }), collapse = ""),
        sep = "")
    }
    
    if (nrow(maximos) > 0) {
      html_content <- paste(html_content,
        "<p class='maximo'>✓ MÁXIMOS LOCALES:</p>",
        paste(sapply(1:nrow(maximos), function(i) {
          paste("<p style='margin-left: 20px;'>
                 Punto (", maximos$x[i], ",", maximos$y[i], ") 
                 con valor f(x,y) = <strong>", maximos$f[i], "</strong>
                 </p>")
        }), collapse = ""),
        sep = "")
    }
    
    if (nrow(sillas) > 0) {
      html_content <- paste(html_content,
        "<p class='silla'>✓ PUNTOS SILLA:</p>",
        paste(sapply(1:nrow(sillas), function(i) {
          paste("<p style='margin-left: 20px;'>
                 Punto (", sillas$x[i], ",", sillas$y[i], ") 
                 con valor f(x,y) = <strong>", sillas$f[i], "</strong>
                 </p>")
        }), collapse = ""),
        sep = "")
    }
    
    if (html_content == "") {
      html_content <- "<p>No se encontraron puntos críticos clasificables.</p>"
    }
    
    HTML(html_content)
  })
  
  # ============================================================
  # OUTPUT 6: Gráfica 3D
  # ============================================================
  output$grafica_3d <- renderPlotly({
    if (!resultados$analizado || is.null(resultados$datos_3d) || !resultados$datos_3d$exito) {
      return(plotly_empty() %>% 
        layout(title = "Ingresa una función válida y presiona 'Analizar'"))
    }
    
    datos <- resultados$datos_3d$datos
    
    # Convertir a matriz
    x_unique <- sort(unique(datos$x))
    y_unique <- sort(unique(datos$y))
    
    z_matrix <- matrix(
      datos$z,
      nrow = length(x_unique),
      ncol = length(y_unique),
      byrow = FALSE
    )
    
    # Crear gráfica 3D
    p <- plot_ly(
      x = x_unique,
      y = y_unique,
      z = z_matrix,
      type = "surface",
      colorscale = "Viridis",
      showscale = TRUE
    ) %>%
      layout(
        title = list(text = "Superficie de la Función f(x,y)", x = 0.5),
        scene = list(
          xaxis = list(title = "x"),
          yaxis = list(title = "y"),
          zaxis = list(title = "f(x,y)")
        ),
        width = 900,
        height = 700
      )
    
    # Añadir puntos críticos
    if (!is.null(resultados$puntos_criticos) && 
        resultados$puntos_criticos$exito && 
        nrow(resultados$puntos_criticos$puntos) > 0) {
      
      puntos <- resultados$puntos_criticos$puntos
      
      for (i in 1:nrow(puntos)) {
        x_p <- puntos$x[i]
        y_p <- puntos$y[i]
        z_p <- evaluar_funcion(resultados$formula, x_p, y_p)
        
        p <- p %>% add_trace(
          x = c(x_p),
          y = c(y_p),
          z = c(z_p),
          mode = 'markers',
          type = 'scatter3d',
          marker = list(size = 8, color = 'red'),
          name = "Punto Crítico",
          showlegend = (i == 1)
        )
      }
    }
    
    p
  })
  
  # ============================================================
  # OUTPUT 7: Gráfica de Contornos
  # ============================================================
  output$grafica_contornos <- renderPlotly({
    if (!resultados$analizado || is.null(resultados$datos_3d) || !resultados$datos_3d$exito) {
      return(plotly_empty() %>% 
        layout(title = "Ingresa una función válida y presiona 'Analizar'"))
    }
    
    datos <- resultados$datos_3d$datos
    
    x_unique <- sort(unique(datos$x))
    y_unique <- sort(unique(datos$y))
    
    z_matrix <- matrix(
      datos$z,
      nrow = length(x_unique),
      ncol = length(y_unique),
      byrow = FALSE
    )
    
    p <- plot_ly(
      x = x_unique,
      y = y_unique,
      z = z_matrix,
      type = "contour",
      colorscale = "Viridis",
      contours = list(
        showlabels = TRUE,
        labelfont = list(size = 12)
      )
    ) %>%
      layout(
        title = list(text = "Curvas de Nivel de f(x,y)", x = 0.5),
        xaxis = list(title = "x"),
        yaxis = list(title = "y"),
        width = 900,
        height = 700
      )
    
    # Añadir puntos críticos
    if (!is.null(resultados$puntos_criticos) && 
        resultados$puntos_criticos$exito &&
        nrow(resultados$puntos_criticos$puntos) > 0) {
      
      puntos <- resultados$puntos_criticos$puntos
      
      p <- p %>% add_trace(
        x = puntos$x,
        y = puntos$y,
        mode = 'markers',
        type = 'scatter',
        marker = list(size = 10, color = 'red'),
        name = "Puntos Críticos",
        showlegend = TRUE
      )
    }
    
    p
  })
  
  # ============================================================
  # OUTPUT 8: Tabla de Valores
  # ============================================================
  output$tabla_valores <- renderDT({
    if (!resultados$analizado) {
      return(datatable(
        data.frame(Mensaje = "Presiona '🔍 Analizar Función'"),
        options = list(dom = 't')
      ))
    }
    
    tabla <- generar_tabla_valores(
      resultados$formula,
      x_min = input$x_min,
      x_max = input$x_max,
      y_min = input$y_min,
      y_max = input$y_max,
      n = 20
    )
    
    if (nrow(tabla) == 0) {
      return(datatable(
        data.frame(Mensaje = "No se pudo generar tabla"),
        options = list(dom = 't')
      ))
    }
    
    datatable(
      tabla,
      options = list(
        pageLength = 15,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
        dom = 'lfrtip'
      ),
      colnames = c("x", "y", "f(x,y)")
    ) %>%
      formatStyle(
        'f_xy',
        background = styleColorBar(tabla$f_xy, 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
}

# ============================================================
# EJECUTAR LA APLICACIÓN
# ============================================================

shinyApp(ui, server)
