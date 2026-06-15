# ============================================================
# FUNCIONES AUXILIARES PARA EL DASHBOARD
# ============================================================
# Este archivo contiene todas las funciones necesarias para
# realizar el análisis matemático de máximos y mínimos
# ============================================================

library(Ryacas)
library(tidyverse)

# ============================================================
# FUNCIÓN 1: Calcular derivadas parciales
# ============================================================
# Entrada: formula_str (string con la función)
# Salida: lista con fx, fy y estado

calcular_derivadas <- function(formula_str, x_var = "x", y_var = "y") {
  tryCatch({
    # Convertir string a expresión simbólica
    formula <- as_sym(formula_str)
    
    # Calcular derivadas parciales
    df_dx <- deriv(formula, x_var)
    df_dy <- deriv(formula, y_var)
    
    return(list(
      fx = as.character(df_dx),
      fy = as.character(df_dy),
      exito = TRUE
    ))
  }, error = function(e) {
    return(list(
      fx = "",
      fy = "",
      exito = FALSE,
      error = as.character(e)
    ))
  })
}

# ============================================================
# FUNCIÓN 2: Encontrar puntos críticos
# ============================================================
# Resuelve el sistema: ∂f/∂x = 0, ∂f/∂y = 0

encontrar_puntos_criticos <- function(formula_str) {
  tryCatch({
    formula <- as_sym(formula_str)
    
    # Derivadas parciales
    fx <- deriv(formula, "x")
    fy <- deriv(formula, "y")
    
    # Resolver sistema de ecuaciones
    solucion <- solve(c(fx, fy), c("x", "y"))
    
    if (length(solucion) > 0) {
      # Convertir a dataframe
      puntos <- as.data.frame(do.call(rbind, solucion))
      colnames(puntos) <- c("x", "y")
      
      # Convertir a numérico
      puntos$x <- as.numeric(as.character(puntos$x))
      puntos$y <- as.numeric(as.character(puntos$y))
      
      # Filtrar valores reales (NaN puede ocurrir)
      puntos <- puntos[complete.cases(puntos), ]
      
      if (nrow(puntos) > 0) {
        return(list(
          puntos = puntos,
          exito = TRUE
        ))
      }
    }
    
    return(list(
      puntos = data.frame(),
      exito = FALSE,
      error = "No se encontraron puntos críticos reales"
    ))
    
  }, error = function(e) {
    return(list(
      puntos = data.frame(),
      exito = FALSE,
      error = as.character(e)
    ))
  })
}

# ============================================================
# FUNCIÓN 3: Calcular segundas derivadas (Hessiano)
# ============================================================
# Calcula la matriz Hessiana en un punto específico

calcular_hessiano <- function(formula_str, x_val, y_val) {
  tryCatch({
    formula <- as_sym(formula_str)
    
    # Segundas derivadas
    fxx <- deriv(deriv(formula, "x"), "x")
    fyy <- deriv(deriv(formula, "y"), "y")
    fxy <- deriv(deriv(formula, "x"), "y")
    
    # Evaluar en el punto
    env <- list(x = x_val, y = y_val)
    
    fxx_val <- as.numeric(subs(fxx, env))
    fyy_val <- as.numeric(subs(fyy, env))
    fxy_val <- as.numeric(subs(fxy, env))
    
    # Matriz Hessiana
    H <- matrix(c(fxx_val, fxy_val, fxy_val, fyy_val), nrow = 2)
    
    # Determinante
    det_H <- det(H)
    
    return(list(
      H = H,
      fxx = fxx_val,
      fyy = fyy_val,
      fxy = fxy_val,
      det_H = det_H,
      exito = TRUE
    ))
  }, error = function(e) {
    return(list(
      H = matrix(NA, 2, 2),
      det_H = NA,
      exito = FALSE,
      error = as.character(e)
    ))
  })
}

# ============================================================
# FUNCIÓN 4: Clasificar punto crítico
# ============================================================
# Aplica el criterio del Hessiano para clasificar un punto

clasificar_punto <- function(det_H, fxx) {
  if (is.na(det_H) || is.na(fxx)) {
    return("No determinado")
  }
  
  # Pequeño margen de tolerancia numérica
  tol <- 1e-6
  
  if (det_H > tol) {
    if (fxx > tol) {
      return("MÍNIMO LOCAL")
    } else if (fxx < -tol) {
      return("MÁXIMO LOCAL")
    } else {
      return("NO CONCLUYENTE")
    }
  } else if (det_H < -tol) {
    return("PUNTO SILLA")
  } else {
    return("NO CONCLUYENTE")
  }
}

# ============================================================
# FUNCIÓN 5: Evaluar función en un punto
# ============================================================
# Calcula f(x, y) para valores específicos

evaluar_funcion <- function(formula_str, x_val, y_val) {
  tryCatch({
    formula <- as_sym(formula_str)
    resultado <- as.numeric(subs(formula, list(x = x_val, y = y_val)))
    return(resultado)
  }, error = function(e) {
    return(NA)
  })
}

# ============================================================
# FUNCIÓN 6: Generar datos para gráfica 3D
# ============================================================
# Crea una malla de puntos y evalúa la función

generar_datos_3d <- function(formula_str, x_min = -3, x_max = 3, 
                             y_min = -3, y_max = 3, n = 50) {
  tryCatch({
    # Crear secuencias
    x_seq <- seq(x_min, x_max, length.out = n)
    y_seq <- seq(y_min, y_max, length.out = n)
    
    # Crear malla de coordenadas
    X <- expand.grid(x = x_seq, y = y_seq)
    
    # Evaluar función
    formula <- as_sym(formula_str)
    X$z <- mapply(function(x, y) {
      as.numeric(subs(formula, list(x = x, y = y)))
    }, X$x, X$y)
    
    # Filtrar valores infinitos o NaN
    X <- X[is.finite(X$z), ]
    
    if (nrow(X) == 0) {
      return(list(
        datos = data.frame(),
        exito = FALSE,
        error = "No se pudieron generar datos válidos"
      ))
    }
    
    return(list(
      datos = X,
      exito = TRUE
    ))
  }, error = function(e) {
    return(list(
      datos = data.frame(),
      exito = FALSE,
      error = as.character(e)
    ))
  })
}

# ============================================================
# FUNCIÓN 7: Generar tabla de valores
# ============================================================
# Crea una tabla con múltiples valores de la función

generar_tabla_valores <- function(formula_str, x_min = -2, x_max = 2, 
                                  y_min = -2, y_max = 2, n = 10) {
  tryCatch({
    # Crear secuencias
    x_vals <- seq(x_min, x_max, length.out = n)
    y_vals <- seq(y_min, y_max, length.out = n)
    
    # Crear tabla
    tabla <- expand.grid(x = x_vals, y = y_vals)
    
    # Evaluar función
    formula <- as_sym(formula_str)
    tabla$f_xy <- mapply(function(x, y) {
      as.numeric(subs(formula, list(x = x, y = y)))
    }, tabla$x, tabla$y)
    
    # Filtrar valores válidos
    tabla <- tabla[is.finite(tabla$f_xy), ]
    
    # Ordenar por valor de función
    tabla <- tabla %>% 
      arrange(f_xy) %>%
      mutate(
        x = round(x, 3),
        y = round(y, 3),
        f_xy = round(f_xy, 3)
      ) %>%
      slice_head(n = 20)  # Mostrar solo los primeros 20
    
    return(tabla)
  }, error = function(e) {
    return(data.frame())
  })
}
