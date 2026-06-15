# 📊 Dashboard: Máximos y Mínimos de Funciones de Varias Variables

Una aplicación interactiva en **RStudio** y **Shiny** para analizar máximos, mínimos y puntos silla de funciones de dos variables.

## 🎯 Características Principales

✅ **Cálculo Automático de Derivadas Parciales**
- Calcula ∂f/∂x y ∂f/∂y automáticamente
- Usa cálculo simbólico (Ryacas)

✅ **Búsqueda de Puntos Críticos**
- Resuelve el sistema: ∂f/∂x = 0, ∂f/∂y = 0
- Encuentra todos los puntos críticos reales

✅ **Prueba del Hessiano**
- Calcula la matriz Hessiana
- Clasificación automática

✅ **Visualización Interactiva**
- Gráfica 3D de la superficie
- Curvas de nivel (contornos)
- Tabla de valores numéricos

✅ **Interfaz Amigable**
- Dise no responsive
- Múltiples tabs para navegación
- Validación automática de errores

## 🚀 Instalación Rápida

### Requisitos
- R 4.0 o superior
- RStudio (recomendado)

### Paso 1: Instalar Paquetes Requeridos

```r
packages <- c("shiny", "plotly", "Ryacas", "tidyverse", "DT", "latex2exp")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}
```

### Paso 2: Descargar los Archivos

Descarga estos archivos en una carpeta:
- `app.R`
- `funciones.R`

### Paso 3: Ejecutar la Aplicación

**Opción A (Recomendado):**
1. Abre `app.R` en RStudio
2. Presiona el botón **"Run App"** (arriba a la derecha)
3. La aplicación se abrirá en tu navegador

**Opción B (Desde terminal):**
```r
setwd("ruta/a/tu/carpeta")
shiny::runApp()
```

## 📖 Cómo Usar

### 1. Ingresa una Función

En el campo **"Ingrese la función f(x,y):"**, escribe tu función.

**Ejemplos:**
```
x^2 + y^2 - 2*x - 4*y + 5
x^2 - y^2
x*y - x - y
x^3 + y^3 - 3*x*y
```

### 2. Presiona "🔍 Analizar Función"

El dashboard:
- Calcula las derivadas parciales
- Encuentra los puntos críticos
- Analiza la matriz Hessiana
- Clasifica cada punto
- Genera gráficas automáticamente

### 3. Explora los Resultados

Usa los diferentes tabs:

- **Tab 1: 📐 Análisis Matemático** - Derivadas, puntos críticos, conclusiones
- **Tab 2: 🎨 Superficie 3D** - Visualiza en 3D
- **Tab 3: 🗺️ Curvas de Nivel** - Contornos
- **Tab 4: 📊 Tabla de Valores** - Valores numéricos
- **Tab 5: ❓ Ayuda** - Instrucciones

## 🧮 Interpretación de Resultados

### Criterio del Hessiano

Para clasificar un punto crítico (x₀, y₀):

1. **Calcula el determinante de H:**
   ```
   det(H) = fₓₓ·f_yy - (fₓᵧ)²
   ```

2. **Aplica la regla:**
   - Si `det(H) > 0` y `fₓₓ > 0` → **MÍNIMO LOCAL**
   - Si `det(H) > 0` y `fₓₓ < 0` → **MÁXIMO LOCAL**
   - Si `det(H) < 0` → **PUNTO SILLA**
   - Si `det(H) = 0` → **NO CONCLUYENTE**

## 📚 Ejemplos

### Ejemplo 1: Paraboloide Simple
```
f(x,y) = x² + y²
```
- Resultado: 1 mínimo en (0, 0)

### Ejemplo 2: Silla de Montar
```
f(x,y) = x² - y²
```
- Resultado: 1 punto silla en (0, 0)

### Ejemplo 3: Múltiples Puntos
```
f(x,y) = x³ + y³ - 3xy
```
- Resultado: 1 mínimo + 1 punto silla

## ⚙️ Estructura del Código

**`app.R`** (Principal)
- Interfaz de usuario (UI)
- Lógica del servidor (Server)
- Aproximadamente 650 líneas

**`funciones.R`** (Funciones Auxiliares)
- `calcular_derivadas()`
- `encontrar_puntos_criticos()`
- `calcular_hessiano()`
- `clasificar_punto()`
- `evaluar_funcion()`
- `generar_datos_3d()`
- `generar_tabla_valores()`

## 📦 Paquetes Utilizados

| Paquete | Función |
|---------|----------|
| `shiny` | Framework web interactivo |
| `plotly` | Gráficas 3D interactivas |
| `Ryacas` | Cálculo simbólico (derivadas) |
| `tidyverse` | Manipulación de datos |
| `DT` | Tablas interactivas |
| `latex2exp` | Fórmulas matemáticas |

## 🎓 Aplicaciones Educativas

✓ Clases de Cálculo en Varias Variables
✓ Tareas y Proyectos
✓ Investigación
✓ Análisis de funciones

---

**¡Divi erte explorando funciones de varias variables! 🚀**
