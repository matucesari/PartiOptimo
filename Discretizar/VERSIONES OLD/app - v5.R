# Instala los paquetes si no están instalados
if (!require(shiny)) install.packages("shiny")
if (!require(classInt)) install.packages("classInt")
if (!require(RColorBrewer)) install.packages("RColorBrewer")
if (!require(dplyr)) install.packages("dplyr")
if (!require(shinyWidgets)) install.packages("shinyWidgets", dependencies = TRUE)
if (!require(shinydashboard)) install.packages("shinydashboard", dependencies = TRUE)


library(shiny)
library(classInt)
library(RColorBrewer)
library(dplyr)
library(shinyWidgets)
library(shinydashboard)


# Define la interfaz de usuario de Shiny

# Encabezado --------------------------------------------------------------
header <- dashboardHeader( title="Discretización de una Variable Cuantitativa en Clases Óptimas" )

# Sidebar -----------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Cargar la Tabla de Datos", tabName = "datos", icon = icon("table")),
    menuItem("Resultados de Discretización", tabName = "parti", icon = icon("list-alt"))
  ),
  actionButton("exit_btn", "Salir")
)

# Cuerpo ------------------------------------------------------------------
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "datos",
      h3("Carga de fichero CSV"), 
      box(width = 12,
          # Opciones de archivo
          materialSwitch(inputId = "header", "El archivo tiene encabezado", value = TRUE),
          radioButtons("sep", "Separador de campos:", 
                       choices = c(Coma = ",", Punto_y_Coma = ";", Tabulador = "\t"), 
                       selected = ";"),
          radioButtons("dec", "Separador decimal:", 
                       choices = c(Coma = ",", Punto = "."), 
                       selected = '.'),
          # Seleccionar el archivo CSV
          fileInput("file", "Selecciona un archivo CSV:", accept = ".csv")
      ),
      h3("Tabla de Datos Cuantitativa"), 
      box(width = 12,
          tableOutput("original_table")
      )
    ),
    tabItem(
      tabName = "parti",
      h3("Partición Univariada"), 
      box(width = 12,
          box(width = 6,
              # Opciones de discretización
              selectInput("variable", "Variable a discretizar:", choices = NULL),
              numericInput("num_intervals", "Número de intervalos de clase:", value = 5, min = 1),
              radioButtons("method", "Método de clasificación:", choices = c("Fisher", "K-means"), selected = "Fisher"),
              numericInput("num_dec", "Número de decimales conservar:", value = 1, min = 0),
              # Botones
              column(6, actionButton("discretize_btn", "Discretizar"))
          ),
          h3("Discretización"), 
          verbatimTextOutput("discretization_results"), 
          h3(" "), 
          h3(" "), 
          downloadButton("download_csv", "Descargar Datos Discretizados en CSV"), 
          h3("Variable Nominal Discretizada"), 
          tableOutput("discretized_table")
      )
    )
  )
)

## App completo ----------------------------------------------------------------
ui <- dashboardPage(
  skin = "green",
  header,
  sidebar,
  body
)

# Define el servidor de Shiny
server <- function(input, output) {
  # Cargar el archivo CSV y obtener las variables numéricas
  datos <- reactive({
      req(input$file)
      arc <- read.csv(input$file$datapath, header = input$header, dec = input$dec, sep = input$sep, stringsAsFactors = T, row.names = 1)
      
      # Actualizar opciones del menú desplegable
      updateSelectInput(getDefaultReactiveDomain(), inputId = "variable", choices = names(arc), selected = names(arc)) 
      
      # Convierte columnas relevantes a numéricas si es necesario
      df <- as.data.frame(lapply(arc, as.double))
      return(df)
  })
  
  # Mostrar la tabla de datos original
  output$original_table <- renderTable({
    req(input$file)
    datos()
   }, rownames = TRUE)
  
  # Crear objeto reactiveVal para almacenar el data frame discretizado
  historial <- reactiveVal(list())
  
  # Función para discretizar variables
  discretizar <- function(datos, variable, num_intervals, metodo, num_dec) {
    variable_name <- names(datos[variable])
    if (metodo == "Fisher") {  style <- "fisher"  } else {  style <- "kmeans"    }
    r_intervals <- classIntervals(datos[,variable], n = num_intervals, style = style, dataPrecision = num_dec)
    # Obtén el número de intervalos discretizados
    intervalos <- length(r_intervals$brks) - 1
    # Crea un vector de números de rango basado en el número de intervalos
    rangos_numericos <- 1:intervalos
    # Crear las etiquetas de las modalidades o rangos
    labels <- paste0(substr(variable_name, start = 1, stop = 3),rangos_numericos,"[",r_intervals$brks[-num_intervals], "a", r_intervals$brks[-1],"]")
    # Crear nueva variable cualitativa
    nombre_variable <- paste0("R_",variable_name)
    var_Nueva <- cut(datos[,variable], breaks = r_intervals$brks, labels = labels, include.lowest = TRUE)
    discretized <- data.frame(var_Nueva)
    colnames(discretized) <- nombre_variable
    # Actualizar objeto reactiveVal con el nuevo data frame discretizado
    historial(list(data_discretized = discretized, 
                   discretization_results = r_intervals, 
                   etiquetas = labels, 
                   nombre_variable = nombre_variable))
  }
  
  # Datos 
  output$discretized_table <- renderTable({
    req(input$discretize_btn)
    historial()$data_discretized
  }, rownames = TRUE)
  
  observeEvent(input$discretize_btn, {
        tc <- datos()
        # Discretizar la variable seleccionada
          discretizar(tc, input$variable, input$num_intervals, input$method, input$num_dec)
 
        # Resultados de discretización en un cuadro de texto
        output$discretization_results <- renderText({
          req(input$discretize_btn)
          r_intervals <- capture.output(print(historial()$discretization_results))
          return(paste(r_intervals, collapse = "\n"))
        })
       
  })
  
  # Descargar los datos discretizados en un archivo CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste(historial()$nombre_variable,"_rangos",".csv")
    },
    content = function(file) {
      write.csv(historial()$data_discretized, file, 
                append = TRUE, 
                row.names = TRUE, 
                sep = input$sep, 
                dec = input$dec)
    }
  )
  
  # Detiene la aplicación Shiny cuando se presiona el botón de salida
  observeEvent(input$exit_btn, {
    stopApp()  
  })
  
}

  
# Ejecutar la aplicación Shiny
shinyApp(ui, server)
