# Instala los paquetes si no están instalados
if (!require(shiny)) install.packages("shiny")
if (!require(classInt)) install.packages("classInt")
if (!require(RColorBrewer)) install.packages("RColorBrewer")
if (!require(dplyr)) install.packages("dplyr")
if (!require(FactoClass)) install.packages("FactoClass")

library(shiny)
library(classInt)
library(RColorBrewer)
library(dplyr)
library(FactoClass)

# Define la interfaz de usuario de Shiny
ui <- fluidPage(
  titlePanel("Discretización de una Variable Cuantitativa en Clases Óptimas"),
  sidebarLayout(
    sidebarPanel(
      # Seleccionar el archivo CSV
      fileInput("file", "Selecciona un archivo CSV:", accept = ".csv"),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ";"),
      radioButtons("dec", "Decimal",
                   choices = c(Comma = ",",
                               Punto = "."),
                   selected = '.'),
      tags$hr(),
      # Seleccionar la variable a discretizar
      numericInput("variable", "Selecciona número de variable a discretizar:", value = 5, min = 1),
            # Número de intervalos de clase
      numericInput("num_intervals", "Número de intervalos de clase:", value = 5, min = 1),
            # Método de clasificación
      radioButtons("method", "Método de Clasificación:",
                   choices = c("Fisher", "K-means"), selected = "Fisher"),
      # Botón para ejecutar la discretización
      actionButton("discretize_btn", "Discretizar"),
      # Agregar el botón de salida
      actionButton("exit_btn", "Salir")  
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Datos", h3("Tabla de Datos Original"),  
                 tableOutput("original_table")  ),
        tabPanel("Resultados de Discretización", 
                 h3("Resultados de Discretización"),
                 verbatimTextOutput("discretization_results"),
                 h3("Gráfico de Pastel"),
                 plotOutput("pie_chart"),
                 downloadButton("download_csv", "Descargar Datos Discretizados en CSV"),
                 h3("Variable Nominal Discretizada"),
                 dataTableOutput("discretized_table")
        )
      )
    )
  )
)

# Define el servidor de Shiny
server <- function(input, output) {
  
  # Cargar el archivo CSV y obtener las variables numéricas
  data <- reactive({
    req(input$file)
    arc <- read.csv(input$file$datapath, 
                    header = input$header, 
                    dec = input$dec, 
                    sep = input$sep, 
                    stringsAsFactors = T, 
                    row.names = 1)
    # Convierte columnas relevantes a numéricas si es necesario
    df <- as.data.frame(lapply(arc, as.double))
    
    # updateSelectInput(session, "variable", choices = names(df$numeric)) # Actualizar opciones del menú desplegable
   
    return(df)
  })
  
  # Mostrar la tabla de datos original
  output$original_table <- renderTable({
    req(input$file)
    data()
  })
  
  # Crear objeto reactiveVal para almacenar el data frame discretizado
  historial <- reactiveVal(list())
  
  # Definir función para discretizar variables
    discretizar <- function(datos, variable, num_intervals, metodo) {
        variable_name <- names(datos[variable])
        
        if (metodo == "Fisher") { style <- "fisher" } else {style <- "kmeans" }
        r_intervals <- classIntervals(datos[,variable], n = num_intervals, style = style, dataPrecision = 1)
        
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
 
  # Función boton Discretizar la variable seleccionada
    discretized_data <- eventReactive(input$discretize_btn, {
      req(input$file)
      discretizar( data(), input$variable, input$num_intervals, input$method )  
      
      return(list(var_discretized = historial()$data_discretized, 
                  int_discretized = historial()$discretization_results, 
                  eti_discretized = historial()$etiquetas, 
                  nombre_variable = historial()$nombre_variable
                  ) 
             )
   })
  
  # Gráfico de pastel
  output$pie_chart <- renderPlot({
    req(discretized_data())
    nombre_var <- discretized_data()$nombre_variable
    col_palette <- brewer.pal(input$num_intervals, "Blues")
    colores <- findColours(discretized_data()$int_discretized, col_palette)
    pie(attributes(colores)$table, main = nombre_var,  col = colores)
  })
  
  # Datos discretizados
  output$discretized_table <- renderDataTable({
    req(historial())
    historial()$data_discretized
  })
  
  # Resultados de discretización en un cuadro de texto
  output$discretization_results <- renderText({
    req(discretized_data())
    r_intervals <- capture.output(print(discretized_data()$int_discretized))
    return(paste(r_intervals, collapse = "\n"))
  })
  
  # Descargar los datos discretizados en un archivo CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste(historial()$nombre_variable, ".csv")
    },
    content = function(archivo) {
      write.csv(historial()$data_discretized,  # Utiliza todas las variables discretizadas
                file = archivo,  # Especifica el archivo como destino
                sep = input$sep,  # Usar el delimitador seleccionado
                dec = input$dec,  # Usar el separador decimal seleccionado
                row.names = TRUE, 
                col.names = TRUE)
     }
  )
  
  # Detiene la aplicación Shiny cuando se presiona el botón de salida
  observeEvent(input$exit_btn, {
    stopApp()  
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
