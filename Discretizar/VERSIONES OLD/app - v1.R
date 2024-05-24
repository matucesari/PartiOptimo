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
  titlePanel("Discretización de Variables"),
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
      actionButton("exit_btn", "Salir")  # Agregar el botón de salida
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Datos", h3("Tabla de Datos Original"),  tableOutput("original_table")  ),
        tabPanel("Resultados de Discretización", 
                 h3("Resultados de Discretización"),
                 verbatimTextOutput("discretization_results"),
                 h3("Gráfico de Pastel"),
                 plotOutput("pie_chart")
        ),
        tabPanel("Variable Nominal Discretizada", 
                 h3("Variable Nominal Discretizada"),
                 downloadButton("download_csv", "Descargar Datos Discretizados en CSV"),
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
    df <- Fac.Num(arc)$numeric
    return(df)
  })
  
  # Mostrar la tabla de datos original
    output$original_table <- renderTable({
    req(input$file)
    data()
  })
  
  # Función para discretizar la variable seleccionada
  discretized_data <- eventReactive(input$discretize_btn, {
    req(input$file)
    variable <- input$variable
    num_intervals <- input$num_intervals
    datos <- data()
    variable_name <- names(datos[variable])
    
    if (input$method == "Fisher") { style <- "fisher" } else {style <- "kmeans" }
    
    r_intervals <- classIntervals(datos[,variable], n = num_intervals, style = style, dataPrecision = 2)
    
    # Obtén el número de intervalos discretizados
    num_intervalos <- length(r_intervals$brks) - 1
    # Crea un vector de números de rango basado en el número de intervalos
    rangos_numericos <- 1:num_intervalos
    
    labels <- paste0(substr(variable_name, start = 1, stop = 3),rangos_numericos,"[",r_intervals$brks[-num_intervals], "a", r_intervals$brks[-1],"]")
    
    nombre_variable <- paste0("R_",variable_name)
    var_Nueva <- cut(datos[,variable], breaks = r_intervals$brks, labels = labels, include.lowest = TRUE)
    
    dataframe <- data.frame(var_Nueva)
    colnames(dataframe) <- nombre_variable
    
    data_discretized <- data()
    data_discretized <- cbind(dataframe)
    
    return(list(data_discretized = data_discretized, discretization_results = r_intervals, etiquetas=labels, nombre_variable = nombre_variable))
  })
  
  # Gráfico de pastel
  output$pie_chart <- renderPlot({
    req(input$discretize_btn)
    nombre_var <- names(data()[input$variable])
    col_palette <- brewer.pal(input$num_intervals, "Blues")
    colores <- findColours(discretized_data()$discretization_results, col_palette)
    pie(attributes(colores)$table,
        main = nombre_var,
        col = colores)
  })
  
  # Datos discretizados
  output$discretized_table <- renderDataTable({
    req(input$discretize_btn)
    discretized_data()$data_discretized
  })
  
  # Resultados de discretización en un cuadro de texto
  output$discretization_results <- renderText({
    req(input$discretize_btn)
    r_intervals <- capture.output(print(discretized_data()$discretization_results))
    return(paste(r_intervals, collapse = "\n"))
  })
  
  # Descargar los datos discretizados en un archivo CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste(discretized_data()$nombre_variable, ".csv")
    },
    content = function(archivo) {
      write.csv(discretized_data()$data_discretized,  # Utiliza el data frame completo
                file = archivo,  # Especifica el archivo como destino
                sep = input$sep,  # Usar el delimitador seleccionado
                dec = input$dec,  # Usar el separador decimal seleccionado
                row.names = TRUE, 
                col.names = TRUE)
    }
  )
  observeEvent(input$exit_btn, {
    stopApp()  # Detiene la aplicación Shiny cuando se presiona el botón de salida
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
