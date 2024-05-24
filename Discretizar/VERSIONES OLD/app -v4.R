# Instala los paquetes si no están instalados
if (!require(shiny)) install.packages("shiny")
if (!require(classInt)) install.packages("classInt")
if (!require(RColorBrewer)) install.packages("RColorBrewer")

library(shiny)
library(classInt)
library(RColorBrewer)

# Define la interfaz de usuario de Shiny
ui <- fluidPage(
  titlePanel("Discretización de Variables"),
  sidebarLayout(
    sidebarPanel(
      # Seleccionar el archivo CSV
      fileInput("file", "Selecciona un archivo CSV:", accept = ".csv"),
      
      # Seleccionar la variable a discretizar
      numericInput("variable", "Selecciona numero de variable a discretizar:", value = 5, min = 1),
      
      # Número de intervalos de clase
      numericInput("num_intervals", "Número de intervalos de clase:", value = 5, min = 1),
      
      # Método de clasificación
      radioButtons("method", "Método de Clasificación:",
                   choices = c("Fisher", "K-means"), selected = "Fisher"),
      
      # Botón para ejecutar la discretización
      actionButton("discretize_btn", "Discretizar")
    ),
    
    mainPanel(
      # Tabla de datos original
      tableOutput("original_table"),
      
      # Resultados de discretización
      verbatimTextOutput("discretization_results"),
      
      # Gráfico de pastel
      plotOutput("pie_chart"),
      
      # Tabla de datos discretizados
      dataTableOutput("discretized_table"),
      
      # Botón para guardar en CSV
      downloadButton("download_csv", "Guardar en CSV")
    )
  )
)

# Define el servidor de Shiny
server <- function(input, output) {
  
  # Cargar el archivo CSV y obtener las variables numéricas
  data <- reactive({
    req(input$file)
    arc <- read.csv(input$file$datapath, dec = ".", sep = ";", stringsAsFactors = FALSE, row.names = 1)
    # Convierte columnas relevantes a numéricas si es necesario
    df <- as.data.frame(lapply(arc, as.double))
    return(df)
  })
  
  # Mostrar la tabla de datos original
  output$original_table <- renderTable({
    req(input$file)
    print(data())
  })
  
  # Función para discretizar la variable seleccionada
  discretized_data <- eventReactive(input$discretize_btn, {
    req(input$file)
    variable <- input$variable
    num_intervals <- input$num_intervals
    datos <- data()
    variable_name <- names(datos[variable])
    
    if (input$method == "Fisher") {
      style <- "fisher"
    } else {
      style <- "kmeans"
    }
    
    r_intervals <- classIntervals(datos[,variable], n = num_intervals, style = style,dataPrecision=1)
   
     # Obtén el número de intervalos discretizados
    num_intervalos <- length(r_intervals$brks) - 1
    # Crea un vector de números de rango basado en el número de intervalos
    rangos_numericos <- 1:num_intervalos
    
    labels <- paste0(substr(variable_name, start = 1, stop = 3),rangos_numericos,"[",r_intervals$brks[-num_intervals], "a", r_intervals$brks[-1],"]")
    nueva_variable <- paste0("R_",variable_name)
    data_discretized <- data()
    data_discretized[[nueva_variable]] <- cut(datos[,variable], breaks = r_intervals$brks, labels = labels, include.lowest = TRUE)
    
    return(list(data_discretized = data_discretized, discretization_results = r_intervals, etiquetas=labels))
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
  
  # Tabla de datos discretizados
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
  
  #  Descargar los datos discretizados en un archivo CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste(names(data(filename)))
    },
    content = function(file) {
      write.csv(discretized_data()$data_discretized, file, row.names = T)
    }
  )
  
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
