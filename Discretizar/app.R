# Librerías
libraries <- c("shiny", "classInt", "shinyWidgets", "shinydashboard", "RColorBrewer",
               "shinyjs", "dplyr", "RColorBrewer", "DT","htmlwidgets","dplyr", "tidyr","ggplot2")

# Instala los paquetes si no están instalados
install.packages(setdiff(libraries, rownames(installed.packages())), dependencies = TRUE)

# Cargar librerías
lapply(libraries, library, character.only = TRUE)


# Define la interfaz de usuario de Shiny

# Encabezado --------------------------------------------------------------
header <- dashboardHeader( title="Discretización de una Variable Cuantitativa en Clases Óptimas" )

# Sidebar -----------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Cargar la Tabla de Datos", tabName = "datos", icon = icon("table")),
    menuItem("Discretización", tabName = "parti", icon = icon("list-alt"))
  ),
  actionButton("exit_btn", "Salir")
)

# Cuerpo ------------------------------------------------------------------
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "datos",
      h2("Carga de fichero CSV"),  # Título de la sección
      # Contenedor para la carga de archivos y configuraciones preliminares
      box(width = 12,
          # Instrucciones para el usuario
          h5("La tabla CSV DEBE tener en 1º coluna: etiqueta de observaciones ID"), 
          # Interruptores para especificar características del archivo CSV
          materialSwitch(inputId = "header", "El archivo tiene encabezado", value = TRUE),
          materialSwitch(inputId = "nominales", "La tabla tiene Variables Categóricas Nominales", value = FALSE),
          # Opciones para especificar el formato del archivo CSV
          radioButtons("sep", "Separador de campos:", choices = c(Coma = ",", Punto_y_Coma = ";", Tabulador = "\t"), selected = ";"),
          radioButtons("dec", "Separador decimal:", choices = c(Coma = ",", Punto = "."), selected = '.'),
          fileInput("file", "Selecciona un archivo CSV:", accept = ".csv")
      ),
      h2("Datos Originales"),
      # Visualización de estadísticas y datos originales
      box(width = 12,
          h3("Estadísitcas de las Variables Cuantitativas"),
          verbatimTextOutput("estadi"),
          box(width = 12, 
              plotOutput("corre"),
              conditionalPanel(
                condition = "input.nominales == true",
                selectInput("catVar", "Seleccione la variable categórica para Colorear Grafico:", choices = NULL)
              )
          ),
          conditionalPanel(
            condition = "input.nominales == true",
            h3("Estadísitcas de las Variables Cualitativas"),
            plotOutput("bar_char")
          )
      ),
      h3("Tabla de Datos"),
      DTOutput("originalTable", height = 700)
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
          box(tableOutput("disc_intervalos")),
          h3(" "), 
          h3(" "), 
          downloadButton("download_csv", "Descargar Datos Discretizados en CSV"), 
          h3("Variable Nominal Discretizada"), 
          DTOutput("discretized_table")
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
  # Inicialización de valores reactivos para almacenar datos, factores, y otros elementos necesarios para la aplicación.
  datos <-  reactiveVal(list())
  factores <- reactiveVal(NULL)
  df <- reactiveVal(NULL)
  
  historial <- reactiveVal(list())
  temp <- reactiveVal(list(p = factor(), e = character(0)))
  
  particiones <- c()
  nombres_parti <- c()
  r <- reactiveVal(NULL)
  tabla_resultados <- reactiveVal(NULL)
  
  # Manejo de la carga de archivos CSV, incluyendo configuraciones de lectura y preprocesamiento inicial.
  observeEvent(input$file, {
    tryCatch({
      # Carga el archivo CSV y actualiza el dataframe reactivo 'df'
      df( read.csv(input$file$datapath, 
                   header = input$header, 
                   dec = input$dec, 
                   sep = input$sep, 
                   stringsAsFactors = TRUE, 
                   row.names = 1 ))
      # Extrae columnas que son factores y numéricas separadamente, 
      # asegurando que no se caiga a un vector si hay una sola columna
      # Identificar columnas de factores
      fact <- as.data.frame(df()[, sapply(df(), is.factor), drop=FALSE])
      # Separar factores de números  
      n <-as.data.frame(df()[, sapply(df(), function(x) is.numeric(x)  && !is.factor(x)), drop = FALSE])
      # Para asegurar que el nombre de la variable se mantenga incluso cuando hay solo una columna, 
      # puedes usar el argumento drop = FALSE al extraer las columnas numéricas
      # Convierte columnas relevantes a numéricas si es necesario
      f <- as.data.frame(lapply(fact, as.double))
      # Actualiza la interfaz las listas de seleccion de variables
      updateSelectInput(getDefaultReactiveDomain(), inputId = "variable", choices = c(names(n),names(f)), selected = NULL) 
      updateSelectInput(getDefaultReactiveDomain(), inputId = "catVar", choices = c(names(fact),"Sin selección"), selected = "Sin selección") 
      # Guarda los factores y datos separados en sus respectivas variables reactivas
      factores(fact)
      datos(list(cuanti=n, cuali=f))
    }, error = function(e) {
      # Maneja errores en la carga del archivo mostrando un diálogo modal con el mensaje de error
      showModal(modalDialog(
        title = "Error en la Carga del fichero CSV ",
        paste("Se ha producido un error al cargar y preprocesamiento del fichero:  ", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  
  # Renderiza la tabla de datos original usando DT::renderDataTable.	
  output$originalTable <- DT::renderDataTable({
    req(input$file)
    # Mostrar la tabla de datos original
    DT::datatable(df(), options = list(scrollX = TRUE))  # scrollX permitir el desplazamiento horizontal
  })
  
  # Renderiza estadísticas descriptivas de las variables cuantitativas.
  output$estadi <- renderPrint({
    req(input$file)
    tryCatch({
      # Mostrar Estadísitcas de la tabla numérica
      summary(datos()$cuanti)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error en Calculo de Estadísitcas",
        paste("Se ha producido un error al intentar calcular las estadísitcas básicas: ", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Renderiza la matriz de correlación de las variables cuantitativas.
  output$corre <- renderPlot({
    req(input$file)
    tryCatch({
      # Matriz de correlación
      #plot_correlation(datos()$cuanti)
      # Ajustes según la entrada de la variable categórica
      if (input$nominales != FALSE && input$catVar != "Sin selección") {
        d <- factores()  # Rescato las variables categoricas
        colorVar <- input$catVar
        # Creación del gráfico Diagrama de pares con ggpairs
        ggpairs(datos()$cuanti,
                title = "Diagrama de pares", axisLabels = "show",
                aes(color = d[[colorVar]], alpha = 0.5),
                lower = list(continuous = "smooth") )
      } else {
        # Creación del gráfico Diagrama de pares con ggpairs
        ggpairs(datos()$cuanti,
                title = "Diagrama de pares", axisLabels = "show",
                lower = list(continuous = "smooth") )
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error en cálculo de matriz de correlación ",
        paste("Se ha producido un error al realizar el cálculo y grafico de las correlaciones entre variables numéricas: ", e$message),
        easyClose = TRUE
      ))
    })
  }) 
  
  # Renderiza gráficos de barras para las distribuciones de las variables categóricas.
  output$bar_char <- renderPlot({
    req(input$nominales)
    if(input$nominales){
      # Gráfico de barras con xray::distributions(data frame factores)
      # xray::distributions(datos()$cuali)
      # Calcular frecuencias para cada factor y almacenar en una lista de tablas
      list_of_tables <- lapply(factores(), table)
      # Convertir la lista de tablas a un dataframe
      frequency_data <- bind_rows(lapply(names(list_of_tables), function(x) {
        data.frame(Factor = x,
                   Level = names(list_of_tables[[x]]),
                   Frequency = as.vector(list_of_tables[[x]]),
                   stringsAsFactors = FALSE)
      }), .id = "Variable")
      # Graficar las frecuencias usando ggplot2
      ggplot(frequency_data, aes(x = Level, y = Frequency, fill = Level)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ Factor, scales = "free_x") +
        theme_minimal() +
        labs(title = "Frecuencia ocurrencia de cada Variables Nominales",
             x = "Modalidades o Categorias",
             y = "Frecuencia") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Función para discretizar variables
  discretizar <- function(d, variable, num_intervals, metodo, num_dec) {
    variable_name <- names(d$cuanti[variable])
    if (metodo == "Fisher") {  style <- "fisher"  } else {  style <- "kmeans"    }
    r_intervals <- classIntervals(d$cuanti[,variable], n = num_intervals, style = style, dataPrecision = num_dec)
    # Obtén el número de intervalos discretizados
    inter <- length(r_intervals$brks) - 1
    # Crea un vector de números de rango basado en el número de intervalos
    rangos_numericos <- 1:inter
    # Crear las etiquetas de las modalidades o rangos
    labels <- paste0(substr(variable_name, start = 1, stop = 3),
                     rangos_numericos,
                     "[",
                     round(r_intervals$brks[-num_intervals],num_dec), 
                     "a", 
                     round(r_intervals$brks[-1],num_dec),"]")
    # Crear nueva variable cualitativa
    nombre_variable <- paste0("R_",variable_name)
    var_Nueva <- cut(d$cuanti[,variable], breaks = r_intervals$brks, labels = labels, include.lowest = TRUE)
    
    # Combina los valores actuales de temp()$p y var_Nueva al lado
    particiones <- cbind(temp()$p, as.character(var_Nueva))
    
    # Combina los valores actuales de temp()$e y nombre_variable
    nombres_parti <- c(temp()$e, nombre_variable)
    
    # Actualiza temp() con los nuevos valores
    temp(list(p = particiones, e = nombres_parti))
    r(round(r_intervals$brks,1))
    return(capture.output(print(r_intervals)))
  }
  
  observeEvent(input$discretize_btn, {
        tc <- datos()
        # Discretizar la variable seleccionada
        x <- discretizar(tc, input$variable, input$num_intervals, input$method, input$num_dec)
        
        # Crear el data frame discretizado
        variables <- temp()$p
        discretized <- data.frame(variables)
        
        etiquetas <- gsub("\\[", "_",  temp()$e) # Reemplaza corchete abierto
        etiquetas <- sub("\\]", "_",  etiquetas) # Reemplaza corchete cerrado
        colnames(discretized) <- etiquetas
        
        # Actualizar objeto reactiveVal con el nuevo data frame discretizado
        historial( list(data_discretized = discretized) )  
        
        # Resultados de discretización en un cuadro de texto
        output$discretization_results <- renderText({
          req(input$discretize_btn)
          return(paste( x, collapse = "\n"))
        })
        
        # Detalle de los rangos
        output$disc_intervalos <- renderTable({
         limites <- r()
          tr <- data.frame(
            Minimo = limites[-length(limites)],
            Maximo = limites[-1],
            Centro = (limites[-length(limites)] + limites[-1]) / 2
          )
          tabla_resultados(tr)
          tr
        }, rownames = TRUE)      
        
        # Datos 
        output$discretized_table <- DT::renderDataTable({
          req(input$discretize_btn)
          DT::datatable(historial()$data_discretized, options = list(scrollX = TRUE))
        })
  })
  
  # Descargar los datos discretizados en un archivo CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste(input$file,"_rangos",".csv")
    },
    content = function(file) {
      write.csv(historial()$data_discretized, file, 
                append = TRUE, 
                row.names = TRUE)
    }
  )
  # Detiene la aplicación Shiny cuando se presiona el botón de salida
  observeEvent(input$exit_btn, {
    stopApp()  
  })
}
  
# Ejecutar la aplicación Shiny
shinyApp(ui, server)
