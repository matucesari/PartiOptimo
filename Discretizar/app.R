# Librerías
libraries <- c("shiny", "classInt", "shinyWidgets", "shinydashboard", "RColorBrewer",
               "shinyjs", "dplyr", "RColorBrewer", "DT","htmlwidgets")

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
  actionButton("exit_btn", "Salir"),
  actionButton("btn_nuevo", "Nuevo")
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
          h3("Estadísitcas de las Variables Cuantitativas"), 
          verbatimTextOutput("estadi"),
          box( plotOutput("histo") ),
          box( plotOutput("corre") ),
          DTOutput("original_table")
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
  # inicializo
  datos <- reactiveVal(NULL)
  historial <- reactiveVal(list())
  temp <- reactiveVal(list(p = factor(), e = character(0)))
  
  particiones <- c()
  nombres_parti <- c()
  r <- reactiveVal(NULL)
  tabla_resultados <- reactiveVal(NULL)
  
  # Cargar el archivo CSV y obtener las variables numéricas
  observeEvent(input$file, {
    df <- read.csv(input$file$datapath, 
                   header = input$header, 
                   dec = input$dec, 
                   sep = input$sep, 
                   stringsAsFactors = TRUE, 
                   row.names = 1 )
    # Separar factores de números  
    n <-as.data.frame(df[, sapply(df, function(x) is.numeric(x) && !is.factor(x)), drop=FALSE])
    
    # Actualizar opciones del menú desplegable
    updateSelectInput(getDefaultReactiveDomain(), inputId = "variable", choices = names(n), selected = NULL) 
    datos(n)
  })
  
  # Mostrar la tabla de datos original
  output$original_table <- DT::renderDataTable({
    req(input$file)
    DT::datatable(datos(), options = list(scrollX = TRUE))
  })
  
  # Mostrar Estadísitcas de la tabla numérica
  output$estadi <- renderPrint({
    req(input$file)
    tryCatch({
      summary(datos())
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error en Calculo de Estadísitcas",
        paste("Se ha producido un error al intentar calcular las estadísitcas básicas: ", e$message),
        easyClose = TRUE
      ))
    })
  })
  # Gráfico de histogramas
  output$histo <- renderPlot({
    req(input$file)
    tryCatch({
      plot_histogram(datos(), ncol = 2L)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error en Gráfico de histogramas",
        paste("Se ha producido un error al realizar los histogramas de cada variable cuantitativa:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Matriz de correlación
  output$corre <- renderPlot({
    req(input$file)
    tryCatch({
      plot_correlation(datos())
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error en cálculo de matriz de correlación ",
        paste("Se ha producido un error al realizar el cálculo y grafico de las correlaciones entre variables numéricas: ", e$message),
        easyClose = TRUE
      ))
    })
  }) 
  
  
  
  
  # Función para discretizar variables
  discretizar <- function(datos, variable, num_intervals, metodo, num_dec) {
    variable_name <- names(datos[variable])
    if (metodo == "Fisher") {  style <- "fisher"  } else {  style <- "kmeans"    }
    r_intervals <- classIntervals(datos[,variable], n = num_intervals, style = style, dataPrecision = num_dec)
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
    var_Nueva <- cut(datos[,variable], breaks = r_intervals$brks, labels = labels, include.lowest = TRUE)
    
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
        colnames(discretized) <- temp()$e
        
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
            Centro = (limites[-length(limites)] + limites[-1]) / 2,
            Desvio = (limites[-1] - limites[-length(limites)]) / 2
          )
          primero <- 1
          ultimo <- length(limites)-1
          tr[primero,4]<-tr$Desvio[primero]*2
          tr[ultimo,4]<-tr$Desvio[ultimo]*2
          tabla_resultados(tr)
          tr
        }, rownames = TRUE)      
        
        # Función para calcular la Gaussiana
        gaussiana <- function(x, a, b) {
          return(exp(-((x - a) / b)^2))
        }
        
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
  
  # Boton para resetear y comenzar de nuevo
  observeEvent(input$btn_nuevo,{
    showModal(modalDialog(
      title = "Confirmar reinicio",
      "¿Está seguro que desea reiniciar la aplicación?",
      footer = tagList(
        modalButton("Cancelar"), 
        actionButton("confirmar", "Confirmar")
      )
    ))
    observeEvent(input$confirmar, {
      # Reiniciar variables globales
      datos <- reactiveVal(NULL)
      historial <- reactiveVal(list())
      temp <- reactiveVal(list(p = factor(), e = character(0)))  
      particiones <- c()
      nombres_parti <- c()
      r <- reactiveVal(NULL)
      tabla_resultados <- reactiveVal(NULL)
      
      # Limpiar salidas
      output$original_table <- NULL
      output$discretization_results <- NULL
      output$disc_intervalos <- NULL
      output$discretized_table <- NULL
      output$download_csv <- NULL
      
      removeModal() # Cerrar modal
    })
  })
  
  # Detiene la aplicación Shiny cuando se presiona el botón de salida
  observeEvent(input$exit_btn, {
    stopApp()  
  })
  
}

  
# Ejecutar la aplicación Shiny
shinyApp(ui, server)
