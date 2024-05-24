
# Librerías
libraries <- c("shiny", "FuzzyR", "shinyWidgets", "shinydashboard", "shinycssloaders",
               "DT","htmlwidgets")

# Instala los paquetes si no están instalados
install.packages(setdiff(libraries, rownames(installed.packages())), dependencies = TRUE)

# Cargar librerías
lapply(libraries, library, character.only = TRUE)

# DEFINIR FUNCIONES VARIAS
# Función para escalar valores a un rango específico
escalar <- function(x, min_d, max_d) {
  max_x <- max(x)
  min_x <- min(x)
  M <- (max_d - min_d) / (max_x - min_x)
  B <- max_d - M * max_x
  scaled <- M * x + B
  return(scaled)
}


# Define la interfaz de usuario de Shiny
# Encabezado --------------------------------------------------------------
header <- dashboardHeader( title="ESCALAR UNA TABLA DE CONTIGENCIA" )

# Sidebar -----------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Cargar la Tabla de Datos", tabName = "datos", icon = icon("table")),
    menuItem("Crear Tabla equivalente", tabName = "scale", icon = icon("list-alt"))
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
      box( width = 12,
           shinycssloaders::withSpinner(							 
             DTOutput("originalTable")
           )
      )
     ),
     tabItem(
          tabName = "scale",
          h3("Escalar la tabla"), 
          box(width = 12,
              sidebarPanel(
                numericInput("min_d", "Valor mínimo a escalar:", 0),
                numericInput("max_d", "Valor máximo a escalar:", 100),
                h3("  "), 
                actionButton("escalarBtn", "Escalar Datos")
              ),
              h3("Tabla Escalada"), 
              downloadButton("download_csv", "Descargar TC en CSV"), 
              DTOutput("scaledTable")
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


# Definir el servidor
server <- function(input, output) {
  
  datos <- reactiveVal(NULL)
  observeEvent(input$file, {
    	df <- read.csv(input$file$datapath, 
    	                  header = input$header, 
    	                  dec = input$dec, 
    	                  sep = input$sep, 
    	                  stringsAsFactors = T, 
    	                  row.names = 1)
	    # Separar factores de números  
	    num <- df[, sapply(df, function(x) is.numeric(x) && !is.factor(x))]
	    tc <- as.data.frame.matrix(num)
	    datos(tc)
  })
  
  output$originalTable <- DT::renderDataTable({
    #data.frame(Original = datos())
    DT::datatable(datos(), options = list(scrollX = TRUE))
  })
  
  observeEvent(input$escalarBtn, {
    if (!is.null(datos())) {
      datos_normalizados <- escalar(datos(), input$min_d, input$max_d)
      
      output$scaledTable <- DT::renderDataTable({
        DT::datatable(datos_normalizados, options = list(scrollX = TRUE))
      })

      output$download_csv <- downloadHandler(
    	filename = function() {
     	  paste(input$file$name,"_escalada", ".csv")
   	  },
    	content = function(file) {
     		 write.csv(datos_normalizados, file, append = TRUE, row.names = TRUE, sep = input$sep, dec = input$dec)
      })
    }
  })
  # Boton para resetear y comenzar de nuevo
  observeEvent(input$btn_nuevo,{
    print("toy dentro de nuevo")
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
      # Limpiar salidas
      output$originalTable <-NULL
      output$scaledTable <-NULL
      output$download_csv <-NULL
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


