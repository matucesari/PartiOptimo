# PartiOptimo
Permite realizar discretización por clases óptimas (Fisher - K-means) de variables cuantitativas

## Cómo ejecutar:
- Clonar el Repositorio:
  * Verificar e Instalar Git
      - Verifica si Git está instalado: Puedes hacerlo ejecutando ```git --version``` en la terminal o consola de comandos. Si Git está instalado, este comando te mostrará la versión actual.
      - Instalar Git: En Windows: Puedes descargarlo desde Git for Windows e instalarlo siguiendo las instrucciones. En macOS: Puedes instalarlo usando Homebrew con el comando brew install git en la terminal. Si no tienes Homebrew, puedes descargar Git desde Git SCM for Mac. En Linux: Usa el gestor de paquetes de tu distribución, por ejemplo, en Ubuntu puedes usar sudo ```apt-get install git```.
  * Clonar un Repositorio
      Una vez que tienes Git instalado, puedes clonar un repositorio de GitHub siguiendo estos pasos:
      - Encuentra la URL del repositorio: Ve al repositorio en GitHub que quieres clonar. Haz clic en el botón "Clone" y copia la URL que te proporcionan. Asegúrate de seleccionar "HTTPS" si no has configurado las llaves SSH en tu máquina.      
      - Abre tu terminal o línea de comandos: Navega al directorio donde deseas guardar el repositorio clonado. Puedes cambiar de directorio usando el comando ```cd [ruta del directorio]```.      
      - Ejecuta el comando de clonación: Escribe ```git clone [URL del repositorio]```. Sustituye ```[URL del repositorio]``` por la URL que copiaste en el primer paso. Por ejemplo:   ```git clone https://github.com/matucesari/PartiOptimo```
    * Verifica que el repositorio se haya clonado correctamente: Una vez completado el comando, deberías tener una nueva carpeta en tu directorio con el nombre del repositorio. Puedes entrar a esta carpeta y comenzar a trabajar con los archivos clonados.
- Abre RStudio y establece el directorio del proyecto clonado como tu directorio de trabajo mediante ``setwd("ruta/a/tu/proyecto")``.
- Abre el archivo ```app.R``` de esta aplicación.
- Instala las dependencias si es necesario.
- Ejecuta la aplicación usando ```shiny::runApp()```.
