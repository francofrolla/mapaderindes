#Instalar paquetes
##output_plots_to_html

if(.Platform$OS.type == "unix") {
	  print("Para instalar paquetes en UNIX se tiene que estar logeado como root, Instalando paquetes para entorno Linux")
          system("sudo apt update")
          system("sudo apt-get install r-cran-spdep")
          system("sudo dpkg-query -l | grep r-cran-spdep")
          system("sudo apt install -y libudunits2-0 libudunits2-dev")
          system("sudo apt install libgdal-dev")
          system("sudo apt install gdal-bin libgdal-dev libproj-dev")
          if("sp" %in% rownames(installed.packages()) == FALSE) {install.packages("sp")} else {print("sp ya esta..")}
          if("gstat" %in% rownames(installed.packages()) == FALSE) {install.packages("gstat")} else {print("gstat ya esta..")}
          if("maptools" %in% rownames(installed.packages()) == FALSE) {install.packages("maptools")} else {print("maptools ya esta..")}
          if("rgeos" %in% rownames(installed.packages()) == FALSE) {install.packages("rgeos")} else {print("rgeos ya esta..")}
          if("raster" %in% rownames(installed.packages()) == FALSE) {install.packages("raster")} else {print("raster ya esta..")}
          if("automap" %in% rownames(installed.packages()) == FALSE) {install.packages("automap")} else {print("automap ya esta..")}
          if("spdep" %in% rownames(installed.packages()) == FALSE) {install.packages("spdep")} else {print("spdep ya esta..")}
	  } else {
	  print("Instalando paquetes para entorno Windows")
          if("sp" %in% rownames(installed.packages()) == FALSE) {install.packages("sp")} else {print("sp ya esta..")}
          if("gstat" %in% rownames(installed.packages()) == FALSE) {install.packages("gstat")} else {print("gstat ya esta..")}
          if("maptools" %in% rownames(installed.packages()) == FALSE) {install.packages("maptools")} else {print("maptools ya esta..")}
          if("rgeos" %in% rownames(installed.packages()) == FALSE) {install.packages("rgeos")} else {print("rgeos ya esta..")}
          if("raster" %in% rownames(installed.packages()) == FALSE) {install.packages("raster")} else {print("raster ya esta..")}
          if("automap" %in% rownames(installed.packages()) == FALSE) {install.packages("automap")} else {print("automap ya esta..")}
          if("spdep" %in% rownames(installed.packages()) == FALSE) {install.packages("spdep")} else {print("spdep ya esta..")}
   

          }
