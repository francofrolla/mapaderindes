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
          if("psych" %in% rownames(installed.packages()) == FALSE) {install.packages("psych")} else {print("psych ya esta..")}
          if("smoothr" %in% rownames(installed.packages()) == FALSE) {install.packages("smoothr")} else {print("smoothr ya esta..")}
          if("e1071" %in% rownames(installed.packages()) == FALSE) {install.packages("e1071")} else {print("e1071 ya esta..")}
          if("rgdal" %in% rownames(installed.packages()) == FALSE) {install.packages("rgdal")} else {print("rgdal ya esta..")}
          if("units" %in% rownames(installed.packages()) == FALSE) {install.packages("units")} else {print("units ya esta..")}
          if("sp" %in% rownames(installed.packages()) == FALSE) {install.packages("sp")} else {print("sp ya esta..")}
          if("gstat" %in% rownames(installed.packages()) == FALSE) {install.packages("gstat")} else {print("gstat ya esta..")}
	  if("maptools" %in% rownames(installed.packages()) == FALSE) {install.packages("maptools")} else {print("maptools ya esta..")}
	  if("rgeos" %in% rownames(installed.packages()) == FALSE) {install.packages("rgeos")} else {print("rgeos ya esta..")}
	  if("raster" %in% rownames(installed.packages()) == FALSE) {install.packages("raster")} else {print("raster ya esta..")}
	  if("automap" %in% rownames(installed.packages()) == FALSE) {install.packages("automap")} else {print("automap ya esta..")}
	  if("spdep" %in% rownames(installed.packages()) == FALSE) {install.packages("spdep")} else {print("spdep ya esta..")}
	  if("grid" %in% rownames(installed.packages()) == FALSE) {install.packages("grid")} else {print("grid ya esta..")}
	  if("ggplotify" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplotify")} else {print("ggplotify ya esta..")}
	  if("gridExtra" %in% rownames(installed.packages()) == FALSE) {install.packages("gridExtra")} else {print("gridExtra ya esta..")}
	  if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")} else {print("ggplot2 ya esta..")}
	  if("ggspatial" %in% rownames(installed.packages()) == FALSE) {install.packages("ggspatial")} else {print("ggspatial ya esta..")}
	  } else {
	  print("Instalando paquetes para entorno Windows")
          if("sp" %in% rownames(installed.packages()) == FALSE) {install.packages("sp")} else {print("sp ya esta..")}
          if("gstat" %in% rownames(installed.packages()) == FALSE) {install.packages("gstat")} else {print("gstat ya esta..")}
          if("maptools" %in% rownames(installed.packages()) == FALSE) {install.packages("maptools")} else {print("maptools ya esta..")}
          if("rgeos" %in% rownames(installed.packages()) == FALSE) {install.packages("rgeos")} else {print("rgeos ya esta..")}
          if("raster" %in% rownames(installed.packages()) == FALSE) {install.packages("raster")} else {print("raster ya esta..")}
          if("automap" %in% rownames(installed.packages()) == FALSE) {install.packages("automap")} else {print("automap ya esta..")}
          if("spdep" %in% rownames(installed.packages()) == FALSE) {install.packages("spdep")} else {print("spdep ya esta..")}
          if("psych" %in% rownames(installed.packages()) == FALSE) {install.packages("psych")} else {print("psych ya esta..")}
          if("smoothr" %in% rownames(installed.packages()) == FALSE) {install.packages("smoothr")} else {print("smoothr ya esta..")}
          if("e1071" %in% rownames(installed.packages()) == FALSE) {install.packages("e1071")} else {print("e1071 ya esta..")}
          if("rgdal" %in% rownames(installed.packages()) == FALSE) {install.packages("rgdal")} else {print("rgdal ya esta..")}
          if("units" %in% rownames(installed.packages()) == FALSE) {install.packages("units")} else {print("units ya esta..")}
          if("sp" %in% rownames(installed.packages()) == FALSE) {install.packages("sp")} else {print("sp ya esta..")}
          if("gstat" %in% rownames(installed.packages()) == FALSE) {install.packages("gstat")} else {print("gstat ya esta..")}
	  if("maptools" %in% rownames(installed.packages()) == FALSE) {install.packages("maptools")} else {print("maptools ya esta..")}
	  if("rgeos" %in% rownames(installed.packages()) == FALSE) {install.packages("rgeos")} else {print("rgeos ya esta..")}
	  if("raster" %in% rownames(installed.packages()) == FALSE) {install.packages("raster")} else {print("raster ya esta..")}
	  if("automap" %in% rownames(installed.packages()) == FALSE) {install.packages("automap")} else {print("automap ya esta..")}
	  if("spdep" %in% rownames(installed.packages()) == FALSE) {install.packages("spdep")} else {print("spdep ya esta..")}
	  if("grid" %in% rownames(installed.packages()) == FALSE) {install.packages("grid")} else {print("grid ya esta..")}
	  if("ggplotify" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplotify")} else {print("ggplotify ya esta..")}
	  if("gridExtra" %in% rownames(installed.packages()) == FALSE) {install.packages("gridExtra")} else {print("gridExtra ya esta..")}
	  if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")} else {print("ggplot2 ya esta..")}
	  if("ggspatial" %in% rownames(installed.packages()) == FALSE) {install.packages("ggspatial")} else {print("ggspatial ya esta..")}
	  if("foreach" %in% rownames(installed.packages()) == FALSE) {install.packages("foreach")} else {print("foreach ya esta..")}
	  if("parallel" %in% rownames(installed.packages()) == FALSE) {install.packages("parallel")} else {print("parallel ya esta..")}
	  if("doParallel" %in% rownames(installed.packages()) == FALSE) {install.packages("doParallel")} else {print("doParallel ya esta..")}
	  if("doSNOW" %in% rownames(installed.packages()) == FALSE) {install.packages("doSNOW")} else {print("doSNOW ya esta..")}  
	    if("RMySQL" %in% rownames(installed.packages()) == FALSE) {install.packages("RMySQL")} else {print("RMySQL ya esta..")}


       }


Instalar<-function(libreria){

  print("Instalando paquetes para entorno Windows")
          if("sp" %in% rownames(installed.packages(), lib= libreria) == FALSE) {install.packages("sp")} else {print("sp ya esta..")}
          if("gstat" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("gstat")} else {print("gstat ya esta..")}
          if("maptools" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("maptools")} else {print("maptools ya esta..")}
          if("rgeos" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("rgeos")} else {print("rgeos ya esta..")}
          if("raster" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("raster")} else {print("raster ya esta..")}
          if("automap" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("automap")} else {print("automap ya esta..")}
          if("spdep" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("spdep")} else {print("spdep ya esta..")}
          if("psych" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("psych")} else {print("psych ya esta..")}
          if("smoothr" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("smoothr")} else {print("smoothr ya esta..")}
          if("e1071" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("e1071")} else {print("e1071 ya esta..")}
          if("rgdal" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("rgdal")} else {print("rgdal ya esta..")}
          if("units" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("units")} else {print("units ya esta..")}
          if("sp" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("sp")} else {print("sp ya esta..")}
          if("gstat" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("gstat")} else {print("gstat ya esta..")}
	  if("maptools" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("maptools")} else {print("maptools ya esta..")}
	  if("rgeos" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("rgeos")} else {print("rgeos ya esta..")}
	  if("raster" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("raster")} else {print("raster ya esta..")}
	  if("automap" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("automap")} else {print("automap ya esta..")}
	  if("spdep" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("spdep")} else {print("spdep ya esta..")}
	  if("grid" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("grid")} else {print("grid ya esta..")}
	  if("ggplotify" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("ggplotify")} else {print("ggplotify ya esta..")}
	  if("gridExtra" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("gridExtra")} else {print("gridExtra ya esta..")}
	  if("ggplot2" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("ggplot2")} else {print("ggplot2 ya esta..")}
	  if("ggspatial" %in% rownames(installed.packages(),lib= libreria) == FALSE) {install.packages("ggspatial")} else {print("ggspatial ya esta..")}
	  if("foreach" %in% rownames(installed.packages()) == FALSE) {install.packages("foreach")} else {print("foreach ya esta..")}
	  if("parallel" %in% rownames(installed.packages()) == FALSE) {install.packages("parallel")} else {print("parallel ya esta..")}
	  if("doParallel" %in% rownames(installed.packages()) == FALSE) {install.packages("doParallel")} else {print("doParallel ya esta..")}
	  if("doSNOW" %in% rownames(installed.packages()) == FALSE) {install.packages("doSNOW")} else {print("doSNOW ya esta..")}  
	  if("RMySQL" %in% rownames(installed.packages()) == FALSE) {install.packages("RMySQL")} else {print("RMySQL ya esta..")}

}




