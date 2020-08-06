##Zonificacion sobre mapas=group
##lista=multiple raster
##Poligono=vector polygon
#Output=output raster
##Zonificacion=output vector
##Zonas=selection 2;3;4
##Reducir = number 2


#27/07/2020- Franco Frolla - Derechos reservados. 
require(psych)
require(e1071)

if("smoothr" %in% rownames(installed.packages()) == FALSE) {install.packages("smoothr")} else {print("smoothr ya esta..")}


require(smoothr)
require(units)

#Genero grilla para hacer los clusters
poligono <- as_Spatial(Poligono)
grid <- makegrid(poligono, cellsize = 5)
coordinates(grid)<-c("x1","x2")
proj4string(grid) <- crs(poligono)
recortegrilla <- raster::crop(grid,poligono)

#Genero un stack de imagenes raster

for (i in 1:length(lista)){
    lista[[i]]<-lista[[i]]/cellStats(lista[[i]], stat='mean', na.rm=TRUE)
    lista[[i]] <- resample(lista[[i]],lista[[1]])
    
  }


lista<-stack(lista)

#Extraigo valores de cada capa normalizada. 

nombres<-c("")
matrizdatos<-seq(1,length(recortegrilla),1)

for (i in 1:length(lista@layers)){
  print(i)
  data<-extract(lista[[i]],recortegrilla)
  matrizdatos<-cbind(matrizdatos,data)
  nombre<-lista[[i]]@data@names
  nombres<-c(nombres,nombre)

}


datos<-data.frame(matrizdatos,recortegrilla@coords[,1],recortegrilla@coords[,2])
colnames(datos)<-c(nombres,"x","y")

#SACO LA VARIABLE DUMMY
datos<-datos[2:ncol(datos)]
#elinimo datos inexistentes
datos<-na.omit(datos)

#Genero los clusters
MC_2<-cmeans(datos[,2:ncol(datos)-2],2,100,method="cmeans",m=1.3)
MC_3<-cmeans(datos[,2:ncol(datos)-2],3,100,method="cmeans",m=1.3)
MC_4<-cmeans(datos[,2:ncol(datos)-2],4,100,method="cmeans",m=1.3)

datos1<-cbind(datos,unname(MC_2$cluster),unname(MC_3$cluster),unname(MC_4$cluster))

colnames(datos1)<-c(colnames(datos),"zonas2","zonas3","zonas4")
coordinates(datos1)<-c("x","y")

>str(datos1)
>print(Zonas)

if(Zonas==0){r <- rasterFromXYZ(data.frame(datos1@coords[,1], datos1@coords[,2],datos1@data["zonas2"]))}
if(Zonas==1){r <- rasterFromXYZ(data.frame(datos1@coords[,1], datos1@coords[,2],datos1@data["zonas3"]))}
if(Zonas==2){r <- rasterFromXYZ(data.frame(datos1@coords[,1], datos1@coords[,2],datos1@data["zonas4"]))}

gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                             pypath=NULL, readpoly=TRUE, quiet=TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}
crs(r)<-crs(poligono)


vectorizado <- gdal_polygonizeR(r)

crs(vectorizado)<-crs(poligono)

r_poly_smooth <- smooth(vectorizado, method = "ksmooth", smoothness = Reducir)

#Suavizado


      
 
Zonificacion <- sf::st_as_sf(r_poly_smooth)


#Output <- r