##Ambienteación = group
##lista=multiple raster
##Poligono=vector polygon
#Output=output raster
##Zonificacion=output vector
##Zonas=selection 2;3;4
##Reducir = number 2
##Area_ha = number 1


#27/07/2020- Franco Frolla - Derechos reservados. 

if("psych" %in% rownames(installed.packages()) == FALSE) {install.packages("psych")} else {print("psych ya esta..")}
if("smoothr" %in% rownames(installed.packages()) == FALSE) {install.packages("smoothr")} else {print("smoothr ya esta..")}
if("e1071" %in% rownames(installed.packages()) == FALSE) {install.packages("e1071")} else {print("e1071 ya esta..")}
if("smoothr" %in% rownames(installed.packages()) == FALSE) {install.packages("smoothr")} else {print("smoothr ya esta..")}
if("units" %in% rownames(installed.packages()) == FALSE) {install.packages("units")} else {print("units ya esta..")}
if("rgeos" %in% rownames(installed.packages()) == FALSE) {install.packages("rgeos")} else {print("rgeos ya esta..")}


require(psych)
require(e1071)
require(rgeos)
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
    if (i>1){lista[[i]] <- resample(lista[[i]],lista[[1]])}    
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
if(Zonas==0){MC_2<-cmeans(datos[,2:ncol(datos)-2],2,100,method="cmeans",m=1.3)}
if(Zonas==1){MC_3<-cmeans(datos[,2:ncol(datos)-2],3,100,method="cmeans",m=1.3)}
if(Zonas==2){MC_4<-cmeans(datos[,2:ncol(datos)-2],4,100,method="cmeans",m=1.3)}

#Extraigo la data
if(Zonas==0){datos1<-cbind(datos,unname(MC_2$cluster))}
if(Zonas==0){colnames(datos1)<-c(colnames(datos),"zonas2")}
if(Zonas==1){datos1<-cbind(datos,unname(MC_3$cluster))}
if(Zonas==1){colnames(datos1)<-c(colnames(datos),"zonas3")}
if(Zonas==2){datos1<-cbind(datos,unname(MC_4$cluster))}
if(Zonas==2){colnames(datos1)<-c(colnames(datos),"zonas4")}


#datos1<-cbind(datos,unname(MC_2$cluster),unname(MC_3$cluster),unname(MC_4$cluster))
#colnames(datos1)<-c(colnames(datos),"zonas2","zonas3","zonas4")

coordinates(datos1)<-c("x","y")

>str(datos1)
>print(Zonas)

if(Zonas==0){r <- rasterFromXYZ(data.frame(datos1@coords[,1], datos1@coords[,2],datos1@data["zonas2"]))}
if(Zonas==1){r <- rasterFromXYZ(data.frame(datos1@coords[,1], datos1@coords[,2],datos1@data["zonas3"]))}
if(Zonas==2){r <- rasterFromXYZ(data.frame(datos1@coords[,1], datos1@coords[,2],datos1@data["zonas4"]))}


#Función para aplicar gdal_polygonize en R

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


#Suavizado


require(smoothr)
require(units)

vectorizado1 <- smooth(vectorizado, method = "ksmooth", smoothness = Reducir)


#REMUEVO POLIGONOS CON UN AREA MENOR A 1 ha
vectorizado1@data$area<-(raster::area(vectorizado1,na.rm=TRUE))/10000
vectorizado2 <- vectorizado1[vectorizado1$area > Area_ha,]


r <- raster(vectorizado2, res=5)
r <- rasterize(vectorizado2, r,field = vectorizado2$DN, background=NA)

#poligono2 = gUnaryUnion(poligono)
rpoly <- raster(poligono, res=5)
rpoligono <- rasterize(poligono, rpoly,background=0)
rpoligono[rpoligono] <- 0



r <- resample(r, rpoligono)
dif <- r - rpoligono
dif[dif == 0] <- NA



fill.na <- function(x) {
  center = 0.5 + (width*width/2) 
  if( is.na(x)[center] ) {
    return( round(modal(x, na.rm=TRUE),0) )
  } else {
    return( round(x[center],0) )
  }
}  

width = 13
r2 <- focal(r, w = matrix(1,width,width), fun = fill.na, 
            pad = TRUE, na.rm = FALSE)
			
print("elimino bordes incorrectos")
ventana<-matrix(1,nrow=9,ncol=9)

r2 <-focal(r2,w=ventana,fun=modal,na.rm=TRUE)

vectorizado <- gdal_polygonizeR(r2)

print("generando buffer para solucionar problemas de geometria")

vectorizado <- gBuffer(vectorizado, width=0, byid = T)
vectorizado<-crop(vectorizado,poligono)
   
 
Zonificacion <- sf::st_as_sf(vectorizado)


#Output <- r
