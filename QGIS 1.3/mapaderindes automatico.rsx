#Version 1.3 - 5/02/2022 - github.com/mapaderindes

##Shape=vector
##Rinde=field Shape
##Poligono=vector polygon

##output_plots_to_html
##Metodo=selection Media+3DS;Media+2DS;Quantil(90-10);Quantil(80-20)
##Distancia_inliers=number 20
##Distancia_kriging=number 40
##Media_real_lote=number 0

##Comentarios_de_version=string La distancia de inliers inica el radio en metros donde se buscaran datos anomalos. En 0 se obia el filtro. La distancia de kriging indica el radio en metros hasta donde el interpolador buca pares para generar el mapa. El peso de balanza genera un ajuste de los datos del monitor por la formula ((dato_monitor/rinde_medio_monitor)*dato_balanza)
##Autores=string Franco Daniel Frolla y grupo de Telegram - t.me/mapaderindes - mapaderindes 1.3 - 5/2/2022

##showplots
##inliers=output vector
##kriging=output raster

message("INICIANDO OUTLIERS")

Distancia<-Distancia_inliers
Distancia_busqueda<-Distancia_kriging



Sys.sleep(0.5)




sp.na.omit <- function(x, col.name = NULL, margin = 1) {
  if (!inherits(x, "SpatialPointsDataFrame") & 
      !inherits(x, "SpatialPolygonsDataFrame") & 
      !inherits(x, "SpatialLinesDataFrame") )
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame class object")
  if(!is.null(col.name)) {
    if(is.na(match(col.name, names(x)))) stop(col.name, "does not exist in data") 
    return( x[-which(is.na(x@data[,col.name])),] )
  } else {    
    na.index <- unique(as.data.frame(which(is.na(x@data), arr.ind = TRUE))[, margin])
    if (margin == 1) {
      cat("Deleting rows: ", na.index, "\n")
      return(x[-na.index, ])
    }
    if (margin == 2) {
      cat("Deleting columns: ", na.index, "\n")
      return(x[, -na.index])
    }
  }
}   



if("sp" %in% rownames(installed.packages()) == FALSE) {install.packages("sp")} else {print("sp ya esta..")}
if("gstat" %in% rownames(installed.packages()) == FALSE) {install.packages("gstat")} else {print("gstat ya esta..")}
if("spdep" %in% rownames(installed.packages()) == FALSE) {install.packages("spdep")} else {print("spdep ya esta..")}
if("raster" %in% rownames(installed.packages()) == FALSE) {install.packages("raster")} else {print("raster ya esta..")}
if("rgdal" %in% rownames(installed.packages()) == FALSE) {install.packages("rgdal")} else {print("rgdal ya esta..")}
if("foreach" %in% rownames(installed.packages()) == FALSE) {install.packages("foreach")} else {print("foreach ya esta..")}
if("parallel" %in% rownames(installed.packages()) == FALSE) {install.packages("parallel")} else {print("parallel ya esta..")}
if("doParallel" %in% rownames(installed.packages()) == FALSE) {install.packages("doParallel")} else {print("doParallel ya esta..")}
if("doSNOW" %in% rownames(installed.packages()) == FALSE) {install.packages("doSNOW")} else {print("doSNOW ya esta..")}  

require(sp)
require(spdep)
require(gstat)
require(foreach)
require(parallel)
require(doParallel)
require(doSNOW)
require(raster)

crs_imagen<-st_crs(Shape)

myshp <- as_Spatial(Shape)
poligono <- as_Spatial(Poligono)

>print("--------------------------------------------------------")
>str(myshp)

#PARA BASE DE DATOS#####
usuario<-Sys.info()[["user"]]
fechainicio<-Sys.time()
npuntos<-nrow(myshp@data)
arealote<-round(raster::area(poligono)/10000,0)

datos<-na.omit(myshp@data[,Rinde])
Media <- mean(na.omit(myshp@data[,Rinde]))
DE <- sd(na.omit(myshp@data[,Rinde]))


if(Metodo==0){LI <- Media-(3*DE); LS <- Media+(3*DE)}
if(Metodo==1){LI <- Media-(2*DE); LS <- Media+(2*DE)}
if(Metodo==2){LI <- quantile(datos,0.1); LS <- quantile(datos,0.9)}
if(Metodo==3){LI <- quantile(datos,0.2); LS <- quantile(datos,0.8)}

#if (LS > Maximo){LS = Maximo}
#if (LI > Minimo){LI = Minimo}

>print(paste("Media:",Media))
>print(paste("DE:",DE))
>print(paste("LI",LI))
>print(paste("LS",LS))


myshp.outlier<-myshp
myshp.outlier[LS<myshp@data[Rinde]|myshp@data[Rinde]<LI] <-NA
myshp.outlier[myshp@data[Rinde]== 0] <-NA

>print("Datos a eliminar:")
>print(sum(is.na(myshp.outlier@data[Rinde])))

if(sum(is.na(myshp.outlier@data[Rinde])) == 0){
    print("No hay datos para eliminar con los filtros propuestos")} 

if(sum(is.na(myshp.outlier@data[Rinde])) > 0) {
    myshp.outlier <- subset(sp.na.omit(myshp.outlier[Rinde]))
    n<-nrow(myshp@data[Rinde])-nrow(myshp.outlier@data[Rinde])
    print(paste("Se filtraron",n,"puntos"))
   }

#output <- sf::st_as_sf(myshp.outlier,crs= crs_imagen)

#INLIERS

message("INICIANDO INLIERS")
Sys.sleep(0.5)

#02/07/2020
#SE RETOMO LA FUNCION moran.plot ORIGINAL DESDE GITHUB, el 6/05/2020 el autor hizo
#cambios y dejo de funcionar las funciones como estaban programadas
moran.plot2 <- function(x, listw, zero.policy=NULL, spChk=NULL,
                        labels=NULL, xlab=NULL, ylab=NULL, quiet=NULL, ...)
{
  if (!inherits(listw, "listw")) stop(paste(deparse(substitute(listw)),
                                            "is not a listw object"))
  if (is.null(quiet)) quiet <- !get("verbose", envir = .spdepOptions)
  stopifnot(is.vector(x))
  stopifnot(is.logical(quiet))
  if (is.null(zero.policy))
    zero.policy <- get("zeroPolicy", envir = .spdepOptions)
  stopifnot(is.logical(zero.policy))
  xname <- deparse(substitute(x))
  if (!is.numeric(x)) stop(paste(xname, "is not a numeric vector"))
  if (any(is.na(x))) stop("NA in X")
  n <- length(listw$neighbours)
  if (n != length(x)) stop("objects of different length")
  if (is.null(spChk)) spChk <- get.spChkOption()
  if (spChk && !chkIDs(x, listw))
    stop("Check of data and weights ID integrity failed")
  labs <- TRUE
  if (is.logical(labels) && !labels) labs <- FALSE
  if (is.null(labels) || length(labels) != n)
    labels <- as.character(attr(listw, "region.id"))
  wx <- lag.listw(listw, x, zero.policy=zero.policy)
  if (is.null(xlab)) xlab <- xname
  if (is.null(ylab)) ylab <- paste("spatially lagged", xname)
  plot(x, wx, xlab=xlab, ylab=ylab, ...)
  if (zero.policy) {
    n0 <- wx == 0.0
    # bug found 100401 Paulo Grahl
    if (any(n0)) {
      symbols(x[n0], wx[n0], inches=FALSE, 
              circles=rep(diff(range(x))/50, length(which(n0))),
              bg="grey", add=TRUE)
    }
  }
  xwx.lm <- lm(wx ~ x)
  abline(xwx.lm)
  abline(h=mean(wx), lty=2)
  abline(v=mean(x), lty=2)
  infl.xwx <- influence.measures(xwx.lm)
  is.inf <- which(apply(infl.xwx$is.inf, 1, any))
  points(x[is.inf], wx[is.inf], pch=9, cex=1.2)
  if (labs)
    text(x[is.inf], wx[is.inf], labels=labels[is.inf], pos=2, cex=0.7)
  rownames(infl.xwx$infmat) <- labels
  if (!quiet) summary(infl.xwx)
  invisible(infl.xwx)
}



if (Distancia!=0){

      crs_imagen<-st_crs(Shape)
      filtro1<-myshp.outlier
      >print(str(filtro1))

      >print("Sacando coordenadas")

      cord <- filtro1@coords

      >print("Generando grilla, suele tardar....")
      >print(str(cord ))

      gri <- dnearneigh(cord,0,Distancia)


      lw <- nb2listw(gri, style = "W")
      par(mfrow=c(1,1))

      >print("Graficando MORAN....")

      ML <- localmoran (filtro1@data[,Rinde], lw, p.adjust.method="bonferroni",alternative ="less",zero.policy=TRUE)
      MP <- moran.plot2(filtro1@data[,Rinde],lw,quiet=T,labels=F,col=3,zero.policy=F,xlab="Rendimiento", ylab="Rendimiento Spatially Lagged")

      Influ <- MP$is.inf 
      datos0 <- data.frame(filtro1@data,filtro1@coords,ML,Influ)

      #eliminacion de datos con ?ndice de Moran Local negativo y estad?sticamente significativos (p<0.05).
      datos1 <- subset(datos0,datos0$Ii > 0 | datos0$Pr.z...0.>0.05)
      myshp.inlier<- subset(datos0,datos0$Ii < 0 | datos0$Pr.z...0.<0.05)

      datos2 <- datos1[datos1$dfb.1_ == FALSE & datos1$dfb.x == FALSE
                      & datos1$dffit == FALSE & datos1$cov.r == FALSE & datos1$cook.d
                      == FALSE & datos1$hat == FALSE, ]

      >print("--------------------------------------------------------")


      datos3<-data.frame(datos2[Rinde],datos2["coords.x1"],datos2["coords.x2"])  
      coordinates(datos2)<-c("coords.x1","coords.x2")
      output <-sf::st_as_sf(datos3, coords = c("coords.x1", "coords.x2"),crs= crs_imagen)

      n<-nrow(filtro1@data[Rinde])-nrow(datos3[Rinde])
      >print(paste("Se filtraron",n,"puntos"))

     

      
}

if (Distancia == 0){
  	message("NO SE FILTRAN INLIERS...")
    output <- sf::st_as_sf(myshp.outlier,crs= crs_imagen)
}

 message("AJUSTANDO VALORES MONITOR A PESO DE BALANZA")
 Sys.sleep(0.5)

if (Media_real_lote==0){
	message("NO SE ASIGNO UN PESO DE BALANZA...")

  }

if (Media_real_lote>0){
  output<-as_Spatial(output)
	message("MEDIA MONITOR DE RENDIMIENTO")
	media_lote_monitor<-mean(na.omit(output@data[,Rinde]))
	print(media_lote_monitor)
	media_lote_balanza<-Media_real_lote

	output@data$rinde_ajustado<-(output@data[,Rinde]/media_lote_monitor)*media_lote_balanza
	print("Rendimiento medio ajustado por peso de balanza")
  mean(output@data$rinde_ajustado)


  output@data$rinde_monitor<-output@data[,Rinde]
	output@data[,Rinde]<-output@data$rinde_ajustado
	message("El rendimiento del monitor fue ajustado por el peso de balanza")
	Sys.sleep(0.5)
  }

#TERMINADO DE FILTROS #########################

output<-sf::st_as_sf(output)
inliers = output

message("INICIANDO KRIGING MULTIPARALELO")
Sys.sleep(0.5)




crs_imagen<-st_crs(Shape)
datos <- as_Spatial(output)


#proj4string(datos) <- CRS(proj4string(datos))


>print("ARMO GRILLA DE MUESTREO")
poligono <- buffer(poligono, width=100) 
grdpts <- makegrid(poligono, cellsize = 5)
spgrd <- SpatialPoints(grdpts, proj4string = crs((poligono)))
spgrdWithin <- SpatialPixels(spgrd[poligono,])
par(mfrow= c(1,1))
>plot(spgrdWithin,col = "red", pch = 10, cex = 0.2,xlab="X",ylab="Y")
gri<-spgrdWithin


>print("ARMO VARIOGRAMA")
require(gstat)
#enestacolumnaestaelrinde = "WetMass"
semivariograma <- variogram(get(Rinde)~1, datos, cutoff=250)


sill<-max(semivariograma$gamma)
nugget<-semivariograma[1,3]
Limite_distancia<-max(semivariograma$dist)

modelovgm<- fit.variogram(semivariograma, fit.method=1, vgm(sill,"Sph",Limite_distancia,nugget))
error1<-attr(modelovgm , 'SSErr')
>print(paste("Error inicial",error1))
modelo_final = "Sph"


seleccion_modelo<-function(){
	modelos<-c("Sph","Exp","Lin","Gau","Ste","Mat")
		for (i in 1:length(modelos)){
                print(i)
		modelovgm<- fit.variogram(semivariograma, fit.method=1, vgm(sill,modelos[i],Limite_distancia,nugget))
	 	error<-(attr(modelovgm , 'SSErr'))
			print(modelos[i])
			print(error)
			if(error < error1){
                        modelo_final <<- modelos[i]
		        error1<-error
 			print(paste("Por ahora el mejor modelo es",modelo_final))	
				
                        }
            }
}

suppressWarnings(seleccion_modelo())
>print(paste("Modelo final",modelo_final))
modelovgm<- fit.variogram(semivariograma, fit.method=1, vgm(sill,modelo_final,Limite_distancia,nugget))
plot(semivariograma,modelovgm ,main="",xlab="Distancia",ylab="Semivarianza")

data2<-remove.duplicates(datos)

>print("Datos originales")
>print(nrow(datos))
>print("Datos filtrados")
>print(nrow(data2))

crs(data2)<-NA
crs(gri)<-NA

>print("---------Distancia de interpolado---------------")
>print(Distancia_busqueda)

#------------------------------------------------------------------
#Inicio procesamiento multiparalelo



# Calculo en numero de nucelos. 
no_cores <- detectCores() - 1

modulo <-1
i<-1

while (modulo != 0){ 
  print(i)
  modulo<-(length(gri)-i)%%no_cores
  if(modulo == 0) {reductor <- i}
  i<-i+1
}

print(paste("el dataset original fue reducido en un ",reductor," punto para poder ser compartimentado",sep=""))

parts <- split(x = 1:(length(gri)-reductor), f = 1:no_cores)

#genero una dummy grid para ir concatenando los procesos
variables<-data.frame(grdpts$x1[1:3],grdpts$x2[1:3],c(-9999,-9999,-9999),c(-9999,-9999,-9999))
colnames(variables)<-c("x","y","var1.pred","var1.var")
coordinates(variables) = c("x", "y") # promote to SpatialPointsDataFrame
gridded(variables) <- TRUE 



cl <- makeCluster(no_cores, outfile="")
registerDoParallel(cl)
registerDoSNOW(cl)

progress <- function(nfin, tag) {
  cat(sprintf('Bucle completado: %d \n', nfin))
}
opts <- list(progress=progress)
 
print("CALCULANDO KRIGING EN MODO MULTIHILO")

datoskriging <- foreach(i=1:no_cores,.init = variables,.combine = rbind,.packages= c('sp','gstat'),
                .options.snow=opts,.inorder = FALSE) %dopar% {
                
                  
                  salida<-krige(formula = get(Rinde)~1, locations = data2, newdata = gri[parts[[i]],], model = modelovgm,maxdist=Distancia_busqueda,debug.level=-1)
                  
                  return(salida)
                } 




stopCluster(cl)


#------------------------------------------------------------------
#Fin procesamiento multiparalelo





raster<- raster(datoskriging,layer=1)
raster[raster==-9999]<-NA
crs(raster)<-crs(poligono)

#PARA BASE DE DATOS#########
fechafinal<-Sys.time()


comunicar_bd<-function(){
  #Me comunico con la base de datos
  if("RMySQL" %in% rownames(installed.packages()) == FALSE) {install.packages("RMySQL")} else {print("RMySQL ya esta..")}
  
  require(RMySQL)
  driver<-dbDriver("MySQL")
  conexion<-dbConnect(MySQL(),user="lmingeni_cuaty",host="lmingenieria.com.ar",port=3306,password="mapaderindes",dbname="lmingeni_datos2")
  dbListTables(conexion)
  dbReadTable(conexion, "datosmapaderindes")
  req <-paste0("INSERT INTO `datosmapaderindes` (`usuario`,`fecha_inicio_mapa`,`fecha_fin_mapa`,`n_puntos`,`area_lote`) VALUES ('", usuario, "', '", fechainicio,"', '", fechafinal,"', '", npuntos,"', '", arealote, "')")
  res <- dbSendQuery(conexion,req)
  dbFetch(res)
  dbClearResult(res)
  print("Datos cargados a la base de datos")
   }


try(comunicar_bd(), silent = TRUE)



kriging<- raster






