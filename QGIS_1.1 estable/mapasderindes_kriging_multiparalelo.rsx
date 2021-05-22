##Shape=vector point
##enestacolumnaestaelrinde=field Shape
##Poligono=vector polygon
##Distancia_busqueda=number 40
##showplots
##kriging=output raster
#output_plots_to_html
##showplots



if("sp" %in% rownames(installed.packages()) == FALSE) {install.packages("sp")} else {print("sp ya esta..")}
if("gstat" %in% rownames(installed.packages()) == FALSE) {install.packages("gstat")} else {print("gstat ya esta..")}
if("spdep" %in% rownames(installed.packages()) == FALSE) {install.packages("spdep")} else {print("spdep ya esta..")}
if("raster" %in% rownames(installed.packages()) == FALSE) {install.packages("raster")} else {print("raster ya esta..")}
if("rgdal" %in% rownames(installed.packages()) == FALSE) {install.packages("rgdal")} else {print("rgdal ya esta..")}
if("foreach" %in% rownames(installed.packages()) == FALSE) {install.packages("foreach")} else {print("foreach ya esta..")}
if("parallel" %in% rownames(installed.packages()) == FALSE) {install.packages("parallel")} else {print("parallel ya esta..")}
if("doParallel" %in% rownames(installed.packages()) == FALSE) {install.packages("doParallel")} else {print("doParallel ya esta..")}
if("doSNOW" %in% rownames(installed.packages()) == FALSE) {install.packages("doSNOW")} else {print("doSNOW ya esta..")}         


library(sp)
library(spdep)
library(gstat)
library(foreach)
library(parallel)
library(doParallel)
library(doSNOW)

crs_imagen<-st_crs(Shape)
datos <- as_Spatial(Shape)
poligono <- as_Spatial(Poligono)

#require(tcltk)
#datos<-tk_choose.files()
#poligono<-tk_choose.files()
#datos<-readOGR(datos)
#poligono<-readOGR(poligono)

proj4string(datos) <- CRS(proj4string(datos))


>print("ARMO GRILLA DE MUESTREO")
poligono <- buffer(poligono, width=100) 
grdpts <- makegrid(poligono, cellsize = 5)
spgrd <- SpatialPoints(grdpts, proj4string = crs((poligono)))
spgrdWithin <- SpatialPixels(spgrd[poligono,])
par(mfrow= c(1,1))
>plot(spgrdWithin,col = "red", pch = 10, cex = 0.2,xlab="X",ylab="Y")
gri<-spgrdWithin
print(str(gri))
print(CRS(proj4string(poligono)))

>print("ARMO VARIOGRAMA")
require(gstat)
#enestacolumnaestaelrinde = "WetMass"
semivariograma <- variogram(get(enestacolumnaestaelrinde)~1, datos, cutoff=250)


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
                
                  
                  salida<-krige(formula = get(enestacolumnaestaelrinde)~1, locations = data2, newdata = gri[parts[[i]],], model = modelovgm,maxdist=Distancia_busqueda,debug.level=-1)
                  
                  return(salida)
                } 




stopCluster(cl)


#------------------------------------------------------------------
#Fin procesamiento multiparalelo





raster<- raster(datoskriging,layer=1)
raster[raster==-9999]<-NA
crs(raster)<-crs(poligono)
kriging<- raster
