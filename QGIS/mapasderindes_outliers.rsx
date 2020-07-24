##Shape=vector
##Rinde=field Shape
##output_plots_to_html
##Limite_minimo=number 0
##Limite_maximo=number 99999 
##Metodo=selection Usuario;Media+3DS;Media+2DS;Quantil(90-10);Quantil(80-20)
##showplots
##outliers=output vector

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

library(sp)
crs_imagen<-st_crs(Shape)

myshp <- as_Spatial(Shape)
>print("--------------------------------------------------------")
>str(myshp)


datos<-na.omit(myshp@data[,Rinde])
Media <- mean(na.omit(myshp@data[,Rinde]))
DE <- sd(na.omit(myshp@data[,Rinde]))
#LI <- Media-(2*DE)
#LS <- Media+(2*DE)


if(Metodo==0){LI <- Media-(3*DE); LS <- Media+(3*DE)}
if(Metodo==1){LI <- Media-(2*DE); LS <- Media+(2*DE)}
if(Metodo==2){LI <- quantile(datos,0.1); LS <- quantile(datos,0.9)}
if(Metodo==3){LI <- quantile(datos,0.2); LS <- quantile(datos,0.8)}
if(Metodo==4){LI <- Limite_minimo; LS <- Limite_maximo}

#if (LS > Maximo){LS = Maximo}
#if (LI > Minimo){LI = Minimo}

>print(paste("Media:",Media))
>print(paste("DE:",DE))
>print(paste("LI",LI))
>print(paste("LS",LS))


myshp.outlier<-myshp
myshp.outlier[LS<myshp@data[Rinde]|myshp@data[Rinde]<LI] <-NA
myshp.outlier <- subset(sp.na.omit(myshp.outlier[Rinde]))

n<-nrow(myshp@data[Rinde])-nrow(myshp.outlier@data[Rinde])
>print(paste("Se filtraron",n,"puntos"))

output <- sf::st_as_sf(myshp.outlier,crs= crs_imagen)
outliers = output
