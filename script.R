instalar_paquetes<-function(){
          #SOLO PARA DISTRIBUCIONES LINUX
          system("sudo apt update")
          system("sudo apt-get install r-cran-spdep")
          system("sudo dpkg-query -l | grep r-cran-spdep")
          system("sudo apt install -y libudunits2-0 libudunits2-dev")
          system("sudo apt install libgdal-dev")
          system("sudo apt install gdal-bin libgdal-dev libproj-dev")
          install.packages("sp")
          install.packages("gstat")
          install.packages("maptools")
          install.packages("rgeos")
          install.packages("raster")
          install.packages("rgdal")
          install.packages("automap")
          install.packages("spdep")
          install.packages("spdep")
              
          require(sp)
          require(gstat)
          require(maptools)
          require(rgeos)
          require(raster)
          require(rgdal)
          require(spdep)
}

listar_archivos<-function(){
          lista<-list.files(getwd())
          largo<-length(lista)
          vector<-c(seq(1:largo))
          lista1<<-data.frame(vector,lista)
          print(lista1)
}

ingresar_datos<-function(indicador_archivos,variable){
        ruta_datos<-paste(getwd(),"/",lista1$lista[indicador_archivos],sep="") 
        myshp <- readOGR(ruta_datos,use_iconv=TRUE, encoding = "UTF-8")  
        #myshp <- readOGR(ruta_datos,stringsAsFactors=FALSE)
        myshp@data[variable]<-as.numeric(as.character(unlist(c(myshp@data[variable]))))
        proj4string(myshp) <- CRS("+init=epsg:4326")
        #reproyecto a metros pseudo mercator
        myshp <- spTransform(myshp, CRS("+init=epsg:3857"))
        #me quedo con la columna de Rinde.
        drops<-c(variable)
        myshp1 <- myshp[,(names(myshp) %in% drops)]
               
       colnames(myshp1@data)<-"Rinde"
       print(spplot(myshp1["Rinde"]))
       datos2<<-myshp1

}

importar_poligono<-function(indicador_archivos){
      ruta_datos<-paste(getwd(),"/",lista1$lista[indicador_archivos],sep="")  
      poligono<-readShapePoly(ruta_datos)
      proj4string(poligono) <- CRS("+init=epsg:4326")
      poligono <- spTransform(poligono, CRS("+init=epsg:3857"))
      Poligono<<-poligono
      grdpts <- makegrid(poligono, cellsize = 2)
      spgrd <- SpatialPoints(grdpts, proj4string = CRS(proj4string(poligono)))
      spgrdWithin <- SpatialPixels(spgrd[poligono,])
      par(mfrow= c(1,1))
      plot(spgrdWithin,col = "red", pch = 10, cex = 0.2,xlab="X",ylab="Y")
      gri<<-spgrdWithin
}



outlier<-function(remplazo){
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
        #visualizacion de outliers 
        myshp<-datos2
        par(mfrow=c(1,2))
        hist(myshp@data$Rinde,col='grey',nclass=20,main="Histograma",ylab='Frecuencia Relativa',xlab='Rendimiento')
        boxplot(myshp@data$Rinde,col='grey',ylab='Rendimiento',main="Box-Plot",ylim = c(min(myshp@data$Rinde), max(myshp@data$Rinde)))

        #calculo de limites max y min media + 3 DS
        Media <- mean(myshp@data$Rinde)
        DE <- sd(myshp@data$Rinde)
        LI <- Media-(2*DE)
        LS <- Media+(2*DE)
        print(paste("Media:",Media))
        print(paste("DE:",DE))
        print(paste("LI",LI))
        print(paste("LS",LS))

        myshp.outlier<<-myshp
        

        if (remplazo== "si") {
          # reemplazo de NA por datos fuera de rango
          myshp.outlier[LS<myshp@data[,"Rinde"]|myshp@data[,"Rinde"]<LI] <-NA
          myshp.outlier.1<-is.na(myshp.outlier@data[,"Rinde"])
          numeroNan<-sum(myshp.outlier.1[myshp.outlier.1==TRUE])
          n<-0
          if(numeroNan > 0){
          print("Filtrando datos")
          myshp.outlier <- subset(sp.na.omit(myshp.outlier[,"Rinde"]))
          n<-length(myshp@data[,"Rinde"])-length(myshp.outlier@data[,"Rinde"])
          print(paste("Se filtraron",n,"puntos"))
          }

          
          #eliminacion de todos los NA


          #par(mfrow=c(1,2))
          hist(myshp.outlier@data$Rinde,col='grey',nclass=20,main="Histograma",ylab='Frecuencia Relativa',xlab='Rendimiento')
          boxplot(myshp.outlier@data$Rinde,col='grey',ylab='Rendimiento',main="Box-Plot",ylim = c(min(myshp.outlier@data$Rinde), max(myshp.outlier@data$Rinde)))
          par(mfrow=c(1,1))
          datos2<<-myshp.outlier  
       }
}

inliers<-function(distanciamax){
          
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
          
          
          
  inicio<-nrow(datos2@data)
       
  filtro1<-datos2
  cord <- filtro1@coords
  gri <- dnearneigh(cord,0,distanciamax)
  lw <- nb2listw(gri, style = "W",zero.policy=TRUE)
  par(mfrow=c(1,1))
 
  ML <- localmoran (filtro1@data$Rinde, lw, p.adjust.method="bonferroni",alternative ="less",zero.policy=TRUE)
  MP <- moran.plot2(filtro1@data$Rinde,lw,quiet=T,labels=F,col=3,zero.policy=T,xlab="Rendimiento", ylab="Rendimiento Spatially Lagged")
  Influ <- MP$is.inf ; Influ
  datos0 <- data.frame(filtro1@data,filtro1@coords,ML,Influ)
 
  #eliminacion de datos con ?ndice de Moran Local negativo y estad?sticamente significativos (p<0.05).
  datos1 <- subset(datos0,datos0$Ii > 0 | datos0$Pr.z...0.>0.05)
  myshp.inlier<<- subset(datos0,datos0$Ii < 0 | datos0$Pr.z...0.<0.05)
  
  datos2 <- datos1[datos1$dfb.1_ == FALSE & datos1$dfb.x == FALSE
                   & datos1$dffit == FALSE & datos1$cov.r == FALSE & datos1$cook.d
                   == FALSE & datos1$hat == FALSE, ]
  
  coordinates(datos2)<-c("coords.x1","coords.x2")
  #spplot(datos2["Rinde"], col.regions=rampa, at=values)
	
  n<-nrow(filtro1@data[Rinde])-nrow(datos2[Rinde])
	
  final<-nrow(datos2@data)
  n<-inicio-final
  print(paste("Se filtraron",n,"puntos"))
  proj4string(datos2) <- CRS("+init=epsg:3857")
  datos2 <<-datos2

}

mapa_idw <-function(){
    prof.idw <<- idw(Rinde~1, datos2, gri,nmax=5)
    print(spplot(prof.idw["var1.pred"], main = ""))
}

semivariograma<-function(){
    semivariograma <- variogram(Rinde~1, datos2, cutoff=250)
    print(plot(semivariograma,main="",xlab="Distancia",ylab="Semivarianza"))
}

modelo_semivariograma<-function(){
    variograma <- variogram(Rinde~1, datos2, cutoff=250)
    modelovgm <<- fit.variogram(variograma, fit.method=1, vgm(sill,modelo,range,nugget))
    print(plot(variograma,modelovgm ,main="",xlab="Distancia",ylab="Semivarianza"))
    #ver SCE
    print(attr(modelovgm , 'SSErr'))
}

auto_semivariograma<-function(){
          datos<-datos2
          print("ARMO VARIOGRAMA")
          semivariograma <- variogram(Rinde~1, datos, cutoff=250)
          sill<-max(semivariograma$gamma)
          nugget<-semivariograma[1,3]
          distancia<-max(semivariograma$dist)
          modelovgm<- fit.variogram(semivariograma, fit.method=1, vgm(sill,"Sph",distancia,nugget))
          error1<-attr(modelovgm , 'SSErr')
          modelo_final = "Sph"

          seleccion_modelo<-function(){
                    modelos<-c("Sph","Exp","Lin","Gau","Ste","Mat")
                              for (i in 1:length(modelos)){
                          print(i)
                              modelovgm<- fit.variogram(semivariograma, fit.method=1, vgm(sill,modelos[i],distancia,nugget))
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
          print(paste("Modelo final",modelo_final))
          modelovgm<<- fit.variogram(semivariograma, fit.method=1, vgm(sill,modelo_final,distancia,nugget))
          plot(semivariograma,modelovgm ,main="",xlab="Distancia",ylab="Semivarianza")

}

mapa_kriging<-function(distancia){
    data2<<-remove.duplicates(datos2)
    print("Datos originales")
    print(nrow(datos2))
    print("Datos filtrados")
    print(nrow(data2))
    Kg_wls <<- krige(Rinde~1, data2, gri, model = modelovgm, debug.level=-1,maxdist=distancia)
    print(spplot(Kg_wls["var1.pred"], col.regions=rev(heat.colors(100))))
}

validacion<-function(distancia){
    require(automap)
    Kg_wls1 <- krige.cv(Rinde~1, data2, model = modelovgm, maxdist=distancia)
    print(compare.cv(list(krige.cv_output = na.omit(Kg_wls1))))
}


##COMPLETAR ESTOS VALORES CON LOS CORRECTOS####
#nombre<-"San Jose 1"
#que_mapa<-"kriging"
#detalleslote <-"aaaaa"
#unidad_de_cosecha<-"tn/ha"
#Lote<- "La prueba4"
#version_simplificada<-"si"  


suavizado<-function(estadistico,Ventana){
if (que_mapa == "idw") {Kg_wls <-prof.idw}

type<-Ventana
type1<-estadistico

if("raster" %in% rownames(installed.packages()) == FALSE) {install.packages("raster")} else {print("raster ya esta..")}
         
library(raster)

if(class(Kg_wls) != 'RasterLayer') {Kg_wls<-raster(Kg_wls)}


if(type==3){celda <- 3}
if(type==5){celda <- 5}
if(type==9){celda <- 9}
if(type==13){celda <- 13}

print(paste(celda,"x",celda))
ventana<-matrix(1,nrow=celda,ncol=celda)

if(type1=="media"){suaveraster<-focal(Kg_wls,w=ventana,fun=mean,na.rm=TRUE); print("Se aplico la media")}
if(type1=="mediana"){suaveraster<-focal(Kg_wls,w=ventana,fun=median,na.rm=TRUE); print("Se aplico la mediana")}
if(type1=="maximo"){suaveraster<-focal(Kg_wls,w=ventana,fun=max,na.rm=TRUE); print("Se aplico la maxima")}
if(type1=="minimo"){suaveraster<-focal(Kg_wls,w=ventana,fun=min,na.rm=TRUE); print("Se aplico la minima")}
if(type1=="moda"){suaveraster<-focal(Kg_wls,w=ventana,fun=modal,na.rm=TRUE); print("Se aplico la moda")}

plot(suaveraster)
suaveraster<<-suaveraster

}


exportar<-function(nombre,detalleslote,unidad_de_cosecha,version_simplificada){
          if (que_mapa == "idw") {Kg_wls <<-prof.idw}

          Kg_wls<-na.omit(Kg_wls)
          r<- raster(Kg_wls,layer=1)
          writeRaster(r, filename=paste(nombre,".tif",sep=""), bandorder='BIL', overwrite=TRUE)
          valores<-na.omit(Kg_wls@data[,1])

          if (version_simplificada == "no"){
                cols <- c("#AA0014","#d73027","#fc8d59","#fee08b","#d9ef8b","#91cf60","#1a9850")
                
                cortes<-quantile(valores, probs = c(0.10,0.20,0.40,0.50,0.60,0.80,0.9))
                print(cortes)
                #reclasifico los valores continuos de raster por la calsificación de tomas.
                values_tg<-unname(cortes)

                lamatrix<-matrix(nrow=8,ncol=3)
                lamatrix[1,1]<--Inf
                lamatrix[1,2]<-values_tg[1]
                lamatrix[1,3]<-values_tg[1]
                lamatrix[8,1]<-values_tg[7]
                lamatrix[8,2]<-Inf
                lamatrix[8,3]<-values_tg[7]

                for (i in 2:7){
                a<-i-1
                lamatrix[i,1]<-values_tg[a]
                b<-i
                lamatrix[i,2]<-values_tg[b]
                a<-i
                lamatrix[i,3]<-values_tg[a]
                }
          }

          if (version_simplificada == "si"){
             cols <- c("#d73027","#fee08b","#d9ef8b","#1a9850")
             cortes<-quantile(valores, probs =c(0.2,0.5,0.8,1))
             #son 5
              #reclasifico los valores continuos de raster por la calsificación de tomas.
              values_tg<-unname(cortes)

              lamatrix<-matrix(nrow=4,ncol=3)
              lamatrix[1,1]<--Inf
              lamatrix[1,2]<-values_tg[1]
              lamatrix[1,3]<-values_tg[1]
              lamatrix[4,1]<-values_tg[3]
              lamatrix[4,2]<-Inf
              lamatrix[4,3]<-values_tg[4]


              for (i in 2:4){
              a<-i-1
              lamatrix[i,1]<-values_tg[a]
              b<-i
              lamatrix[i,2]<-values_tg[b]
              a<-i
              lamatrix[i,3]<-values_tg[a]
              }

          }


          rc <- reclassify(r, lamatrix,include.lowest=TRUE)
          vectorizado<-rasterToPolygons(rc,na.rm=TRUE,dissolve=TRUE)
          vectorizado@data$colores<-as.factor(unname(cortes))
          #spplot(vectorizado1,zcol="var1.pred",col.region=as.vector(cols))
          print(plot(vectorizado["var1.pred"],col=cols,border="NA"))
          writeOGR(vectorizado, layer = paste(nombre," vectorizado",sep=""), dsn="vectorizado R", driver="ESRI Shapefile",overwrite_layer=TRUE)
          #ACA ARMO EL PDF   
          valores<-na.omit(Kg_wls@data[,1])
          #cortes<-quantile(valores, probs = c(0,0.10,0.25,0.50,0.75,0.90,1))
          h <- hist(valores, breaks=20, plot=F) # h$breaks and h$mids
          #cols <- c("#AA0014","#d73027","#fc8d59","#fee08b","#d9ef8b","#91cf60","#1a9850")
          k <- cols[findInterval(h$breaks, unname(cortes), rightmost.closed=T, all.inside=F) + 1]
          #plot(h, col=k,main=paste("Distribucion de rendimiento, Lote:", Lote),xlab= paste("Rendimiento",unidad_de_cosecha),ylab="Frecuencia")


          ruta<-getwd()
          nombre1<-paste("Mapa de rendimiento-",nombre,".pdf",sep= "")
          pdf(file= paste(ruta,"/",nombre1,sep=""))
          #par(mar = c(2,2,2,2)
           par(mfrow=c(1,1))


          plot(vectorizado["var1.pred"], col=cols, border="NA",main=paste("Mapa de Rendimiento",nombre,sep=" "))
          max<-length(cortes)
          legend("topleft",as.character(round(unname(cortes[1:max]),digits=2)),fill=cols,cex = 0.8,bty="n",title=unidad_de_cosecha)
          mtext(detalleslote,side=1,cex=1)
          mtext("Desarrollodo en INTA Bordenave - @FrancoFrolla",side=4,cex=0.5)

          plot(h, col=k,main=paste("Histograma rendimiento"),xlab=paste("Rendimiento", unidad_de_cosecha),ylab="Frecuencia")
          mtext("Desarrollodo en INTA Bordenave - @FrancoFrolla",side=4,cex=0.5)

          dev.off()
          
          hacerkml<-function(){
            proj4string(vectorizado) <<- CRS("+init=epsg:3857")
            obj <- spTransform(vectorizado, CRS("+init=epsg:4326"))
            inners<-c("")
            outers<-c("")
            union<-c("")
            superkml<-c("")
            stack<-c("")

            if (version_simplificada == "no"){
            cabezera<-c('<?xml version="1.0"?>
                        <kml xmlns:xsd="http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd" version="1.0">
                        <Document><Style id="1"><LineStyle><width>2.0</width><color>ff1400aa</color></LineStyle><PolyStyle><color>ff1400aa</color></PolyStyle></Style><Style id="2"><LineStyle><width>2.0</width><color>ff2730d7</color></LineStyle><PolyStyle><color>ff2730d7</color></PolyStyle></Style><Style id="3"><LineStyle><width>2.0</width><color>ff598dfc</color></LineStyle><PolyStyle><color>ff598dfc</color></PolyStyle></Style><Style id="4"><LineStyle><width>2.0</width><color>ff8be0fe</color></LineStyle><PolyStyle><color>ff8be0fe</color></PolyStyle></Style><Style id="5"><LineStyle><width>2.0</width><color>ff8befd9</color></LineStyle><PolyStyle><color>ff8befd9</color></PolyStyle></Style><Style id="6"><LineStyle><width>2.0</width><color>ff60cf91</color></LineStyle><PolyStyle><color>ff60cf91</color></PolyStyle></Style><Style id="7"><LineStyle><width>2.0</width><color>ff50981a</color></LineStyle><PolyStyle><color>ff50981a</color></PolyStyle></Style><Style id="8"><LineStyle><width>2.0</width><color>ff882754</color></LineStyle><PolyStyle><color>ff882754</color></PolyStyle></Style>
                        <ScreenOverlay><name>Legend: Logo</name><Icon> <href>http://lmingenieria.com.ar/images/logo.png</href> </Icon> <overlayXY x="0" y="0" xunits="fraction" yunits="fraction"/>
                         <screenXY x="25" y="40" xunits="pixels" yunits="pixels"/>
                        <rotationXY x="0.5" y="0.5" xunits="fraction" yunits="fraction"/>
                        <size x="0" y="0" xunits="pixels" yunits="pixels"/>
                          </ScreenOverlay>')}
             if (version_simplificada == "si"){
            cabezera<-c('<?xml version="1.0"?>
                        <kml xmlns:xsd="http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd" version="1.0">
                        <Document><Style id="1"><LineStyle><width>0</width><color>ff2730d7</color></LineStyle><PolyStyle><color>ff2730d7</color></PolyStyle></Style><Style id="2"><LineStyle><width>0</width><color>ff8be0fe</color></LineStyle><PolyStyle><color>ff8be0fe</color></PolyStyle></Style><Style id="3"><LineStyle><width>0</width><color>ff8befd9</color></LineStyle><PolyStyle><color>ff8befd9</color></PolyStyle></Style><Style id="4"><LineStyle><width>0</width><color>ff50981a</color></LineStyle><PolyStyle><color>ff50981a</color></PolyStyle></Style><Style id="5"><LineStyle><width>2.0</width><color>ff8befd9</color></LineStyle><PolyStyle><color>ff8befd9</color></PolyStyle></Style><Style id="6"><LineStyle><width>2.0</width><color>ff60cf91</color></LineStyle><PolyStyle><color>ff60cf91</color></PolyStyle></Style><Style id="7"><LineStyle><width>2.0</width><color>ff50981a</color></LineStyle><PolyStyle><color>ff50981a</color></PolyStyle></Style><Style id="8"><LineStyle><width>2.0</width><color>ff882754</color></LineStyle><PolyStyle><color>ff882754</color></PolyStyle></Style>
                        <ScreenOverlay><name>Legend: Logo</name><Icon> <href>http://lmingenieria.com.ar/images/logo.png</href> </Icon> <overlayXY x="0" y="0" xunits="fraction" yunits="fraction"/>
                         <screenXY x="25" y="40" xunits="pixels" yunits="pixels"/>
                        <rotationXY x="0.5" y="0.5" xunits="fraction" yunits="fraction"/>
                        <size x="0" y="0" xunits="pixels" yunits="pixels"/>
                          </ScreenOverlay>')}
            #ACTIVAR PARA SACAR LOS MULTYPOLYGON Y SILENCIAR LOS DE ABAJO
            #prefijo<-c('<Placemark><name>30</name><styleUrl>#1</styleUrl><Polygon>') 
            #sufijo<-c('</Polygon></Placemark>')
            fin<-c('</Document></kml>')

            ########VARIANTE CON MULTYPOLYGON 

            #prefijo<-c('<Placemark><name>1</name><styleUrl>#1</styleUrl><MultiGeometry>') 
            sufijo<-c('</MultiGeometry></Placemark>')
            prefijo.poligono<-c('<Polygon>') 
            sufijo.poligono<-c('</Polygon>')

            for (a in 1:length(obj@polygons)){

              if (version_simplificada == "no"){
              #if (obj$var1.pred[a] == unname(cortes[7])){estilo.id = 8}
              if (obj$var1.pred[a] == unname(cortes[7])){estilo.id = 7}
              if (obj$var1.pred[a] == unname(cortes[6])){estilo.id = 6}
              if (obj$var1.pred[a] == unname(cortes[5])){estilo.id = 5}
              if (obj$var1.pred[a] == unname(cortes[4])){estilo.id = 4}
              if (obj$var1.pred[a] == unname(cortes[3])){estilo.id = 3}
              if (obj$var1.pred[a] == unname(cortes[2])){estilo.id = 2}
              if (obj$var1.pred[a] == unname(cortes[1])){estilo.id = 1}
              }

              if (version_simplificada == "si"){
              if (obj$var1.pred[a] == unname(cortes[4])){estilo.id = 4}
              if (obj$var1.pred[a] == unname(cortes[3])){estilo.id = 3}
              if (obj$var1.pred[a] == unname(cortes[2])){estilo.id = 2}
              if (obj$var1.pred[a] == unname(cortes[1])){estilo.id = 1}
              }



              prefijo<-sprintf('<Placemark><name>%s</name><styleUrl>#%s</styleUrl><MultiGeometry>',obj$var1.pred[a],estilo.id) 


              #print(paste("corrida numero",a))  
              agujeros<-c()
              superkml<-c("")
              for (i in 1:length(obj@polygons[[a]]@Polygons)){
                agujero<-obj@polygons[[a]]@Polygons[[i]]@hole
                agujeros<-c(agujeros,agujero)
              }  
              #print(agujeros)  




              for (i in 1:length(agujeros)){
                if(agujeros[i] == FALSE){
                  #obtengo el outboundary  
                  xyz<-obj@polygons[[a]]@Polygons[[i]]@coords
                  coords.out <- paste(xyz[,1], ',', xyz[,2], ',', 0, collapse='\n ', sep = "")  
                  outers<-sprintf('<outerBoundaryIs><LinearRing><coordinates>%s</coordinates></LinearRing></outerBoundaryIs>', coords.out)
                  if(is.na(agujeros[i+1])){
                    union<-paste(prefijo.poligono,outers,sufijo.poligono)
                    #union<-paste(prefijo,outers,sufijo)
                    superkml<-paste(superkml,union)
                    #stop("Se termino la ejecución porque no habia mas poligonos")
                  } else {    
                    repeat {
                      if(agujeros[i+1] == TRUE){
                        xyz<-obj@polygons[[a]]@Polygons[[i+1]]@coords
                        coords.int <- paste(xyz[,1], ',', xyz[,2], ',', 0, collapse='\n ', sep = "")
                        xt<-sprintf('<innerBoundaryIs><LinearRing><coordinates>%s</coordinates></LinearRing></innerBoundaryIs>',coords.int)
                        inners<-paste(inners,xt)
                        i<-i+1
                      }
                      if (agujeros[i+1] == FALSE  | is.na(agujeros[i+1])){
                        union<-paste(prefijo.poligono,outers,inners,sufijo.poligono)
                        #activar para sacar los multypolygons
                        #union<-paste(prefijo,outers,inners,sufijo)
                        inners<-c("")
                        outers<-c("")
                        #print("era false")
                        #print(i+1)
                        break
                      }
                    }
                  }
                  superkml<-paste(superkml,union)
                  #superkml<-paste(prefijo.poligono,superkml,union,sufijo.poligono)
                  #stack<-paste(stack,superkml)
                } else {#print("era verdadero")
                }
              }
              stack<-paste(stack,prefijo,superkml,sufijo)

            }


            write(paste(cabezera,stack,fin), paste(nombre,".kml",sep=""))

          }
   hacerkml()
 
          
}







exportar2<-function(nombre,detalleslote,unidad_de_cosecha,version_simplificada,descargar_suaveraster){
          if (que_mapa == "idw") {Kg_wls <<-prof.idw}
          
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
	
	library(sp)
	library(spdep)
	library(gstat)
	library(raster)
	library(rgdal)
	library(rgeos)
	library(grid)
	library(ggplotify)
	library(gridExtra)
	library(ggplot2)
	library(ggspatial)
          
          Kg_wls<-na.omit(Kg_wls)
	  if(descargar_suaveraster == "si") { r<-suaveraster}  else {r<- raster(Kg_wls,layer=1)}
          
          writeRaster(r, filename=paste(nombre,".tif",sep=""), bandorder='BIL', overwrite=TRUE)
          valores<-na.omit(Kg_wls@data[,1])

         

		if (version_simplificada == "no"){
				cols <- c("#AA0014","#d73027","#fc8d59","#fee08b","#d9ef8b","#91cf60","#1a9850")
				cortes<-quantile(valores, probs = c(0.10,0.20,0.40,0.50,0.60,0.80,1))
				#reclasifico los valores continuos de raster por la calsificación de tomas.
				values_tg<-unname(cortes)

				lamatrix<-matrix(nrow=8,ncol=3)
				lamatrix[1,1]<--Inf
				lamatrix[1,2]<-values_tg[1]
				lamatrix[1,3]<-values_tg[1]
				lamatrix[8,1]<-values_tg[7]
				lamatrix[8,2]<-Inf
				lamatrix[8,3]<-values_tg[7]

				for (i in 2:7){
				a<-i-1
				lamatrix[i,1]<-values_tg[a]
				b<-i
				lamatrix[i,2]<-values_tg[b]
				a<-i
				lamatrix[i,3]<-values_tg[a]
			    }
			}

		 if (version_simplificada == "si"){
			     cols <- c("#d73027","#fee08b","#d9ef8b","#1a9850")
			     cortes<-quantile(valores, probs =c(0.2,0.5,0.8,1))
			     #son 5
			      #reclasifico los valores continuos de raster por la calsificación de tomas.
			      values_tg<-unname(cortes)

			      lamatrix<-matrix(nrow=4,ncol=3)
			      lamatrix[1,1]<--Inf
			      lamatrix[1,2]<-values_tg[1]
			      lamatrix[1,3]<-values_tg[1]
			      lamatrix[4,1]<-values_tg[3]
			      lamatrix[4,2]<-Inf
			      lamatrix[4,3]<-values_tg[4]


			      for (i in 2:4){
			      a<-i-1
			      lamatrix[i,1]<-values_tg[a]
			      b<-i
			      lamatrix[i,2]<-values_tg[b]
			      a<-i
			      lamatrix[i,3]<-values_tg[a]
			      }

			}

		if (version_simplificada == "ni"){
			    cols <- c("#AA0014","#d73027","#fc8d59","#fee08b","#d9ef8b","#91cf60","#1a9850","#0b5229")
				values_tg<-c(15,30,45,60,75,90,105,120)
			    cortes<-values_tg
			    lamatrix<-matrix(nrow=9,ncol=3)

					lamatrix[1,1]<--Inf
					lamatrix[1,2]<-values_tg[1]
					lamatrix[1,3]<-values_tg[1]
					lamatrix[9,1]<-values_tg[8]
					lamatrix[9,2]<-Inf
					lamatrix[9,3]<-values_tg[8]

					for (i in 2:8){
					a<-i-1
					lamatrix[i,1]<-values_tg[a]
					b<-i
					lamatrix[i,2]<-values_tg[b]
					a<-i
					lamatrix[i,3]<-values_tg[a]
					}

       				 }


          rc <- reclassify(r, lamatrix,include.lowest=TRUE)
           print("ACA ESTA LA VECTORIZACION")
      vectorizado<-rasterToPolygons(rc,na.rm=TRUE,dissolve=TRUE)
	  #Este error me lo marco el Juani Orcellet.
	  colnames(vectorizado@data)<-c("output")


      #vectorizado@data$colores<-as.factor(unname(cortes))
      #Reordeno los valores para hacer coincidir la escala de colores con la del dataframe. Error marcado por Wilson y Zilio
      vectorizado<-vectorizado[order(vectorizado$output),]
      plot(vectorizado["output"],col=cols,border="NA")

	  gIsValid(vectorizado, reason = T)
          vectorizado <- gBuffer(vectorizado, width=0, byid = T)
          print("generando buffer para solucionar problemas de geometria")
          gIsValid(vectorizado, reason = T)

	  #Calculo del area del lote por rendimiento
	  vectorizado <- spTransform(vectorizado,CRS("+init=epsg:4326"))
	  #calculo de area para cada zona para pasar al informe. 
	  raster::area(vectorizado,na.rm=TRUE)/10000
	  areas_lote<-raster::area(vectorizado)
	  datos_pdf<-data.frame(round(vectorizado@data$output,2),round(areas_lote/10000,2))
	  arealote<-sum(round(areas_lote/10000,2))
	  valores[valores == 0] <- NA
	  valores<-na.omit(valores)
	  media_lote<-round(mean(valores),2)
	  colnames(datos_pdf)<-c("Rendimiento","Area (ha)")
	  total<-data.frame(media_lote,arealote)
	  colnames(total)<-c("Rendimiento","Area (ha)")
	  datos_pdf <- rbind(datos_pdf, total)	


         writeOGR(vectorizado, layer = paste(nombre," vectorizado",sep=""), dsn="vectorizado R", driver="ESRI Shapefile",overwrite_layer=TRUE)
         print("ACA ARMO EL PDF")

      factores<-c("")
	  for (i in 1:nrow(datos_pdf)){
		  rinde<-round(datos_pdf[i,1],2)
		  area<-round(datos_pdf[i,2],2)
		  valor<-paste(as.character(rinde)," ","(",as.character(area)," ha)",sep="")
		  factores<-c(factores,valor)
		  }

	 vectorizado1<-st_as_sf(vectorizado)
		if (version_simplificada == "no"){
		  rindemedio<-factores[9]
		  factores<-factores[2:8]
		  sizes <- factor(factores, levels = c(factores[1],factores[2],factores[3],factores[4],factores[5],factores[6],factores[7]))
		}
		if (version_simplificada == "si"){
		  rindemedio<-factores[6]
		  factores<-factores[2:5]
		  sizes <- factor(factores, levels = c(factores[1],factores[2],factores[3],factores[4],factores[5]))
		}
		if (version_simplificada == "ni"){
			rindemedio<-factores[length(factores)]
			factores<-factores[2:(length(factores)-1)]
			sizes <- factor(factores, levels = c(factores[1:(length(factores))]))
		}
	 
	 
	 mapa<-ggplot(data = vectorizado1["output"]) +
  	 xlab("Longitud") + ylab("Latitud") +
 	 ggtitle(paste0(nombre), subtitle = paste0(detalleslote,"\n","Media(",unidad_de_cosecha,"): ",rindemedio))+
  	 geom_sf(color = "black", aes(fill = sizes))+
  	 scale_fill_manual(values = cols, name= unidad_de_cosecha)+ 
    	 theme(axis.text.x = element_text(size = 6),axis.text.y = element_text(size = 6),legend.position = "right", panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))
         
         title <- textGrob(paste("Realizado usando https://github.com/francofrolla/mapaderindes/ - INTA Bordenave"),gp=gpar(fontsize=7),just = "center")
  
	 arreglo<-grid.arrange(
    		mapa,
    		title,
    		nrow = 2,
    		ncol=1,
    		heights = c(1,0.1))
         
         ruta<-getwd()
         nombre1<-paste("Mapa-",nombre,".pdf",sep= "")
         ggsave(file=paste(ruta,"/",nombre1,sep=""), arreglo) 
          
         print("ACA ARMO EL KML")   
          hacerkml<-function(){
            #obj <- spTransform(vectorizado, CRS("+init=epsg:4326"))
            obj<-vectorizado
			inners<-c("")
            outers<-c("")
            union<-c("")
            superkml<-c("")
            stack<-c("")
	    print(str(obj))	
            if (version_simplificada == "no"){
            cabezera<-c('<?xml version="1.0"?>
                        <kml xmlns:xsd="http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd" version="1.0">
                        <Document><Style id="1"><LineStyle><width>2.0</width><color>ff1400aa</color></LineStyle><PolyStyle><color>ff1400aa</color></PolyStyle></Style><Style id="2"><LineStyle><width>2.0</width><color>ff2730d7</color></LineStyle><PolyStyle><color>ff2730d7</color></PolyStyle></Style><Style id="3"><LineStyle><width>2.0</width><color>ff598dfc</color></LineStyle><PolyStyle><color>ff598dfc</color></PolyStyle></Style><Style id="4"><LineStyle><width>2.0</width><color>ff8be0fe</color></LineStyle><PolyStyle><color>ff8be0fe</color></PolyStyle></Style><Style id="5"><LineStyle><width>2.0</width><color>ff8befd9</color></LineStyle><PolyStyle><color>ff8befd9</color></PolyStyle></Style><Style id="6"><LineStyle><width>2.0</width><color>ff60cf91</color></LineStyle><PolyStyle><color>ff60cf91</color></PolyStyle></Style><Style id="7"><LineStyle><width>2.0</width><color>ff50981a</color></LineStyle><PolyStyle><color>ff50981a</color></PolyStyle></Style><Style id="8"><LineStyle><width>2.0</width><color>ff882754</color></LineStyle><PolyStyle><color>ff882754</color></PolyStyle></Style>
                        <ScreenOverlay><name>Legend: Logo</name><Icon> <href>http://lmingenieria.com.ar/images/logo.png</href> </Icon> <overlayXY x="0" y="0" xunits="fraction" yunits="fraction"/>
                         <screenXY x="25" y="40" xunits="pixels" yunits="pixels"/>
                        <rotationXY x="0.5" y="0.5" xunits="fraction" yunits="fraction"/>
                        <size x="0" y="0" xunits="pixels" yunits="pixels"/>
                          </ScreenOverlay>')}
             if (version_simplificada == "si"){
            cabezera<-c('<?xml version="1.0"?>
                        <kml xmlns:xsd="http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd" version="1.0">
                        <Document><Style id="1"><LineStyle><width>0</width><color>ff2730d7</color></LineStyle><PolyStyle><color>ff2730d7</color></PolyStyle></Style><Style id="2"><LineStyle><width>0</width><color>ff8be0fe</color></LineStyle><PolyStyle><color>ff8be0fe</color></PolyStyle></Style><Style id="3"><LineStyle><width>0</width><color>ff8befd9</color></LineStyle><PolyStyle><color>ff8befd9</color></PolyStyle></Style><Style id="4"><LineStyle><width>0</width><color>ff50981a</color></LineStyle><PolyStyle><color>ff50981a</color></PolyStyle></Style><Style id="5"><LineStyle><width>2.0</width><color>ff8befd9</color></LineStyle><PolyStyle><color>ff8befd9</color></PolyStyle></Style><Style id="6"><LineStyle><width>2.0</width><color>ff60cf91</color></LineStyle><PolyStyle><color>ff60cf91</color></PolyStyle></Style><Style id="7"><LineStyle><width>2.0</width><color>ff50981a</color></LineStyle><PolyStyle><color>ff50981a</color></PolyStyle></Style><Style id="8"><LineStyle><width>2.0</width><color>ff882754</color></LineStyle><PolyStyle><color>ff882754</color></PolyStyle></Style>
                        <ScreenOverlay><name>Legend: Logo</name><Icon> <href>http://lmingenieria.com.ar/images/logo.png</href> </Icon> <overlayXY x="0" y="0" xunits="fraction" yunits="fraction"/>
                         <screenXY x="25" y="40" xunits="pixels" yunits="pixels"/>
                        <rotationXY x="0.5" y="0.5" xunits="fraction" yunits="fraction"/>
                        <size x="0" y="0" xunits="pixels" yunits="pixels"/>
                          </ScreenOverlay>')}
		    if (version_simplificada == "ni"){
            cabezera<-c('<?xml version="1.0"?>
                        <kml xmlns:xsd="http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd" version="1.0">
                        <Document><Style id="1"><LineStyle><width>2.0</width><color>ff1400aa</color></LineStyle><PolyStyle><color>ff1400aa</color></PolyStyle></Style><Style id="2"><LineStyle><width>2.0</width><color>ff2730d7</color></LineStyle><PolyStyle><color>ff2730d7</color></PolyStyle></Style><Style id="3"><LineStyle><width>2.0</width><color>ff598dfc</color></LineStyle><PolyStyle><color>ff598dfc</color></PolyStyle></Style><Style id="4"><LineStyle><width>2.0</width><color>ff8be0fe</color></LineStyle><PolyStyle><color>ff8be0fe</color></PolyStyle></Style><Style id="5"><LineStyle><width>2.0</width><color>ff8befd9</color></LineStyle><PolyStyle><color>ff8befd9</color></PolyStyle></Style><Style id="6"><LineStyle><width>2.0</width><color>ff60cf91</color></LineStyle><PolyStyle><color>ff60cf91</color></PolyStyle></Style><Style id="7"><LineStyle><width>2.0</width><color>ff50981a</color></LineStyle><PolyStyle><color>ff50981a</color></PolyStyle></Style><Style id="8"><LineStyle><width>2.0</width><color>ff882754</color></LineStyle><PolyStyle><color>ff882754</color></PolyStyle></Style>
						<ScreenOverlay><name>Legend: Logo</name><Icon> <href>http://lmingenieria.com.ar/images/logo.png</href> </Icon> <overlayXY x="0" y="0" xunits="fraction" yunits="fraction"/>
                         <screenXY x="25" y="40" xunits="pixels" yunits="pixels"/>
                        <rotationXY x="0.5" y="0.5" xunits="fraction" yunits="fraction"/>
                        <size x="0" y="0" xunits="pixels" yunits="pixels"/>
                          </ScreenOverlay>')}
            #ACTIVAR PARA SACAR LOS MULTYPOLYGON Y SILENCIAR LOS DE ABAJO
            #prefijo<-c('<Placemark><name>30</name><styleUrl>#1</styleUrl><Polygon>') 
            #sufijo<-c('</Polygon></Placemark>')
            fin<-c('</Document></kml>')



            #prefijo<-c('<Placemark><name>1</name><styleUrl>#1</styleUrl><MultiGeometry>') 
            sufijo<-c('</MultiGeometry></Placemark>')
            prefijo.poligono<-c('<Polygon>') 
            sufijo.poligono<-c('</Polygon>')

            for (a in 1:length(obj@polygons)){
			
			
			  if (version_simplificada == "ni"){
			  
			  #cols[1:nrow(vectorizado1@data["output"])]
			  minimo_datos<- min(vectorizado@data["output"])
  			  maximo_datos<- max(vectorizado@data["output"])
			  	
              indice_min<-which(cortes == minimo_datos)[[1]] 
              indice_max<-which(cortes == maximo_datos)[[1]]
					
  				  if (obj$output[a] == minimo_datos){estilo.id = 1}
				  if(is.na(match(cortes[indice_min+1],cortes)) == FALSE){if (obj$output[a] == cortes[indice_min+1]){estilo.id = 2}}
   				  if(is.na(match(cortes[indice_min+2],cortes)) == FALSE){if (obj$output[a] == cortes[indice_min+2]){estilo.id = 3}}
				  if(is.na(match(cortes[indice_min+3],cortes)) == FALSE){if (obj$output[a] == cortes[indice_min+3]){estilo.id = 4}}
				  if(is.na(match(cortes[indice_min+4],cortes)) == FALSE){if (obj$output[a] == cortes[indice_min+4]){estilo.id = 5}}
				  if(is.na(match(cortes[indice_min+5],cortes)) == FALSE){if (obj$output[a] == cortes[indice_min+5]){estilo.id = 6}}
				  if(is.na(match(cortes[indice_min+6],cortes)) == FALSE){if (obj$output[a] == cortes[indice_min+6]){estilo.id = 7}}
				  if(is.na(match(cortes[indice_min+7],cortes)) == FALSE){if (obj$output[a] == cortes[indice_min+7]){estilo.id = 8}}
				  if(is.na(match(cortes[indice_min+8],cortes)) == FALSE){if (obj$output[a] == cortes[indice_min+8]){estilo.id = 8}}
				  #>print(paste(obj$output[a],estilo.id))

	
                  #if (obj$output[a] == cortes[indice_min+7]){estilo.id = 8}
                  #if (obj$output[a] == cortes[indice_min+6]){estilo.id = 7}
         		  #if (obj$output[a] == cortes[indice_min+5]){estilo.id = 6}
    			  #if (obj$output[a] == cortes[indice_min+4]){estilo.id = 5}
                  #if (obj$output[a] == cortes[indice_min+3]){estilo.id = 4}
         		  #if (obj$output[a] == cortes[indice_min+2]){estilo.id = 3}
				
              }

              if (version_simplificada == "no"){


              if (obj$output[a] == unname(cortes[7])){estilo.id = 7}
              if (obj$output[a] == unname(cortes[6])){estilo.id = 6}
              if (obj$output[a] == unname(cortes[5])){estilo.id = 5}
              if (obj$output[a] == unname(cortes[4])){estilo.id = 4}
              if (obj$output[a] == unname(cortes[3])){estilo.id = 3}
              if (obj$output[a] == unname(cortes[2])){estilo.id = 2}
              if (obj$output[a] == unname(cortes[1])){estilo.id = 1}
              }

              if (version_simplificada == "si"){

              if (obj$output[a] == unname(cortes[4])){estilo.id = 4}
              if (obj$output[a] == unname(cortes[3])){estilo.id = 3}
              if (obj$output[a] == unname(cortes[2])){estilo.id = 2}
              if (obj$output[a] == unname(cortes[1])){estilo.id = 1}
              }



              prefijo<-sprintf('<Placemark><name>%s</name><styleUrl>#%s</styleUrl><MultiGeometry>',obj$output[a],estilo.id) 


              #print(paste("corrida numero",a))  
              agujeros<-c()
              superkml<-c("")
              for (i in 1:length(obj@polygons[[a]]@Polygons)){
                agujero<-obj@polygons[[a]]@Polygons[[i]]@hole
                agujeros<-c(agujeros,agujero)
              }  
              #print(agujeros)  




              for (i in 1:length(agujeros)){
                if(agujeros[i] == FALSE){
                  #obtengo el outboundary  
                  xyz<-obj@polygons[[a]]@Polygons[[i]]@coords
                  coords.out <- paste(xyz[,1], ',', xyz[,2], ',', 0, collapse='\n ', sep = "")  
                  outers<-sprintf('<outerBoundaryIs><LinearRing><coordinates>%s</coordinates></LinearRing></outerBoundaryIs>', coords.out)
                  if(is.na(agujeros[i+1])){
                    union<-paste(prefijo.poligono,outers,sufijo.poligono)
                    #union<-paste(prefijo,outers,sufijo)
                    superkml<-paste(superkml,union)
                    #stop("Se termino la ejecución porque no habia mas poligonos")
                  } else {    
                    repeat {
                      if(agujeros[i+1] == TRUE){
                        xyz<-obj@polygons[[a]]@Polygons[[i+1]]@coords
                        coords.int <- paste(xyz[,1], ',', xyz[,2], ',', 0, collapse='\n ', sep = "")
                        xt<-sprintf('<innerBoundaryIs><LinearRing><coordinates>%s</coordinates></LinearRing></innerBoundaryIs>',coords.int)
                        inners<-paste(inners,xt)
                        i<-i+1
                      }
                      if (agujeros[i+1] == FALSE  | is.na(agujeros[i+1])){
                        union<-paste(prefijo.poligono,outers,inners,sufijo.poligono)
                        #activar para sacar los multypolygons
                        #union<-paste(prefijo,outers,inners,sufijo)
                        inners<-c("")
                        outers<-c("")
                        #print("era false")
                        #print(i+1)
                        break
                      }
                    }
                  }
                  superkml<-paste(superkml,union)
                  #superkml<-paste(prefijo.poligono,superkml,union,sufijo.poligono)
                  #stack<-paste(stack,superkml)
                } else {#print("era verdadero")
                }
              }
              stack<-paste(stack,prefijo,superkml,sufijo)

            }


            write(paste(cabezera,stack,fin), paste(nombre,".kml",sep=""))

          }
   hacerkml()
 
          
}

rmn<-function(nombre,Zonas,Reducir,Area_ha){
  if (que_mapa == "idw") {Kg_wls <<-prof.idw}
  Kg_wls<-raster(Kg_wls)

  lista<-list(Kg_wls)
	#27/07/2020- Franco Frolla - Derechos reservados. 

	if("psych" %in% rownames(installed.packages()) == FALSE) {install.packages("psych")} else {print("psych ya esta..")}
	if("smoothr" %in% rownames(installed.packages()) == FALSE) {install.packages("smoothr")} else {print("smoothr ya esta..")}
	if("e1071" %in% rownames(installed.packages()) == FALSE) {install.packages("e1071")} else {print("e1071 ya esta..")}
	if("rgdal" %in% rownames(installed.packages()) == FALSE) {install.packages("rgdal")} else {print("rgdal ya esta..")}
	if("units" %in% rownames(installed.packages()) == FALSE) {install.packages("units")} else {print("units ya esta..")}
	if("rgeos" %in% rownames(installed.packages()) == FALSE) {install.packages("rgeos")} else {print("rgeos ya esta..")}


	require(psych)
	require(e1071)
	require(rgeos)
	require(smoothr)
	require(units)

	#Genero grilla para hacer los clusters
	grid <- makegrid(Poligono, cellsize = 5)
	coordinates(grid)<-c("x1","x2")
	proj4string(grid) <- crs(Poligono)
	recortegrilla <- raster::crop(grid,Poligono)

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
	if(Zonas==2){MC_2<-cmeans(datos[,2:ncol(datos)-2],2,100,method="cmeans",m=1.3)}
	if(Zonas==3){MC_3<-cmeans(datos[,2:ncol(datos)-2],3,100,method="cmeans",m=1.3)}
	if(Zonas==4){MC_4<-cmeans(datos[,2:ncol(datos)-2],4,100,method="cmeans",m=1.3)}

	#Extraigo la data
	if(Zonas==2){datos1<-cbind(datos,unname(MC_2$cluster))}
	if(Zonas==2){colnames(datos1)<-c(colnames(datos),"zonas2")}
	if(Zonas==3){datos1<-cbind(datos,unname(MC_3$cluster))}
	if(Zonas==3){colnames(datos1)<-c(colnames(datos),"zonas3")}
	if(Zonas==4){datos1<-cbind(datos,unname(MC_4$cluster))}
	if(Zonas==4){colnames(datos1)<-c(colnames(datos),"zonas4")}


	#datos1<-cbind(datos,unname(MC_2$cluster),unname(MC_3$cluster),unname(MC_4$cluster))
	#colnames(datos1)<-c(colnames(datos),"zonas2","zonas3","zonas4")

	coordinates(datos1)<-c("x","y")



	if(Zonas==2){r <- rasterFromXYZ(data.frame(datos1@coords[,1], datos1@coords[,2],datos1@data["zonas2"]))}
	if(Zonas==3){r <- rasterFromXYZ(data.frame(datos1@coords[,1], datos1@coords[,2],datos1@data["zonas3"]))}
	if(Zonas==4){r <- rasterFromXYZ(data.frame(datos1@coords[,1], datos1@coords[,2],datos1@data["zonas4"]))}


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
	crs(r)<-crs(Poligono)


	vectorizado <- gdal_polygonizeR(r)
	crs(vectorizado)<-crs(Poligono)


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
	rpoly <- raster(Poligono, res=5)
	rpoligono <- rasterize(Poligono, rpoly,background=0)
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
	vectorizado<-crop(vectorizado,Poligono)
  salida<<-vectorizado
  
 if(length(unique(vectorizado@data$DN))==2){cols<-c("#1a9850","#f5f107")}
 if(length(unique(vectorizado@data$DN))==3){cols<-c("#1a9850","#f5f107","#fc1212")}
 if(length(unique(vectorizado@data$DN))==4){cols<-c("#1a9850","#f5f107","#fc1212","#5412fc")}
  plot(vectorizado["DN"],col=cols,border="NA")




	writeOGR(vectorizado, layer = paste(Nombre," zonas",sep=""), dsn="vectorizado R", driver="ESRI Shapefile",overwrite_layer=TRUE)

}







