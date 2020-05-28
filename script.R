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
  filtro1<-datos2
  cord <- filtro1@coords
  gri <- dnearneigh(cord,0,distanciamax)
  lw <- nb2listw(gri, style = "W",zero.policy=TRUE)
  par(mfrow=c(1,1))
 
  ML <- localmoran (filtro1@data$Rinde, lw, p.adjust.method="bonferroni",alternative ="less",zero.policy=TRUE)
  MP <- moran.plot(filtro1@data$Rinde,lw,quiet=T,labels=F,col=3,zero.policy=T,xlab="Rendimiento", ylab="Rendimiento Spatially Lagged")
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


exportar<-function(nombre,detalleslote,unidad_de_cosecha,version_simplificada,cada20cm){
          if (que_mapa == "idw") {Kg_wls <<-prof.idw}

          Kg_wls<-na.omit(Kg_wls)
          r<- raster(Kg_wls,layer=1)
          writeRaster(r, filename=paste(nombre,".tif",sep=""), bandorder='BIL', overwrite=TRUE)
          valores<-na.omit(Kg_wls@data[,1])

          if (version_simplificada == "no"){
                cols <- c("#AA0014","#d73027","#fc8d59","#fee08b","#d9ef8b","#91cf60","#1a9850")
                
                cortes<-quantile(valores, probs = c(0.10,0.20,0.40,0.50,0.60,0.80,0.9))
                if(cada20cm == "si"){cortes<-c(20,40,60,80,100,120,140)}
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



