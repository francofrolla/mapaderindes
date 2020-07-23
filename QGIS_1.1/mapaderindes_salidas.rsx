##Raster=raster
##Output=output vector
##Carpeta=folder
##Nombre=string Lote prueba 1
##Unidad_de_Cosecha=string kg/ha
##Comentarios=string Establecimiento "El calabozo", cosecha: 13/01/2020
##Version_simplificada=string no

##output_plots_to_html
##showplots


nombre<-Nombre
detalleslote <-Comentarios
unidad_de_cosecha<-Unidad_de_Cosecha
version_simplificada<-Version_simplificada



library(sp)
library(spdep)
library(gstat)
library(raster)
library(rgdal)
library(rgeos)

print(Carpeta)
setwd(Carpeta)
Kg_wls<-as(Raster, "SpatialPixelsDataFrame")
valores<-na.omit(Kg_wls@data[,1])
print(str(Kg_wls))
print(paste("Los archvos seran guardados en:", getwd()))
r<-Raster

     


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


          rc <- reclassify(r, lamatrix,include.lowest=TRUE)
          print("ACA ESTA LA VECTORIZACION")
          vectorizado<-rasterToPolygons(rc,na.rm=TRUE,dissolve=TRUE)
	  #Este error me lo marco el Juani Orcellet.
	  colnames(vectorizado@data)<-c("output")


          vectorizado@data$colores<-as.factor(unname(cortes))
          #Reordeno los valores para hacer coincidir la escala de colores con la del dataframe. Error marcado por Wilson y Zilio
          vectorizado<-vectorizado[order(vectorizado$output),]
          plot(vectorizado["output"],col=cols,border="NA")

	  gIsValid(vectorizado, reason = T)
          vectorizado <- gBuffer(vectorizado, width=0, byid = T)
          print("generando buffer para solucionar problemas de geometria")
          gIsValid(vectorizado, reason = T)


          writeOGR(vectorizado, layer = paste(nombre," vectorizado",sep=""), dsn="vectorizado R", driver="ESRI Shapefile",overwrite_layer=TRUE)
          print("ACA ARMO EL PDF")   
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


          plot(vectorizado["output"], col=cols, border="NA",main=paste("Mapa de Rendimiento",nombre,sep=" "))
          max<-length(cortes)
          legend("topleft",as.character(round(unname(cortes[1:max]),digits=2)),fill=cols,cex = 0.8,bty="n",title=unidad_de_cosecha)
          mtext(detalleslote,side=1,cex=1)
          mtext("Desarrollodo en INTA Bordenave - @FrancoFrolla",side=4,cex=0.5)

          plot(h, col=k,main=paste("Histograma rendimiento"),xlab=paste("Rendimiento", unidad_de_cosecha),ylab="Frecuencia")
          mtext("Desarrollodo en INTA Bordenave - @FrancoFrolla",side=4,cex=0.5)

          dev.off()
          print("ACA ARMO EL KML")   
          hacerkml<-function(){

            obj <- spTransform(vectorizado, CRS("+init=epsg:4326"))
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
            #ACTIVAR PARA SACAR LOS MULTYPOLYGON Y SILENCIAR LOS DE ABAJO
            #prefijo<-c('<Placemark><name>30</name><styleUrl>#1</styleUrl><Polygon>') 
            #sufijo<-c('</Polygon></Placemark>')
            fin<-c('</Document></kml>')



            #prefijo<-c('<Placemark><name>1</name><styleUrl>#1</styleUrl><MultiGeometry>') 
            sufijo<-c('</MultiGeometry></Placemark>')
            prefijo.poligono<-c('<Polygon>') 
            sufijo.poligono<-c('</Polygon>')

            for (a in 1:length(obj@polygons)){

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
   
Output <- sf::st_as_sf(vectorizado)
