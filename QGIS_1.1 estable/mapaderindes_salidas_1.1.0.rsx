##Raster=raster
##Poligono=vector polygon
##Output=output vector
##Carpeta=folder
##Nombre=string Lote prueba 1
##Unidad_de_medicion=string kg/ha
##Comentarios=string Poner alguna informacion importante...
#Version_simplificada=string no
##Escala=selection quantiles(7);quantiles(4);15cm (15-120)


##output_plots_to_html
##showplots


nombre<-Nombre
detalleslote <-Comentarios
unidad_de_cosecha<-Unidad_de_medicion
#version_simplificada<-Escala

if(Escala == 0){version_simplificada = "no"}
if(Escala == 1){version_simplificada = "si"}
if(Escala == 2){version_simplificada = "ni"}



poligono <- as_Spatial(Poligono)


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

print(Carpeta)
setwd(Carpeta)
Kg_wls<-as(Raster, "SpatialPixelsDataFrame")
valores<-na.omit(Kg_wls@data[,1])
print(str(Kg_wls))
print(paste("Los archvos seran guardados en:", getwd()))
   


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
		  
		  
	  
		  
		  
		  
	  #Recorto al area del lote....	
	  r <- mask(Raster, poligono)
	
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
	 
	 >print(sizes)
	 
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
				  >print(paste(obj$output[a],estilo.id))

	
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
   
Output <- sf::st_as_sf(vectorizado)
