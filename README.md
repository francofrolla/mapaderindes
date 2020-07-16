# mapaderindes
Una coleccion de funciones en R para filtrar puntos de rendimiento y generar mapas interpolados por el metodo de Kriging. Exportando los mapas a pdf, shp, kml y tiff. 

Prueba en vivo sobre Colab (requiere cuenta de Google)
https://colab.research.google.com/drive/1ffs1YmgtqWQkEfvTiF82sVS1fSU6PfQg

# mapaderindes/QGIS
Mismas funciones pero ajustadas para su uso en QGIS mediante el complemento "Processing R Provider" (https://plugins.qgis.org/plugins/processing_r/). 

# mapaderindes/QGIS en WINDOWS.
probado bajo Qgis 3.10, R 4.02 y Proccesing R provider 2.0 (el 2.2 marca error).
Proccesing R provider 2.0 se encuentra en este repositorio para descarga directa.

para instalarlo desde QGIS> plugins> instalar desde ZIP > processing_r-2.0.0.zip

# mapaderindes/datosdeprueba
Conjunto de datos de prueba probados en:

Qgis 3.4, Ubuntu 18.04,R 4.02, R proccesing 2.0 

Qgis 3.10, Windows 10, R 4.02, R proccesing 2.0 






El calculo del modelo del semivariograma fue automatizado para facilitar el uso del programa. Se ajusta un modelo por la siguiente función.

```R
error1 = 9999
modelo_final = "aaa"
seleccion_modelo<-function(){
	modelos<-c("Sph","Exp","Lin","Gau","Ste","Mat")
		for (i in 1:length(modelos)){
                print(i)
		modelovgm<- fit.variogram(semivariograma, fit.method=1, vgm(sill,modelos[i],distancia,nugget))
	 	error<-(attr(modelovgm , 'SSErr'))
			if(error < error1){
                        modelo_final <<- modelos[i]
		        error1<-error
 			print(modelo_final)	
			print(error1)	
                        }
            }
}
```
Todos los datos ingresados en QGIS tienen que estar reproyectadas a coordenas planas. No funcionan con coordendas geograficas.

Tw @FrancoFrolla
email: frolla.franco@inta.gob.ar
