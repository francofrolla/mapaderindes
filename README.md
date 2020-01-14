# mapaderindes
Una coleccion de funciones en R para filtrar puntos de rendimiento y generar mapas interpolados por el metodo de Kriging. Exportando los mapas a pdf, shp, kml y tiff. 

Prueba en vivo sobre Colab (requiere cuenta de Google)
https://colab.research.google.com/drive/11BpKhk6nR8FTrWzlTLqf2RVvQolwKAoe

# mapaderindes/QGIS
Mismas funciones pero ajustadas para su uso en QGIS mediante el complemento "Processing R Provider" (https://plugins.qgis.org/plugins/processing_r/). El calculo del modelo del semivariograma fue automatizado para facilitar el uso del programa. Se ajusta un modelo por la siguiente función.

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
