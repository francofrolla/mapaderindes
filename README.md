# mapaderindes
Una coleccion de funciones en R para filtrar puntos de rendimiento y generar mapas interpolados por el metodo de Kriging. Exportando los mapas a pdf, shp, kml y tiff. 

### Ultimo video subido a YouTube con el paso a paso:
https://youtu.be/fz-wO6WUr1w
### Ultima versión:
#### QGIS 1.2
### Versión estable:
#### QGIS 1.1
### Versión antigua:
#### QGIS 

# mapaderindes/QGIS
Funciones ajustadas para su uso en QGIS mediante el complemento "Processing R Provider" (https://plugins.qgis.org/plugins/processing_r/). 

# mapaderindes/QGIS en WINDOWS.
probado bajo Qgis 3.10, R 4.02 y Proccesing R provider 2.0 (el 2.2 marca error).
Proccesing R provider 2.0 se encuentra en este repositorio para descarga directa.

para instalarlo desde QGIS> plugins> instalar desde ZIP > processing_r-2.0.0.zip

# mapaderindes/datosdeprueba
Conjunto de datos de prueba probados en:

Qgis 3.4, Ubuntu 18.04,R 4.02, R proccesing 2.0 
Qgis 3.10, Windows 10, R 4.02, R proccesing 2.0 

# Requisitos para hacer los mapas:

1-Todos los datos ingresados en QGIS tienen que estar reproyectadas a coordenas planas. No funcionan con coordendas geograficas. <br />
2-El shapefile del mapa de rendimiento tiene que ser de Geometria: *Point*, no se soporta *Multipoint*. En Qgis, sobre la capa de puntos, Propiedades, Información, Geometria.
En caso de ser una capa *Multipoint*, usar la herramienta *Multiparte a monoparte* <br />
3-El nombre que identifica la columna de rendimiento no tiene que tener simbolos raros (%,%,/,!) o espacios. Usar palabras simples como Rendimiento, Masa_seca, rinde, etc. <br />
4-La geometria no debe incluir dimension Z (PointZ, PolygonZ no son soportados) <br />


# email: frolla.franco@inta.gob.ar


Prueba en vivo sobre Colab (requiere cuenta de Google)
https://colab.research.google.com/drive/1ffs1YmgtqWQkEfvTiF82sVS1fSU6PfQg
