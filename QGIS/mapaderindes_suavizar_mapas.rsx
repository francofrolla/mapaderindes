##Raster= raster
##Suavizado=output raster
##output_plots_to_html
##showplots
##type=selection 3x3;6x6;9x9;13x13
##type1=selection media;mediana;maximo;minimo;moda




library(raster)

if(type==0){celda <- 3}
if(type==1){celda <- 6}
if(type==2){celda <- 9}
if(type==3){celda <- 13}

print(paste(celda,"x",celda))
ventana<-matrix(1,nrow=celda,ncol=celda)

if(type1==0){suaveraster<-focal(Raster[[1]],w=ventana,fun=mean,na.rm=TRUE); print("Se aplico la media")}
if(type1==1){suaveraster<-focal(Raster[[1]],w=ventana,fun=median,na.rm=TRUE); print("Se aplico la mediana")}
if(type1==2){suaveraster<-focal(Raster[[1]],w=ventana,fun=max,na.rm=TRUE); print("Se aplico la maxima")}
if(type1==3){suaveraster<-focal(Raster[[1]],w=ventana,fun=min,na.rm=TRUE); print("Se aplico la minima")}
if(type1==4){suaveraster<-focal(Raster[[1]],w=ventana,fun=mode,na.rm=TRUE); print("Se aplico la moda")}



   
   
Suavizado <- suaveraster
