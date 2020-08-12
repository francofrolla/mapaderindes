##Ambientación = group
##Raster=raster
##Ambientes=vector polygon
##Zonas=field Ambientes
##muestras=number 300
##output_plots_to_html


if("nlme" %in% rownames(installed.packages()) == FALSE) {install.packages("nlme")} else {print("nlme ya esta..")}
if("multcomp" %in% rownames(installed.packages()) == FALSE) {install.packages("multcomp")} else {print("multcomp ya esta..")}
if("multcompView" %in% rownames(installed.packages()) == FALSE) {install.packages("multcompView")} else {print("multcompView ya esta..")}
if("lsmeans" %in% rownames(installed.packages()) == FALSE) {install.packages("lsmeans")} else {print("lsmeans ya esta..")}

require(sf)
require(nlme)
require(multcomp)
require(multcompView)
require(lsmeans)



Ambientes <- as_Spatial(Ambientes)

crs<-proj4string(Ambientes) 
rendimiento<-as(Raster, "SpatialPixelsDataFrame")
colnames(rendimiento@data)<-c("rinde")

str(rendimiento@data)
z<-rendimiento@data["rinde"]
x<-rendimiento@coords[,1]
y<-rendimiento@coords[,2]

print(length(rendimiento@data))
rendimiento <- data.frame(x,y,z)

rendimiento <-rendimiento[sample(nrow(rendimiento),as.integer(muestras)),]
coordinates(rendimiento)<-c("x","y") 

proj4string(rendimiento) <- crs
proj4string(Ambientes) <- crs

muestra<-cbind(rendimiento@data,rendimiento@coords[,1],rendimiento@coords[,2],over(rendimiento,Ambientes[,Zonas]))
muestra<-data.frame(muestra)


colnames(muestra)<-c("rinde","X","Y","Zona")

colnames(muestra)

muestra$Zona<-as.factor(muestra$Zona)
muestra<-na.omit(muestra)

# Model with exponencial spatial correlation
mod1_Wy <-gls(rinde~1+Zona
              ,correlation=corExp(form=~as.numeric(as.character(X))+as.numeric(as.character(Y))
                                  ,metric="euclidean"
                                  ,nugget=FALSE)
              ,method="REML"
              ,na.action=na.omit
              ,data=muestra)
# Model with exponential spatial correlation and nugget effect
mod2_Wy <-gls(rinde~1+Zona
              ,correlation=corExp(form=~as.numeric(as.character(X))+as.numeric(as.character(Y))
                                  ,metric="euclidean"
                                  ,nugget=TRUE)
              ,method="REML"
              ,na.action=na.omit
              ,data=muestra)

# AModel with spherical spatial correlation
mod3_Wy <-gls(rinde~1+Zona
              ,correlation=corSpher(form=~as.numeric(as.character(X))+as.numeric(as.character(Y))
                                    ,metric="euclidean"
                                    ,nugget=FALSE)
              ,method="REML"
              ,na.action=na.omit
              ,data=muestra)

# Model with spherical spatial correlation and nugget effect
mod4_Wy <-gls(rinde~1+Zona
              ,correlation=corSpher(form=~as.numeric(as.character(X))+as.numeric(as.character(Y))
                                    ,metric="euclidean"
                                    ,nugget=TRUE)
              ,method="REML"
              ,na.action=na.omit
              ,data=muestra)

# Model of independent errors
mod5_Wy <-gls(rinde~1+Zona
              ,method="REML"
              ,na.action=na.omit
              ,data=muestra)

# Selecting spatial correlation model using the Akaike information criterion
AICmod1_Wy <- AIC(mod1_Wy)
AICmod2_Wy <- AIC(mod2_Wy)
AICmod3_Wy <- AIC(mod3_Wy)
AICmod4_Wy <- AIC(mod4_Wy)
AICmod5_Wy <- AIC(mod5_Wy)

modelos<-c("mod1_Wy","mod2_Wy","mod3_Wy","mod4_Wy","mod5_Wy")
criterio<-c(AICmod1_Wy,AICmod2_Wy,AICmod3_Wy,AICmod4_Wy,AICmod5_Wy)


modelofinal<-AICmod1_Wy
indicefinal <- 1

for (i in 1:5){
  if(criterio[i] < modelofinal){indicefinal = i}
}

summary(get(modelos[indicefinal]))
#Wymeans <- summary(lsmeans(get(modelos[indicefinal]),"Zone")); Wymeans

require(lsmeans)
modelo.lm<-lsmeans(get(modelos[indicefinal]),"Zona")
Wymeans <- summary(modelo.lm)

require(multcompView)
require(multcomp)

valores<-cld(modelo.lm, Letters = c("a","b","c","d","e"),sort=TRUE,reversed=TRUE)
letras<- c(valores$.group)
#letras<-letras[length(letras):1]


Wymean <-by(Wymeans$lsmean,Wymeans$Zona,mean)
Wyse<-by(Wymeans$SE,Wymeans$Zona,mean)
wy <- barplot(Wymean,xlab="Zonas de manejo", ylab="Rendimiento",
              ,ylim=c(0,max(max(Wymean)+max(Wymean)*0.1)),xpd=F)

letters = letras
text(x=wy,y=Wymean+Wymean*0.05,label=letters,cex = 1)
