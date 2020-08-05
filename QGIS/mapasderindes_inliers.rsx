##Shape=vector point
##Rinde=field Shape
##output_plots_to_html
##Distancia=number 20
##showplots
##inliers=output vector

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




library(sp)
library(spdep)

crs_imagen<-st_crs(Shape)
filtro1 <- as_Spatial(Shape)

cord <- filtro1@coords
gri <- dnearneigh(cord,0,Distancia)
lw <- nb2listw(gri, style = "W",zero.policy=TRUE)
par(mfrow=c(1,1))
 
ML <- localmoran (filtro1@data[,Rinde], lw, p.adjust.method="bonferroni",alternative ="less",zero.policy=TRUE)
MP <- moran.plot2(filtro1@data[,Rinde],lw,quiet=T,labels=F,col=3,zero.policy=T,xlab="Rendimiento", ylab="Rendimiento Spatially Lagged")
Influ <- MP$is.inf 
datos0 <- data.frame(filtro1@data,filtro1@coords,ML,Influ)
 
#eliminacion de datos con ?ndice de Moran Local negativo y estad?sticamente significativos (p<0.05).
datos1 <- subset(datos0,datos0$Ii > 0 | datos0$Pr.z...0.>0.05)
myshp.inlier<- subset(datos0,datos0$Ii < 0 | datos0$Pr.z...0.<0.05)

datos2 <- datos1[datos1$dfb.1_ == FALSE & datos1$dfb.x == FALSE
                   & datos1$dffit == FALSE & datos1$cov.r == FALSE & datos1$cook.d
                   == FALSE & datos1$hat == FALSE, ]

>print("--------------------------------------------------------")
>str(datos2)

datos3<-data.frame(datos2[Rinde],datos2["coords.x1"],datos2["coords.x2"])  
coordinates(datos2)<-c("coords.x1","coords.x2")
output <-sf::st_as_sf(datos3, coords = c("coords.x1", "coords.x2"),crs= crs_imagen)

n<-nrow(filtro1@data[Rinde])-nrow(datos3[Rinde])
>print(paste("Se filtraron",n,"puntos"))


inliers = output
