
#generate a random sample from the rasters
# todos los raster se derivan de una DEM a resolucion data por el analista o disponible
# los rasters fueron derivados de GRASS GIS
# QUE ALGORITMOS?


library("sf")
library("corrr")
library("stars")


ras_path = list.files("rasters", pattern = "\\.tif$", full.names = TRUE) # direccion de los  files raster geomorfometricos de GRASS GIS
rasters = stars::read_stars(ras_path, proxy = TRUE) # cargarlos como rasters

varnames = basename(ras_path) # nombres cortos de los rasters stars_proxy object STACK
varnames = substr(varnames, 1, nchar(varnames) - 4) # recortar el nombre completo
varnames = substr(varnames, 4, nchar(varnames)) #"ELEV" "EBAS" "RESE" "LTPI" "RELF" "SPOS" "SNIS" "MCON" "RUGN" "FLAT"
names(rasters) = varnames # asignar nombres a los rasterlayers

# rasters$ELEV # ruta completa del raster


# subsampling

n = 5000 # 5000000 # considerar solo 5000 puntos para probar el codigo
set.seed(123)

vals = sf::st_sample(rasters, n) #  estadisticos para n puntos muestrales de cada raster
vals = as.data.frame(vals)[, -c(1:2)] # dataframe de valores para cada raster decada punto muestreado

complete_idx = which(complete.cases(vals)) # indices de los puntos validos (eliminar NAS)
vals = vals[complete_idx, ] # solo considerar los puntos sin Nas
rm(complete_idx) # remueve el objeto  complete_idx.... ya no es necesario
rownames(vals) = NULL # reseta los indices indices

# correlation
cor = corrr::correlate(vals) # correlacion entre todas las variables ,  "pearson" (default),
corrr::rplot(cor, print_cor = TRUE) # grafica

#guardar puntos muestreados

if (!dir.exists("data")) dir.create("data")
saveRDS(vals, "data/sample.rds")
