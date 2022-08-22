library("sf")
library("stars")
library("mclust")
library("recipes")
source("code/utils/spatial_predict.R")


ras_path = list.files("rasters", pattern = "\\.tif$", full.names = TRUE) # cargar rasters (covariables?)

varnames = basename(ras_path)
varnames = substr(varnames, 1, nchar(varnames) - 4)
varnames = substr(varnames, 4, nchar(varnames))

# create virtual raster with lower resolution
tmp = tempfile(fileext = ".vrt")
resolution = c("-tr", 1000, 1000)
resample = c("-r", "nearest")

#creamos el raster virtual de baja resolucion

sf::gdal_utils(util = "buildvrt", source = ras_path, destination = tmp,
               options = c(resolution, resample, "-separate"))

rasters = stars::read_stars(tmp, proxy = FALSE)
rasters = stars::st_set_dimensions(rasters, 3, values = varnames, names = "var")
rasters = split(rasters)

# mejor modelo GMM estimado (mayor bic)

mdl = readRDS("data/GMM_model.rds")

#pipeline covariables

transformator = readRDS("data/transformator.rds")

# FUNCION DE PREDICCION ESPACIAL "code/utils/spatial_predict.R")
# generalmente la prediccion toma mucho tiempo y requiero muchos recursos por que es
# inensiva en el dominio

result = spatial_predict(rasters, transformator, mdl) # raster cluster es una vraiable categorica

# PLOT  MAPA DE CLUSTERS Y DE INCERTIDUMBRES

plot(result["cluster"], col = sf.colors(n = mdl$G, categorical = TRUE))
plot(result["uncertainty"], breaks = "equal")

#GUARDAR MAPAS
stars::write_stars(result["cluster"], "data/clusters_10.tif",
                   options = "COMPRESS=LZW", type = "Byte", NA_value = 0)
stars::write_stars(result["uncertainty"], "data/uncertainty_10.tif",
                   options = "COMPRESS=LZW", type = "Float32", NA_value = 999)
