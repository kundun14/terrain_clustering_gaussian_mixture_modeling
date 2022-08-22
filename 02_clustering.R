# data transformer and Gaussian mixture models (GMM) estimation
# Finally, the model with the highest BIC value is selected.

library("mclust") # GMM clus
library("future") # parallelize training
library("recipes") # pipeline


data <- readRDS("data/sample.rds") # Los puntos muestreados en el script 01_extract

dplyr::glimpse(data)

# transform data, standarizacion usando tranformacion en potencia de YEO - Johnson
# creamos un recipe (pipeline)


recipe_  <- recipes::recipe( ~ ., data = data) %>%  # recipe basica para todas las varibales
  recipes::step_YeoJohnson(recipes::all_numeric(), -FLAT) %>%  # step YEOJohn... menos a FLAT
  recipes::step_normalize(recipes::all_numeric()) # steps standarizar

#summary del pipeline
recipes::tidy(recipe_)

#estimaciones
# estimate the required parameters del pipeline (recipe) para aplicarlso a la data training
#aun no es la data transformada
estimates <- recipes::prep(recipe_, training = data, retain = FALSE)


saveRDS(estimates, "data/transformator.rds") # guarda los parametros de la transformacion
data_trans <- recipes::bake(estimates, data, composition = "data.frame") # aplica los parametros de estimates a nueva data
rm(data, recipe_) # elimina recipez_ y data del enviroment



# parallel clustering


clusters = c(5, 10, 20) # 12:20 # specify the number of clusters, solo tres configuraciones para probar el codigo
ncores = parallelly::availableCores(omit = 1) # 3 cores

if (length(clusters) < ncores) ncores = length(clusters) # si se tiene mas cores que clusters, se usan cores iguales al numero de clusters
future::plan(multisession, workers = ncores) #it specifies how future():s are resolved, e.g. sequentially or in parallel.



# definir la funcion mclust_par en el enviroment local

mclust_par = local(
  function(data, G) {
    mdl = Mclust(data = data, G = G) # mclust::Mclust ---> GMM
    # G es el numero de clusters, es un vector numerico, eg 1:9, para cada uno calcula un BIC

    # reduce model size
    mdl$data = mdl$data[0, ] # remove original data
    mdl$z = mdl$z[0, ] # remove probability
    mdl$classification = as.integer(mdl$classification)
    return(mdl)
  }
)

# APLICA LA FUNCION Y LA GUARDA EN UNA LISTA ELEMENT-WISE

results = vector("list", length(clusters)) # crear un lista con 9 elementos ( 9 modelos GMM), es un futuro

for (i in seq_along(clusters)) {
  results[[i]] = future::future(mclust_par(data_trans, clusters[i]), # la configuracion se determino en future::plan()
                                seed = 123L)
}

results = future::value(results) #extrae los valores de cada modelo?????

# seleccion del mejor modelo ---> BIC

best = which.max(sapply(results, function(x) {x$bic})) # selecciona el modelo con el BIC mayor
results = results[[best]] # asigna el mejor modeo a los resultados

summary(results)
# ----------------------------------------------------
#   Gaussian finite mixture model fitted by EM algorithm
# ----------------------------------------------------
#
#   Mclust VEV (ellipsoidal, equal shape) model with 10 components:
#
#   log-likelihood    n  df       BIC       ICL
# -23536.38 3304 578 -51756.23 -52753.74
#
# Clustering table:
#   1   2   3   4   5   6   7   8   9  10
# 353 541 335 299 305 327 460 173 356 15


saveRDS(results, "data/GMM_model.rds") # guardar el mejor modelo
