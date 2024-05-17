# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/Users/ayrtonvignolo/ITBA/DataMining") # Establezco el Working Directory

file = "OutArbol_K101_4001"

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dtrain[, ctrx_quarter_rank := frankv(ctrx_quarter)/.N] # para convertir en ranking, y la .N para normalizar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo



# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "clase_ternaria ~ . - ctrx_quarter",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.100409235573381, # esto significa no limitar la complejidad de los splits
        minsplit = 1215, # minima cantidad de registros para que se haga el split (0-100)
        minbucket = 601, # tamaño minimo de una hoja (entre 0 y la mitad de minsplit)
        maxdepth = 15 #(entre 1 a 30)
) # profundidad maxima del arbol
#los parametros son para el lenguaje, los hiperparametros son los nros que definen adentro del modelo

# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)



# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

pdf(paste0("/Users/ayrtonvignolo/ITBA/DataMining/exp/KA2001/",file, ".pdf"))


prp(modelo,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)

dev.off()

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_4001.csv",
        sep = ","
)
