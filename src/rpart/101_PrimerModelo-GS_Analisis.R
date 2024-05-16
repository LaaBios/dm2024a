# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/Users/ayrtonvignolo/ITBA/DataMining") # Establezco el Working Directory

file = "OutArbol_K101_2005"

# cargo el dataset
dataset <- fread("./datasets/GS_Analisis_AYRTON.csv")


# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "ganancia_promedio ~ .",
        data = dataset, # los datos donde voy a entrenar
        xval = 0,
        cp = -1, # esto significa no limitar la complejidad de los splits
        minsplit = 100, # minima cantidad de registros para que se haga el split (0-100)
        minbucket = 50, # tamaño minimo de una hoja (entre 0 y la mitad de minsplit)
        maxdepth = 8 #(entre 1 a 30)
) # profundidad maxima del arbol
#los parametros son para el lenguaje, los hiperparametros son los nros que definen adentro del modelo

# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)




# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

pdf(paste0("/Users/ayrtonvignolo/ITBA/DataMining/exp/KA2001/",file, ".pdf"))


prp(modelo,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)


