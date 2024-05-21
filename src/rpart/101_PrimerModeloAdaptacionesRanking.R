# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
require("tidyverse")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/Users/ayrtonvignolo/ITBA/DataMining") # Establezco el Working Directory

# en varSacar incluir las variables a convertirse en rankeadas y quitarse de la fórmula
varSacar = c("ctrx_quarter", "mcuentas_saldo")

# calcula el nombre de las nuevas agregándole _rank al final
varAgregar = paste0(varSacar, "_rank")

# concateno los elementos del vector para darle nombre al archivo
file = paste0("KTest_Rankeada_", paste(varSacar, collapse = "_"))

# cargo el dataset. Verificar el path
dataset <- fread("./datasets/dataset_pequeno.csv")

# agrego las columnas rank
dataset[, c(varAgregar) := lapply(varSacar, function(x) frankv(get(x)) /.N)]

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables

# calculo la fornmula del modelo quitando las columnas viejas y dejando las nuevas rankeadas
columnasModelo = setdiff(names(dataset), c(varSacar, "clase_ternaria"))
formulaModelo = paste0("clase_ternaria ~ ", paste(columnasModelo, collapse = " + "))
modelo <- rpart(
        formula = formulaModelo,
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -1, # esto significa no limitar la complejidad de los splits
        minsplit = 250, # minima cantidad de registros para que se haga el split
        minbucket = 20, # tamaño minimo de una hoja
        maxdepth = 6 # profundidad maxima del arbol
) # profundidad maxima del arbol


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

# en este path genera el pdf
pdf(paste0('/Users/ayrtonvignolo/ITBA/DataMining', file, '.pdf'))

prp(modelo,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)

dev.off()

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = paste0("./exp/KA2001/", file, ".csv"),
        sep = ","
)

