rm(list = ls()); gc()

Sys.setlocale(category = "LC_ALL", locale = "English_USA")
options(error = NULL)

setwd("C:/Users/Edwin.Mejia/Documents/Danilo Mejia/Proyectos/CEN")
source("Scripts/utilities.R")

# source("utilities.R")
# initialize(getwd())

setWorkspace(getwd())

suppressWarnings(loadLibraries())

#espacio_trabajo("Seguimiento-20210903")

source("Scripts/funciones.R")
carga_limpieza()
