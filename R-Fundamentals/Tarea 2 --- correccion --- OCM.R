############################################################################################################
#                                                                                                          #
#                                                                                                          #
#             Tarea 2: Tema 2 - - - Importar, exportar e inspeccionar el archivo de daos - - -             #
#                                                                                                          #
#                                                                                                          #   
############################################################################################################

#############
# Librerías #
#############

library(readxl)
library(readr)
library(writexl)


#############################################################################################################
# 1.  Volver a re leer el curso “Tema 2 - - - Importar, exportar e inspeccionar el archivo de daos - - -”   #
#############################################################################################################

# Listo #

####################################################################################################################################
# 2.  Todo el código expresado en el tema “Tema 2 ---- .... ”  traducirlo a un Script de R, y compilar todos los elementos vistos. #
####################################################################################################################################

# Listo #

#########################################################
# 3. Determine la ubicación al iniciar una sesión de R. #
#########################################################

setwd("C:/Users/oscar/Desktop/R --- SAF/Tema 2")

####################################################################################
# 4. Verificar la pestaña de “Files” para corroborar todos los archivos que posee. #
####################################################################################

# Listo #

###############################################################################################################################
# 5. Realice la importación por código de los archivos “Gastos_mensuales-2020-07-21”. Nótese que debe llevar a cabo 3 tipos   #
#     de exportaciones según las extensiones, y debe buscar cómo importar un archivo .txt                                     # 
###############################################################################################################################

# a. Importación Excel

datos1 <- read_excel("Gastos_mensuales-2020-07-21.xlsx")

# b. Importación .csv

datos2 <- read.csv2("Gastos_mensuales-2020-07-21.csv", header = TRUE, sep = ";") # espacial para separadores por ";"

# c. Importación .txt

datos3 <- read_tsv("Gastos_mensuales-2020-07-21.txt") # atención al str() de datos3
 
##############################################################################################################################
# 6. Realice la importación por ide de los archivos “Gastos_mensuales-2020-07-21”. Nótese que debe llevar a cabo 3 tipos     #
#    de exportaciones según las extensiones, y debe buscar cómo importar un archivo .txt.                                    #
##############################################################################################################################

# a. Importación Excel

## File --> Import Dataset --> From Excel --> Seleccionar en Browse --> Import 

# b. Importación .csv

## File --> Import Dataset --> From text(readr) --> Seleccionar en Browse --> Delimiter "semi colom"  --> Import 

# c. Importación .txt

## File --> Import Dataset --> From text(readr) --> Seleccionar en Browse --> Delimiter "tab" 

##############################################################################################################################
# 7.  Realice la inspección general del archivo “Gastos_mensuales-2020-07-21”. Determine: la estructura, dimensión, total de #
#  líneas, total de columnas y nombre de las variables.                                                                      #
##############################################################################################################################

# Estructura 

str(datos1)

# dimensión 

dim(datos1)

# total filas

nrow(datos1)

# total columnas

ncol(datos1)

# nombre de variables

names(datos1)

#############################################################################################################################
# 8. Realice la inspección específica del archivo “Gastos_mensuales-2020-07-21”. Determine: la clase del archivo de datos,  #
#    clase de todas las variables, además realice 3 inspecciones por columna, fila y de forma conjunta.                     #
#############################################################################################################################

attach(datos1)

# Clase del archivo de datos

class(datos1)

# Clase de la variable Octubre


class(datos1$Octubre)

# Datos para Septiembre y Octubre

datos1[,c("Septiembre","Octubre")]

# Y así consecutivamente ....


#####################################################################################################################################
# 9. Realice la exportación del archivo “Gastos_mensuales-2020-07-21”. Para esto debe exportarlo bajo el nombre de “exportación”,   #
#    bajo las extensiones de .xlsx, .csv y .txt. Nótese que la exportación de la extensión .txt debe buscarse a parte.              #
#####################################################################################################################################

# a. Expotar a  Excel

write_xlsx(datos1, "export_1_tarea.xlsx")

# b. Expotar a .csv

write_xlsx(datos1, "export_2_tarea.csv")

# c. Expotar a .txt

# delim ;

write_delim(datos1, "export_3_tarea.txt", delim=";")

# delim tab 

write_tsv(datos1, "export_4_tarea.txt")



