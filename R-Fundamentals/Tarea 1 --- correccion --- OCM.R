#########################################################################
#                                                                       #
#      Tarea 1: Tema 1 - - - Ambiente general del R Studio - - -        #
#                                                                       #
#########################################################################

#######################################################################################
#  1. Volver a re leer el curso “Tema 1 - - - Ambiente general del R Studio - - -”.   #
#######################################################################################

# Listo 

#####################################################################################################
#  2. Todo el código expresado en el archivo “Tema 1 - - - ... compilar todos los elementos vistos  #
#####################################################################################################

# Listo

###########################################################
#  3.	Determine la ubicación al iniciar una sesión de R.  #
###########################################################

getwd()

  ####################################################################################
# 4.	Cambiar la ubicación a otra (donde vaya a poner los documentos del Tema 1).  #
####################################################################################

setwd("C:/Users/oscar/Desktop/R ---Introducción/Tema 2")
getwd()

#######################################
# 5.	Modifique el ENCODING a UTF-8.  #
#######################################

# File --> Reopen with Encoding 
# y
# File --> Save with Encoding

##########################################################################
# 6. Modifique el ide a otro más oscuro (ver todas las opciones además)  #
##########################################################################

# Tools --> Global Opcions... --> Appearance 

########################################################################################
# 7.	Descargue en forma de “click” las siguientes librerías: “abe”, “bacr” y “Cairo”. #
########################################################################################

# Packages --> Install --> "abe"
# Packages --> Install --> "bacr"
# Packages --> Install --> "Cairo#

##################################################################################
# 8.  Descargue en forma de código las librerías “d3plus”, “eaf” y “factoextra”. #
##################################################################################

install.packages("d3plus")
install.packages("eaf")
install.packages("factoextra")

################################################################################################################
# 9.	Cree 2 operaciones para: suma, resta, multiplicación, división, exponencial, resto, división entera,     #
#     exponencial, raíz cuadrada, y logaritmo.                                                                 #
################################################################################################################ 

# Suma 

1+2
3+4

# resta

1-2
3-4

# multiplicación

1*2
3*4

# división

1/2
3/4

# exponencial 

exp(1)
exp(2)

# resto = Dividendo - (divisor x cociente)

2%%1
4%%3

# división entera

2%/%1
10%/%4

# raíz cuadrada 

sqrt(4)
sqrt(9)

# logaritmo 

log(1)
log(2.718282)

###############################################
# 10.	Cree un total de 5 operaciones lógicas. #
###############################################

10 > 4
10 < 4
104 >= 75
(2) == (1+1)
(4*4) != 16


#######################################################
# 11.	Cree un total de 5 secuencias + 5 repeticiones. #
#######################################################

# secuencias

seq(from=1, to=10, by=1) 

# repeticiones 

rep(1,10)  

# y así 

###########################################################################
# 12.	Realice 2 asignaciones para: números, caracteres y números lógicos. #
###########################################################################

# números

numero1 <- 30
numero2 <- 40

# caracteres

caracter1 <- "Kaysa"
caracter2 <- 'Ippo'

# Lógico 

Oscar <- TRUE
Steven <- FALSE

###########################################################################
# 13.	Cree un escalar de miles (1 000) y otro de millones (1 000 000).    #
###########################################################################

escalar1 <- 1000
escalar2 <- 1000000

####################################################################################################################
# 14.	Cree un vector con el nombre de sus familiares y otro con la edad. Una esos dos vectores de forma vertical.  #
####################################################################################################################

nombre <- c("Mami", "Erick", "Kaysa", "Ippo", "Viviana", "Daniela")
edad <- c(50,30,7,4,28,29)

# union vertical

union_v <- cbind(nombre,edad)
union_v

# union horizontal

union_h <- rbind(nombre,edad)

###############################################################################################################################
# 15.	Cree una matriz 3*5 con cualquier número entero que desee. Luego ubique los valores según [1,4] y [3,2]. ¿Qué obtuvo?   #
###############################################################################################################################

M <- matrix(data = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),# los datos de la matriz
             nrow = 3, # numero de filas
             ncol = 5, # numero de columnas
             byrow = FALSE)

M[1,4]

M[3,2]

###############################################################################################################################
# 16.	Cree un data frame con las siguientes columnas: nombre, apellido 1, apellido 2, edad, peso, estatura, salario,          #
#     y años de trabajo. Ud debe inventar un total de 10 valores para cada columna. Calcule la suma y el promedio de todas    #
#     las variables numéricas.                                                                                                #
###############################################################################################################################

datos <- data.frame( Nombre = c("Steven","Ana Lucia","Zeidy","Carlos","Oscar","Erick","Daniela","Viviana","Joseph","Maria"),
                     Apellido1 = c("Wu","Hernández","Chávez","Wu","Centeno","Mora","Rodríguez","Pérez","Arrieta","Solís"),
                     Apellido2 = c("Solís","Arrieta","Pérez","Rodríguez","Mora","Centeno","Wu","Chávez","Hernández","Chang"),
                     Edad = c(32,33,43,54,54,52,23,43,56,34),
                     Peso = c(54,64,67,78,87,77,65,74,65,86),
                     Estatura = c(180,156,167,154,187,168,197,176,187,175),
                     Salario = c(34234,3423465,6657657,35457,78678,45467,78567856,6756756,5656,563767),
                     `Anos.trabajo` = c(34,12,34,12,14,13,23,8,9,10)
  
)

tail(datos)

str(datos)

### Sumas ###

sumas1 <- rbind(sum(datos$Edad),
               sum(datos$Peso),
               sum(datos$Estatura),
               sum(datos$Salario),
               sum(datos$`Anos.trabajo`) 
               )

sumas2 <- cbind(sum(datos$Edad),
                sum(datos$Peso),
                sum(datos$Estatura),
                sum(datos$Salario),
                sum(datos$`Anos.trabajo`) 
)


sumas1 ; sumas2


### promedios ###

promedios1 <- rbind(mean(datos$Edad),
                    mean(datos$Peso),
                    mean(datos$Estatura),
                    mean(datos$Salario),
                    mean(datos$`Anos.trabajo`) 
)

promedios2 <- cbind(mean(datos$Edad),
                    mean(datos$Peso),
                    mean(datos$Estatura),
                    mean(datos$Salario),
                    mean(datos$`Anos.trabajo`) 
)

promedios1 ; promedios2

###############################################################################################################################
# 17.	Junte los vectores, matrices, escalares y data frame creados en una lista (tal vez uno de cada tipo en la lista).      #
###############################################################################################################################


lista <- list(numero1, numero2, Oscar, Steven,escalar1, escalar2,nombre, edad, M, datos)
lista

