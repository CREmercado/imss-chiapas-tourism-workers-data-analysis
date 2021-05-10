#CÓDIGO

#Instalación de paquete dplyr
install.packages("dplyr")

#Carga de csv general
asg <- read.csv(file = '/Users/christian/Desktop/asg-2020-03-31_0.csv', header = TRUE, sep = "|")

#Descripción de la base de datos general
nrow(asg) #núm. de registros
ncol(asg) #núm.de variables
dimnames(asg) #observar valores y variables
names(asg) #nombre de las variables
head(asg) #datos en las primeras líeas
tail(asg) #datos en las últimas líeas
summary(asg) #información csv
str(asg) #información por varibale

#Correlaciones
cor(asg[ , purrr::map_lgl(asg, is.numeric)])


#OPCIÓN 1
# Estado 7: Chiapas (asg$cve_delegacion)
# Sector económico: Preparación y servicio de alimentos 
#	sector_economico_1: 8, Servicios para empresas, personas y el hogar (sector_economico_1)
#	sector_economico_2: 87, Preparación y servicio de alimentos y bebidas (sector_economico_2)
#	sector_economico_4: 8701, Preparación y servicio de alimentos (sector_economico_4)

#Creación del arreglo
asgChi <- filter(asg,  asg$cve_delegacion == 7 & asg$sector_economico_1 == 8 & asg$sector_economico_2 == 87 & asg$sector_economico_4 == 8701)

#Descripción de la base de datos general
nrow(asgChi) #núm. de registros
ncol(asgChi) #núm.de variables
dimnames(asgChi) #observar valores y variables
names(asgChi) #nombre de las variables
head(asgChi) #datos en las primeras líeas
tail(asgChi) #datos en las últimas líeas
summary(asgChi) #información csv
str(asgChi) #información por varibale

#Correlaciones
cor(asgChi[ , purrr::map_lgl(asgChi, is.numeric)]) #Solo númericos
cor(select(asgChi, 2:3, 9,13:29)) #Eliminando las variables, que siempre son iguales, y variables no númericas
cor(Filter(function(x) sd(x) != 0, asgChi)) #Eliminando valores con desviación estandar null
cor(select(asgChi, 2, 9,13,15:16,18,20:21,23,25:26,28)) #Eliminando valores con desviación estandar null


#OPCIÓN 2
# Estado 7: Chiapas (asg$cve_delegacion)
# Sector económico: Construcción de edificaciones; excepto obra pública
#	sector_economico_1: 4, Industria de la construcción(sector_economico_1)
#	sector_economico_2: 41, Construcción de edificaciones y de obras de ingeniería civil(sector_economico_2)
#	sector_economico_4: 4101, Construcción de edificaciones; excepto obra pública (sector_economico_4)

#Creación del arreglo
asgChi <- filter(asg,  asg$cve_delegacion == 7 & asg$sector_economico_1 == 4 & asg$sector_economico_2 == 41 & asg$sector_economico_4 == 4101)

#Descripción de la base de datos general
nrow(asgChi) #núm. de registros
ncol(asgChi) #núm.de variables
dimnames(asgChi) #observar valores y variables
names(asgChi) #nombre de las variables
head(asgChi) #datos en las primeras líeas
tail(asgChi) #datos en las últimas líeas
summary(asgChi) #información csv
str(asgChi) #información por varibale

#Correlaciones
cor(asgChi[ , purrr::map_lgl(asgChi, is.numeric)]) #Solo númericos
cor(select(asgChi, 2:3, 9,13:29)) #Eliminando las variables, que siempre son iguales, y variables no númericas
cor(Filter(function(x) sd(x) != 0, asgChi)) #Eliminando valores con desviación estandar null
cor(select(asgChi, 2, 9,13,15:16,18,20:21,23,25:26,28)) #Eliminando valores con desviación estandar null


#OPCIÓN 3
# Estado 7: Chiapas (asg$cve_delegacion)
# Sector económico: Industrias químico-farmacéuticas y de medicamentos
#	sector_economico_1: 3, Industrias de transformación(sector_economico_1)
#	sector_economico_2: 30, Industria química(sector_economico_2)
#	sector_economico_4: 3005, Industrias químico-farmacéuticas y de medicamentos (sector_economico_4)

#Creación del arreglo
asgChi <- filter(asg,  asg$cve_delegacion == 7 & asg$sector_economico_1 == 3 & asg$sector_economico_2 == 30 & asg$sector_economico_4 == 3005)

#Descripción de la base de datos general
nrow(asgChi) #núm. de registros
ncol(asgChi) #núm.de variables
dimnames(asgChi) #observar valores y variables
names(asgChi) #nombre de las variables
head(asgChi) #datos en las primeras líeas
tail(asgChi) #datos en las últimas líeas
summary(asgChi) #información csv
str(asgChi) #información por varibale

#Correlaciones
cor(asgChi[ , purrr::map_lgl(asgChi, is.numeric)]) #Solo númericos
cor(select(asgChi, 2:3, 9,13:29)) #Eliminando las variables, que siempre son iguales, y variables no númericas
cor(Filter(function(x) sd(x) != 0, asgChi)) #Eliminando valores con desviación estandar null
cor(select(asgChi, 9,13,15,18,20,23,25,28)) #Eliminando valores con desviación estandar null


'
a$cve_municipio <- as.numeric(a$cve_municipio)
a$tama.o_patron <- as.numeric(a$tama.o_patron)
a$rango_edad <- as.numeric(a$rango_edad)
a$rango_salarial <- as.numeric(a$rango_salarial)
a$rango_uma <- as.numeric(a$rango_uma)
'
