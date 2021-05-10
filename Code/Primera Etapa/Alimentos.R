#Alimentos

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

'
Descripción de las variables:
1 $ cve_delegacion` 			Constante(7-chiapas) 
2 $ cve_subdelegacion			Binaria(1-tuxla y 2-tapachula) ------------------------------------> X ª
3 $ cve_entidad				Constante(7-chiapas) ---------------------------------------------->
4 $ cve_municipio`			Categorica(36) ----------------------------------------------------> X ª
**************************************************************************************************************
5 $ sector_economico_1		Constante(8-servicios para empresas, personas y el hogar) --------->
6 $ sector_economico_2		Constante(87-preparación y servicio de alimentos y bebidas) ------->
7 $ sector_economico_4		Constante(8701-preparación y servicio de alimentos) --------------->
**************************************************************************************************************
8 $ tama.o_patron`			Categorica(5) -----------------------------------------------------> X ª
9 $ sexo						Binaria(1-hombre, 2-mujer) ----------------------------------------> X ª
10 $ rango_edad				Categorica(14) ----------------------------------------------------> X ª
11 $ rango_salarial			Categorica(14) ----------------------------------------------------> X ª
12 $ rango_uma				Categorica(18) ----------------------------------------------------> X
**************************************************************************************************************
13 $ asegurados				Continua(61) ------------------------------------------------------> X Y 
14 $ no_trabajadores			No Continua(1) ---------------------------------------------------->
**************************************************************************************************************
15 $ ta						Continua(61) ------------------------------------------------------> X Y 
..............................................................................................................
16 $ teu						Continua(10) ------------------------------------------------------> X Y 
17 $ tec						No Continua(1) ---------------------------------------------------->
..............................................................................................................
18 $ tpu						Continua(61) ------------------------------------------------------> X Y 
19 $ tpc 					No Continua(1) ---------------------------------------------------->
**************************************************************************************************************
20 $ ta_sal					Continua(62) ------------------------------------------------------> X Y
..............................................................................................................
21 $ teu_sal					Continua(10) ------------------------------------------------------> X Y
22 $ tec_sal					No Continua(1) ---------------------------------------------------->
..............................................................................................................
23 $ tpu_sal					Continua(61) ------------------------------------------------------> X Y
24 $ tpc_sal					No Continua(1) ---------------------------------------------------->
**************************************************************************************************************
25 $ masa_sal_ta				Continua(672) -----------------------------------------------------> X Y
..............................................................................................................
26 $ masa_sal_teu			Continua(55) ------------------------------------------------------> X Y
27 $ masa_sal_tec			No Continua(1) ---------------------------------------------------->
..............................................................................................................
28 $ masa_sal_tpu			Continua(667) -----------------------------------------------------> X Y
29 $ masa_sal_tpc			No Continua(1) ---------------------------------------------------->
'


asgChi$rango_edad_num[asgChi$rango_edad=="E1"] <- 1
asgChi$rango_edad_num[asgChi$rango_edad=="E2"] <- 2
asgChi$rango_edad_num[asgChi$rango_edad=="E3"] <- 3
asgChi$rango_edad_num[asgChi$rango_edad=="E4"] <- 4
asgChi$rango_edad_num[asgChi$rango_edad=="E5"] <- 5
asgChi$rango_edad_num[asgChi$rango_edad=="E6"] <- 6
asgChi$rango_edad_num[asgChi$rango_edad=="E7"] <- 7
asgChi$rango_edad_num[asgChi$rango_edad=="E8"] <- 8
asgChi$rango_edad_num[asgChi$rango_edad=="E9"] <- 9
asgChi$rango_edad_num[asgChi$rango_edad=="E10"] <- 10
asgChi$rango_edad_num[asgChi$rango_edad=="E11"] <- 11
asgChi$rango_edad_num[asgChi$rango_edad=="E12"] <- 12
asgChi$rango_edad_num[asgChi$rango_edad=="E13"] <- 13
asgChi$rango_edad_num[asgChi$rango_edad=="E14"] <- 14
media.1 <- mean(asgChi[asgChi$rango_edad_num == 1, "sal_bas_cotdi_ta"])
media.2 <- media.1 +26.567
media.3 <- media.1 +72.399
media.4 <- media.1 +115.831 
media.5 <- media.1 +128.283
media.6 <- media.1 +169.852
media.7 <- media.1 +157.756
media.8 <- media.1 +138.655
media.9 <- media.1 +82.307
media.10 <- media.1 +101.356
media.11 <- media.1 +50.815
media.12 <- media.1 +34.457
media.13 <- media.1 +29.901
media.14 <- media.1 +3.617
1 "E1" ----> Menores de 15 años de edad
2 "E2" ----> Mayor o igual a 15 y menor a 20 años de edad
3 "E3" -----> Mayor o igual a 20 y menor a 25 años de edad
4 "E4" -----> Mayor o igual a 25 y menor a 30 años de edad
5 "E5" -----> Mayor o igual a 30 y menor a 35 años de edad
6 "E6" -----> Mayor o igual a 35 y menor a 40 años de edad
7 "E7" -----> Mayor o igual a 40 y menor a 45 años de edad
8 "E8" -----> Mayor o igual a 45 y menor a 50 años de edad
9 "E9" ----> Mayor o igual a 50 y menor a 55 años de edad
10 "E10" ----> Mayor o igual a 55 y menor a 60 años de edad
11 "E11" ----> Mayor o igual a 60 y menor a 65 años de edad
12 "E12" ----> Mayor o igual a 65 y menor a 70 años de edad
13 "E13" ---> Mayor o igual a 70 y menor a 75 años de edad
14 "E14" ---> 75 o más años de edad

asgChi$rango_edad_num1[asgChi$rango_edad_num=="1"] <- 10
asgChi$rango_edad_num1[asgChi$rango_edad_num=="2"] <- 4
asgChi$rango_edad_num1[asgChi$rango_edad_num=="3"] <- 5
asgChi$rango_edad_num1[asgChi$rango_edad_num=="4"] <- 8
asgChi$rango_edad_num1[asgChi$rango_edad_num=="5"] <- 11
asgChi$rango_edad_num1[asgChi$rango_edad_num=="6"] <- 12
asgChi$rango_edad_num1[asgChi$rango_edad_num=="7"] <- 7
asgChi$rango_edad_num1[asgChi$rango_edad_num=="8"] <- 3
asgChi$rango_edad_num1[asgChi$rango_edad_num=="9"] <- 6
asgChi$rango_edad_num1[asgChi$rango_edad_num=="10"] <- 9
asgChi$rango_edad_num1[asgChi$rango_edad_num=="11"] <- 2
asgChi$rango_edad_num1[asgChi$rango_edad_num=="12"] <- 13
asgChi$rango_edad_num1[asgChi$rango_edad_num=="13"] <- 14
asgChi$rango_edad_num1[asgChi$rango_edad_num=="14"] <- 1



asgChi$tama.o_patron_num[asgChi$tama.o_patron=="S1"] <- 1
asgChi$tama.o_patron_num[asgChi$tama.o_patron=="S2"] <- 2
asgChi$tama.o_patron_num[asgChi$tama.o_patron=="S3"] <- 3
asgChi$tama.o_patron_num[asgChi$tama.o_patron=="S4"] <- 4
asgChi$tama.o_patron_num[asgChi$tama.o_patron=="S5"] <- 5
