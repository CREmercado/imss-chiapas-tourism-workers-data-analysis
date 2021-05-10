#MODELOS TA

asgChi <- filter(asg,  asg$cve_delegacion == 7 & asg$sector_economico_1 == 8 & asg$sector_economico_2 == 87 & asg$sector_economico_4 == 8701)

------------------------------------------------------------------------------------------------------------------
#5. Modelo: Salario base de cotización diario de trabajadores asegurados(continua, sal_bas_cotdi_ta) respecto a Sexo(binaria, sexo)

#Transformación de varaible binaria
asgChi$sexo_bin[asgChi$sexo==1] <- 0
asgChi$sexo_bin[asgChi$sexo==2] <- 1

#Transformación de variable continua
asgChi$sal_bas_cotdi_ta = asgChi$masa_sal_ta/asgChi$ta_sal
asgChi$sal_bas_cotdi_ta[is.na(asgChi$sal_bas_cotdi_ta)] <- 0

#Descripción de variables
asgChi$sexo_bin
str(asgChi$sexo_bin)
summary(asgChi$sexo_bin)
table(asgChi$sexo_bin)
asgChi$sal_bas_cotdi_ta
str(asgChi$sal_bas_cotdi_ta)
summary(asgChi$sal_bas_cotdi_ta)
table(asgChi$sal_bas_cotdi_ta)
cor(asgChi$sexo_bin, asgChi$sal_bas_cotdi_ta)

#Modelo
m5 <- lm(sal_bas_cotdi_ta ~ sexo_bin, asgChi)
m5
summary(m5)

#Medias
media.hom <- mean(asgChi[asgChi$sexo_bin == 0, "sal_bas_cotdi_ta"])
media.muj <- media.hom - 49.84

#Gráficación
plot(asgChi$sexo_bin, asgChi$sal_bas_cotdi_ta)
points(c(0,1), c(media.hom, media.muj), col = "blue", lwd = 2)
lines(c(0,1), c(media.hom, media.muj), col = "blue", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_ta respecto a la variable sexo_bin. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la 
	razón de la masa salarial o nómina y el número de asegurados asociados a un empleo. La segunda es una 
	variable categórica que describe el sexo, o más específicamente el género del asegurado.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.1204936.
		3. El error descrito por RMSE es de 205.3.
		4. El coeficiente de determinación ajustado es de 0.01347.
		5. Tanto la b0 como b1 son significantes.
		
	La media (b0) para cuando hay ausencia del valor de la variable independiente, es decir, cuando es 0,
	de sexo hombre, el salario base de cotización es de 267.7432 (268). La media para cuando se precenta el valor, 
	es decir, cuando es 1, de sexo mujer, el salario base de cotización es de 217.9032. La diferencia entre ambas 
	medias (b1), es de -49.84.
	
	Es decir, el salario medio de cotización medio de un hombre está por encima del de una mujer, en el estado de 
	Chiapas para el sector económico de preparación y servicio de alimentos.

#Relación de subdelegación
0 ----> "Hombre"
1 ----> "Mujer"

------------------------------------------------------------------------------------------------------------------
#6. Modelo: Salario base de cotización diario de trabajadores asegurados(continua, sal_bas_cotdi_ta) respecto a Rango de edad (categórica, rango_edad)

#Transformación de varaible categórica
asgChi$rango_edad_num <- as.integer(asgChi$rango_edad_num)
'%notin%' <- Negate('%in%')
acumulador <- 0
contar <- 1
contar1<- 0
for (i in c(asgChi$rango_edad)) {
if(asgChi$rango_edad[contar] %notin% c(acumulador)){
	acumulador[contar] <- asgChi$rango_edad[contar]
	contar1 <- contar1 + 1
	asgChi$rango_edad_num[contar]<-contar1
} else {
	acumulador[contar] <- asgChi$rango_edad[contar]
	asgChi$rango_edad_num[contar]<- c(asgChi$rango_edad_num)[c(acumulador)==asgChi$rango_edad[contar]]
}
contar <- contar +1 
}
contar <- 1
for (i in unique(asgChi$rango_edad)) {
print(unique(asgChi$rango_edad_num)[contar]) 
print(unique(asgChi$rango_edad)[contar])
contar <- contar + 1
}

#Transformación de variable continua
asgChi$sal_bas_cotdi_ta = asgChi$masa_sal_ta/asgChi$ta_sal
asgChi$sal_bas_cotdi_ta[is.na(asgChi$sal_bas_cotdi_ta)] <- 0

#Descripción de variables
asgChi$rango_edad_num
str(asgChi$rango_edad_num)
summary(asgChi$rango_edad_num)
table(asgChi$rango_edad_num)
asgChi$sal_bas_cotdi_ta
str(asgChi$sal_bas_cotdi_ta)
summary(asgChi$sal_bas_cotdi_ta)
table(asgChi$sal_bas_cotdi_ta)
cor(asgChi$rango_edad_num, asgChi$sal_bas_cotdi_ta)

#Modelo
m6 <- lm(sal_bas_cotdi_ta ~ factor(rango_edad_num), asgChi)
m6
summary(m6)

#Medias
media.1 <- mean(asgChi[asgChi$rango_edad_num == 1, "sal_bas_cotdi_ta"])
media.2 <- media.1 +14.47
media.3 <- media.1 +26.93
media.4 <- media.1 +37.30
media.5 <- media.1 -50.54
media.6 <- media.1 -66.90
media.7 <- media.1 +56.40
media.8 <- media.1 -28.96
media.9 <- media.1 +68.50 
media.10 <- media.1 -19.05
media.11 <- media.1 -74.79
media.12 <- media.1 -71.45
media.13 <- media.1 -97.74
media.14 <- media.1 -101.36

#Gráficación
plot(asgChi$rango_edad_num, asgChi$sal_bas_cotdi_ta)
points(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11, media.12, media.13, media.14), col = "blue", lwd = 2)
lines(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11, media.12, media.13, media.14), col = "blue", lwd = 2)
abline(h = mean(asgChi$sal_bas_cotdi_ta), col = "red")

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_ta respecto a la variable rango_edad_num. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la 
	razón de la masa salarial o nómina y el número de asegurados asociados a un empleo. La segunda es una 
	variable categórica que describe el rango de edad asociado al asegurado o derechohabiente adscrito.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.0253042.
		3. El error descrito por RMSE es de 203.7.
		4. El coeficiente de determinación ajustado es de 0.02855.
		5. Solo los valores de b0, b6,b8 y b10 son significantes.
		
	La media (b0) de salario base de cotización para el primer rango de edad que va de entre 55 y 60 años es de 230.1456. 
	El salario base de cotización media más alto es para el rango de edad de entre 35 y 40 con 298.6456, seguido por el 
	rango de edad de entre 40 y 45 con 286.5456, despues se encuentra la categoria de entre 45 y 50 con 267.4456, así como 
	el segmento de entre 30 y 35 con 257.0756 y el segmento de entre 25 y 30 con 244.6156; son las categorias que superan
	a la media general.
	
	Es decir, el rango de edad que cuenta con el salario base de cotización medio más alto para el sector económico de
	preparación y servicio de alimentos para el estado de Chiapas, es el de entre 35 y 40 años de edad, seguido por el de
	entre 40 y 45, entre 45 y 50, entre 30 y 35, y finalmente de entre 25 y 30, respectivamente. Por ello, los grupos entre
	30 y 45 años son los que aparentemente cuentan con un salario base de cotización media más alto. Pasando los 50 años tiende
	a caer la media.
	
#Relación de rango de edad
1 "E10" ----> Mayor o igual a 55 y menor a 60 años de edad
2 "E4" -----> Mayor o igual a 25 y menor a 30 años de edad
3 "E5" -----> Mayor o igual a 30 y menor a 35 años de edad
4 "E8" -----> Mayor o igual a 45 y menor a 50 años de edad
5 "E11" ----> Mayor o igual a 60 y menor a 65 años de edad
6 "E12" ----> Mayor o igual a 65 y menor a 70 años de edad
7 "E7" -----> Mayor o igual a 40 y menor a 45 años de edad
8 "E3" -----> Mayor o igual a 20 y menor a 25 años de edad
9 "E6" -----> Mayor o igual a 35 y menor a 40 años de edad
10 "E9" ----> Mayor o igual a 50 y menor a 55 años de edad
11 "E2" ----> Mayor o igual a 15 y menor a 20 años de edad
12 "E13" ---> Mayor o igual a 70 y menor a 75 años de edad
13 "E14" ---> 75 o más años de edad
14 "E1" ----> Menores de 15 años de edad

------------------------------------------------------------------------------------------------------------------
#7. Modelo: Salario base de cotización diario de trabajadores asegurados(continua, sal_bas_cotdi_ta) respecto a Rango salarial (categórica, rango_salarial)

#Transformación de varaible categórica
asgChi$rango_salarial[is.na(asgChi$rango_salarial)] <- "W0"
asgChi$rango_salarial_num <- as.integer(asgChi$rango_salarial_num)
'%notin%' <- Negate('%in%')
acumulador <- 0
contar <- 1
contar1<- 0
for (i in c(asgChi$rango_salarial)) {
if(asgChi$rango_salarial[contar] %notin% c(acumulador)){
	acumulador[contar] <- asgChi$rango_salarial[contar]
	contar1 <- contar1 + 1
	asgChi$rango_salarial_num[contar]<-contar1
} else {
	acumulador[contar] <- asgChi$rango_salarial[contar]
	asgChi$rango_salarial_num[contar]<- c(asgChi$rango_salarial_num)[c(acumulador)==asgChi$rango_salarial[contar]]
}
contar <- contar +1 
}
contar <- 1
for (i in unique(asgChi$rango_salarial)) {
print(unique(asgChi$rango_salarial_num)[contar]) 
print(unique(asgChi$rango_salarial)[contar])
contar <- contar + 1
}

#Transformación de variable continua
asgChi$sal_bas_cotdi_ta = asgChi$masa_sal_ta/asgChi$ta_sal
asgChi$sal_bas_cotdi_ta[is.na(asgChi$sal_bas_cotdi_ta)] <- 0

#Descripción de variables
asgChi$rango_salarial_num
str(asgChi$rango_salarial_num)
summary(asgChi$rango_salarial_num)
table(asgChi$rango_salarial_num)
asgChi$sal_bas_cotdi_ta
str(asgChi$sal_bas_cotdi_ta)
summary(asgChi$sal_bas_cotdi_ta)
table(asgChi$sal_bas_cotdi_ta)
cor(asgChi$rango_salarial_num, asgChi$sal_bas_cotdi_ta)

#Modelo
m7 <- lm(sal_bas_cotdi_ta ~ factor(rango_salarial_num), asgChi)
m7
summary(m7)

#Medias
media.1 <- mean(asgChi[asgChi$rango_salarial_num == 1, "sal_bas_cotdi_ta"])
media.2 <- media.1 +284.55
media.3 <- media.1 +402.70
media.4 <- media.1 +145.27
media.5 <- media.1 -25.15
media.6 <- media.1 +785.15
media.7 <- media.1 +911.04
media.8 <- media.1 +510.71
media.9 <- media.1 +628.82
media.10 <- media.1 -148.37
media.11 <- media.1 +1544.78
media.12 <- media.1 +1593.63
media.13 <- media.1 +1003.47
media.14 <- media.1 +1963.88
media.15 <- media.1 +1100.87

#Gráficación
plot(asgChi$rango_salarial_num, asgChi$sal_bas_cotdi_ta)
points(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11, media.12, media.13, media.14, media.15), col = "blue", lwd = 2)
lines(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11, media.12, media.13, media.14, media.15), col = "blue", lwd = 2)
abline(h = mean(asgChi$sal_bas_cotdi_ta), col = "red")

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_ta respecto a la variable rango_salarial_num. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la 
	razón de la masa salarial o nómina y el número de asegurados asociados a un empleo. La segunda es una 
	variable categórica que describe el rango de salarial asociado al asegurado, que representa el número de veces el 
	salario mínimo de la Ciudad de México.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de 0.7803583.
		3. El error descrito por RMSE es de 31.93.
		4. El coeficiente de determinación ajustado es de 0.9761.
		5. Todolos los valores (b) son significantes.
		
	La media (b0) de salario base de cotización para el primer rango salarial, de entre 1 y 2 salarios mínimos es 
	de 148.3709. El mayor salario base de cotización medio es para la categoria de entre 17 y 18 salarios mínimos,
	con 2112.251. Así,todos los valores son significanes.
	
	Es decir, el rango salarial más alto es el que cuenta con el salario base cotización medio más alto. Lo anterior,
	refleja, junto con las métricas producidas por el modelo, que ambas variables se relacionan bastante, sin embargo,
	lo anterior es previsible y realmente este modelo no es certero, puesto que el salario base de cotización, depende
	de forma directa del salarío minimo percivido por el asegurado, por lo cual la utilidad que se produce aquí, no es
	sustancial.

#Relación de rango de edad
1 "W2" -----> Mayor a 1 y hasta 2 veces el salario mínimo
2 "W4" -----> Mayor a 3 y hasta 4 veces el salario mínimo
3 "W5" -----> Mayor a 4 y hasta 5 veces el salario mínimo
4 "W3" -----> Mayor a 2 y hasta 3 veces el salario mínimo
5 "W1" -----> Hasta 1 vez el salario mínimo
6 "W8" -----> Mayor a 7 y hasta 8 veces el salario mínimo
7 "W9" -----> Mayor a 8 y hasta 9 veces el salario mínimo
8 "W6" -----> Mayor a 5 y hasta 6 veces el salario mínimo
9 "W7" -----> Mayor a 6 y hasta 7 veces el salario mínimo
10 "W0" ----> No aplica
11 "W14" ---> Mayor a 13 y hasta 14 veces el salario mínimo
12 "W15" ---> Mayor a 14 y hasta 15 veces el salario mínimo
13 "W10" ---> Mayor a 9 y hasta 10 veces el salario mínimo
14 "W18" ---> Mayor a 17 y hasta 18 veces el salario mínimo
15 "W11" ---> Mayor a 10 y hasta 11 veces el salario mínimo

------------------------------------------------------------------------------------------------------------------
#8. Modelo: Salario base de cotización diario de trabajadores asegurados(continua, sal_bas_cotdi_ta) respecto al Tamaño de registro patronal (categórica, tama.o_patron)

#Transformación de varaible categorica
asgChi$tama.o_patron_num <- as.integer(asgChi$tama.o_patron_num)
acumulador <- 0
contar <- 1
contar1<- 0
for (i in c(asgChi$tama.o_patron)) {
if(asgChi$tama.o_patron[contar] %notin% c(acumulador)){
	acumulador[contar] <- asgChi$tama.o_patron[contar]
	contar1 <- contar1 + 1
	asgChi$tama.o_patron_num[contar]<-contar1
} else {
	acumulador[contar] <- asgChi$tama.o_patron[contar]
	asgChi$tama.o_patron_num[contar]<- c(asgChi$tama.o_patron_num)[c(acumulador)==asgChi$tama.o_patron[contar]]
}
contar <- contar +1 
}
contar <- 1
for (i in unique(asgChi$tama.o_patron)) {
print(unique(asgChi$tama.o_patron_num)[contar]) 
print(unique(asgChi$tama.o_patron)[contar])
contar <- contar + 1
}

#Transformación de variable continua
asgChi$sal_bas_cotdi_ta = asgChi$masa_sal_ta/asgChi$ta_sal
asgChi$sal_bas_cotdi_ta[is.na(asgChi$sal_bas_cotdi_ta)] <- 0

#Descripción de variables
asgChi$tama.o_patron_num
str(asgChi$tama.o_patron_num)
summary(asgChi$tama.o_patron_num)
table(asgChi$tama.o_patron_num)
asgChi$sal_bas_cotdi_ta
str(asgChi$sal_bas_cotdi_ta)
summary(asgChi$sal_bas_cotdi_ta)
table(asgChi$sal_bas_cotdi_ta)
cor(asgChi$tama.o_patron_num, asgChi$sal_bas_cotdi_ta)

#Modelo
m8 <- lm(sal_bas_cotdi_ta ~ factor(tama.o_patron_num), asgChi)
m8
summary(m8)

#Medias
media.1 <- mean(asgChi[asgChi$tama.o_patron_num == 1, "sal_bas_cotdi_ta"])
media.2 <- media.1 +6.809
media.3 <- media.1 +94.306
media.4 <- media.1 +210.303
media.5 <- media.1 +296.589

#Gráficación
plot(asgChi$tama.o_patron_num, asgChi$sal_bas_cotdi_ta)
points(c(1,2,3,4,5), c(media.1, media.2, media.3, media.4, media.5), col = "blue", lwd = 2)
lines(c(1,2,3,4,5), c(media.1, media.2, media.3, media.4, media.5), col = "blue", lwd = 2)
abline(h = mean(asgChi$sal_bas_cotdi_ta), col = "red")

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_ta respecto a la variable rango_salarial_num. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la 
	razón de la masa salarial o nómina y el número de asegurados asociados a un empleo. La segunda es una 
	variable categórica que describe el rango de salarial asociado al asegurado, que representa el número de veces el 
	salario mínimo de la Ciudad de México.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de 0.4219743.
		3. El error descrito por RMSE es de 185.9.
		4. El coeficiente de determinación ajustado es de 0.1907.
		5. Todolos los valores (b) son significantes, excepto por b1.
		
	La media (b0) de salario base de cotización para la primera categoría de tamaño de registro patronal, de 1 asegurado
	es de 147.0719. La categoria con la media más alta es la de entre 251 y 500 asegurados.
	
	Es decir, el tamaño de registro patronal, de entre 251 y 500 asegurados, es la categoria que tiene un salario base de 
	cotización medio más alto en el estado de Chiapas, en el rubro de preparación y servicio de alimentos.

#Relación de tamaño de registro patronal
1 "S1" ----> 1 asegurado
2 "S2" ----> entre 2 y 5 asegurado
3 "S3" ----> entre 6 y 50 asegurados
4 "S4" ----> entre 51 y 250
5 "S5" ----> entre 251 y 500

------------------------------------------------------------------------------------------------------------------
#9. Modelo: Salario base de cotización diario de trabajadores asegurados(continua, sal_bas_cotdi_ta) respecto a Subdelegación (binaria, cve_subdelegacion)

#Transformación de varaible binaria
asgChi$cve_subdelegacion_bin[asgChi$cve_subdelegacion==1] <- 0
asgChi$cve_subdelegacion_bin[asgChi$cve_subdelegacion==2] <- 1

#Transformación de variable continua
asgChi$sal_bas_cotdi_ta = asgChi$masa_sal_ta/asgChi$ta_sal
asgChi$sal_bas_cotdi_ta[is.na(asgChi$sal_bas_cotdi_ta)] <- 0

#Descripción de variables
asgChi$cve_subdelegacion_bin
str(asgChi$cve_subdelegacion_bin)
summary(asgChi$cve_subdelegacion_bin)
table(asgChi$cve_subdelegacion_bin)
asgChi$sal_bas_cotdi_ta
str(asgChi$sal_bas_cotdi_ta)
summary(asgChi$sal_bas_cotdi_ta)
table(asgChi$sal_bas_cotdi_ta)
cor(asgChi$cve_subdelegacion_bin, asgChi$sal_bas_cotdi_ta)

#Modelo
m9 <- lm(sal_bas_cotdi_ta ~ factor(cve_subdelegacion_bin), asgChi)
m9
summary(m9)

#Medias
media.tux <- mean(asgChi[asgChi$cve_subdelegacion_bin == 0, "sal_bas_cotdi_ta"])
media.tap <- media.tux - 21.63

#Gráficación
plot(asgChi$cve_subdelegacion_bin, asgChi$sal_bas_cotdi_ta)
points(c(0,1), c(media.tux, media.tap), col = "blue", lwd = 2)
lines(c(0,1), c(media.tux, media.tap), col = "blue", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_ta respecto a la variable cve_subdelegacion. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de 
	la masa salarial o nómina y el número de asegurados asociados a un empleo. La segunda es una variable binaria 
	que describe a la subdelegación de abscripción operativa del IMSS en el estado de Chiapas, donde 0 es para la 
	circunscripción territorial a Tuxtla Gutierrez (capital) y 1 a Tapachula.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.0486575.
		3. El error descrito por RMSE es de 206.5.
		4. El coeficiente de determinación ajustado es de 0.001303.
		5. Solo b0 es significante significantes.
		
	La media (b0) del salario base de cotización para cuando hay ausencia del valor de la variable independiente, 
	es decir, cuando es 0, Tuxtla Gutierresz, es de 250.8883. La media para cuando se precenta el valor, es decir, 
	cuando es 1, Tapachula, es de 229.2583. La diferencia entre ambas medias (b1), es de -21.63.
	
	Es decir, la subdelegación de abscripción operativa del IMSS con circunscipción territorial en Tapachula tienen 
	una media de salario base de cotización diario menor que el de la subdelegación en Tuxtla Gutierrez, para el sector 
	económico de preparación y servicio de alimentos, en el estado de Chiapas.
	
#Relación de subdelegación
0 ----> "Tuxtla Gutierrez"
1 ----> "Tapachula"

------------------------------------------------------------------------------------------------------------------
#10. Modelo: Salario base de cotización diario de trabajadores asegurados(continua, sal_bas_cotdi_ta) respecto a Municipio (categórica, cve_municipio)

#Transformación de varaible categorica
asgChi$cve_municipio_num <- as.integer(asgChi$cve_municipio_num)
contar <- 2
contar1<- 1
asgChi$cve_municipio_num[contar-1]<- 1
for (i in c(asgChi$cve_municipio)) {
if(asgChi$cve_municipio[contar]==asgChi$cve_municipio[contar-1]){
	asgChi$cve_municipio_num[contar]<-contar1
} else {
	contar1<-contar1+1
	asgChi$cve_municipio_num[contar]<-contar1
}
	contar <- contar +1 
}
contar <- 1
for (i in unique(asgChi$cve_municipio)) {
print(unique(asgChi$cve_municipio_num)[contar]) 
print(unique(asgChi$cve_municipio)[contar])
contar <- contar + 1
}

#Transformación de variable continua
asgChi$sal_bas_cotdi_ta = asgChi$masa_sal_ta/asgChi$ta_sal
asgChi$sal_bas_cotdi_ta[is.na(asgChi$sal_bas_cotdi_ta)] <- 0

#Descripción de variables
asgChi$cve_municipio_num
str(asgChi$cve_municipio_num)
summary(asgChi$cve_municipio_num)
table(asgChi$cve_municipio_num)
asgChi$sal_bas_cotdi_ta
str(asgChi$sal_bas_cotdi_ta)
summary(asgChi$sal_bas_cotdi_ta)
table(asgChi$sal_bas_cotdi_ta)
cor(asgChi$cve_municipio_num, asgChi$sal_bas_cotdi_ta)

#Modelo
m10 <- lm(sal_bas_cotdi_ta ~ factor(cve_municipio_num), asgChi)
m10
summary(m10)

#Medias
media.1 <- mean(asgChi[asgChi$cve_municipio_num == 1, "sal_bas_cotdi_ta"])
media.2 <- media.1 -30.308
media.3 <- media.1 -34.601 
media.4 <- media.1 +57.974 
media.5 <- media.1 +150.078 
media.6 <- media.1 -20.135
media.7 <- media.1 -42.401
media.8 <- media.1 -42.401
media.9 <- media.1 -38.467 
media.10 <- media.1 +37.849
media.11 <- media.1 -39.364
media.12 <- media.1 -42.401
media.13 <- media.1 +5.553
media.14 <- media.1 -42.401
media.15 <- media.1 -42.401
media.16 <- media.1 -33.780
media.17 <- media.1 -32.080
media.18 <- media.1 -31.787
media.19 <- media.1 +28.809
media.20 <- media.1 -23.150
media.21 <- media.1 -42.401
media.22 <- media.1 -4.100
media.23 <- media.1 +14.094
media.24 <- media.1 -42.401
media.25 <- media.1 -36.897
media.26 <- media.1 -40.541
media.27 <- media.1 -16.796
media.28 <- media.1 +97.763
media.29 <- media.1 -4.254
media.30 <- media.1 -42.344
media.31 <- media.1 -42.401
media.32 <- media.1 -42.401
media.33 <- media.1 -41.891
media.34 <- media.1 -42.401
media.35 <- media.1 -42.401
media.36 <- media.1 -42.401

#Gráficación
plot(asgChi$cve_municipio_num, asgChi$sal_bas_cotdi_ta)
points(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11, media.12, media.13, media.14, media.15, media.16, media.17, media.18, media.19, media.20, media.21, media.22, media.23, media.24, media.25, media.26, media.27, media.28, media.29, media.30, media.31, media.32, media.33, media.34, media.35, media.36), col = "blue", lwd = 2)
lines(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11, media.12, media.13, media.14, media.15, media.16, media.17, media.18, media.19, media.20, media.21, media.22, media.23, media.24, media.25, media.26, media.27, media.28, media.29, media.30, media.31, media.32, media.33, media.34, media.35, media.36), col = "blue", lwd = 2)
abline(h = mean(asgChi$sal_bas_cotdi_ta), col = "red")

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_ta respecto a la variable cve_municipio. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de 
	la masa salarial o nómina y el número de asegurados asociados a un empleo. La segunda es una variable categórica 
	que describe al municipio asociado a la ubicación del patrón asegurado ante el IMSS, la cuál cuenta con 36 municipios 
	diferentes (subdelegación/delegación puede tener más adscritos que el estado/municipio, ya que esta no considera 
	frenteras estatales).
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.1005208.
		3. El error descrito por RMSE es de 197.5.
		4. El coeficiente de determinación ajustado es de 0.08681.
		5. El únicos valores significantes son b0, b4 y b27.
		
	La media (b0) de salario base de cotización diario para la primera categoria, que es el municipio de Comitán de 
	Domínguez, es de 171.191. Muchas de las categorias o municipios, alrededor de 12, tienen una diferencia de sus media 
	con respecto a la de la primera categoria de -42.401, la cual media de tales municipios, es de 128.79.  La media más 
	alta es del municipio de Tuxtla Gutiérrez, la cual es de 321.269, y la única que es significante, junto con el 
	municipio de Comitán de Domínguez y el de Tapachula con media en 268.954. También, el municipio de San Cristóbal de 
	las Casas, registra un salario base de cotización alto, cercano al medio total, con 244.0685.
	
	Es decir, el municipio asociado a la ubicación del patrón asegurado ante el IMSS del estado de Chiapas con una
	media de salario base de cotización diario más alto es Tuxtla Gutierrez. Una mayoría de los municipios, 12, tiene 
	una media 128.79 (cercano al salario mínimo). El único municipis que también superan en su media a 245 (alrededor de
	2 salarios mínimos) es Tapachula, con San Cristóbal de las Casas muy cerca.
	
#Relación de municipios
1 "A54" ----> Comitán de Domínguez
2 "A55" ----> Chiapa de Corzo
3 "A59" ----> Las Rosas
4 "A62" ----> San Cristóbal de las Casas
5 "A68" ----> Tuxtla Gutiérrez
6 "H28" ----> Cintalapa
7 "J97" ----> Bochil
8 "K01" ----> Copainalá
9 "K14" ----> Ocosingo
10 "K19" ---> Simojovel
11 "K23" ---> Teopisca
12 "L20" ---> San Fernando
13 "M72" ---> Villaflores
14 "M73" ---> Angel Albino Corzo
15 "M74" ---> Jiquipilas
16 "M81" ---> Berriozábal
17 "M86" ---> Ocozocoautla de Espinosa
18 "Q11" ---> Pichucalco
19 "Q13" ---> Juárez
20 "Q16" ---> Palenque
21 "R26" ---> Tzimol
22 "A52" ---> Arriaga
23 "A53" ---> Cacahoatán
24 "A57" ---> Huehuetán
25 "A58" ---> Huixtla
26 "A61" ---> Metapa
27 "A64" ---> Suchiate
28 "A65" ---> Tapachula
29 "A67" ---> Tonalá
30 "A69" ---> Tuxtla Chico
31 "A71" ---> Unión Juárez
32 "M69" ---> Pijijiapan
33 "M70" ---> Mapastepec
34 "M77" ---> Acapetahua
35 "M85" ---> Villa Comaltitlán
36 "M87" ---> Motozintla

------------------------------------------------------------------------------------------------------------------
#11. Modelo: Salario base de cotización diario de trabajadores asegurados(continua, sal_bas_cotdi_ta) respecto a Asegurados (continua, asegurados)

#Transformación de variable dependiente
asgChi$sal_bas_cotdi_ta = asgChi$masa_sal_ta/asgChi$ta_sal
asgChi$sal_bas_cotdi_ta[is.na(asgChi$sal_bas_cotdi_ta)] <- 0

#Descripción de variables
asgChi$asegurados
str(asgChi$asegurados)
summary(asgChi$asegurados)
table(asgChi$asegurados)
asgChi$sal_bas_cotdi_ta
str(asgChi$sal_bas_cotdi_ta)
summary(asgChi$sal_bas_cotdi_ta)
table(asgChi$sal_bas_cotdi_ta)
cor(asgChi$asegurados, asgChi$sal_bas_cotdi_ta)

#Modelo
#Lineal
m11 <- lm(sal_bas_cotdi_ta ~ asegurados, asgChi)
m11
summary(m11)
#Cuadrático
asegurados2 <- asgChi$asegurados^2
m112 <- lm(sal_bas_cotdi_ta ~ asegurados + asegurados2, asgChi)
m112
summary(m112)
#Cúbico
asegurados3 <- asgChi$asegurados^3
m113 <- lm(sal_bas_cotdi_ta ~ asegurados + asegurados2 + asegurados3, asgChi)
m113
summary(m113)
#Potencia 4
asegurados4 <- asgChi$asegurados^4
m114 <- lm(sal_bas_cotdi_ta ~ asegurados + asegurados2 + asegurados3 + asegurados4, asgChi)
m114
summary(m114)


#Cálculo de estimaciones
#Lineal
x <- seq(min(asgChi$asegurados), max(asgChi$asegurados), by = 0.01)
y <- 260.094 - 2.442 * x
#Cuadrático
x2 <- x^2
y2 <- 272.63399 - 6.09491 * x + 0.04251 * x^2
#Cúbico
x3 <- x^3
y3 <- 2.841e+02 - 1.101e+01 * x + 1.870e-01 * x^2 - 8.197e-04 * x^3
#Potencia 4
x4 <- x^4
y4 <- 2.950e+02 - 1.710e+01  * x + 5.053e-01 * x^2 - 5.212e-03 * x^3 + 1.702e-05 * x^4

#Gráficación
plot(asgChi$asegurados, asgChi$sal_bas_cotdi_ta)
lines(x, y, col = "blue", lwd = 2)
lines(x, y2, col = "green", lwd = 2)
lines(x, y3, col = "orange", lwd = 2)
lines(x, y4, col = "yellow", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_ta respecto a la variable asegurados. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de 
	la masa salarial o nómina y el número de asegurados asociados a un empleo. La segunda es una variable continua o 
	númerica que describe el número de personas que están aseguradas en el IMSS bajo un patrón.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.1626034.
		3. El error descrito por RMSE del modelo lienal es 203.8, cuádratico es 202.1, cúbico es 201.1, de potencia 4 es 200.5.
		4. El coeficiente de determinación ajustado del modelo lienal es 0.02618, cuádratico es 0.04218, cúbico es 0.05196, de potencia 4 es 0.05779.
		5. Tanto b0, como b1 son significantes, e el modelo lienal.
		
	El coeficiente de correlación tiene signo negativo por lo que la realción entre variables en inversamente proporcional.
	La magnitud del mismo es considerablemente baja. El coeficiente de determinación ajustado es bajo, también. Sin embargo,
	las dos b son significantes. Cunado el salario base de cotización aumenta, el número de asegurados bajo un mismo patrón
	disminuye.
	
	Es decir, mientas más alto es el salario base de cotización de un asegurado, menos asegurados tiene ese mismo patrón. 
	Sin embargo, al no existir una relación fuerte entre variables, y ya que las metricas indican una relación débil, la
	conclusión anterior no es tan plausible, pues hay asegurados con un salario base de cotización igual, sin importar,
	que su patron cuente con 1 asegurado o con más de 150. Por ello las predicciónes resultantes del modelo no son, ni de
	cerca, exactas. El modelo no sigue un modelo lineal, ni cuadrático, ni cúbico, ni a la potencia 4, por más que mejoren 
	las métricas, aunque el que mejor podría predecir es el cuadrático, ya que no cae en valores negativos.
	
------------------------------------------------------------------------------------------------------------------
#12. Modelo: Salario base de cotización diario de trabajadores asegurados(continua, sal_bas_cotdi_ta) respecto a Masa salarial de trabajadores asegurados (continua, masa_sal_ta)

#Transformación de variable dependiente
asgChi$sal_bas_cotdi_ta = asgChi$masa_sal_ta/asgChi$ta_sal
asgChi$sal_bas_cotdi_ta[is.na(asgChi$sal_bas_cotdi_ta)] <- 0

#Descripción de variables
asgChi$masa_sal_ta
str(asgChi$masa_sal_ta)
summary(asgChi$masa_sal_ta)
table(asgChi$masa_sal_ta)
asgChi$sal_bas_cotdi_ta
str(asgChi$sal_bas_cotdi_ta)
summary(asgChi$sal_bas_cotdi_ta)
table(asgChi$sal_bas_cotdi_ta)
cor(asgChi$masa_sal_ta, asgChi$sal_bas_cotdi_ta)
	
#Modelo
#Lineal
m12 <- lm(sal_bas_cotdi_ta ~ masa_sal_ta, asgChi)
m12
summary(m12)
#Cuadrático
masa_sal_ta2 <- asgChi$masa_sal_ta^2
m122 <- lm(sal_bas_cotdi_ta ~ masa_sal_ta + masa_sal_ta2, asgChi)
m122
summary(m122)
#Cúbico
masa_sal_ta3 <- asgChi$masa_sal_ta^3
m123 <- lm(sal_bas_cotdi_ta ~ masa_sal_ta + masa_sal_ta2 + masa_sal_ta3, asgChi)
m123
summary(m123)
#Potencia 4
masa_sal_ta4 <- asgChi$masa_sal_ta^4
m124 <- lm(sal_bas_cotdi_ta ~ masa_sal_ta + masa_sal_ta2 + masa_sal_ta3 + masa_sal_ta4, asgChi)
m124
summary(m124)

#Cálculo de estimaciones
#Lineal
x <- seq(min(asgChi$masa_sal_ta), max(asgChi$masa_sal_ta), by = 0.01)
y <- 249.358817 - 0.004128 * x
#Cuadrático
x2 <- x^2
y2 <- 2.397e+02 +  1.149e-02 * x - 1.307e-06 * x^2
#Cúbico
x3 <- x^3
y3 <- 2.141e+02 + 7.052e-02 * x - 1.393e-05 * x^2 + 5.203e-10 * x^3
#Potencia 4
x4 <- x^4
y4 <- 1.732e+02 + 1.940e-01 * x - 6.075e-05 * x^2 + 5.230e-09 * x^3 - 1.328e-13 * x^4

#Gráficación
plot(asgChi$masa_sal_ta, asgChi$sal_bas_cotdi_ta)
lines(x, y, col = "blue", lwd = 2)
lines(x, y2, col = "green", lwd = 2)
lines(x, y3, col = "orange", lwd = 2)
lines(x, y4, col = "yellow", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_ta respecto a la variable masa_sal_ta. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de 
	la masa salarial o nómina y el número de asegurados asociados a un empleo. La segunda es una variable continua o 
	númerica que describe la nómina que considera tanto el salario como la plantilla de trabajadores a cargo de un patrón.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.03623681.
		3. El error descrito por RMSE del modelo lineal es 206.5, cuadrático es 205.9, cúbico es 202.6, de potencia 4 es 196.4.
		4. El coeficiente de determinación ajustado del modelo lineal es 0.0004534, cuadrático es 0.005684, cúbico es 0.03721, de potencia 4 es 0.09562.
		5. Solo b0 es significante en el modelo lineal.
		
	El coeficiente de correlación tiene signo negativo por lo que la realción entre variables en inversamente proporcional.
	La magnitud del mismo es considerablemente baja. El coeficiente de determinación ajustado es bajo, también. Sin embargo,
	solo b0 es sigificante. Cunado el salario base de cotización aumenta, la masa salarial que paga un patrón por sus
	trabajadores asegurados disminuye.
	
	Es decir, mientas más alto es el salario base de cotización de un asegurado, menos masa salarial tendrá que pagar el
	patrón por sus trabajadores asegurados. Sin embargo, al no existir una relación fuerte entre variables, y ya que las 
	metricas indican una relación débil, la conclusión anterior no es tan plausible, pues hay asegurados con un salario 
	base de cotización igual, sin importar, que su patron tenga que pagar una cantidad inferior o mayor de masa salarial 
	a trabajadores asegurados. Por ello las predicciónes resultantes del modelo no son, ni de cerca, exactas. El modelo 
	no sigue un modelo lineal, ni cuadrático, ni cúbico, ni a la potencia 4, por más que mejoren las métricas, aunque el 
	que mejor podría predecir es el lineal, ya que no cae en valores negativos.

------------------------------------------------------------------------------------------------------------------
#13. Modelo: Salario base de cotización diario de trabajadores asegurados (continua, sal_bas_cotdi_ta) respecto a Sexo (categórica, sexo) y Tamaño de registro patronal (categórica, tama.o_patron)

#Descripción de variables
cor(asgChi$sexo_bin, asgChi$tama.o_patron_num)
cor(asgChi$tama.o_patron_num, asgChi$sal_bas_cotdi_ta)
cor(asgChi$sexo_bin, asgChi$sal_bas_cotdi_ta)

#Modelo
#Lineal
m13 <- lm(sal_bas_cotdi_ta ~ sexo_bin + tama.o_patron_num, asgChi)
m13
summary(m13)

#Cálculo de estimaciones
#Lineal
xs <- seq(min(asgChi$sexo_bin), max(asgChi$sexo_bin), length.out = 100)
xt <- seq(min(asgChi$tama.o_patron_num), max(asgChi$tama.o_patron_num), length.out = 100)
y <- 40.80 - 35.81 * xs + 78.35 * xt

#Gráficación
plot((asgChi$sexo_bin + asgChi$tama.o_patron_num), asgChi$sal_bas_cotdi_ta)
lines(xs+xt, y, col = "blue", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_ta respecto a la variable sexo_bin y tama.o_patron_num. La primera es 
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de la masa 
	salarial o nómina y el número de asegurados asociados a un empleo. La segunda es una variable categórica que describe 
	el sexo, o más específicamente el género del asegurado. La tercera es una variable categórica que describe el rango de 
	salarial asociado al asegurado, que representa el número de veces el salario mínimo de la Ciudad de México.
	
	Las métricas el modelo son:
		1. La correlación entre variables independientes es -0.08174308. Entre sal_bas_cotdi_tpu y tama.o_patron_num es 0.4219743, y entre sal_bas_cotdi_teu y asegurados es -0.1204936.
		3. El error descrito por RMSE del modelo lineal es 186.7.
		4. El coeficiente de determinación ajustado del modelo lineal es 0.1838.
		5. Todos los valores de b son significantes (ya que los valores de x no se explican así mismos, tiene una correlación baja entre sí).
		
	El la recta de regresión tiende a positivo, por lo que realción entre variables es proporcionales. El coeficiente de 
	determinación ajustado es bajo, también. Sin embargo, todos los valores de b son significantes, ya que los valores de 
	x no se explican así mismos, tiene una correlación baja entre sí. Cunado el sexo es 0, hombre, b1 no tiene valor, pero 
	cuendo es 1, mujer, el cambio al salario será en una proporción de - 35.81. Cuando hay un cambio del tamaño de registro 
	patronal cambia, el cambio al salario será en un proporción de 78.35.
	
	Es decir, mientas más alto es el salario base de cotización de un asegurado en el estado de Chiapas en el rubro económico 
	de preparación y servicio de aliemntos; el sexo o género tenderá a ser hombre y el tamaño de registro patronal tenderá a 
	ser mayor.
		
------------------------------------------------------------------------------------------------------------------
#14. Modelo: Salario base de cotización diario de trabajadores asegurados (continua, sal_bas_cotdi_ta) respecto a Asegurados (categórica, sexo) y Masa salarial de trabajadores asegurados (continua, masa_sal_ta)

#Descripción de variables
cor(asgChi$asegurados, asgChi$masa_sal_ta)
cor(asgChi$asegurados, asgChi$sal_bas_cotdi_ta)
cor(asgChi$masa_sal_ta, asgChi$sal_bas_cotdi_ta)

#Modelo
#Lineal
m14 <- lm(sal_bas_cotdi_ta ~ asegurados + masa_sal_ta, asgChi)
m14
summary(m14)

#Cálculo de estimaciones
#Lineal
xa <- seq(min(asgChi$asegurados), max(asgChi$asegurados), length.out = 1000)
xm <- seq(min(asgChi$masa_sal_ta), max(asgChi$masa_sal_ta), length.out = 1000)
y <- 179.3186 - 62.8402 * xa + 0.4382 * xm

#Gráficación
plot((asgChi$asegurados + asgChi$masa_sal_ta), asgChi$sal_bas_cotdi_ta)
lines(xa+xm, y, col = "blue", lwd = 2)
		
#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_ta respecto a la variable asegurados y masa_sal_ta. La primera es 
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de la masa 
	salarial o nómina y el número de asegurados asociados a un empleo. La segunda es una variable continua o númerica que 
	describe el número de personas que están aseguradas en el IMSS bajo un patrón. La tercera es una variable continua númerica 
	que describe la nómina que considera tanto el salario como la plantilla de trabajadores a cargo de un patrón.
	
	Las métricas el modelo son:
		1. La correlación entre variables independientes es 0.9848975. Entre sal_bas_cotdi_teu y asegurados es -0.1626034, y entre sal_bas_cotdi_teu y masa_sal_teu es -0.03623681.
		3. El error descrito por RMSE del modelo lineal es 140.5.
		4. El coeficiente de determinación ajustado del modelo lineal es 0.5376.
		5. Todos los valores de b son significantes en un nivel bajo (los valores de x tienen una correlación alta entre sí, pero ya que y, es producto de ambas variables de x, los valores siguen siendo significantes)
		
	El la recta de regresión tiende a negativo, por lo que realción entre variables es inversamente proporcional. El coeficiente 
	de determinación ajustado es bajo, también. Sin embargo, todos los valores de b son significantes, aunque los valores de x 
	tienen una correlación alta entre sí, pero ya que y, es producto de ambas variables de x, los valores siguen siendo 
	significantes.
	
	Cunado hay un cambio en el número de asegurados por un mismo patrón, el cambio al salario será en una proporción de -62.8402. 
	Cuando hay un cambio en la masa salarial que tiene que pagar un mismo patrón, de trabajadores eventuales urbanos, el cambio 
	al salario será en un proporción de 0.4382.
	
	Es decir, mientas más alto es el salario base de cotización de un asegurado en el estado de Chiapas en el rubro económico 
	de preparación y servicio de aliemntos; el número de asegurados por un mismo patrón tenderá a ser menor y la masa de 
	salarios de trabajadores asegurados que paga un patrón tenderá a ser mayor.
		
