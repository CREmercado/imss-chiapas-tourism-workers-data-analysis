#MODELOS TPU

asgChi_tpu <- filter(asg,  asg$cve_delegacion == 7 & asg$sector_economico_1 == 8 & asg$sector_economico_2 == 87 & asg$sector_economico_4 == 8701 & asg$tpu_sal!=0)

------------------------------------------------------------------------------------------------------------------
#5. Modelo: Salario base de cotización diario de trabajadores permanentes urbanos asegurados (continua, sal_bas_cotdi_tpu) respecto a Sexo(binaria, sexo)

#Transformación de varaible binaria
asgChi_tpu$sexo_bin[asgChi_tpu$sexo==1] <- 0
asgChi_tpu$sexo_bin[asgChi_tpu$sexo==2] <- 1

#Transformación de variable continua
asgChi_tpu$sal_bas_cotdi_tpu = asgChi_tpu$masa_sal_tpu/asgChi_tpu$tpu_sal
asgChi_tpu$sal_bas_cotdi_tpu[is.na(asgChi_tpu$sal_bas_cotdi_tpu)] <- 0

#Descripción de variables
asgChi_tpu$sexo_bin
str(asgChi_tpu$sexo_bin)
summary(asgChi_tpu$sexo_bin)
table(asgChi_tpu$sexo_bin)
asgChi_tpu$sal_bas_cotdi_tpu
str(asgChi_tpu$sal_bas_cotdi_tpu)
summary(asgChi_tpu$sal_bas_cotdi_tpu)
table(asgChi_tpu$sal_bas_cotdi_tpu)
cor(asgChi_tpu$sexo_bin, asgChi_tpu$sal_bas_cotdi_tpu)

#Modelo
m5 <- lm(sal_bas_cotdi_tpu ~ sexo_bin, asgChi_tpu)
m5
summary(m5)

#Medias
media.hom <- mean(asgChi_tpu[asgChi_tpu$sexo_bin == 0, "sal_bas_cotdi_tpu"])
media.muj <- media.hom - 51.13 

#Gráficación
plot(asgChi_tpu$sexo_bin, asgChi_tpu$sal_bas_cotdi_tpu)
points(c(0,1), c(media.hom, media.muj), col = "blue", lwd = 2)
lines(c(0,1), c(media.hom, media.muj), col = "blue", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_tpu respecto a la variable sexo_bin. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la 
	razón de la masa salarial o nómina y el número de asegurados asociados a un empleo, en este caso a 
	empleos permanentes urbanos. La segunda es una variable categórica que describe el sexo, o más 
	específicamente el género del asegurado.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.1232945.
		2. El error descrito por RMSE es de 205.7.
		3. El coeficiente de determinación ajustado es de 0.01414.
		4. Tanto la b0 como b1 son significantes.
		5 AIC es 9866.786.
		
	La media (b0) para cuando hay ausencia del valor de la variable independiente, es decir, cuando es 0,
	de sexo hombre, el salario base de cotización es de 270.5283. La media para cuando se precenta el valor, 
	es decir, cuando es 1, de sexo mujer, el salario base de cotización es de 219.3983. La diferencia entre ambas 
	medias (b1), es de -51.13.
	
	Es decir, el salario base de cotización medio para empleos permenentes urbanos de un hombre está por encima del de 
	una mujer, en el estado de Chiapas para el sector económico de preparación y servicio de alimentos.

#Relación de subdelegación
0 ----> "Hombre"
1 ----> "Mujer"

------------------------------------------------------------------------------------------------------------------
#6. Modelo: Salario base de cotización diario de trabajadores permanentes urbanos asegurados (continua, sal_bas_cotdi_tpu) respecto a Rango de edad (categórica, rango_edad)

#Transformación de varaible categórica
asgChi_tpu$rango_edad_num <- as.integer(asgChi_tpu$rango_edad_num)
'%notin%' <- Negate('%in%')
acumulador <- 0
contar <- 1
contar1<- 0
for (i in c(asgChi_tpu$rango_edad)) {
if(asgChi_tpu$rango_edad[contar] %notin% c(acumulador)){
	acumulador[contar] <- asgChi_tpu$rango_edad[contar]
	contar1 <- contar1 + 1
	asgChi_tpu$rango_edad_num[contar]<-contar1
} else {
	acumulador[contar] <- asgChi_tpu$rango_edad[contar]
	asgChi_tpu$rango_edad_num[contar]<- c(asgChi_tpu$rango_edad_num)[c(acumulador)==asgChi_tpu$rango_edad[contar]]
}
contar <- contar +1 
}
contar <- 1
for (i in unique(asgChi_tpu$rango_edad)) {
print(unique(asgChi_tpu$rango_edad_num)[contar]) 
print(unique(asgChi_tpu$rango_edad)[contar])
contar <- contar + 1
}

#Transformación de variable continua
asgChi_tpu$sal_bas_cotdi_tpu = asgChi_tpu$masa_sal_tpu/asgChi_tpu$tpu_sal
asgChi_tpu$sal_bas_cotdi_tpu[is.na(asgChi_tpu$sal_bas_cotdi_tpu)] <- 0

#Descripción de variables
asgChi_tpu$rango_edad_num
str(asgChi_tpu$rango_edad_num)
summary(asgChi_tpu$rango_edad_num)
table(asgChi_tpu$rango_edad_num)
asgChi_tpu$sal_bas_cotdi_tpu
str(asgChi_tpu$sal_bas_cotdi_tpu)
summary(asgChi_tpu$sal_bas_cotdi_tpu)
table(asgChi_tpu$sal_bas_cotdi_tpu)
cor(asgChi_tpu$rango_edad_num, asgChi_tpu$sal_bas_cotdi_tpu)

#Modelo
m6 <- lm(sal_bas_cotdi_tpu ~ factor(rango_edad_num), asgChi_tpu)
m6
summary(m6)

#Medias
media.1 <- mean(asgChi_tpu[asgChi_tpu$rango_edad_num == 1, "sal_bas_cotdi_tpu"])
media.2 <- media.1 +11.85
media.3 <- media.1 +23.37
media.4 <- media.1 +36.00
media.5 <- media.1 -49.95
media.6 <- media.1 -64.36
media.7 <- media.1 +51.84
media.8 <- media.1 -31.74
media.9 <- media.1 +63.96 
media.10 <- media.1 -22.34
media.11 <- media.1 -78.73
media.12 <- media.1 -76.00
media.13 <- media.1 -102.28
media.14 <- media.1 -105.90

#Gráficación
plot(asgChi_tpu$rango_edad_num, asgChi_tpu$sal_bas_cotdi_tpu)
points(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11, media.12, media.13, media.14), col = "blue", lwd = 2)
lines(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11, media.12, media.13, media.14), col = "blue", lwd = 2)
abline(h = mean(asgChi_tpu$sal_bas_cotdi_tpu), col = "red")

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_tpu respecto a la variable rango_edad_num. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la 
	razón de la masa salarial o nómina y el número de asegurados asociados a un empleo, en este caso a 
	empleos permanentes urbanos. La segunda es una variable categórica que describe el rango de edad asociado 
	al asegurado o derechohabiente adscrito.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.0253042.
		2. El error descrito por RMSE es de 204.5.
		3. El coeficiente de determinación ajustado es de 0.02632.
		4. Solo los valores de b0, b6,b8 y b10 son significantes.
		5.AIC es 9867.165, para 14 términos.
		
	La media (b0) de salario base de cotización de empleos permanentes urbanos para el primer rango de edad que va de entre 
	55 y 60 años es de 234.6917. El salario base de cotización media más alto es para el rango de edad de entre 35 y 40 con 
	298.6456, seguido por el rango de edad de entre 40 y 45 con 286.5317, despues se encuentra la categoria de entre 45 y 50 
	con 270.6917, así como el segmento de entre 30 y 35 con 258.0617 y el segmento de entre 25 y 30 con 246.5417; son las 
	categorias que superan a la media general.
	
	Es decir, el rango de edad que cuenta con el salario base de cotización medio más alto de empleos permanentes urbanos, 
	para el sector económico de preparación y servicio de alimentos para el estado de Chiapas, es el de entre 35 y 40 años 
	de edad, seguido por el de entre 40 y 45, entre 45 y 50, entre 30 y 35, y finalmente de entre 25 y 30, respectivamente. 
	Por ello, los grupos entre 30 y 45 años son los que aparentemente cuentan con un salario base de cotización media más alto,
	el cual tiende a caer después de los 55 años de edad.
	
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
#7. Modelo: Salario base de cotización diario de trabajadores permanentes urbanos asegurados (continua, sal_bas_cotdi_tpu) respecto a Rango salarial (categórica, rango_salarial)

#Transformación de varaible categórica
asgChi_tpu$rango_salarial[is.na(asgChi_tpu$rango_salarial)] <- "W0"
asgChi_tpu$rango_salarial_num <- as.integer(asgChi_tpu$rango_salarial_num)
'%notin%' <- Negate('%in%')
acumulador <- 0
contar <- 1
contar1<- 0
for (i in c(asgChi_tpu$rango_salarial)) {
if(asgChi_tpu$rango_salarial[contar] %notin% c(acumulador)){
	acumulador[contar] <- asgChi_tpu$rango_salarial[contar]
	contar1 <- contar1 + 1
	asgChi_tpu$rango_salarial_num[contar]<-contar1
} else {
	acumulador[contar] <- asgChi_tpu$rango_salarial[contar]
	asgChi_tpu$rango_salarial_num[contar]<- c(asgChi_tpu$rango_salarial_num)[c(acumulador)==asgChi_tpu$rango_salarial[contar]]
}
contar <- contar +1 
}
contar <- 1
for (i in unique(asgChi_tpu$rango_salarial)) {
print(unique(asgChi_tpu$rango_salarial_num)[contar]) 
print(unique(asgChi_tpu$rango_salarial)[contar])
contar <- contar + 1
}

#Transformación de variable continua
asgChi_tpu$sal_bas_cotdi_tpu = asgChi_tpu$masa_sal_tpu/asgChi_tpu$tpu_sal
asgChi_tpu$sal_bas_cotdi_tpu[is.na(asgChi_tpu$sal_bas_cotdi_tpu)] <- 0

#Descripción de variables
asgChi_tpu$rango_salarial_num
str(asgChi_tpu$rango_salarial_num)
summary(asgChi_tpu$rango_salarial_num)
table(asgChi_tpu$rango_salarial_num)
asgChi_tpu$sal_bas_cotdi_tpu
str(asgChi_tpu$sal_bas_cotdi_tpu)
summary(asgChi_tpu$sal_bas_cotdi_tpu)
table(asgChi_tpu$sal_bas_cotdi_tpu)
cor(asgChi_tpu$rango_salarial_num, asgChi_tpu$sal_bas_cotdi_tpu)

#Modelo
m7 <- lm(sal_bas_cotdi_tpu ~ factor(rango_salarial_num), asgChi_tpu)
m7
summary(m7)

#Medias
media.1 <- mean(asgChi_tpu[asgChi_tpu$rango_salarial_num == 1, "sal_bas_cotdi_tpu"])
media.2 <- media.1 +284.33
media.3 <- media.1 +402.44
media.4 <- media.1 +145.00
media.5 <- media.1 -25.41
media.6 <- media.1 +784.88
media.7 <- media.1 +910.78
media.8 <- media.1 +510.44
media.9 <- media.1 +628.56
media.10 <- media.1 +1544.52
media.11 <- media.1 +1593.37
media.12 <- media.1 +1003.21
media.13 <- media.1 +1963.62
media.14 <- media.1 +1100.61

#Gráficación
plot(asgChi_tpu$rango_salarial_num, asgChi_tpu$sal_bas_cotdi_tpu)
points(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11, media.12, media.13, media.14), col = "blue", lwd = 2)
lines(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11, media.12, media.13, media.14), col = "blue", lwd = 2)
abline(h = mean(asgChi_tpu$sal_bas_cotdi_tpu), col = "red")

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_tpu respecto a la variable rango_salarial_num. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de la 
	masa salarial o nómina y el número de asegurados asociados a un empleo, en este caso a empleos permanentes urbanos. 
	La segunda es una variable categórica que describe el rango de salarial asociado al asegurado, que representa el 
	número de veces el salario mínimo de la Ciudad de México.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de 0.8277915.
		2. El error descrito por RMSE es de 32.1.
		3. El coeficiente de determinación ajustado es de 0.976.
		4. Todolos los valores (b) son significantes.
		5. AIC es 6438.178, para 14 términos
		
	La media (b0) de salario base de cotización de empleos permanentes urbanos para el primer rango salarial, de entre 
	1 y 2 salarios mínimos es de 148.6314. El mayor salario base de cotización medio es para la categoria de entre 14 y 15 
	salarios mínimos, con 1742.001. Así,todos los valores son significanes.
	
	Es decir, el rango salarial más alto es el que cuenta con el salario base cotización medio más alto para empleos 
	permanentes urbanos. Lo anterior, refleja, junto con las métricas producidas por el modelo, que ambas variables se 
	relacionan bastante, sin embargo, lo anterior es previsible y realmente este modelo no es certero, puesto que el 
	salario base de cotización, depende de forma directa del salarío minimo percivido por el asegurado, por lo cual la 
	utilidad que se produce aquí, no es sustancial.

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
10 "W14" ---> Mayor a 13 y hasta 14 veces el salario mínimo
11 "W15" ---> Mayor a 14 y hasta 15 veces el salario mínimo
12 "W10" ---> Mayor a 9 y hasta 10 veces el salario mínimo
13 "W18" ---> Mayor a 17 y hasta 18 veces el salario mínimo
14 "W11" ---> Mayor a 10 y hasta 11 veces el salario mínimo

------------------------------------------------------------------------------------------------------------------
#8. Modelo: Salario base de cotización diario de trabajadores permanentes urbanos asegurados (continua, sal_bas_cotdi_tpu) respecto al Tamaño de registro patronal (categórica, tama.o_patron)

#Transformación de varaible categorica
asgChi_tpu$tama.o_patron_num <- as.integer(asgChi_tpu$tama.o_patron_num)
acumulador <- 0
contar <- 1
contar1<- 0
for (i in c(asgChi_tpu$tama.o_patron)) {
if(asgChi_tpu$tama.o_patron[contar] %notin% c(acumulador)){
	acumulador[contar] <- asgChi_tpu$tama.o_patron[contar]
	contar1 <- contar1 + 1
	asgChi_tpu$tama.o_patron_num[contar]<-contar1
} else {
	acumulador[contar] <- asgChi_tpu$tama.o_patron[contar]
	asgChi_tpu$tama.o_patron_num[contar]<- c(asgChi_tpu$tama.o_patron_num)[c(acumulador)==asgChi_tpu$tama.o_patron[contar]]
}
contar <- contar +1 
}
contar <- 1
for (i in unique(asgChi_tpu$tama.o_patron)) {
print(unique(asgChi_tpu$tama.o_patron_num)[contar]) 
print(unique(asgChi_tpu$tama.o_patron)[contar])
contar <- contar + 1
}

#Transformación de variable continua
asgChi_tpu$sal_bas_cotdi_tpu = asgChi_tpu$masa_sal_tpu/asgChi_tpu$tpu_sal
asgChi_tpu$sal_bas_cotdi_tpu[is.na(asgChi_tpu$sal_bas_cotdi_tpu)] <- 0

#Descripción de variables
asgChi_tpu$tama.o_patron_num
str(asgChi_tpu$tama.o_patron_num)
summary(asgChi_tpu$tama.o_patron_num)
table(asgChi_tpu$tama.o_patron_num)
asgChi_tpu$sal_bas_cotdi_tpu
str(asgChi_tpu$sal_bas_cotdi_tpu)
summary(asgChi_tpu$sal_bas_cotdi_tpu)
table(asgChi_tpu$sal_bas_cotdi_tpu)
cor(asgChi_tpu$tama.o_patron_num, asgChi_tpu$sal_bas_cotdi_tpu)

#Modelo
m8 <- lm(sal_bas_cotdi_tpu ~ factor(tama.o_patron_num), asgChi_tpu)
m8
summary(m8)

#Medias
media.1 <- mean(asgChi_tpu[asgChi_tpu$tama.o_patron_num == 1, "sal_bas_cotdi_tpu"])
media.2 <- media.1 +5.574
media.3 <- media.1 +92.695
media.4 <- media.1 +207.607
media.5 <- media.1 +293.826

#Gráficación
plot(asgChi_tpu$tama.o_patron_num, asgChi_tpu$sal_bas_cotdi_tpu)
points(c(1,2,3,4,5), c(media.1, media.2, media.3, media.4, media.5), col = "blue", lwd = 2)
lines(c(1,2,3,4,5), c(media.1, media.2, media.3, media.4, media.5), col = "blue", lwd = 2)
abline(h = mean(asgChi_tpu$sal_bas_cotdi_tpu), col = "red")

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_tpu respecto a la variable rango_salarial_num. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de la masa 
	salarial o nómina y el número de asegurados asociados a un empleo, en este caso a empleos permanentes urbanos. La 
	segunda es una variable categórica que describe el rango de salarial asociado al asegurado, que representa el número 
	de veces el salario mínimo de la Ciudad de México.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de 0.4177112.
		2. El error descrito por RMSE es de 186.8.
		3. El coeficiente de determinación ajustado es de 0.1869.
		4. Todolos los valores (b) son significantes, excepto por b1.
		5. AIC es 9691.349 para 5 términos.
		
	La media (b0) de salario base de cotización de empleos permanentes urbanos para la primera categoría de tamaño de 
	registro patronal, de 1 asegurado es de 149.8351. La categoria con la media más alta es la de entre 251 y 500 
	asegurados con 443.6611.
	
	Es decir, el tamaño de registro patronal, de entre 251 y 500 asegurados, es la categoria que tiene un salario base de 
	cotización medio para empleos permanentes urbanos más alto en el estado de Chiapas, en el rubro de preparación y servicio 
	de alimentos.

#Relación de tamaño de registro patronal
1 "S1" ----> 1 asegurado
2 "S2" ----> entre 2 y 5 asegurado
3 "S3" ----> entre 6 y 50 asegurados
4 "S4" ----> entre 51 y 250
5 "S5" ----> entre 251 y 500

------------------------------------------------------------------------------------------------------------------
#9. Modelo: Salario base de cotización diario de trabajadores permanentes urbanos asegurados (continua, sal_bas_cotdi_tpu) respecto a Subdelegación (binaria, cve_subdelegacion)

#Transformación de varaible binaria
asgChi_tpu$cve_subdelegacion_bin[asgChi_tpu$cve_subdelegacion==1] <- 0
asgChi_tpu$cve_subdelegacion_bin[asgChi_tpu$cve_subdelegacion==2] <- 1

#Transformación de variable continua
asgChi_tpu$sal_bas_cotdi_tpu = asgChi_tpu$masa_sal_tpu/asgChi_tpu$tpu_sal
asgChi_tpu$sal_bas_cotdi_tpu[is.na(asgChi_tpu$sal_bas_cotdi_tpu)] <- 0

#Descripción de variables
asgChi_tpu$cve_subdelegacion_bin
str(asgChi_tpu$cve_subdelegacion_bin)
summary(asgChi_tpu$cve_subdelegacion_bin)
table(asgChi_tpu$cve_subdelegacion_bin)
asgChi_tpu$sal_bas_cotdi_tpu
str(asgChi_tpu$sal_bas_cotdi_tpu)
summary(asgChi_tpu$sal_bas_cotdi_tpu)
table(asgChi_tpu$sal_bas_cotdi_tpu)
cor(asgChi_tpu$cve_subdelegacion_bin, asgChi_tpu$sal_bas_cotdi_tpu)

#Modelo
m9 <- lm(sal_bas_cotdi_tpu ~ factor(cve_subdelegacion_bin), asgChi_tpu)
m9
summary(m9)

#Medias
media.tux <- mean(asgChi_tpu[asgChi_tpu$cve_subdelegacion_bin == 0, "sal_bas_cotdi_tpu"])
media.tap <- media.tux - 19.71

#Gráficación
plot(asgChi_tpu$cve_subdelegacion_bin, asgChi_tpu$sal_bas_cotdi_tpu)
points(c(0,1), c(media.tux, media.tap), col = "blue", lwd = 2)
lines(c(0,1), c(media.tux, media.tap), col = "blue", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_tpu respecto a la variable cve_subdelegacion. La primera es una variable 
	continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de la masa salarial o nómina 
	y el número de asegurados asociados a un empleo, en este caso a empleos permanentes urbanos. La segunda es una variable 
	binaria que describe a la subdelegación de abscripción operativa del IMSS en el estado de Chiapas, donde 0 es para la 
	circunscripción territorial a Tuxtla Gutierrez (capital) y 1 a Tapachula.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.04413608.
		2. El error descrito por RMSE es de 207.1.
		3. El coeficiente de determinación ajustado es de 0.0008679.
		4. Solo b0 es significante significantes.
		5. AIC es 9879.165 para 2 términos
		
	La media (b0) del salario base de cotización de empleos permanentes urbanos para cuando hay ausencia del valor de la 
	variable independiente, es decir, cuando es 0, Tuxtla Gutierresz, es de 252.4058. La media para cuando se precenta el 
	valor, es decir, cuando es 1, Tapachula, es de 232.6958. La diferencia entre ambas medias (b1), es de -19.71.
	
	Es decir, la subdelegación de abscripción operativa del IMSS con circunscipción territorial en Tapachula tienen una media 
	de salario base de cotización diario para empleos permanentes urbanos, menor que el de la subdelegación en Tuxtla Gutierrez, 
	para el sector económico de preparación y servicio de alimentos, en el estado de Chiapas.
	
#Relación de subdelegación
0 ----> "Tuxtla Gutierrez"
1 ----> "Tapachula"

------------------------------------------------------------------------------------------------------------------
#10. Modelo: Salario base de cotización diario de trabajadores permanentes urbanos asegurados (continua, sal_bas_cotdi_tpu) respecto a Municipio (categórica, cve_municipio)

#Transformación de varaible categorica
asgChi_tpu$cve_municipio_num <- as.integer(asgChi_tpu$cve_municipio_num)
contar <- 2
contar1<- 1
asgChi_tpu$cve_municipio_num[contar-1]<- 1
for (i in c(asgChi_tpu$cve_municipio)) {
if(asgChi_tpu$cve_municipio[contar]==asgChi_tpu$cve_municipio[contar-1]){
	asgChi_tpu$cve_municipio_num[contar]<-contar1
} else {
	contar1<-contar1+1
	asgChi_tpu$cve_municipio_num[contar]<-contar1
}
	contar <- contar +1 
}
contar <- 1
for (i in unique(asgChi_tpu$cve_municipio)) {
print(unique(asgChi_tpu$cve_municipio_num)[contar]) 
print(unique(asgChi_tpu$cve_municipio)[contar])
contar <- contar + 1
}

#Transformación de variable continua
asgChi_tpu$sal_bas_cotdi_tpu = asgChi_tpu$masa_sal_tpu/asgChi_tpu$tpu_sal
asgChi_tpu$sal_bas_cotdi_tpu[is.na(asgChi_tpu$sal_bas_cotdi_tpu)] <- 0

#Descripción de variables
asgChi_tpu$cve_municipio_num
str(asgChi_tpu$cve_municipio_num)
summary(asgChi_tpu$cve_municipio_num)
table(asgChi_tpu$cve_municipio_num)
asgChi_tpu$sal_bas_cotdi_tpu
str(asgChi_tpu$sal_bas_cotdi_tpu)
summary(asgChi_tpu$sal_bas_cotdi_tpu)
table(asgChi_tpu$sal_bas_cotdi_tpu)
cor(asgChi_tpu$cve_municipio_num, asgChi_tpu$sal_bas_cotdi_tpu)

#Modelo
m10 <- lm(sal_bas_cotdi_tpu ~ factor(cve_municipio_num), asgChi_tpu)
m10
summary(m10)

#Medias
media.1 <- mean(asgChi_tpu[asgChi_tpu$cve_municipio_num == 1, "sal_bas_cotdi_tpu"])
media.2 <- media.1 -34.146
media.3 <- media.1 -38.217 
media.4 <- media.1 +54.533 
media.5 <- media.1 +147.426 
media.6 <- media.1 -23.751
media.7 <- media.1 -46.017
media.8 <- media.1 -46.017
media.9 <- media.1 -42.083 
media.10 <- media.1 +34.233
media.11 <- media.1 -42.980
media.12 <- media.1 -46.017
media.13 <- media.1 +1.938
media.14 <- media.1 -46.017
media.15 <- media.1 -46.017
media.16 <- media.1 -37.396
media.17 <- media.1 -34.549
media.18 <- media.1 -35.403
media.19 <- media.1 +25.193
media.20 <- media.1 -26.766
media.21 <- media.1 -46.017
media.22 <- media.1 -7.716
media.23 <- media.1 +10.478
media.24 <- media.1 -46.017
media.25 <- media.1 -40.513
media.26 <- media.1 -20.412
media.27 <- media.1 +99.129
media.28 <- media.1 -6.586
media.29 <- media.1 -45.960
media.30 <- media.1 -46.017
media.31 <- media.1 -46.017
media.32 <- media.1 -45.507
media.33 <- media.1 -46.017
media.34 <- media.1 -46.017
media.35 <- media.1 -46.017

#Gráficación
plot(asgChi_tpu$cve_municipio_num, asgChi_tpu$sal_bas_cotdi_tpu)
points(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11, media.12, media.13, media.14, media.15, media.16, media.17, media.18, media.19, media.20, media.21, media.22, media.23, media.24, media.25, media.26, media.27, media.28, media.29, media.30, media.31, media.32, media.33, media.34, media.35), col = "blue", lwd = 2)
lines(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11, media.12, media.13, media.14, media.15, media.16, media.17, media.18, media.19, media.20, media.21, media.22, media.23, media.24, media.25, media.26, media.27, media.28, media.29, media.30, media.31, media.32, media.33, media.34, media.35), col = "blue", lwd = 2)
abline(h = mean(asgChi_tpu$sal_bas_cotdi_tpu), col = "red")

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_tpu respecto a la variable cve_municipio. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de 
	la masa salarial o nómina y el número de asegurados asociados a un empleo. La segunda es una variable categórica 
	que describe al municipio asociado a la ubicación del patrón asegurado ante el IMSS, la cuál cuenta con 36 municipios 
	diferentes (subdelegación/delegación puede tener más adscritos que el estado/municipio, ya que esta no considera 
	frenteras estatales).
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.1019347.
		2. El error descrito por RMSE es de 197.8.
		3. El coeficiente de determinación ajustado es de 0.08831.
		4. El únicos valores significantes son b0, b4 y b26.
		5. AIC es 9826.675 para 35 términos
		
	La media (b0) de salario base de cotización diario de empleos permanentes urbanos para la primera categoria, que es el 
	municipio de Comitán de Domínguez, es de 174.8068. Muchas de las categorias o municipios, alrededor de 12, tienen una 
	diferencia de sus media con respecto a la de la primera categoria de -46.017, la cual media de tales municipios, es de 
	128.7898. La media más alta es del municipio de Tuxtla Gutiérrez, la cual es de 322.2328, y la única que está arriva de 
	la media general, junto con el municipio del de Tapachula con media en 273.9358. También, el municipio de San Cristóbal 
	de las Casas, registra un salario base de cotización alto, cercano al medio total, con 229.3398.
	
	Es decir, el municipio asociado a la ubicación del patrón asegurado ante el IMSS del estado de Chiapas con una media de 
	salario base de cotización diario para empleos permanentes urbanos más alto es Tuxtla Gutierrez. Una mayoría de los 
	municipios, 12, tiene una media 128.7898 (cercano al salario mínimo). Los únicos municipios que también superan en su media a 
	245 (alrededor de 2 salarios mínimos) es Tapachula,  aunque con San Cristóbal de las Casas muy cerca.
	
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
26 "A64" ---> Suchiate
27 "A65" ---> Tapachula
28 "A67" ---> Tonalá
29 "A69" ---> Tuxtla Chico
30 "A71" ---> Unión Juárez
31 "M69" ---> Pijijiapan
32 "M70" ---> Mapastepec
33 "M77" ---> Acapetahua
34 "M85" ---> Villa Comaltitlán
35 "M87" ---> Motozintla

------------------------------------------------------------------------------------------------------------------
#11. Modelo: Salario base de cotización diario de trabajadores permanentes urbanos asegurados (continua, sal_bas_cotdi_tpu) respecto a Asegurados (continua, asegurados)

#Transformación de variable dependiente
asgChi_tpu$sal_bas_cotdi_tpu = asgChi_tpu$masa_sal_tpu/asgChi_tpu$tpu_sal
asgChi_tpu$sal_bas_cotdi_tpu[is.na(asgChi_tpu$sal_bas_cotdi_tpu)] <- 0

#Descripción de variables
asgChi_tpu$asegurados
str(asgChi_tpu$asegurados)
summary(asgChi_tpu$asegurados)
table(asgChi_tpu$asegurados)
asgChi_tpu$sal_bas_cotdi_tpu
str(asgChi_tpu$sal_bas_cotdi_tpu)
summary(asgChi_tpu$sal_bas_cotdi_tpu)
table(asgChi_tpu$sal_bas_cotdi_tpu)
cor(asgChi_tpu$asegurados, asgChi_tpu$sal_bas_cotdi_tpu)

#Modelo
#Lineal
m11 <- lm(sal_bas_cotdi_tpu ~ asegurados, asgChi_tpu)
m11
summary(m11)
#Cuadrático
asegurados2 <- asgChi_tpu$asegurados^2
m112 <- lm(sal_bas_cotdi_tpu ~ asegurados + asegurados2, asgChi_tpu)
m112
summary(m112)
#Cúbico
asegurados3 <- asgChi_tpu$asegurados^3
m113 <- lm(sal_bas_cotdi_tpu ~ asegurados + asegurados2 + asegurados3, asgChi_tpu)
m113
summary(m113)
#Potencia 4
asegurados4 <- asgChi_tpu$asegurados^4
m114 <- lm(sal_bas_cotdi_tpu ~ asegurados + asegurados2 + asegurados3 + asegurados4, asgChi_tpu)
m114
summary(m114)


#Cálculo de estimaciones
#Lineal
x <- seq(min(asgChi_tpu$asegurados), max(asgChi_tpu$asegurados), by = 0.01)
y <- 261.537 - 2.475 * x
#Cuadrático
x2 <- x^2
y2 <- 274.36140 - 6.18259 * x + 0.04312 * x^2
#Cúbico
x3 <- x^3
y3 <- 2.861e+02  - 1.119e+01 * x + 1.903e-01 * x^2 - 8.352e-04 * x^3
#Potencia 4
x4 <- x^4
y4 <- 2.974e+02 - 1.743e+01  * x + 5.164e-01 * x^2 - 5.331e-03 * x^3 + 1.742e-05 * x^4

#Gráficación
plot(asgChi_tpu$asegurados, asgChi_tpu$sal_bas_cotdi_tpu)
lines(x, y, col = "blue", lwd = 2)
lines(x, y2, col = "green", lwd = 2)
lines(x, y3, col = "orange", lwd = 2)
lines(x, y4, col = "yellow", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_tpu respecto a la variable asegurados. La primera es una variable 
	continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de la masa salarial o 
	nómina y el número de asegurados asociados a un empleo, en este caso a empleos permanentes urbanos. La segunda es una 
	variable continua o númerica que describe el número de personas que están aseguradas en el IMSS bajo un patrón.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.1673212.
		3. El error descrito por RMSE del modelo lienal es 204.4, cuádratico es 202.6, cúbico es 201.6, de potencia 4 es 200.9.
		4. El coeficiente de determinación ajustado del modelo lienal es 0.02694, cuádratico es 0.04346, cúbico es 0.05367, de potencia 4 es 0.05983.
		5. Tanto b0, como b1 son significantes.
		
	El coeficiente de correlación tiene signo negativo por lo que la realción entre variables en inversamente proporcional.
	La magnitud del mismo es considerablemente baja. El coeficiente de determinación ajustado es bajo, también. Sin embargo,
	las dos b son significantes. Cunado el salario base de cotización para empleos permanentes urbanos aumenta, el número de 
	asegurados bajo un mismo patrón disminuye.
	
	Es decir, mientas más alto es el salario base de cotización de un asegurado permanente urbano, menos asegurados tiene ese 
	mismo patrón. 
	Sin embargo, al no existir una relación fuerte entre variables, y ya que las metricas indican una relación débil, la
	conclusión anterior no es tan plausible, pues hay asegurados con un salario base de cotización igual, sin importar,
	que su patron cuente con 1 asegurado o con más de 150. Por ello las predicciónes resultantes del modelo no son, ni de
	cerca, exactas. El modelo no sigue un modelo lineal, ni cuadrático, ni cúbico, ni a la potencia 4, por más que mejoren 
	las métricas, aunque el que mejor podría predecir es el cuadrático, ya que no cae en valores negativos.
	
------------------------------------------------------------------------------------------------------------------
#12. Modelo: Salario base de cotización diario de trabajadores permanentes urbanos asegurados (continua, sal_bas_cotdi_tpu) respecto a Masa salarial de trabajadores asegurados (continua, masa_sal_ta)

#Transformación de variable dependiente
asgChi_tpu$sal_bas_cotdi_tpu = asgChi_tpu$masa_sal_tpu/asgChi_tpu$tpu_sal
asgChi_tpu$sal_bas_cotdi_tpu[is.na(asgChi_tpu$sal_bas_cotdi_tpu)] <- 0

#Descripción de variables
asgChi_tpu$masa_sal_ta
str(asgChi_tpu$masa_sal_ta)
summary(asgChi_tpu$masa_sal_ta)
table(asgChi_tpu$masa_sal_ta)
asgChi_tpu$sal_bas_cotdi_tpu
str(asgChi_tpu$sal_bas_cotdi_tpu)
summary(asgChi_tpu$sal_bas_cotdi_tpu)
table(asgChi_tpu$sal_bas_cotdi_tpu)
cor(asgChi_tpu$masa_sal_ta, asgChi_tpu$sal_bas_cotdi_tpu)
	
#Modelo
#Lineal
m12 <- lm(sal_bas_cotdi_tpu ~ masa_sal_ta, asgChi_tpu)
m12
summary(m12)
#Cuadrático
masa_sal_ta2 <- asgChi_tpu$masa_sal_ta^2
m122 <- lm(sal_bas_cotdi_tpu ~ masa_sal_ta + masa_sal_ta2, asgChi_tpu)
m122
summary(m122)
#Cúbico
masa_sal_ta3 <- asgChi_tpu$masa_sal_ta^3
m123 <- lm(sal_bas_cotdi_tpu ~ masa_sal_ta + masa_sal_ta2 + masa_sal_ta3, asgChi_tpu)
m123
summary(m123)
#Potencia 4
masa_sal_ta4 <- asgChi_tpu$masa_sal_ta^4
m124 <- lm(sal_bas_cotdi_tpu ~ masa_sal_ta + masa_sal_ta2 + masa_sal_ta3 + masa_sal_ta4, asgChi_tpu)
m124
summary(m124)

#Cálculo de estimaciones
#Lineal
x <- seq(min(asgChi_tpu$masa_sal_ta), max(asgChi_tpu$masa_sal_ta), by = 0.01)
y <- 250.793014 - 0.004395 * x
#Cuadrático
x2 <- x^2
y2 <- 2.413e+02 +  1.080e-02 * x - 1.270e-06 * x^2
#Cúbico
x3 <- x^3
y3 <- 2.157e+02 + 6.929e-02 * x - 1.376e-05 * x^2 + 5.145e-10 * x^3
#Potencia 4
x4 <- x^4
y4 <- 1.747e+02 + 1.923e-01 * x - 6.035e-05 * x^2 + 5.197e-09 * x^3 - 1.320e-13 * x^4

#Cálculo AIC
extractAIC(m12)
extractAIC(m122)
extractAIC(m123)
extractAIC(m124)

#Gráficación
plot(asgChi_tpu$masa_sal_ta, asgChi_tpu$sal_bas_cotdi_tpu)
lines(x, y, col = "blue", lwd = 2)
lines(x, y2, col = "green", lwd = 2)
lines(x, y3, col = "orange", lwd = 2)
lines(x, y4, col = "yellow", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_tpu respecto a la variable masa_sal_ta. La primera es una variable 
	continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de la masa salarial o nómina 
	y el número de asegurados asociados a un empleo, en este caso a empleos permanentes urbanos. La segunda es una variable 
	continua o númerica que describe la nómina que considera tanto el salario como la plantilla de trabajadores a cargo de un 
	patrón.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.04157793.
		3. El error descrito por RMSE del modelo lineal es 207.1, cuadrático es 206.6, cúbico es 203.4, de potencia 4 es 197.2.
		4. El coeficiente de determinación ajustado del modelo lineal es 0.0006483, cuadrático es 0.005527, cúbico es 0.03635, de potencia 4 es 0.09406.
		5. Solo b0 es significante en el modelo lineal. Para el cuadrático, solo b0 y b1. Para el modelo cúbico y de potencia 4, todos los valores son significantes pero para ciertos valores, predicen valores negativos.
		6. AIC para el modelo lineal es 9879.368, cuadrático es 9875.834, cúbico es 9847.677, de potencia 4 es 9791.489.
		
	El coeficiente de correlación tiene signo negativo por lo que la realción entre variables en inversamente proporcional.
	La magnitud del mismo es considerablemente baja. El coeficiente de determinación ajustado es bajo, también. Sin embargo,
	solo b0 es significante en el modelo lineal. Para el cuadrático, solo b0 y b1. Para el modelo cúbico y de potencia 4, todos
	los valores son significantes pero para ciertos valores, predicen valores negativos. Cunado el salario base de cotización 
	en empleos permanentes urbanos aumenta, la masa salarial que paga un patrón por sus trabajadores asegurados disminuye.
	
	Es decir, mientas más alto es el salario base de cotización de un asegurado en empleos permanentes urbanos, menos masa 
	salarial tendrá que pagar el patrón por sus trabajadores asegurados. Sin embargo, al no existir una relación fuerte entre 
	variables, y ya que las métricas indican una relación débil, la conclusión anterior no es tan plausible, pues hay asegurados 
	con un salario base de cotización igual, sin importar, que su patron tenga que pagar una cantidad inferior o mayor de masa 
	salarial a trabajadores asegurados. Por ello las predicciónes resultantes del modelo no son, ni de cerca, exactas. El modelo 
	no sigue un modelo lineal, ni cuadrático, ni cúbico, ni a la potencia 4, por más que mejoren las métricas, aunque el que 
	mejor podría predecir es el lineal, ya que no cae en valores negativos.
		
------------------------------------------------------------------------------------------------------------------
#13. Modelo: Salario base de cotización diario de trabajadores permanentes urbanos asegurados (continua, sal_bas_cotdi_tpu) respecto a Sexo (categórica, sexo) y Tamaño de registro patronal (categórica, tama.o_patron)

#Descripción de variables
cor(asgChi_tpu$sexo_bin, asgChi_tpu$tama.o_patron_num)
cor(asgChi_tpu$tama.o_patron_num, asgChi_tpu$sal_bas_cotdi_tpu)
cor(asgChi_tpu$sexo_bin, asgChi_tpu$sal_bas_cotdi_tpu)

#Modelo
#Lineal
m13 <- lm(sal_bas_cotdi_tpu ~ sexo_bin + tama.o_patron_num, asgChi_tpu)
m13
summary(m13)

#Cálculo de estimaciones
#Lineal
xs <- seq(min(asgChi_tpu$sexo_bin), max(asgChi_tpu$sexo_bin), length.out = 100)
xt <- seq(min(asgChi_tpu$tama.o_patron_num), max(asgChi_tpu$tama.o_patron_num), length.out = 100)
y <- 43.93 - 36.97 * xs + 77.83 * xt

#Cálculo AIC
extractAIC(m13)

#Gráficación
plot((asgChi_tpu$sexo_bin + asgChi_tpu$tama.o_patron_num), asgChi_tpu$sal_bas_cotdi_tpu)
lines(xs+xt, y, col = "blue", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_tpu respecto a la variable sexo_bin y tama.o_patron_num. La primera es 
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de la masa 
	salarial o nómina y el número de asegurados asociados a un empleo, en este caso a empleos permanentes urbanos. La segunda 
	es una variable categórica que describe el sexo, o más específicamente el género del asegurado. La tercera es una variable 
	categórica que describe el rango de salarial asociado al asegurado, que representa el número de veces el salario mínimo de 
	la Ciudad de México.
	
	Las métricas el modelo son:
		1. La correlación entre variables independientes es -0.08324123. Entre sal_bas_cotdi_tpu y tama.o_patron_num es 0.4177112, y entre sal_bas_cotdi_teu y asegurados es -0.1232945.
		3. El error descrito por RMSE del modelo lineal es 187.6.
		4. El coeficiente de determinación ajustado del modelo lineal es 0.1806.
		5. Todos los valores de b son significantes (ya que los valores de x no se explican así mismos, tiene una correlación baja entre sí).
		6. AIC es 9696.52
		
	En la recta de regresión tiende a positivo, por lo que realción entre variables es proporcionales. El coeficiente de 
	determinación ajustado es bajo, también. Sin embargo, todos los valores de b son significantes, ya que los valores de 
	x no se explican así mismos, tiene una correlación baja entre sí. Cunado el sexo es 0, hombre, b1 no tiene valor, pero 
	cuendo es 1, mujer, el cambio al salario será en una proporción de - 36.97. Cuando hay un cambio del tamaño de registro 
	patronal cambia, el cambio al salario será en un proporción de 77.83.
	
	Es decir, mientas más alto es el salario base de cotización de un asegurado en empleos permanentes urbanos en el estado de
	Chiapas en el rubro económico de preparación y servicio de aliemntos; el sexo o género tenderá a ser hombre y el tamaño de 
	registro patronal tenderá a ser mayor.
		
------------------------------------------------------------------------------------------------------------------
#14. Modelo: Salario base de cotización diario de trabajadores permanentes urbanos asegurados (continua, sal_bas_cotdi_tpu) respecto a Asegurados (categórica, sexo) y Masa salarial de trabajadores asegurados eventuales urbanos (continua, masa_sal_teu)

#Descripción de variables
cor(asgChi_tpu$asegurados, asgChi_tpu$masa_sal_tpu)
cor(asgChi_tpu$asegurados, asgChi_tpu$sal_bas_cotdi_tpu)
cor(asgChi_tpu$masa_sal_tpu, asgChi_tpu$sal_bas_cotdi_tpu)

#Modelo
#Lineal
m14 <- lm(sal_bas_cotdi_tpu ~ asegurados + masa_sal_tpu, asgChi_tpu)
m14
summary(m14)

#Cálculo de estimaciones
#Lineal
xa <- seq(min(asgChi_tpu$asegurados), max(asgChi_tpu$asegurados), length.out = 1000)
xm <- seq(min(asgChi_tpu$masa_sal_tpu), max(asgChi_tpu$masa_sal_tpu), length.out = 1000)
y <- 183.3730 - 55.4116 * xa + 0.4009 * xm

#Gráficación
plot((asgChi_tpu$asegurados + asgChi_tpu$masa_sal_tpu), asgChi_tpu$sal_bas_cotdi_tpu)
lines(xa+xm, y, col = "blue", lwd = 2)
		
#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_tpu respecto a la variable asegurados y masa_sal_teu. La primera es 
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de la masa 
	salarial o nómina y el número de asegurados asociados a un empleo, en este caso a empleos permanentes urbanos. La segunda 
	es una variable continua o númerica que describe el número de personas que están aseguradas en el IMSS bajo un patrón. La 
	tercera es una variable continua númerica que describe la nómina que considera tanto el salario como la plantilla de 
	trabajadores a cargo de un patrón, en empleos eventuales urbanos.
	
	Las métricas el modelo son:
		1. La correlación entre variables independientes es 0.9823809. Entre sal_bas_cotdi_teu y asegurados es -0.1673212, y entre sal_bas_cotdi_teu y masa_sal_teu es -0.03711862.
		3. El error descrito por RMSE del modelo lineal es 147.9.
		4. El coeficiente de determinación ajustado del modelo lineal es 0.4905.
		5. Todos los valores de b son significantes en un nivel bajo (los valores de x tienen una correlación alta entre sí, pero ya que y, es producto de ambas variables de x, los valores siguen siendo significantes)
		
	El la recta de regresión tiende a negativo, por lo que realción entre variables es inversamente proporcional. El coeficiente 
	de determinación ajustado es bajo, también. Sin embargo, todos los valores de b son significantes, aunque los valores de x 
	tienen una correlación alta entre sí, pero ya que y, es producto de ambas variables de x, los valores siguen siendo 
	significantes.
	
	Cunado hay un cambio en el número de asegurados por un mismo patrón, el cambio al salario será en una proporción de -55.4116. 
	Cuando hay un cambio en la masa salarial que tiene que pagar un mismo patrón, de trabajadores eventuales urbanos, el cambio 
	al salario será en un proporción de 0.4009.
	
	Es decir, mientas más alto es el salario base de cotización de un asegurado en empleos permanentes urbanos en el estado de
	Chiapas en el rubro económico de preparación y servicio de aliemntos; el número de asegurados por un mismo patrón tendera 
	a ser menor y la masa de salarios de trabajadores eventuales urbanos que paga un patrón tenderá a ser mayor.
		
