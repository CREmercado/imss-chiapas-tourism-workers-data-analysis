#MODELOS TRABAJADORES EVENTUALES URBANOS

asgChi_teu <- filter(asg,  asg$cve_delegacion == 7 & asg$sector_economico_1 == 8 & asg$sector_economico_2 == 87 & asg$sector_economico_4 == 8701 & asg$teu_sal!=0)

------------------------------------------------------------------------------------------------------------------
#5. Modelo: Salario base de cotización diario de trabajadores eventuales urbanos asegurados (continua, sal_bas_cotdi_teu) respecto a Sexo(binaria, sexo)

#Transformación de varaible binaria
asgChi_teu$sexo_bin[asgChi_teu$sexo==1] <- 0
asgChi_teu$sexo_bin[asgChi_teu$sexo==2] <- 1

#Transformación de variable continua
asgChi_teu$sal_bas_cotdi_teu = asgChi_teu$masa_sal_teu/asgChi_teu$teu_sal
asgChi_teu$sal_bas_cotdi_teu[is.na(asgChi_teu$sal_bas_cotdi_teu)] <- 0

#Descripción de variables
asgChi_teu$sexo_bin
str(asgChi_teu$sexo_bin)
summary(asgChi_teu$sexo_bin)
table(asgChi_teu$sexo_bin)
asgChi_teu$sal_bas_cotdi_teu
str(asgChi_teu$sal_bas_cotdi_teu)
summary(asgChi_teu$sal_bas_cotdi_teu)
table(asgChi_teu$sal_bas_cotdi_teu)
cor(asgChi_teu$sexo_bin, asgChi_teu$sal_bas_cotdi_teu)

#Modelo
m5 <- lm(sal_bas_cotdi_teu ~ sexo_bin, asgChi_teu)
m5
summary(m5)

#Medias
media.hom <- mean(asgChi_teu[asgChi_teu$sexo_bin == 0, "sal_bas_cotdi_teu"])
media.muj <- media.hom - 16.26

#Gráficación
plot(asgChi_teu$sexo_bin, asgChi_teu$sal_bas_cotdi_teu)
points(c(0,1), c(media.hom, media.muj), col = "blue", lwd = 2)
lines(c(0,1), c(media.hom, media.muj), col = "blue", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_teu respecto a la variable sexo_bin. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la 
	razón de la masa salarial o nómina y el número de asegurados asociados a un empleo, en este caso a 
	empleos eventuales urbanos. La segunda es una variable categórica que describe el sexo, o más 
	específicamente el género del asegurado.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.1777015.
		2. El error descrito por RMSE es de 45.46.
		3. El coeficiente de determinación ajustado es de 0.01977.
		4. Solo b0 es significantes.
		5. AIC es 643.2221
		
	La media (b0) para cuando hay ausencia del valor de la variable independiente, es decir, cuando es 0,
	de sexo hombre, el salario base de cotización para empleos eventuales urbanos es de 155.3659. La media 
	para cuando se precenta el valor, es decir, cuando es 1, de sexo mujer, el salario base de cotización 
	para empleos eventuales urbanos es de 139.1059. La diferencia entre ambas medias (b1), es de -16.26.
	
	Es decir, el salario medio de cotización medio para un empleo urbano eventual de un hombre está por encima 
	del de una mujer, en el estado de Chiapas para el sector económico de preparación y servicio de alimentos.

#Relación de subdelegación
0 ----> "Hombre"
1 ----> "Mujer"

------------------------------------------------------------------------------------------------------------------
#6. Modelo: Salario base de cotización diario de trabajadores eventuales urbanos asegurados (continua, sal_bas_cotdi_teu) respecto a Rango de edad (categórica, rango_edad)

#Transformación de varaible categórica
asgChi_teu$rango_edad_num <- as.integer(asgChi_teu$rango_edad_num)
'%notin%' <- Negate('%in%')
acumulador <- 0
contar <- 1
contar1<- 0
for (i in c(asgChi_teu$rango_edad)) {
if(asgChi_teu$rango_edad[contar] %notin% c(acumulador)){
	acumulador[contar] <- asgChi_teu$rango_edad[contar]
	contar1 <- contar1 + 1
	asgChi_teu$rango_edad_num[contar]<-contar1
} else {
	acumulador[contar] <- asgChi_teu$rango_edad[contar]
	asgChi_teu$rango_edad_num[contar]<- c(asgChi_teu$rango_edad_num)[c(acumulador)==asgChi_teu$rango_edad[contar]]
}
contar <- contar +1 
}
contar <- 1
for (i in unique(asgChi_teu$rango_edad)) {
print(unique(asgChi_teu$rango_edad_num)[contar]) 
print(unique(asgChi_teu$rango_edad)[contar])
contar <- contar + 1
}

#Transformación de variable continua
asgChi_teu$sal_bas_cotdi_teu = asgChi_teu$masa_sal_teu/asgChi_teu$teu_sal
asgChi_teu$sal_bas_cotdi_teu[is.na(asgChi_teu$sal_bas_cotdi_teu)] <- 0

#Descripción de variables
asgChi_teu$rango_edad_num
str(asgChi_teu$rango_edad_num)
summary(asgChi_teu$rango_edad_num)
table(asgChi_teu$rango_edad_num)
asgChi_teu$sal_bas_cotdi_teu
str(asgChi_teu$sal_bas_cotdi_teu)
summary(asgChi_teu$sal_bas_cotdi_teu)
table(asgChi_teu$sal_bas_cotdi_teu)
cor(asgChi_teu$rango_edad_num, asgChi_teu$sal_bas_cotdi_teu)

#Modelo
m6 <- lm(sal_bas_cotdi_teu ~ factor(rango_edad_num), asgChi_teu)
m6
summary(m6)

#Medias
media.1 <- mean(asgChi_teu[asgChi_teu$rango_edad_num == 1, "sal_bas_cotdi_teu"])
media.2 <- media.1 -0.5575 
media.3 <- media.1 -2.1450
media.4 <- media.1 -2.8600
media.5 <- media.1 +1.9325
media.6 <- media.1 +1.7758
media.7 <- media.1 +16.3496
media.8 <- media.1 +11.9639
media.9 <- media.1 +41.0994 
media.10 <- media.1 +12.5157
media.11 <- media.1 +33.5800

#Gráficación
plot(asgChi_teu$rango_edad_num, asgChi_teu$sal_bas_cotdi_teu)
points(c(1,2,3,4,5,6,7,8,9,10,11), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11), col = "blue", lwd = 2)
lines(c(1,2,3,4,5,6,7,8,9,10,11), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11), col = "blue", lwd = 2)
abline(h = mean(asgChi_teu$sal_bas_cotdi_teu), col = "red")

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_teu respecto a la variable rango_edad_num. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la 
	razón de la masa salarial o nómina y el número de asegurados asociados a un empleo, en este caso a 
	empleos eventuales urbanos. La segunda es una variable categórica que describe el rango de edad asociado al 
	asegurado o derechohabiente adscrito.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de 0.2445232.
		2. El error descrito por RMSE es de 46.11.
		3. El coeficiente de determinación ajustado es de -0.008372.
		4. Solo el valor de b0 es significante.
		5. AIC es 653.8337 para 11 términos.
		
	La media (b0) de salario base de cotización para el primer rango de edad que va de entre 60 y 65 años es de 133.08. 
	El salario base de cotización media más alto es para el rango de edad de entre 30 y 35 con 174.1794, seguido por el 
	rango de edad de entre 65 y 70 con 166.66, despues se encuentra la categoria de entre 40 y 45 con 145.5957, así como 
	el segmento de entre 35 y 40 con 145.0439. 
	
	Es decir, el rango de edad que cuenta con el salario base de cotización medio más alto de empleo eventual urbano para el 
	sector económico de preparación y servicio de alimentos para el estado de Chiapas, es el de entre 30 y 35 años de edad, 
	seguido por el de entre 65 y 70, donde realmente solo hay 1 dato. De este modo, las categorias más altas, van de entre 30 
	a 45 años de edad. Con lo cual resalta que este tipos de empleos eventuales, registra rangos de edad menores que al total 
	de empleados asegurados.
	
#Relación de rango de edad
1 "E11" ----> Mayor o igual a 60 y menor a 65 años de edad
2 "E8" -----> Mayor o igual a 45 y menor a 50 años de edad
3 "E10" ----> Mayor o igual a 55 y menor a 60 años de edad
4 "E9" ----> Mayor o igual a 50 y menor a 55 años de edad
5 "E2" ----> Mayor o igual a 15 y menor a 20 años de edad
6 "E3" -----> Mayor o igual a 20 y menor a 25 años de edad
7 "E4" -----> Mayor o igual a 25 y menor a 30 años de edad
8 "E6" -----> Mayor o igual a 35 y menor a 40 años de edad
9 "E5" -----> Mayor o igual a 30 y menor a 35 años de edad
10 "E7" -----> Mayor o igual a 40 y menor a 45 años de edad
11 "E12" ----> Mayor o igual a 65 y menor a 70 años de edad

------------------------------------------------------------------------------------------------------------------
#7. Modelo: Salario base de cotización diario de trabajadores eventuales urbanos asegurados (continua, sal_bas_cotdi_teu) respecto a Rango salarial (categórica, rango_salarial)

#Transformación de varaible categórica
asgChi_teu$rango_salarial[is.na(asgChi_teu$rango_salarial)] <- "W0"
asgChi_teu$rango_salarial_num <- as.integer(asgChi_teu$rango_salarial_num)
'%notin%' <- Negate('%in%')
acumulador <- 0
contar <- 1
contar1<- 0
for (i in c(asgChi_teu$rango_salarial)) {
if(asgChi_teu$rango_salarial[contar] %notin% c(acumulador)){
	acumulador[contar] <- asgChi_teu$rango_salarial[contar]
	contar1 <- contar1 + 1
	asgChi_teu$rango_salarial_num[contar]<-contar1
} else {
	acumulador[contar] <- asgChi_teu$rango_salarial[contar]
	asgChi_teu$rango_salarial_num[contar]<- c(asgChi_teu$rango_salarial_num)[c(acumulador)==asgChi_teu$rango_salarial[contar]]
}
contar <- contar +1 
}
contar <- 1
for (i in unique(asgChi_teu$rango_salarial)) {
print(unique(asgChi_teu$rango_salarial_num)[contar]) 
print(unique(asgChi_teu$rango_salarial)[contar])
contar <- contar + 1
}

#Transformación de variable continua
asgChi_teu$sal_bas_cotdi_teu = asgChi_teu$masa_sal_teu/asgChi_teu$teu_sal
asgChi_teu$sal_bas_cotdi_teu[is.na(asgChi_teu$sal_bas_cotdi_teu)] <- 0

#Descripción de variables
asgChi_teu$rango_salarial_num
str(asgChi_teu$rango_salarial_num)
summary(asgChi_teu$rango_salarial_num)
table(asgChi_teu$rango_salarial_num)
asgChi_teu$sal_bas_cotdi_teu
str(asgChi_teu$sal_bas_cotdi_teu)
summary(asgChi_teu$sal_bas_cotdi_teu)
table(asgChi_teu$sal_bas_cotdi_teu)
cor(asgChi_teu$rango_salarial_num, asgChi_teu$sal_bas_cotdi_teu)

#Modelo
m7 <- lm(sal_bas_cotdi_teu ~ factor(rango_salarial_num), asgChi_teu)
m7
summary(m7)

#Medias
media.1 <- mean(asgChi_teu[asgChi_teu$rango_salarial_num == 1, "sal_bas_cotdi_teu"])
media.2 <- media.1 +113.9
media.3 <- media.1 +339.8

#Gráficación
plot(asgChi_teu$rango_salarial_num, asgChi_teu$sal_bas_cotdi_teu)
points(c(1,2,3), c(media.1, media.2, media.3), col = "blue", lwd = 2)
lines(c(1,2,3), c(media.1, media.2, media.3), col = "blue", lwd = 2)
abline(h = mean(asgChi_teu$sal_bas_cotdi_teu), col = "red")

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_ta respecto a la variable rango_salarial_num. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de la 
	masa salarial o nómina y el número de asegurados asociados a un empleo, en este caso a empleos eventuales urbanos. 
	La segunda es una variable categórica que describe el rango de salarial asociado al asegurado, que representa el 
	número de veces el salario mínimo de la Ciudad de México.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de 0.8731799.
		2. El error descrito por RMSE es de 21.49.
		3. El coeficiente de determinación ajustado es de 0.7809.
		4. Todolos los valores (b) son significantes.
		5. AIC es 518.3196 para 3 términos
		
	La media (b0) de salario base de cotización para el primer rango salarial, de entre 1 y 2 salarios mínimos es 
	de 141.0562. El mayor salario base de cotización medio es para la categoria de entre 3 y 4 salarios mínimos,
	con 480.8562. Así,todos los valores son significanes.
	
	Es decir, el rango salarial más alto es el que cuenta con el salario base cotización medio más alto. Lo anterior,
	refleja, junto con las métricas producidas por el modelo, que ambas variables se relacionan bastante, sin embargo,
	estp es previsible y realmente este modelo no es certero, puesto que el salario base de cotización, depende de forma 
	directa del salarío minimo percivido por el asegurado, por lo cual la utilidad que se produce aquí, no es sustancial.
	Por otro lado, se puede mencionar que para los empleos eventuales urbanos, el salario mínimo comun es el que cae en la 
	categoria de entre 1 y 2. Los datos por encima de esta categoria realmente son un par.

#Relación de rango de edad
1 "W2" -----> Mayor a 1 y hasta 2 veces el salario mínimo
4 "W3" -----> Mayor a 2 y hasta 3 veces el salario mínimo
2 "W4" -----> Mayor a 3 y hasta 4 veces el salario mínimo

------------------------------------------------------------------------------------------------------------------
#8. Modelo: Salario base de cotización diario de trabajadores eventuales urbanos asegurados (continua, sal_bas_cotdi_teu) respecto al Tamaño de registro patronal (categórica, tama.o_patron)

#Transformación de varaible categorica
asgChi_teu$tama.o_patron_num <- as.integer(asgChi_teu$tama.o_patron_num)
acumulador <- 0
contar <- 1
contar1<- 0
for (i in c(asgChi_teu$tama.o_patron)) {
if(asgChi_teu$tama.o_patron[contar] %notin% c(acumulador)){
	acumulador[contar] <- asgChi_teu$tama.o_patron[contar]
	contar1 <- contar1 + 1
	asgChi_teu$tama.o_patron_num[contar]<-contar1
} else {
	acumulador[contar] <- asgChi_teu$tama.o_patron[contar]
	asgChi_teu$tama.o_patron_num[contar]<- c(asgChi_teu$tama.o_patron_num)[c(acumulador)==asgChi_teu$tama.o_patron[contar]]
}
contar <- contar +1 
}
contar <- 1
for (i in unique(asgChi_teu$tama.o_patron)) {
print(unique(asgChi_teu$tama.o_patron_num)[contar]) 
print(unique(asgChi_teu$tama.o_patron)[contar])
contar <- contar + 1
}

#Transformación de variable continua
asgChi_teu$sal_bas_cotdi_teu = asgChi_teu$masa_sal_teu/asgChi_teu$teu_sal
asgChi_teu$sal_bas_cotdi_teu[is.na(asgChi_teu$sal_bas_cotdi_teu)] <- 0

#Descripción de variables
asgChi_teu$tama.o_patron_num
str(asgChi_teu$tama.o_patron_num)
summary(asgChi_teu$tama.o_patron_num)
table(asgChi_teu$tama.o_patron_num)
asgChi_teu$sal_bas_cotdi_teu
str(asgChi_teu$sal_bas_cotdi_teu)
summary(asgChi_teu$sal_bas_cotdi_teu)
table(asgChi_teu$sal_bas_cotdi_teu)
cor(asgChi_teu$tama.o_patron_num, asgChi_teu$sal_bas_cotdi_teu)

#Modelo
m8 <- lm(sal_bas_cotdi_teu ~ factor(tama.o_patron_num), asgChi_teu)
m8
summary(m8)

#Medias
media.1 <- mean(asgChi_teu[asgChi_teu$tama.o_patron_num == 1, "sal_bas_cotdi_teu"])
media.2 <- media.1 +2.529
media.3 <- media.1 -6.022
media.4 <- media.1 +39.713

#Gráficación
plot(asgChi_teu$tama.o_patron_num, asgChi_teu$sal_bas_cotdi_teu)
points(c(1,2,3,4), c(media.1, media.2, media.3, media.4), col = "blue", lwd = 2)
lines(c(1,2,3,4), c(media.1, media.2, media.3, media.4), col = "blue", lwd = 2)
abline(h = mean(asgChi_teu$sal_bas_cotdi_teu), col = "red")

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_teu respecto a la variable rango_salarial_num. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de la 
	masa salarial o nómina y el número de asegurados asociados a un empleo, en este caso a empleos eventuales urbanos. 
	La segunda es una variable categórica que describe el rango de salarial asociado al asegurado, que representa el 
	número de veces el salario mínimo de la Ciudad de México.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de 0.3572078.
		2. El error descrito por RMSE es de 42.96.
		3. El coeficiente de determinación ajustado es de 0.1248.
		4. Solo b0 y b3 son significantes.
		5. AIC es 635.6235 para 4 términos
		
	La media (b0) de salario base de cotización para la primera categoría de tamaño de registro patronal, de entre 6 y
	50 asegurados es de 135.7097. La categoria con la media más alta es la de entre 51 y 250 asegurados, con 175.4227.
	
	Es decir, el tamaño de registro patronal, de entre 51 y 250 asegurados, es la categoria que tiene un salario base de 
	cotización medio más alto para empleos eventuales urbanos en el estado de Chiapas, en el rubro de preparación y servicio 
	de alimentos; con lo cual se podría rezonar, que los patrones con un registro de aseugrados grande son los que mejor pagan
	en este tipo de empleos.

#Relación de tamaño de registro patronal
1 "S3" ----> entre 6 y 50 asegurados
2 "S2" ----> entre 2 y 5 asegurado
3 "S1" ----> 1 asegurado
4 "S4" ----> entre 51 y 250

------------------------------------------------------------------------------------------------------------------
#9. Modelo: Salario base de cotización diario de trabajadores eventuales urbanos asegurados (continua, sal_bas_cotdi_teu) respecto a Subdelegación (binaria, cve_subdelegacion)

#Transformación de varaible binaria
asgChi_teu$cve_subdelegacion_bin[asgChi_teu$cve_subdelegacion==1] <- 0
asgChi_teu$cve_subdelegacion_bin[asgChi_teu$cve_subdelegacion==2] <- 1

#Transformación de variable continua
asgChi_teu$sal_bas_cotdi_teu = asgChi_teu$masa_sal_teu/asgChi_teu$teu_sal
asgChi_teu$sal_bas_cotdi_teu[is.na(asgChi_teu$sal_bas_cotdi_teu)] <- 0

#Descripción de variables
asgChi_teu$cve_subdelegacion_bin
str(asgChi_teu$cve_subdelegacion_bin)
summary(asgChi_teu$cve_subdelegacion_bin)
table(asgChi_teu$cve_subdelegacion_bin)
asgChi_teu$sal_bas_cotdi_teu
str(asgChi_teu$sal_bas_cotdi_teu)
summary(asgChi_teu$sal_bas_cotdi_teu)
table(asgChi_teu$sal_bas_cotdi_teu)
cor(asgChi_teu$cve_subdelegacion_bin, asgChi_teu$sal_bas_cotdi_teu)

#Modelo
m9 <- lm(sal_bas_cotdi_teu ~ factor(cve_subdelegacion_bin), asgChi_teu)
m9
summary(m9)

#Medias
media.tux <- mean(asgChi_teu[asgChi_teu$cve_subdelegacion_bin == 0, "sal_bas_cotdi_teu"])
media.tap <- media.tux - 14.99

#Gráficación
plot(asgChi_teu$cve_subdelegacion_bin, asgChi_teu$sal_bas_cotdi_teu)
points(c(0,1), c(media.tux, media.tap), col = "blue", lwd = 2)
lines(c(0,1), c(media.tux, media.tap), col = "blue", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_teu respecto a la variable cve_subdelegacion. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de 
	la masa salarial o nómina y el número de asegurados asociados a un empleo, en este caso a empleos eventuales 
	urbanos. La segunda es una variable binaria que describe a la subdelegación de abscripción operativa del IMSS 
	en el estado de Chiapas, donde 0 es para la circunscripción territorial a Tuxtla Gutierrez (capital) y 1 a 
	Tapachula.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.1223687.
		2. El error descrito por RMSE es de 45.85.
		3. El coeficiente de determinación ajustado es de 0.002962.
		4. Solo b0 es significante.
		5. AIC es 644.6501 para 2 términos
		
	La media (b0) del salario base de cotización para cuando hay ausencia del valor de la variable independiente, 
	es decir, cuando es 0, Tuxtla Gutierresz, es de 150.3125. La media para cuando se precenta el valor, es decir, 
	cuando es 1, Tapachula, es de 135.3225. La diferencia entre ambas medias (b1), es de -14.99.
	
	Es decir, la subdelegación de abscripción operativa del IMSS con circunscipción territorial en Tapachula tienen 
	una media de salario base de cotización diario menor, para empleos eventuales urbanos que el de la subdelegación 
	en Tuxtla Gutierrez, para el sector económico de preparación y servicio de alimentos, en el estado de Chiapas.
	
#Relación de subdelegación
0 ----> "Tuxtla Gutierrez"
1 ----> "Tapachula"

------------------------------------------------------------------------------------------------------------------
#10. Modelo: Salario base de cotización diario de trabajadores eventuales urbanos asegurados (continua, sal_bas_cotdi_teu) respecto a Municipio (categórica, cve_municipio)

#Transformación de varaible categorica
asgChi_teu$cve_municipio_num <- as.integer(asgChi_teu$cve_municipio_num)
contar <- 2
contar1<- 1
asgChi_teu$cve_municipio_num[contar-1]<- 1
for (i in c(asgChi_teu$cve_municipio)) {
if(asgChi_teu$cve_municipio[contar]==asgChi_teu$cve_municipio[contar-1]){
	asgChi_teu$cve_municipio_num[contar]<-contar1
} else {
	contar1<-contar1+1
	asgChi_teu$cve_municipio_num[contar]<-contar1
}
	contar <- contar +1 
}
contar <- 1
for (i in unique(asgChi_teu$cve_municipio)) {
print(unique(asgChi_teu$cve_municipio_num)[contar]) 
print(unique(asgChi_teu$cve_municipio)[contar])
contar <- contar + 1
}

#Transformación de variable continua
asgChi_teu$sal_bas_cotdi_teu = asgChi_teu$masa_sal_teu/asgChi$teu_sal
asgChi_teu$sal_bas_cotdi_teu[is.na(asgChi_teu$sal_bas_cotdi_teu)] <- 0

#Descripción de variables
asgChi_teu$cve_municipio_num
str(asgChi_teu$cve_municipio_num)
summary(asgChi_teu$cve_municipio_num)
table(asgChi_teu$cve_municipio_num)
asgChi_teu$sal_bas_cotdi_teu
str(asgChi_teu$sal_bas_cotdi_teu)
summary(asgChi_teu$sal_bas_cotdi_teu)
table(asgChi_teu$sal_bas_cotdi_teu)
cor(asgChi_teu$cve_municipio_num, asgChi_teu$sal_bas_cotdi_teu)

#Modelo
m10 <- lm(sal_bas_cotdi_teu ~ factor(cve_municipio_num), asgChi_teu)
m10
summary(m10)

#Medias
media.1 <- mean(asgChi_teu[asgChi_teu$cve_municipio_num == 1, "sal_bas_cotdi_teu"])
media.2 <- media.1 +5.715 
media.3 <- media.1 +4.578 
media.4 <- media.1 +29.781
media.5 <- media.1 -4.290 
media.6 <- media.1 -2.430
media.7 <- media.1 -1.534
media.8 <- media.1 +9.225 

#Gráficación
plot(asgChi_teu$cve_municipio_num, asgChi_teu$sal_bas_cotdi_teu)
points(c(1,2,3,4,5,6,7,8), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8), col = "blue", lwd = 2)
lines(c(1,2,3,4,5,6,7,8), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8), col = "blue", lwd = 2)
abline(h = mean(asgChi_teu$sal_bas_cotdi_teu), col = "red")

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_teu respecto a la variable cve_municipio. La primera es
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de 
	la masa salarial o nómina y el número de asegurados asociados a un empleo, en este caso a empleos eventuales 
	urbanos. La segunda es una variable categórica que describe al municipio asociado a la ubicación del patrón 
	asegurado ante el IMSS, la cuál cuenta con 36 municipios diferentes (subdelegación/delegación puede tener más 
	adscritos que el estado/municipio, ya que esta no considera frenteras estatales).
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.006713102.
		2. El error descrito por RMSE es de 45.91.
		3. El coeficiente de determinación ajustado es de 0.0004089.
		4. El único valor significante es b0.
		5. AIC es 650.4821 para 8 términos
		
	La media (b0) de salario base de cotización diario para la primera categoria, que es el municipio de Comitán de 
	Domínguez, es de 133.08. Solo 8 municipios registran este tipo de empleos eventuales urbanos. La media más alta 
	es del municipio de Tuxtla Gutiérrez, la cual es de 162.861, y la única que supera la media general y donde se
	registran más datos.
	
	Es decir, el municipio asociado a la ubicación del patrón asegurado ante el IMSS del estado de Chiapas con una
	media de salario base de cotización diario más alto, para empleos eventuales urbanos es Tuxtla Gutierrez. Los otros 
	municipios que también registran bastantes datos son Chiapa de Corzo, San Cristóbal de las Casas y Tapachula, pero no
	llegan a la media. El municipio de Tonalá se aproxima a la media general, pero con menos datos.
	
#Relación de municipios
1 "A54" ----> Comitán de Domínguez
2 "A55" ----> Chiapa de Corzo
3 "A62" ----> San Cristóbal de las Casas
4 "A68" ----> Tuxtla Gutiérrez
5 "M86" ---> Ocozocoautla de Espinosa
6 "A61" ---> Metapa
7 "A65" ---> Tapachula
8 "A67" ---> Tonalá

	
------------------------------------------------------------------------------------------------------------------
#11. Modelo: Salario base de cotización diario de trabajadores eventuales urbanos asegurados (continua, sal_bas_cotdi_teu) respecto a Masa salarial de trabajadores asegurados (continua, masa_sal_ta)

#Transformación de variable dependiente
asgChi_teu$sal_bas_cotdi_teu = asgChi_teu$masa_sal_teu/asgChi_teu$teu_sal
asgChi_teu$sal_bas_cotdi_teu[is.na(asgChi_teu$sal_bas_cotdi_teu)] <- 0

#Descripción de variables
asgChi_teu$masa_sal_ta
str(asgChi_teu$masa_sal_ta)
summary(asgChi_teu$masa_sal_ta)
table(asgChi_teu$masa_sal_ta)
asgChi_teu$sal_bas_cotdi_teu
str(asgChi_teu$sal_bas_cotdi_teu)
summary(asgChi_teu$sal_bas_cotdi_teu)
table(asgChi_teu$sal_bas_cotdi_teu)
cor(asgChi_teu$masa_sal_ta, asgChi_teu$sal_bas_cotdi_teu)
	
#Modelo
#Lineal
m11 <- lm(sal_bas_cotdi_teu ~ masa_sal_ta, asgChi_teu)
m11
summary(m11)
#Cuadrático
masa_sal_ta2 <- asgChi_teu$masa_sal_ta^2
m112 <- lm(sal_bas_cotdi_teu ~ masa_sal_ta + masa_sal_ta2, asgChi_teu)
m112
summary(m112)
#Cúbico
masa_sal_ta3 <- asgChi_teu$masa_sal_ta^3
m113 <- lm(sal_bas_cotdi_teu ~ masa_sal_ta + masa_sal_ta2 + masa_sal_ta3, asgChi_teu)
m113
summary(m113)
#Potencia 4
masa_sal_ta4 <- asgChi_teu$masa_sal_ta^4
m114 <- lm(sal_bas_cotdi_teu ~ masa_sal_ta + masa_sal_ta2 + masa_sal_ta3 + masa_sal_ta4, asgChi_teu)
m114
summary(m114)

#Cálculo de estimaciones
#Lineal
x <- seq(min(asgChi_teu$masa_sal_ta), max(asgChi_teu$masa_sal_ta), by = 0.01)
y <- 1.513e+02 - 9.124e-04 * x
#Cuadrático
x2 <- x^2
y2 <- 1.538e+02 - 2.361e-03 * x + 8.946e-08 * x^2
#Cúbico
x3 <- x^3
y3 <- 1.517e+02 - 1.065e-04 * x - 2.666e-07 * x^2 + 1.281e-11 * x^3
#Potencia 4
x4 <- x^4
y4 <- 1.487e+02 + 4.775e-03 * x - 1.658e-06 * x^2 + 1.331e-10 * x^3 - 3.102e-15 * x^4

#Cálculo AIC
extractAIC(m11)
extractAIC(m112)
extractAIC(m113)
extractAIC(m114)

#Gráficación
plot(asgChi_teu$masa_sal_ta, asgChi_teu$sal_bas_cotdi_teu)
lines(x, y, col = "blue", lwd = 2)
lines(x, y2, col = "green", lwd = 2)
lines(x, y3, col = "orange", lwd = 2)
lines(x, y4, col = "yellow", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_teu respecto a la variable masa_sal_ta. La primera es una variable 
	continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de la masa salarial o 
	nómina y el número de asegurados asociados a un empleo, en este caso a empleos eventuales urbanos. La segunda es una 
	variable continua númerica que describe la nómina que considera tanto el salario como la plantilla de trabajadores a 
	cargo de un patrón, en empleos eventuales urbanos.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.08988315.
		3. El error descrito por RMSE del modelo lineal es 46.01, cuadrático es 46.23, cúbico es 46.47, de potencia 4 es 46.68.
		4. El coeficiente de determinación ajustado del modelo lineal es -0.004018, cuadrático es -0.01329, cúbico es -0.02386, de potencia 4 es -0.03327.
		5. Solo b0 es significante en todos los modelo.
		6. AIC para el modelo lineal es 645.236, cuadrático es 646.9777, cúbico es 648.8058, de potencia 4 es 650.5179.
		
	El coeficiente de correlación tiene signo negativo por lo que la realción entre variables es inversamente proporcional. 
	La magnitud del mismo es considerablemente baja. El coeficiente de determinación ajustado es bajo, también. Sin embargo,
	solo b0 es sigificante en todos lo modelos. Cunado el salario base de cotización para empleos eventuales urbanos aumenta, 
	la masa salarial que paga un patrón por sus trabajadores asegurados disminuye.
	
	Es decir, mientas más alto es el salario base de cotización de un asegurado, menos masa salarial tendrá que pagar el
	patrón por sus trabajadores asegurados. Sin embargo, al no existir una relación fuerte entre variables, y ya que las 
	metricas indican una relación débil, la conclusión anterior no es tan plausible, pues hay asegurados con un salario 
	base de cotización igual, sin importar, que su patron tenga que pagar una cantidad inferior o mayor de masa salarial 
	a trabajadores asegurados. Por ello las predicciónes resultantes del modelo no son, ni de cerca, exactas. El modelo 
	no sigue un modelo lineal, ni cuadrático, ni cúbico, ni a la potencia 4, por más que mejoren las métricas, aunque el 
	que mejor podría predecir es el de potencia 4, ya que se acerca más a lo valores reales.
		
------------------------------------------------------------------------------------------------------------------
#12. Modelo: Salario base de cotización diario de trabajadores eventuales urbanos asegurados (continua, sal_bas_cotdi_teu) respecto a Sexo (categórica, sexo) y Tamaño de registro patronal (categórica, tama.o_patron)

#Descripción de variables
cor(asgChi_teu$sexo_bin, asgChi_teu$tama.o_patron_num)
cor(asgChi_teu$tama.o_patron_num, asgChi_teu$sal_bas_cotdi_teu)
cor(asgChi_teu$sexo_bin, asgChi_teu$sal_bas_cotdi_teu)

#Modelo
#Lineal
m12 <- lm(sal_bas_cotdi_teu ~ sexo_bin + tama.o_patron_num, asgChi_teu)
m12
summary(m12)

#Cálculo de estimaciones
#Lineal
xs <- seq(min(asgChi_teu$sexo_bin), max(asgChi_teu$sexo_bin), length.out = 100)
xt <- seq(min(asgChi_teu$tama.o_patron_num), max(asgChi_teu$tama.o_patron_num), length.out = 100)
y <- 127.00 - 16.05 * xs + 12.77 * xt

#Cálculo AIC
extractAIC(m12)

#Gráficación
plot((asgChi_teu$sexo_bin + asgChi_teu$tama.o_patron_num), asgChi_teu$sal_bas_cotdi_teu)
lines(xs+xt, y, col = "blue", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable sal_bas_cotdi_teu respecto a la variable sexo_bin y tama.o_patron_num. La primera es 
	una variable continua o númerica que describe el salario base de cotización (SBC) que refiere a la razón de la masa 
	salarial o nómina y el número de asegurados asociados a un empleo, en este caso a empleos eventuales urbanos. La segunda 
	es una variable categórica que describe el sexo, o más específicamente el género del asegurado. La tercera es una variable 
	categórica que describe el rango de salarial asociado al asegurado, que representa el número de veces el salario mínimo de 
	la Ciudad de México.
	
	Las métricas el modelo son:
		1. La correlación entre variables independientes es -0.006696465. Entre sal_bas_cotdi_teu y tama.o_patron_num es 0.3572078, y entre sal_bas_cotdi_teu y asegurados es -0.1777015.
		3. El error descrito por RMSE del modelo lineal es 42.65.
		4. El coeficiente de determinación ajustado del modelo lineal es 0.1376.
		5. Todos los valores de b son significantes (ya que los valores de x no se explican así mismos, tiene una correlación baja entre sí).
		6. AIC es 633.4384
		
	En la recta de regresión tiende a positivo, por lo que realción entre variables es proporcionales. El coeficiente de 
	determinación ajustado es bajo, también. Sin embargo, todos los valores de b son significantes, ya que los valores de 
	x no se explican así mismos, tiene una correlación baja entre sí. Cunado el sexo es 0, hombre, b1 no tiene valor, pero 
	cuendo es 1, mujer, el cambio al salario será en una proporción de - 16.05. Cuando hay un cambio del tamaño de registro 
	patronal cambia, el cambio al salario será en una proporción de 12.77.
	
	Es decir, mientas más alto es el salario base de cotización de un asegurado en empleos eventuales urbanos en el estado de
	Chiapas en el rubro económico de preparación y servicio de aliemntos; el sexo o género tenderá a ser hombre y el tamaño de 
	registro patronal tenderá a ser mayor.
