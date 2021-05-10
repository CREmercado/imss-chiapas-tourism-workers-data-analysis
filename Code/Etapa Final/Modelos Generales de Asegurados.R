#MODELOS GENERALES DE ASEGURADOS

asgChi <- filter(asg,  asg$cve_delegacion == 7 & asg$sector_economico_1 == 8 & asg$sector_economico_2 == 87 & asg$sector_economico_4 == 8701)

------------------------------------------------------------------------------------------------------------------
#1. Modelo: Asegurados(continua, asegurados) respecto a Subdelegación(binaria, cve_subdelegacion)

#Transformación de varaible binaria
asgChi$cve_subdelegacion_bin[asgChi$cve_subdelegacion==1] <- 0
asgChi$cve_subdelegacion_bin[asgChi$cve_subdelegacion==2] <- 1

#Descripción de variables
asgChi$cve_subdelegacion_bin
str(asgChi$cve_subdelegacion_bin)
summary(asgChi$cve_subdelegacion_bin)
table(asgChi$cve_subdelegacion_bin)
asgChi$asegurados
str(asgChi$asegurados)
summary(asgChi$asegurados)
table(asgChi$asegurados)
cor(asgChi$cve_subdelegacion_bin, asgChi$asegurados)

#Modelo
m1 <- lm(asegurados ~ cve_subdelegacion_bin, asgChi)
m1
summary(m1)

#Medias
media.tux <- mean(asgChi[asgChi$cve_subdelegacion_bin == 0, "asegurados"])
media.tap <- media.tux - 2.443 

#Gráficación
plot(asgChi$cve_subdelegacion_bin, asgChi$asegurados)
points(c(0,1), c(media.tux, media.tap), col = "blue", lwd = 2)
lines(c(0,1), c(media.tux, media.tap), col = "blue", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable asegurados respecto a la variable cve_subdelegacion. La primera es
	una variable continua o númerica que describe el número de personas que están aseguradas en el IMSS
	bajo un patrón. La segunda es una variable binaria que describe a la subdelegación de abscripción
	operativa del IMSS en el estado de Chiapas, donde 0 es para la circunscripción territorial a Tuxtla
	Gutierrez (capital) y 1 a Tapachula.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.08154166.
		2. El error descrito por RMSE es de 13.89.
		3. El coeficiente de determinación ajustado es de 0.005589.
		4. Tanto la b0 como b1 son significantes.
		5. AIC es 4942.786
		
	La media (b0) para cuando hay ausencia del valor de la variable independiente, es decir, cuando es 0,
	Tuxtla Gutierresz, es de 6.881804 (7) asegurados. La media para cuando se precenta el valor, es decir, 
	cuando es 1, Tapachula, es de 4.438804 (5) asegurados. La diferencia entre ambas medias (b1), es de -2.443 
	(-3) asegurados.
	
	Es decir, la subdelegación de abscripción operativa del IMSS con circunscipción territorial en
	Tapachula tienen una media menor de asegurados por patrón que la subdelegación en Tuxtla Gutierrez, para
	el sector económico de preparación y servicio de alimentos.

#Relación de subdelegación
0 ----> "Tuxtla Gutierrez"
1 ----> "Tapachula"

------------------------------------------------------------------------------------------------------------------
#2. Modelo: Asegurados(continua, asegurados) respecto a Municipio (categórica, cve_municipio)

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

#Descripción de variables
asgChi$cve_municipio_num
str(asgChi$cve_municipio_num)
summary(asgChi$cve_municipio_num)
table(asgChi$cve_municipio_num)
asgChi$asegurados
str(asgChi$asegurados)
summary(asgChi$asegurados)
table(asgChi$asegurados)
cor(asgChi$cve_municipio_num, asgChi$asegurados)

#Modelo
m2 <- lm(asegurados ~ factor(asgChi$cve_municipio_num), asgChi)
m2
summary(m2)

#Medias
media.1 <- mean(asgChi[asgChi$cve_municipio_num == 1, "asegurados"])
media.2 <- media.1 -0.6924
media.3 <- media.1 -1.4737 
media.4 <- media.1 +3.2056 
media.5 <- media.1 +7.9397 
media.6 <- media.1 -1.4737
media.7 <- media.1 -1.4737
media.8 <- media.1 -1.4737
media.9 <- media.1 -0.8070
media.10 <- media.1 -1.4737
media.11 <- media.1 -1.3070
media.12 <- media.1 -1.4737
media.13 <- media.1 -1.1100
media.14 <- media.1 -1.4737
media.15 <- media.1 -1.4737
media.16 <- media.1 -0.8737
media.17 <- media.1 -1.1737
media.18 <- media.1 -1.4737
media.19 <- media.1 -1.4737
media.20 <- media.1 +0.3741
media.21 <- media.1 -1.4737
media.22 <- media.1 -1.2862
media.23 <- media.1 -1.4737
media.24 <- media.1 -1.4737
media.25 <- media.1 -0.8650
media.26 <- media.1 -1.4737
media.27 <- media.1 -1.4737
media.28 <- media.1 +3.5057
media.29 <- media.1 -0.6987
media.30 <- media.1 -0.6737
media.31 <- media.1 -1.4737
media.32 <- media.1 -1.4737
media.33 <- media.1 -1.4737
media.34 <- media.1 -1.4737
media.35 <- media.1 -1.4737
media.36 <- media.1 -1.4737

#Gráficación
plot(asgChi$cve_municipio_num, asgChi$asegurados)
points(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11, media.12, media.13, media.14, media.15, media.16, media.17, media.18, media.19, media.20, media.21, media.22, media.23, media.24, media.25, media.26, media.27, media.28, media.29, media.30, media.31, media.32, media.33, media.34, media.35, media.36), col = "blue", lwd = 2)
lines(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36), c(media.1, media.2, media.3, media.4, media.5, media.6, media.7, media.8, media.9, media.10, media.11, media.12, media.13, media.14, media.15, media.16, media.17, media.18, media.19, media.20, media.21, media.22, media.23, media.24, media.25, media.26, media.27, media.28, media.29, media.30, media.31, media.32, media.33, media.34, media.35, media.36), col = "blue", lwd = 2)
abline(h = mean(asgChi$asegurados), col = "red")

#Descripción del modelo
	Modelo que describe a la variable asegurados respecto a la variable cve_municipio. La primera es
	una variable continua o númerica que describe el número de personas que están aseguradas en el IMSS
	bajo un patrón. La segunda es una variable categórica que describe al municipio asociado a la ubicación 
	del patrón asegurado ante el IMSS, la cuál cuenta con 36 municipios diferentes (subdelegación/delegación 
	puede tener más adscritos que el estado/municipio, ya que esta no considera frenteras estatales).
	
	Las métricas el modelo son:
		1. La correlación del modelo es de -0.1125391.
		2. El error descrito por RMSE es de 13.71.
		3. El coeficiente de determinación ajustado es de 0.03008.
		4. El único valor que es significante b4, que es el municipio A68.
		5. AIC es 4952.664.
		
	La media (b0) para la primera categoría, que es el municipio de Comitán de Domínguez, es de 2.4737 (3) asegurados.
	Muchas de las categorías o municipios, alrededor de 21, tienen una diferencia de sus media con respecto a la de la 
	primera categoría de -1.4737 (-2) asegurados, la cual media de tales municipios, es de 0.9999842, es decir, de 1
	asegurado. La media más alta es del municipio de Tuxtla Gutiérrez, la cual es de 10.41338 (11) asegurados, y la 
	única que significante, pues es mayor a la media total, que es de 6.111821 (7) asegurados. Sin 
	embargo, el municipio de San Cristóbal de las Casas con media en 5.679284 (6) asegurados, y el de Tapachula con 
	media en 5.979384 (6) asegurados, son los que siguen, y están solo un poco por debajo de la media total.
	
	Es decir, el municipio asociado a la ubicación del patrón asegurado ante el IMSS del estado de Chiapas con una
	media de asegurados por patrón más alta es Tuxtla Gutierrez. Una mayoría de los municipios, 21, tiene una media 
	de 1 asegurado por patrón. Los únicos municipios que también superan en sus medias a 5 asegurados por patrón, son
	el municipio San Cristóbal de las Casas y Tapachula, que están serca de la media total.
	
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
#3. Modelo: Masa salarial de trabajador asegurado (continua, masa_sal_ta) respecto a Tamaño de registro patronal (categórica, tama.o_patron)

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

#Descripción de variables
asgChi$tama.o_patron_num
str(asgChi$tama.o_patron_num)
summary(asgChi$tama.o_patron_num)
table(asgChi$tama.o_patron_num)
asgChi$masa_sal_ta
str(asgChi$masa_sal_ta)
summary(asgChi$masa_sal_ta)
table(asgChi$masa_sal_ta)
cor(asgChi$tama.o_patron_num, asgChi$masa_sal_ta)

#Modelo
m3 <- lm(masa_sal_ta ~ factor(tama.o_patron_num), asgChi)
m3
summary(m3)

#Medias
media.1 <- mean(asgChi[asgChi$tama.o_patron_num == 1, "masa_sal_ta"])
media.2 <- media.1 +293.6
media.3 <- media.1 +985.7 
media.4 <- media.1 +1382.6 
media.5 <- media.1 +1300.6 

#Gráficación
plot(asgChi$tama.o_patron_num, asgChi$masa_sal_ta)
points(c(1,2,3,4,5), c(media.1, media.2, media.3, media.4, media.5), col = "blue", lwd = 2)
lines(c(1,2,3,4,5), c(media.1, media.2, media.3, media.4, media.5), col = "blue", lwd = 2)
abline(h = mean(asgChi$masa_sal_ta), col = "red")

#Descripción del modelo
	Modelo que describe a la variable masa_sal_ta respecto a la variable tama.o_patron_num. La primera es
	una variable continua o númerica que describe la nómina que considera tanto el salario como la plantilla 
	de trabajadores a cargo de un patrón. La segunda es una variable categórica que describe el tamaño del 
	patrón determinado con base en el número de asegurados vigentes que registra ante el IMSS.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de 0.2483132.
		2. El error descrito por RMSE es de 1887.
		3. El coeficiente de determinación ajustado es de  0.06303.
		4. Los valores de b2, b3 y b4 son significantes., pero la b0 y b1, no.
		5. AIC es 14170.45.
		
	La media (b0) para la primera categoria, que es el tamaño de registro patronal, deonde el patrón cuenta con solo
	1 asegurado, la masa salarial que el patrón paga, es de 236.1958. Entre 2 y 5 asegurados es de 529.7958, entre 6 y 
	50 asegurados de 1221.896, entre 51 y 250 de 1618.796 y finalmente entre 251 y 500 con 1536.796.
	
	Es decir, en el rubro económico de preparación y servicio de alimentos en el estado de Chiapas, normalmente los
	patrones con un tamaño de registro patronal de entre 51 y 250 asegurados a su cargo, son los que en promedio, 
	paganan una masa salarial mayor, inclusive mayor a los de entre 251 y 500 asegurados. De igual forma, entre la
	categoria de 2 a 5 asegurados (categoria 2) y de 6 a 50 asegurados (categoria 3) hay una distancia de medias 
	bastante abultada.

#Relación de tamaño de registro patronal
1 "S1" ----> 1 asegurado
2 "S2" ----> entre 2 y 5 asegurado
3 "S3" ----> entre 6 y 50 asegurados
4 "S4" ----> entre 51 y 250
5 "S5" ----> entre 251 y 500

------------------------------------------------------------------------------------------------------------------
#4. Modelo: Asegurados(continua, asegurados)  respecto a Tamaño de registro patronal (categórica, tama.o_patron)

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

#Descripción de variables
asgChi$tama.o_patron_num
str(asgChi$tama.o_patron_num)
summary(asgChi$tama.o_patron_num)
table(asgChi$tama.o_patron_num)
asgChi$asegurados
str(asgChi$asegurados)
summary(asgChi$asegurados)
table(asgChi$asegurados)
cor(asgChi$tama.o_patron_num, asgChi$asegurados)

#Modelo
m4 <- lm(asegurados ~ factor(asgChi$tama.o_patron_num), asgChi)
m4
summary(m4)

#Medias
media.1 <- mean(asgChi[asgChi$tama.o_patron_num == 1, "asegurados"])
media.2 <- media.1 +2.156
media.3 <- media.1 +6.070 
media.4 <- media.1 +6.873
media.5 <- media.1 +5.913

#Gráficación
plot(asgChi$tama.o_patron_num, asgChi$asegurados)
points(c(1,2,3,4,5), c(media.1, media.2, media.3, media.4, media.5), col = "blue", lwd = 2)
lines(c(1,2,3,4,5), c(media.1, media.2, media.3, media.4, media.5), col = "blue", lwd = 2)
abline(h = mean(asgChi$asegurados), col = "red")

#Descripción del modelo
	Modelo que describe a la variable asegurados respecto a la variable tama.o_patron_num. La primera es
	una variable continua o númerica que describe el número de personas que están aseguradas en el IMSS bajo 
	un patrón. La segunda es una variable categórica que describe el tamaño del patrón determinado con base 
	en el número de asegurados vigentes que registra ante el IMSS.
	
	Las métricas el modelo son:
		1. La correlación del modelo es de 0.1645799.
		2. El error descrito por RMSE es de 13.72.
		3. El coeficiente de determinación ajustado es de  0.02881.
		4. Los valores de b2, b3 y b4 son significantes.
		5. AIC es 4923.592
		
	La media (b0) para la primera categoria, que es el tamaño de registro patronal, deonde el patrón cuenta con solo
	1 asegurado; los asegurados son de 1.661538 (2). Entre 2 y 5 asegurados es de 3.817538 (4), entre 6 y 50 asegurados 
	de 7.731538 (8), entre 51 y 250 de 8.534538 (9) y finalmente entre 251 y 500 con 7.574538 (8).
	
	Es decir, en el rubro económico de preparación y servicio de alimentos en el estado de Chiapas, normalmente los
	patrones con un tamaño de registro patronal de entre 51 y 250 asegurados a su cargo, son los que en promedio, 
	tienen más asegurados a su cargo, inclusive mayor a los de entre 251 y 500 asegurados. De igual forma, entre la
	categoria de 2 a 5 asegurados (categoria 2) y de 6 a 50 asegurados (categoria 3) hay una distancia de medias 
	en la cual, la segunda dobla a la primera. Esto explica a anterior modelo, pues ya que tienen más asegurados, 
	pagan una mayor masa salarial.

#Relación de tamaño de registro patronal
1 "S1" ----> 1 asegurado
2 "S2" ----> entre 2 y 5 asegurado
3 "S3" ----> entre 6 y 50 asegurados
4 "S4" ----> entre 51 y 250
5 "S5" ----> entre 251 y 500
	
------------------------------------------------------------------------------------------------------------------
#5. Modelo: Masa salarial de trabajadores asegurados (continua, masa_sal_ta) respecto a respecto a Asegurados (continua, asegurados)

#Descripción de variables
asgChi$asegurados
str(asgChi$asegurados)
summary(asgChi$asegurados)
table(asgChi$asegurados)
asgChi$asegurados
str(asgChi$masa_sal_ta)
summary(asgChi$masa_sal_ta)
table(asgChi$masa_sal_ta)
cor(asgChi$asegurados, asgChi$masa_sal_ta)
	
#Modelo
#Lineal
m13 <- lm(masa_sal_ta ~ asegurados, asgChi)
m13
summary(m13)
#Cuadrático
asegurados2 <- asgChi$asegurados^2
m132 <- lm(masa_sal_ta ~ asegurados + asegurados2, asgChi)
m132
summary(m132)
#Cúbico
asegurados3 <- asgChi$asegurados^3
m133 <- lm(masa_sal_ta ~ asegurados + asegurados2 + asegurados3, asgChi)
m133
summary(m133)
#Potencia 4
asegurados4 <- asgChi$asegurados^4
m134 <- lm(masa_sal_ta ~ asegurados + asegurados2 + asegurados3 + asegurados4, asgChi)
m134
summary(m134)

#Cálculo de estimaciones
#Lineal
x <- seq(min(asgChi$asegurados), max(asgChi$asegurados), by = 0.01)
y <- 181.4 + 137.9 * x
#Cuadrático
x2 <- x^2
y2 <- 180.922930 +  138.042308 * x - 0.001703 * x^2
#Cúbico
x3 <- x^3
y3 <- 1.746e+02 + 1.408e+02 * x - 8.137e-02 * x^2 + 4.521e-04 * x^3
#Potencia 4
x4 <- x^4
y4 <- 1.553e+02 + 1.515e+02 * x - 6.460e-01 * x^2 + 8.242e-03 * x^3 - 3.019e-05 * x^4

#Gráficación
plot(asgChi$asegurados, asgChi$masa_sal_ta)
lines(x, y, col = "blue", lwd = 2)
lines(x, y2, col = "green", lwd = 2)
lines(x, y3, col = "orange", lwd = 2)
lines(x, y4, col = "yellow", lwd = 2)

#Descripción del modelo
	Modelo que describe a la variable masa_sal_ta respecto a la variable asegurados. La primera es una variable continua 
	o númerica que describe la nómina que considera tanto el salario como la plantilla de trabajadores a cargo de un patrón. 
	La segunda es una variable continua o númerica que describe el número de personas que están aseguradas en el IMSS bajo 
	un patrón.

	Las métricas el modelo son:
		1. La correlación del modelo es de 0.9848975.
		2. El error descrito por RMSE del modelo lineal es 337.7, cuadrático es 337.9, cúbico es 337.9, de potencia 4 es 336.7.
		3. El coeficiente de determinación ajustado del modelo lineal es 0.97, cuadrático es 0.97, cúbico es 0.97, de potencia 4 es 0.09562.
		4. Tanto b1 como b0 son significantes en el modelo lineal.
		
	El coeficiente de correlación tiene signo positivo por lo que la realción entre variables en proporcional. La magnitud 
	del mismo es bastante alta. El coeficiente de determinación ajustado es alto, para todos los modelos, también. Tanto b0 
	como b1 son significantes en el modelo lineal. Cunado el número de asegurados por un mismo patrón aumenta, la masa 
	salarial que paga el patrón por sus trabajadores asegurados también aumenta
	
	Es decir, mientas más asegurados tenga un patrón, más masa salarial tendrá que pagar el patrón por sus trabajadores 
	asegurados. Este modelo solo biene a comprobar que es lógica la relación entre variables, puesto que en una lógica,
	mientras más obligaciones de pago tenga un patrón, hablando del número de trabajadores asegurados por los que tenga
	que dar aportaciones al IMSS,una mayor cantidad de dinero será obviamenta, con lo cual no es raro, ni un descubrimiento
	determinar una correlación alta entre variables, puesto que una depende de la otra, es decir, la variable masa_sal_ta
	es linealmente dependiente de la variable asegurados.
			