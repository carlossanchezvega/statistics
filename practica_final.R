library(PASWR2)
datos <- BATTERY

# Actividad 1
library(ggplot2)

# 1.1
qplot(datos$lifetime, huegeom="histogram")

# 1.2
datosA = data.frame(lifetime = datos[datos$facility == 'A','lifetime'])
datosB = data.frame(lifetime = datos[datos$facility == 'B','lifetime'])

datosA
# 1.3


qplot(datosA, huegeom="histogram")
qplot(datosB, huegeom="histogram")

# Con un mayor nivel de complejidad en la representación: 
ggplot(data=datosA) +
  geom_histogram( aes(lifetime, ..density..) ) +
  geom_density( aes(lifetime, ..density..) ) +
  geom_rug( aes(lifetime) )+
  labs(title="Histogram para baterias A ") +
  labs(x="Baterias", y="Lifetime")

ggplot(data=datosB) +
  geom_histogram( aes(lifetime, ..density..) ) +
  geom_density( aes(lifetime, ..density..) ) +
  geom_rug( aes(lifetime) )+
  labs(title="Histogram para baterias B ") +
  labs(x="Baterias", y="Lifetime")

# En principio parecen que siguen una distribucción normal pero con alguna perturbación, sobre todo por el lado derecho.

# 1.4

# Comparación para conjunto de datos de A
str(datosA)
summary(datosA$lifetime)
hist(datosA$lifetime, main = expression("Histograma datos datosA"),
     col = "steelblue", border = "white", bg = "white", freq = FALSE)
curve(dnorm(x, mean(datosA$lifetime), sd(datosA$lifetime)), add = TRUE, lwd = 2, lty = 2)

# Gráfico cuantil-cuantil
library(scales)
qqnorm(datosA$lifetime, pch = 20, col = alpha("red4", 0.5),
       las = 1)
grid()
qqline(datosA$lifetime, lwd = 2)

shapiro.test(datosA$lifetime)

library(nortest)
ad.test(datosA$lifetime)

# Comparación para conjunto de datos de B
str(datosB)
summary(datosB$lifetime)
hist(datosB$lifetime, main = expression("Histograma datos datosA"),
     col = "steelblue", border = "white", bg = "white", freq = FALSE)
curve(dnorm(x, mean(datosB$lifetime), sd(datosA$lifetime)), add = TRUE, lwd = 2, lty = 2)

# Gráfico cuantil-cuantil
library(scales)
qqnorm(datosB$lifetime, pch = 20, col = alpha("red4", 0.5),
       las = 1)
grid()
qqline(datosB$lifetime, lwd = 2)

shapiro.test(datosB$lifetime)
ad.test(datosB$lifetime)


# En ambos casos no se puede negar que no sigan una distribucción normal, por tanto siguen una distribucción normal.

# Actividad 2

# Ejercicio 2.1

mean(datosA$lifetime)
sd(datosA$lifetime)

mean(datosB$lifetime)
sd(datosB$lifetime)

# Ejercicio 2.2 Probabilidad de que una batería tomada de A dure más de 210 h.

pnorm(q = 210, mean = mean(datosA$lifetime), sd = sd(datosA$lifetime), lower.tail = FALSE)

# Ejercicio 2.3 Probabilidad de que una bateria de B dure menos de 175 horas
pnorm(q = 175, mean = mean(datosB$lifetime), sd = sd(datosB$lifetime))

# Ejercicio 2.4
qnorm(0.03, mean = mean(datosB$lifetime), sd = sd(datosB$lifetime))

# Actividad 3

# Ejercicio 3.1
dbinom(x = 0, size = 10, prob = pnorm(q = 175, mean = mean(datosB$lifetime), sd = sd(datosB$lifetime)))

# Ejercicio 3.2
pgeom(q = 4, prob = pnorm(q = 175, mean = mean(datosB$lifetime), sd = sd(datosB$lifetime)), lower.tail = FALSE)

# Ejercicio 3.3
dhyper(x = 1, m = 3, k = 5, n = 17, , lower.tail = FALSE)

# Actividad 4

# Ejercicio 4.1

# He puesto que el valor que mide esa distribucción es una poisson
1 - ppois(q = 20, lambda = 12)

# Ejercicio 4.2
ppois(q = 0, lambda = 12)

# Otra opción para las dos anteriores es aproximación a la binomial, ya que n tiene a infinito y p tiende a 0...
n = 1000
p = 12/1000

# Repetición 4.1

1-dbinom(20, 1000, 12/1000)

# Repetición 4.2
dbinom(0, 1000, 12/1000)

# Ejercicio 4.3
# Cuando el valor de lambda es bastante alto como consecuencia del aumento del valor de lambda con media m y desviacion estándar de sqrt(m).
# (Valor de lambda mayor de 25)

# Posiblemente Poisson por la propiedad de la aditividad o puede que se pueda redondear a la normal.



# Actividad 5
# Ejercicio 5.1
rw <- rweibull(5000, 100, 185)
plot(rw)



# Ejercicio 5.2
E_x = 185*gamma(1+(1/100))
summary(rw)
rw[rw< 175]

# Si que se mejora el resultado, ya que en el primer caso al emdia es 179 y en el segundo caso la media es 183.9503. Total de 20 piezas defectuosas en 5 días laborables

# Ejercicio 5.3
# Ofrece una posibilidad mucho menor
pweibull(175, 100, 185)
