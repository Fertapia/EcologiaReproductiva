############# Pr?ctica 1: Ecolog?a reproductiva de organismos acu?ticos #########

########## Curva de maduraci?n #################################################


#### Instalar paquetes ###########

Paquetes <- c("FSA", 
              "tidyverse", 
              "magrittr", 
              "lubridate", 
              "car")

install.packages(Paquetes)

library("FSA")
library("tidyverse")
library("lubridate")
library("magrittr")
library("car")

#### Base de datos ##############

## Directorio de trabajo ########

setwd("D:/Documentos/Trabajo/Docencia/UCSUR/2020-2/Clases/Practica")

## Datos #######################

df <- read.csv("PracticaRepro.csv")

## Procesar variables y limpiar base de datos

# Fecha a formato de fecha y cambiamos las variables Madurez y Sexo a factores
df <- df %>% 
  mutate(Fecha = as.POSIXct(Fecha, format = "%m/%d/%Y"),
         Madurez = as.factor(Madurez),
         Sexo = as.factor(Sexo)) 


# Clasificamos en era los a?os antes de 2002 y despu?s de 2002

df <- df %>% 
  mutate(
         year = year(Fecha), 
         era = ifelse(year < 2002, "Pre-2002", "Post-2002"),
         era = factor(era, levels = c("Pre-2002", "Post-2002"))
        )

df <- df %>% filterD(!is.na(Madurez))


## 1. An?lisis exploratorio

df <- df %>% 
  mutate(lcat2 = lencat(Longitud, w = 2))

freq <- xtabs(~lcat2 + Madurez, data = df)

props <- prop.table(freq, margin = 1)

props.df <- as.data.frame(props)

props.df <- props.df %>% 
  filter(Madurez == "Maduro")

lcat1 <- as.numeric(levels(props.df$lcat2))

props.df$lcat1 <- lcat1

ggplot(props.df, aes(x = lcat2, y = Freq)) +
  geom_point() + 
  labs(title = paste("Proporci?n de individuos aclanzando la madurez"), 
       x = "Longitud (cm)", 
       y = "Frecuencia") + 
  theme_bw()

# 2. Ajustando la regersi?n log?stica.
glm1 <- glm(Madurez ~ Longitud, data = df, family = binomial)


# Obtenemos coeficientes

coef(glm1)

# Calculamos los intervalos de confianza

bcL <- bootCase(glm1, B = 100)
cbind(Ests = coef(glm1), confint(bcL))


## 2.1 Calculamos la probabilidad de que el individuo est? maduro

predict(glm1, data.frame(Longitud = c(32,42)), type = "response")

predP <- function(cf, x) exp(cf[1] + cf[2] * x) / (1 + exp(cf[1] + cf[2] * x))

p32 <- apply(bcL, 1, predP, x = 32)

quantile(p32, c(0.025, 0.975))


## Resumen gr?fico

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

ggplot(df, aes(Longitud, as.numeric(Madurez) - 1)) + 
  binomial_smooth() + 
  geom_point(position = position_jitter(height = 0.03, width = 0)) + 
  xlab("Longitud (cm)") + 
  ylab("Probabilidad de individuo maduro") + 
  theme_bw()


## 2.2 Calculamos la longitud de que un individuo est? maduro dada una probabilidad

lrPerc <- function(cf,p) (log(p/(1-p)) - cf[[1]]) /cf[[2]]

(L50 <- lrPerc(coef(glm1), 0.5))

(L90 <- lrPerc(coef(glm1), 0.9))

# Calculamos los intervalos de confianza

bL50 <- apply(bcL, 1, lrPerc, p = 0.5)
(L50ci <- quantile(bL50, c(0.025, 0.975)))

bL90 <- apply(bcL, 1, lrPerc, p = 0.9)
(L90ci <- quantile(bL90, c(0.025, 0.975)))


ggplot(df, aes(Longitud, as.numeric(Madurez) - 1)) + 
  binomial_smooth() + 
  (position = position_jitter(height = 0.03, width = 0)) + 
  geom_point(data = props.df, aes(x = lcat1, y = Freq), size = 2, shape = 3, color = "darkgreen") + 
  xlab("Longitud (cm)") + 
  ylab("Probabilidad de individuo maduro") + 
  geom_segment(aes(x = L50, y = 0, xend = L50, yend = 0.5), color = "darkred") + 
  geom_segment(aes(x = min(df$Longitud), y = 0.5, xend = L50, yend = 0.5), color = "darkred") + 
  theme_bw()

##### Comparando entre Grupos ##############

glm2 <- glm(Madurez ~ Longitud * Sexo, data = df, family = binomial)

Anova(glm2)

coef(glm2)

bcL2 <- bootCase(glm2, B = 1000)

L50.pre = apply(bcL2[, 1:2], 1, lrPerc, p = 0.5)
L50.pre.m <- mean(L50.pre)
L50.post = apply(bcL2[, 1:2] + bcL2[, 3:4], 1, lrPerc, p = 0.5)
L50.post.m <- mean(L50.post)

L50.diff <- L50.pre - L50.post

(p.L50.diff <- 2 * min(c(mean(L50.diff > 0), mean(L50.diff < 0))))

(ci.L50.pre <- quantile(L50.pre, c(0.025, 0.975)))

(ci.L50.post <- quantile(L50.post, c(0.025, 0.975)))

(L50.pre.Fin <- c(L50.pre.m, ci.L50.pre))

(L50.post.Fin <- c(L50.post.m, ci.L50.post))

ggplot(df, aes(Longitud, as.numeric(Madurez) - 1, color = Sexo)) + 
  binomial_smooth() + 
  geom_point(position = position_jitter(height = 0.03, width = 0)) +
  xlab("Longitud (cm)") + 
  ylab("Probabilidad de individuo maduro") + 
  theme_bw()
