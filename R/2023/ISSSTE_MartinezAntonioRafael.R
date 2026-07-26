## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggpmisc)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("MartinezAntonioRafael_Data.xlsx"))


## ----echo=FALSE------------------------
data


## ----echo=FALSE------------------------
data_sex <- data.frame(data %>% count(Sexo..1..Hombre..2..Mujer., sort = FALSE))
data_sex

## ----echo=FALSE------------------------
ggplot(data_sex, aes(x = "", y = n, fill = factor(Sexo..1..Hombre..2..Mujer.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Sexo",
       x=NULL,
       y=NULL,
       title="Proporciones entre Sexos") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Hombre", "Mujer")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = Edad..años., fill = factor(Sexo..1..Hombre..2..Mujer.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 4,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Edad",
       y="Frequencia",
       title="Histograma de Edad por Sexo") +
  scale_fill_discrete(labels = c("Hombre", "Mujer")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
summary(data$Edad..años.)


## ----echo=FALSE------------------------
var(data$Edad..años.)


## ----echo=FALSE------------------------
sd(data$Edad..años.)


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = Peso...Kilogramos., fill = factor(Sexo..1..Hombre..2..Mujer.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 4,
                 alpha = 0.5) +
  labs(fill = "Sexo",
       x="Peso (Kg)",
       y="Frequencia",
       title="Histograma de Peso por Grupo - Sexo") +
  scale_fill_discrete(labels = c("Hombre", "Mujer")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
summary(data$Peso...Kilogramos.)


## ----echo=FALSE------------------------
var(data$Peso...Kilogramos.)


## ----echo=FALSE------------------------
sd(data$Peso...Kilogramos.)


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = as.numeric(Estatura..metros.), fill = factor(Sexo..1..Hombre..2..Mujer.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.025,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Estatura (m)",
       y="Frequencia",
       title="Histograma de Talla por Grupo - Sexo") +
  scale_fill_discrete(labels = c("Hombre", "Mujer")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
summary(data$Estatura..metros.)


## ----echo=FALSE------------------------
var(data$Estatura..metros.)


## ----echo=FALSE------------------------
sd(data$Estatura..metros.)


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = as.numeric(IMC), fill = factor(Sexo..1..Hombre..2..Mujer.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 2,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="IMC",
       y="Frequencia",
       title="Histograma de IMC por Grupo - Sexo") +
  scale_fill_discrete(labels = c("Hombre", "Mujer")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
summary(data$IMC)


## ----echo=FALSE------------------------
var(data$IMC)


## ----echo=FALSE------------------------
sd(data$IMC)


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = as.numeric(Circunferencia.de.cuello..cm.), fill = factor(Sexo..1..Hombre..2..Mujer.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 2,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Circunferencia de Cuello (cm)",
       y="Frequencia",
       title="Histograma de Circunferencia de Cuello (cm) por Grupo - Sexo") +
  scale_fill_discrete(labels = c("Hombre", "Mujer")) +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
summary(as.numeric(data$Circunferencia.de.cuello..cm.))


## ----echo=FALSE------------------------
var(as.numeric(data$Circunferencia.de.cuello..cm.))


## ----echo=FALSE------------------------
sd(as.numeric(data$Circunferencia.de.cuello..cm.))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = as.numeric(Numero.de.Operadores), fill = factor(Sexo..1..Hombre..2..Mujer.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="No. de Operadores",
       y="Frequencia",
       title="Histograma de No. de Operadores por Grupo - Sexo") +
  scale_fill_discrete(labels = c("Hombre", "Mujer")) +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
summary(na.omit(data$Numero.de.Operadores))


## ----echo=FALSE------------------------
var(na.omit(data$Numero.de.Operadores))


## ----echo=FALSE------------------------
sd(na.omit(data$Numero.de.Operadores))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = as.numeric(POGO....0..100...1.80.10...2...10..), fill = factor(Sexo..1..Hombre..2..Mujer.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.5,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="POGO",
       y="Frequencia",
       title="Histograma de POGO por Grupo - Sexo") +
  scale_fill_discrete(labels = c("Hombre", "Mujer")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
summary(na.omit(data$POGO....0..100...1.80.10...2...10..))


## ----echo=FALSE------------------------
var(na.omit(data$POGO....0..100...1.80.10...2...10..))


## ----echo=FALSE------------------------
sd(na.omit(data$POGO....0..100...1.80.10...2...10..))


## ----echo=FALSE------------------------
estil <- data.frame(na.omit(data) %>% count(Uso.de.estiletes..0..No.1..Sí., sort = FALSE))
estil


## ----echo=FALSE------------------------
ggplot(estil, aes(x = "", y = n, fill = factor(Uso.de.estiletes..0..No.1..Sí.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Estiletes",
       x=NULL,
       y=NULL,
       title="Proporciones de Uso de Estiletes") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("No", "Sí")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
burp <- data.frame(na.omit(data) %>% count(BURP..0..No..1..Sí., sort = FALSE))
burp


## ----echo=FALSE------------------------
ggplot(burp, aes(x = "", y = n, fill = factor(BURP..0..No..1..Sí.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "BURP",
       x=NULL,
       y=NULL,
       title="Proporciones de BURP: No - Sí") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("No", "Sí")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
voc <- data.frame(na.omit(data) %>% count(Cuerdas.vocales..0..Abducción..1..Aduccion., sort = FALSE))
voc


## ----echo=FALSE------------------------
ggplot(voc, aes(x = "", y = n, fill = factor(Cuerdas.vocales..0..Abducción..1..Aduccion.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Cuerdas Vocals",
       x=NULL,
       y=NULL,
       title="Proporciones de Cuerdas Vocales: Abducción - Aducctión") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Abducción", "Aducción")) +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = as.numeric(Intentos.de.Intubación), fill = factor(Sexo..1..Hombre..2..Mujer.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.5,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Intentos",
       y="Frequencia",
       title="Histograma de Intentos de Intubación por Grupo - Sexo") +
  scale_fill_discrete(labels = c("Hombre", "Mujer")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
summary(na.omit(data$Intentos.de.Intubación))


## ----echo=FALSE------------------------
var(na.omit(data$Intentos.de.Intubación))


## ----echo=FALSE------------------------
sd(na.omit(data$Intentos.de.Intubación))


## ----echo=FALSE------------------------

data$Dificultad[data$Escala.de.Intubacion.difícil...0..Fácil..1.4..Ligera.dificultad..5..Moderada.dificultad..6.8..Severa.dificultad. == 0] <- "Fácil"
data$Dificultad[is.na(data$Escala.de.Intubacion.difícil...0..Fácil..1.4..Ligera.dificultad..5..Moderada.dificultad..6.8..Severa.dificultad.)] <- "Fácil"


data$Dificultad[data$Escala.de.Intubacion.difícil...0..Fácil..1.4..Ligera.dificultad..5..Moderada.dificultad..6.8..Severa.dificultad. > 1 & data$Escala.de.Intubacion.difícil...0..Fácil..1.4..Ligera.dificultad..5..Moderada.dificultad..6.8..Severa.dificultad. <= 4] <- "Ligera Dif."


data$Dificultad[data$Escala.de.Intubacion.difícil...0..Fácil..1.4..Ligera.dificultad..5..Moderada.dificultad..6.8..Severa.dificultad. == 5] <- "Moderada Dif."


data$Dificultad[data$Escala.de.Intubacion.difícil...0..Fácil..1.4..Ligera.dificultad..5..Moderada.dificultad..6.8..Severa.dificultad. > 5] <- "Severa Dif."

data$Dificultad


## ----echo=FALSE------------------------
intub <- data.frame(data %>% count(Dificultad, sort = FALSE))
intub


## ----echo=FALSE------------------------
ggplot(intub, aes(x = "", y = n, fill = factor(Dificultad))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Intubación",
       x=NULL,
       y=NULL,
       title="Proporciones de Dificultad de Intubación") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Fácil", "Ligera Dif.", "Moderadad Dif.", "Severa Dif.")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Promedio de Dificultad por Circunferencia de Cuello única 
circunf <- sort(unique(as.numeric(data$Circunferencia.de.cuello..cm.[!is.na(data$Escala.de.Intubacion.difícil...0..Fácil..1.4..Ligera.dificultad..5..Moderada.dificultad..6.8..Severa.dificultad.)])))

diff <- c(
  mean(data$Escala.de.Intubacion.difícil...0..Fácil..1.4..Ligera.dificultad..5..Moderada.dificultad..6.8..Severa.dificultad.[data$Circunferencia.de.cuello..cm. == circunf[1]]),
  mean(data$Escala.de.Intubacion.difícil...0..Fácil..1.4..Ligera.dificultad..5..Moderada.dificultad..6.8..Severa.dificultad.[data$Circunferencia.de.cuello..cm. == circunf[2]]),
  mean(data$Escala.de.Intubacion.difícil...0..Fácil..1.4..Ligera.dificultad..5..Moderada.dificultad..6.8..Severa.dificultad.[data$Circunferencia.de.cuello..cm. == circunf[3]]),
  mean(data$Escala.de.Intubacion.difícil...0..Fácil..1.4..Ligera.dificultad..5..Moderada.dificultad..6.8..Severa.dificultad.[data$Circunferencia.de.cuello..cm. == circunf[4]]),
  mean(data$Escala.de.Intubacion.difícil...0..Fácil..1.4..Ligera.dificultad..5..Moderada.dificultad..6.8..Severa.dificultad.[data$Circunferencia.de.cuello..cm. == circunf[5]]),
  mean(data$Escala.de.Intubacion.difícil...0..Fácil..1.4..Ligera.dificultad..5..Moderada.dificultad..6.8..Severa.dificultad.[data$Circunferencia.de.cuello..cm. == circunf[6]]),
  mean(data$Escala.de.Intubacion.difícil...0..Fácil..1.4..Ligera.dificultad..5..Moderada.dificultad..6.8..Severa.dificultad.[data$Circunferencia.de.cuello..cm. == circunf[7]])
)

cor(circunf, diff)


## ----echo=FALSE------------------------
cor_df <- data.frame(circunf, diff)
cor_df


## ----echo=FALSE------------------------
ggplot(data = cor_df, aes(x = circunf, y = diff)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  labs(x="Circunferencia Cuello (cm)",
       y="Dificultad Media",
       title="Correlación: Circunferencia de Cuello - Dificultad Media de Intubación ") +
  theme(plot.title = element_text(size=10))


## ----echo=FALSE------------------------
cor_df_1 <- cor_df[cor_df$circunf != 44,]
cor_df_1

## ----echo=FALSE------------------------
ggplot(data = cor_df_1, aes(x = circunf, y = diff)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  labs(x="Circunferencia Cuello (cm)",
       y="Dificultad Media",
       title="Correlación: Circunferencia de Cuello - Dificultad Media de Intubación ") +
  theme(plot.title = element_text(size=10))

