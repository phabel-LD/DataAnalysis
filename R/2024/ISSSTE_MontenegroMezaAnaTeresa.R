## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(stringr)
library(ROCR)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("Data.xlsx"))
data


## ----echo=FALSE------------------------

# Clean categorical features

data$GENERO..2.[data$GENERO..2. == 1] <- "Masculino"
data$GENERO..2.[data$GENERO..2. == 2] <- "Femenino"

data$CONSUMO.DE.ANALGÉSICOS..11.[data$CONSUMO.DE.ANALGÉSICOS..11. == 1] <- "Sí"
data$CONSUMO.DE.ANALGÉSICOS..11.[data$CONSUMO.DE.ANALGÉSICOS..11. == 2] <- "No"

data$NAÚSEA.Y.VÓMITO..22.[data$NAÚSEA.Y.VÓMITO..22. == 1] <- "Sí"
data$NAÚSEA.Y.VÓMITO..22.[data$NAÚSEA.Y.VÓMITO..22. == 2] <- "No"

data$PRURITO..23.[data$PRURITO..23. == 1] <- "Sí"
data$PRURITO..23.[data$PRURITO..23. == 2] <- "No"

data$DESATURACIÓN..24.[data$DESATURACIÓN..24. == 1] <- "Sí"
data$DESATURACIÓN..24.[data$DESATURACIÓN..24. == 2] <- "No"

data$O2..25.[data$O2..25. == 1] <- "Sí"
data$O2..25.[data$O2..25. == 2] <- "No"

data$IET..26.[data$IET..26. == 1] <- "Sí"
data$IET..26.[data$IET..26. == 2] <- "No"


data


## ----echo=FALSE------------------------
group_1 <- data[data$GRUPO == 'A',]
group_1


## ----echo=FALSE------------------------
group_2 <- data[data$GRUPO == 'B',]
group_2


## ----echo=FALSE------------------------
data_groups <- data.frame(data %>% count(GRUPO, sort = FALSE))
ggplot(data_groups, aes(x = "", y = n, fill = factor(GRUPO
))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Grupo",
       x=NULL,
       y=NULL,
       title="Proporciones entre Grupos") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_sexo <- data.frame(group_1 %>% count(GENERO..2., sort = FALSE))
ggplot(data_sexo, aes(x = "", y = n, fill = factor(GENERO..2.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género - Grupo A") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_sexo <- data.frame(group_2 %>% count(GENERO..2., sort = FALSE))
ggplot(data_sexo, aes(x = "", y = n, fill = factor(GENERO..2.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género - Grupo B") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_1 %>% count(ASA..3., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(ASA..3.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "ASA",
       x=NULL,
       y=NULL,
       title="Proporciones de ASA - Grupo A") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_2 %>% count(ASA..3., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(ASA..3.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "ASA",
       x=NULL,
       y=NULL,
       title="Proporciones de ASA - Grupo B") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$EVERA.PREVIO..4.)


## ----echo=FALSE------------------------
# Variance
var(group_1$EVERA.PREVIO..4.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$EVERA.PREVIO..4.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$EVERA.PREVIO..4.)


## ----echo=FALSE------------------------
# Variance
var(group_2$EVERA.PREVIO..4.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$EVERA.PREVIO..4.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=EVERA.PREVIO..4., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="EVERA",
       title="Violin Plots de EVERA previo por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = EVERA.PREVIO..4., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="EVERA",
       y="Frequencia",
       title="Histograma de EVERA previo por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$PESO..5.)


## ----echo=FALSE------------------------
# Variance
var(group_1$PESO..5.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$PESO..5.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$PESO..5.)


## ----echo=FALSE------------------------
# Variance
var(group_2$PESO..5.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$PESO..5.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=PESO..5., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Peso (kg)",
       title="Violin Plots de Peso (kg) por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = PESO..5., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Peso (kg)",
       y="Frequencia",
       title="Histograma de Peso (kg) por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$TALLA..6.)


## ----echo=FALSE------------------------
# Variance
var(group_1$TALLA..6.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$TALLA..6.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$TALLA..6.)


## ----echo=FALSE------------------------
# Variance
var(group_2$TALLA..6.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$TALLA..6.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=TALLA..6., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Talla (m)",
       title="Violin Plots de Talla por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = TALLA..6., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Talla (m)",
       y="Frequencia",
       title="Histograma de Talla por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$IMC..7.)


## ----echo=FALSE------------------------
# Variance
var(group_1$IMC..7.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$IMC..7.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$IMC..7.)


## ----echo=FALSE------------------------
# Variance
var(group_2$IMC..7.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$IMC..7.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=IMC..7., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="IMC (kg/m2)",
       title="Violin Plots de IMC por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = IMC..7., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 2,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="IMC (kg/m2)",
       y="Frequencia",
       title="Histograma de IMC por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$TIEMPO.CIRUGÍA..8.)


## ----echo=FALSE------------------------
# Variance
var(group_1$TIEMPO.CIRUGÍA..8.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$TIEMPO.CIRUGÍA..8.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$TIEMPO.CIRUGÍA..8.)


## ----echo=FALSE------------------------
# Variance
var(group_2$TIEMPO.CIRUGÍA..8.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$TIEMPO.CIRUGÍA..8.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=TIEMPO.CIRUGÍA..8., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Tiempo en Cirugía (min)",
       title="Violin Plots de Cirugía (min) por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = TIEMPO.CIRUGÍA..8., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 50,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Cirugía (min)",
       y="Frequencia",
       title="Histograma de Cirugía por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$TIEMPO.ANESTESIA..9.)


## ----echo=FALSE------------------------
# Variance
var(group_1$TIEMPO.ANESTESIA..9.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$TIEMPO.ANESTESIA..9.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$TIEMPO.ANESTESIA..9.)


## ----echo=FALSE------------------------
# Variance
var(group_2$TIEMPO.ANESTESIA..9.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$TIEMPO.ANESTESIA..9.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=TIEMPO.ANESTESIA..9., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Tiempo de Anestesia (min)",
       title="Violin Plots de Tiempo de Anestesia por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = TIEMPO.ANESTESIA..9., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 100,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Tiempo de Anestesia (min)",
       y="Frequencia",
       title="Histograma de Tiempo de Anestesia por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$DOSIS.OPIOIDE..10.)


## ----echo=FALSE------------------------
# Variance
var(group_1$DOSIS.OPIOIDE..10.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$DOSIS.OPIOIDE..10.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$DOSIS.OPIOIDE..10.)


## ----echo=FALSE------------------------
# Variance
var(group_2$DOSIS.OPIOIDE..10.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$DOSIS.OPIOIDE..10.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=DOSIS.OPIOIDE..10., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Dois de Opioide",
       title="Violin Plots de Dois de Opioide por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = DOSIS.OPIOIDE..10., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 250,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Dois de Opioide",
       y="Frequencia",
       title="Histograma de Dois de Opioide por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_1 %>% count(CONSUMO.DE.ANALGÉSICOS..11., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(CONSUMO.DE.ANALGÉSICOS..11.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Consumo de Analgésicos",
       x=NULL,
       y=NULL,
       title="Proporciones de Consumo de Analgésicos - Grupo A") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_2 %>% count(CONSUMO.DE.ANALGÉSICOS..11., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(CONSUMO.DE.ANALGÉSICOS..11.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Consumo de Analgésicos",
       x=NULL,
       y=NULL,
       title="Proporciones de Consumo de Analgésicos - Grupo B") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$EVERA.POST.T0...12.)


## ----echo=FALSE------------------------
# Variance
var(group_1$EVERA.POST.T0...12.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$EVERA.POST.T0...12.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$EVERA.POST.T0...12.)


## ----echo=FALSE------------------------
# Variance
var(group_2$EVERA.POST.T0...12.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$EVERA.POST.T0...12.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=EVERA.POST.T0...12., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="EVERA",
       title="Violin Plots de EVERA por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = EVERA.POST.T0...12., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="EVERA",
       y="Frequencia",
       title="Histograma de EVERA por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$EVERA.POST.T1..13.)


## ----echo=FALSE------------------------
# Variance
var(group_1$EVERA.POST.T1..13.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$EVERA.POST.T1..13.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$EVERA.POST.T1..13.)


## ----echo=FALSE------------------------
# Variance
var(group_2$EVERA.POST.T1..13.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$EVERA.POST.T1..13.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=EVERA.POST.T1..13., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="EVERA",
       title="Violin Plots de EVERA por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = EVERA.POST.T1..13., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="EVERA",
       y="Frequencia",
       title="Histograma de EVERA por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$EVERA.POST.T2..14.)


## ----echo=FALSE------------------------
# Variance
var(group_1$EVERA.POST.T2..14.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$EVERA.POST.T2..14.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$EVERA.POST.T2..14.)


## ----echo=FALSE------------------------
# Variance
var(group_2$EVERA.POST.T2..14.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$EVERA.POST.T2..14.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=EVERA.POST.T2..14., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="EVERA",
       title="Violin Plots de EVERA por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = EVERA.POST.T2..14., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="EVERA",
       y="Frequencia",
       title="Histograma de EVERA por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$EVERA.POST.T3..15.)


## ----echo=FALSE------------------------
# Variance
var(group_1$EVERA.POST.T3..15.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$EVERA.POST.T3..15.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$EVERA.POST.T3..15.)


## ----echo=FALSE------------------------
# Variance
var(group_2$EVERA.POST.T3..15.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$EVERA.POST.T3..15.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=EVERA.POST.T3..15., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="EVERA",
       title="Violin Plots de EVERA por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = EVERA.POST.T3..15., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="EVERA",
       y="Frequencia",
       title="Histograma de EVERA por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$EVERA.POST.T4..16.)


## ----echo=FALSE------------------------
# Variance
var(group_1$EVERA.POST.T4..16.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$EVERA.POST.T4..16.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$EVERA.POST.T4..16.)


## ----echo=FALSE------------------------
# Variance
var(group_2$EVERA.POST.T4..16.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$EVERA.POST.T4..16.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=EVERA.POST.T4..16., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="EVERA",
       title="Violin Plots de EVERA por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = EVERA.POST.T4..16., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="EVERA",
       y="Frequencia",
       title="Histograma de EVERA por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$RESCATES.T0..17.)


## ----echo=FALSE------------------------
# Variance
var(group_1$RESCATES.T0..17.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$RESCATES.T0..17.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$RESCATES.T0..17.)


## ----echo=FALSE------------------------
# Variance
var(group_2$RESCATES.T0..17.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$RESCATES.T0..17.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=RESCATES.T0..17., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Rescates",
       title="Violin Plots de Rescates por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = RESCATES.T0..17., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Rescates",
       y="Frequencia",
       title="Histograma de Rescates por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$RESCATES.T1..18.)


## ----echo=FALSE------------------------
# Variance
var(group_1$RESCATES.T1..18.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$RESCATES.T1..18.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$RESCATES.T1..18.)


## ----echo=FALSE------------------------
# Variance
var(group_2$RESCATES.T1..18.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$RESCATES.T1..18.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=RESCATES.T1..18., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Rescates",
       title="Violin Plots de Rescates por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = RESCATES.T1..18., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Rescates",
       y="Frequencia",
       title="Histograma de Rescates por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$RESCATES.T2..19.)


## ----echo=FALSE------------------------
# Variance
var(group_1$RESCATES.T2..19.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$RESCATES.T2..19.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$RESCATES.T2..19.)


## ----echo=FALSE------------------------
# Variance
var(group_2$RESCATES.T2..19.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$RESCATES.T2..19.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=RESCATES.T2..19., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Rescates",
       title="Violin Plots de Rescates por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = RESCATES.T2..19., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Rescates",
       y="Frequencia",
       title="Histograma de Rescates por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$RESCATES.T3..20.)


## ----echo=FALSE------------------------
# Variance
var(group_1$RESCATES.T3..20.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$RESCATES.T3..20.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$RESCATES.T3..20.)


## ----echo=FALSE------------------------
# Variance
var(group_2$RESCATES.T3..20.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$RESCATES.T3..20.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=RESCATES.T3..20., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Rescates",
       title="Violin Plots de Rescates por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = RESCATES.T3..20., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Rescates",
       y="Frequencia",
       title="Histograma de Rescates por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$RESCATES.4..21.)


## ----echo=FALSE------------------------
# Variance
var(group_1$RESCATES.4..21.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$RESCATES.4..21.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$RESCATES.4..21.)


## ----echo=FALSE------------------------
# Variance
var(group_2$RESCATES.4..21.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$RESCATES.4..21.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=RESCATES.4..21., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Rescates",
       title="Violin Plots de Rescates por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = RESCATES.4..21., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Rescates",
       y="Frequencia",
       title="Histograma de Rescates por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_1 %>% count(NAÚSEA.Y.VÓMITO..22., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(NAÚSEA.Y.VÓMITO..22.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Náusea y Vómito",
       x=NULL,
       y=NULL,
       title="Proporciones de Consumo de Analgésicos - Grupo A") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_2 %>% count(NAÚSEA.Y.VÓMITO..22., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(NAÚSEA.Y.VÓMITO..22.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Náusea y Vómito",
       x=NULL,
       y=NULL,
       title="Proporciones de Náusea y Vómito - Grupo B") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_1 %>% count(PRURITO..23., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(PRURITO..23.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Prurito",
       x=NULL,
       y=NULL,
       title="Proporciones de Prurito - Grupo A") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_2 %>% count(PRURITO..23., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(PRURITO..23.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Prurito",
       x=NULL,
       y=NULL,
       title="Proporciones de Prurito - Grupo B") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_1 %>% count(DESATURACIÓN..24., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(DESATURACIÓN..24.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Desaturación",
       x=NULL,
       y=NULL,
       title="Proporciones de Desaturación - Grupo A") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_2 %>% count(DESATURACIÓN..24., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(DESATURACIÓN..24.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Desaturación",
       x=NULL,
       y=NULL,
       title="Proporciones de Desaturación - Grupo B") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_1 %>% count(O2..25., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(O2..25.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "O2",
       x=NULL,
       y=NULL,
       title="Proporciones de O2 - Grupo A") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_2 %>% count(O2..25., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(O2..25.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "O2",
       x=NULL,
       y=NULL,
       title="Proporciones de O2 - Grupo B") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_1 %>% count(IET..26., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(IET..26.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "IET",
       x=NULL,
       y=NULL,
       title="Proporciones de IET - Grupo A") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_2 %>% count(IET..26., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(IET..26.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "IET",
       x=NULL,
       y=NULL,
       title="Proporciones de IET - Grupo B") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
EVERA_df <- data.frame(
  c(rep(group_1$GRUPO, 5), rep(group_2$GRUPO, 5)),
  c(group_1$EVERA.POST.T0...12., group_1$EVERA.POST.T1..13., group_1$EVERA.POST.T2..14., group_1$EVERA.POST.T3..15., group_1$EVERA.POST.T4..16.,
    group_2$EVERA.POST.T0...12., group_2$EVERA.POST.T1..13., group_2$EVERA.POST.T2..14., group_2$EVERA.POST.T3..15., group_2$EVERA.POST.T4..16.),
  c(rep("T0", length(group_1$EVERA.POST.T0...12.)), rep("T1", length(group_1$EVERA.POST.T1..13.)), rep("T2", length(group_1$EVERA.POST.T2..14.)), rep("T3", length(group_1$EVERA.POST.T3..15.)), rep("T4", length(group_1$EVERA.POST.T4..16.)),
    rep("T0", length(group_2$EVERA.POST.T0...12.)), rep("T1", length(group_2$EVERA.POST.T1..13.)), rep("T2", length(group_2$EVERA.POST.T2..14.)), rep("T3", length(group_2$EVERA.POST.T3..15.)), rep("T4", length(group_2$EVERA.POST.T4..16.)))
)
colnames(EVERA_df) <- c("Grupo", "EVERA", "Tiempo")
EVERA_df


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- EVERA_df %>%
  group_by(Tiempo) %>%
  t_test(EVERA ~ Grupo, paired=FALSE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")

stat.test$p.adj.signif[1] <- 'ns'
stat.test


## ----echo=FALSE, fig.height=10, fig.width=10----
# Grouped Boxplots
bxp <- ggboxplot(
  EVERA_df, x = "Tiempo", y = "EVERA", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="EVERA",
       title="Comparación de EVERA entre Grupos a través del tiempo") +
  theme(plot.title = element_text(size=12))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Tiempo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE, fig.height=10, fig.width=10----
# Grouped Lineplots
lp <- ggline(
  EVERA_df, x = "Tiempo", y = "EVERA", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="EVERA",
       title="Tendencias de EVERA entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=12))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Tiempo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )

