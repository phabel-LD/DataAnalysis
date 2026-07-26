## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(stringr)
library(plotly)
library(car)
library(babynames)
library(gapminder)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("Data.xlsx"))
data


## ----echo=FALSE------------------------

# Clean categorical features
data$GRUPO[data$GRUPO == 1] <- 'Tapentadol'
data$GRUPO[data$GRUPO == 2] <- 'Pregabalina'

data$GENERO[data$GENERO == 1] <- 'Masculino'
data$GENERO[data$GENERO == 2] <- 'Femenino'

data$NAUSEA.VÓMITO[data$NAUSEA.VÓMITO == 1] <- 'Sí'
data$NAUSEA.VÓMITO[data$NAUSEA.VÓMITO == 2] <- 'No'


data$EVERA.T0[data$EVERA.T0 == 1] <- 'Ausencia'
data$EVERA.T0[data$EVERA.T0 == 2] <- 'Leve'
data$EVERA.T0[data$EVERA.T0 == 3] <- 'Moderado'
data$EVERA.T0[data$EVERA.T0 == 4] <- 'Severo'

data$EVERA.T1[data$EVERA.T1 == 1] <- 'Ausencia'
data$EVERA.T1[data$EVERA.T1 == 2] <- 'Leve'
data$EVERA.T1[data$EVERA.T1 == 3] <- 'Moderado'
data$EVERA.T1[data$EVERA.T1 == 4] <- 'Severo'

data$EVERA.T2[data$EVERA.T2 == 1] <- 'Ausencia'
data$EVERA.T2[data$EVERA.T2 == 2] <- 'Leve'
data$EVERA.T2[data$EVERA.T2 == 3] <- 'Moderado'
data$EVERA.T2[data$EVERA.T2 == 4] <- 'Severo'

data$EVERA.T3[data$EVERA.T3 == 1] <- 'Ausencia'
data$EVERA.T3[data$EVERA.T3 == 2] <- 'Leve'
data$EVERA.T3[data$EVERA.T3 == 3] <- 'Moderado'
data$EVERA.T3[data$EVERA.T3 == 4] <- 'Severo'

data$EVERA.T4[data$EVERA.T4 == 1] <- 'Ausencia'
data$EVERA.T4[data$EVERA.T4 == 2] <- 'Leve'
data$EVERA.T4[data$EVERA.T4 == 3] <- 'Moderado'
data$EVERA.T4[data$EVERA.T4 == 4] <- 'Severo'


data


## ----echo=TRUE-------------------------
group_Tapentadol <- data[data$GRUPO == 'Tapentadol',]
group_Tapentadol


## ----echo=TRUE-------------------------
group_Pregabalina <- data[data$GRUPO == 'Pregabalina',]
group_Pregabalina


## ----echo=FALSE------------------------
data_groups <- data.frame(data %>% count(GRUPO, sort = FALSE))
p <- ggplot(data_groups, aes(x = "", y = n, fill = factor(GRUPO
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
p


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Tapentadol$EDAD)


## ----echo=FALSE------------------------
# Variance
var(group_Tapentadol$EDAD)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Tapentadol$EDAD)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Pregabalina$EDAD)


## ----echo=FALSE------------------------
# Variance
var(group_Pregabalina$EDAD)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Pregabalina$EDAD)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=EDAD, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Premedicación",
       x="Premedicación",
       y="Edad",
       title="Violin Plots de Edad por Premedicación") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = EDAD, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Premedicación",
       x="Edad",
       y="Frequencia",
       title="Histograma de Edad por Premedicación") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
data_sexo <- data.frame(group_Tapentadol %>% count(GENERO, sort = FALSE))
p <- ggplot(data_sexo, aes(x = "", y = n, fill = factor(GENERO))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género - Tapentadol") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
data_sexo <- data.frame(group_Pregabalina %>% count(GENERO, sort = FALSE))
p <- ggplot(data_sexo, aes(x = "", y = n, fill = factor(GENERO))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género - Pregabalina") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Tapentadol$PESO)


## ----echo=FALSE------------------------
# Variance
var(group_Tapentadol$PESO)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Tapentadol$PESO)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Pregabalina$PESO)


## ----echo=FALSE------------------------
# Variance
var(group_Pregabalina$PESO)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Pregabalina$PESO)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=PESO, color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Premedicación",
       x="Premedicación",
       y="Peso (kg)",
       title="Violin Plots de Peso (kg) por Premedicación") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = PESO, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Premedicación",
       x="Peso (kg)",
       y="Frequencia",
       title="Histograma de Peso (kg) por Premedicación") +
  theme(plot.title = element_text(size=12))

p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Tapentadol$IMC)


## ----echo=FALSE------------------------
# Variance
var(group_Tapentadol$IMC)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Tapentadol$IMC)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Pregabalina$IMC)


## ----echo=FALSE------------------------
# Variance
var(group_Pregabalina$IMC)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Pregabalina$IMC)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=IMC, color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Premedicación",
       x="Premedicación",
       y="IMC (Kg/m2)",
       title="Violin Plots de IMC (Kg/m2) por Premedicación") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = IMC, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Premedicación",
       x="IMC (Kg/m2)",
       y="Frequencia",
       title="Histograma de IMC (Kg/m2) por Premedicación") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_Tapentadol %>% count(ASA, sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(ASA))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "ASA",
       x=NULL,
       y=NULL,
       title="Proporciones de ASA - Tapentadol") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_Pregabalina %>% count(ASA, sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(ASA))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "ASA",
       x=NULL,
       y=NULL,
       title="Proporciones de ASA - Pregabalina") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Tapentadol$TIEMPO.QX)


## ----echo=FALSE------------------------
# Variance
var(group_Tapentadol$TIEMPO.QX)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Tapentadol$TIEMPO.QX)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Pregabalina$TIEMPO.QX)


## ----echo=FALSE------------------------
# Variance
var(group_Pregabalina$TIEMPO.QX)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Pregabalina$TIEMPO.QX)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=TIEMPO.QX, color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Premedicación",
       x="Premedicación",
       y="Tiempo de Cirugía (min)",
       title="Violin Plots de Tiempo de Cirugía por Premedicación") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = TIEMPO.QX, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Premedicación",
       x="Tiempo de Cirugía (m)",
       y="Frequencia",
       title="Histograma de Tiempo de Cirugía por Premedicación") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
evera_df <- data.frame(
  c(rep(group_Tapentadol$GRUPO, 5), rep(group_Pregabalina$GRUPO, 5)),
  c(group_Tapentadol$EVERA.T0, group_Tapentadol$EVERA.T1, group_Tapentadol$EVERA.T2, group_Tapentadol$EVERA.T3, group_Tapentadol$EVERA.T4, group_Pregabalina$EVERA.T0, group_Pregabalina$EVERA.T1, group_Pregabalina$EVERA.T2, group_Pregabalina$EVERA.T3, group_Pregabalina$EVERA.T4),
  c(rep("T0", length(group_Tapentadol$EVERA.T0)), rep("T1", length(group_Tapentadol$EVERA.T1)), rep("T2", length(group_Tapentadol$EVERA.T2)), rep("T3", length(group_Tapentadol$EVERA.T3)), rep("T4", length(group_Tapentadol$EVERA.T4)), rep("T0", length(group_Pregabalina$EVERA.T0)), rep("T1", length(group_Pregabalina$EVERA.T1)), rep("T2", length(group_Pregabalina$EVERA.T2)), rep("T3", length(group_Pregabalina$EVERA.T3)), rep("T4", length(group_Pregabalina$EVERA.T4)))
)
colnames(evera_df) <- c("Premedicación", "EVERA", "Tiempo")
evera_df


## ----echo=FALSE------------------------
# Tabla Contingencia
evera_freqs <- data.frame(table(evera_df))
evera_freqs


## ----echo=FALSE, fig.height=10, fig.width=10----
# Grouped Bar Plots
p <- ggplot(evera_freqs,
       aes(x = Premedicación,
           y = Freq,
           fill = EVERA)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Tiempo) +
  labs(fill="EVERA",
       x="Premedicación",
       y="Frecuencia",
       title="Frecuencias Agrupadas de EVERA por Premedicación y Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=20))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Tapentadol$ENA.T0)


## ----echo=FALSE------------------------
# Variance
var(group_Tapentadol$ENA.T0)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Tapentadol$ENA.T0)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Pregabalina$ENA.T0)


## ----echo=FALSE------------------------
# Variance
var(group_Pregabalina$ENA.T0)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Pregabalina$ENA.T0)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=ENA.T0, color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Premedicación",
       x="Premedicación",
       y="ENA T0",
       title="Violin Plots de ENA T0 por Premedicación") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = ENA.T0, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Premedicación",
       x="ENA T0",
       y="Frequencia",
       title="Histograma de ENA T0 por Premedicación") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Tapentadol$ENA.T1)


## ----echo=FALSE------------------------
# Variance
var(group_Tapentadol$ENA.T1)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Tapentadol$ENA.T1)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Pregabalina$ENA.T1)


## ----echo=FALSE------------------------
# Variance
var(group_Pregabalina$ENA.T1)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Pregabalina$ENA.T1)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=ENA.T1, color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Premedicación",
       x="Premedicación",
       y="ENA T1",
       title="Violin Plots de ENA T1 por Premedicación") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = ENA.T1, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Premedicación",
       x="ENA T1",
       y="Frequencia",
       title="Histograma de ENA T1 por Premedicación") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Tapentadol$ENA.T2)


## ----echo=FALSE------------------------
# Variance
var(group_Tapentadol$ENA.T2)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Tapentadol$ENA.T2)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Pregabalina$ENA.T2)


## ----echo=FALSE------------------------
# Variance
var(group_Pregabalina$ENA.T2)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Pregabalina$ENA.T2)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=ENA.T2, color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Premedicación",
       x="Premedicación",
       y="ENA T2",
       title="Violin Plots de ENA T2 por Premedicación") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = ENA.T2, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Premedicación",
       x="ENA T2",
       y="Frequencia",
       title="Histograma de ENA T2 por Premedicación") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Tapentadol$ENA.T3)


## ----echo=FALSE------------------------
# Variance
var(group_Tapentadol$ENA.T3)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Tapentadol$ENA.T3)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Pregabalina$ENA.T3)


## ----echo=FALSE------------------------
# Variance
var(group_Pregabalina$ENA.T3)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Pregabalina$ENA.T3)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=ENA.T3, color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Premedicación",
       x="Premedicación",
       y="ENA T3",
       title="Violin Plots de ENA T3 por Premedicación") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = ENA.T3, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Premedicación",
       x="ENA T3",
       y="Frequencia",
       title="Histograma de ENA T3 por Premedicación") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Tapentadol$ENA.T4)


## ----echo=FALSE------------------------
# Variance
var(group_Tapentadol$ENA.T4)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Tapentadol$ENA.T4)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Pregabalina$ENA.T4)


## ----echo=FALSE------------------------
# Variance
var(group_Pregabalina$ENA.T4)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Pregabalina$ENA.T4)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=ENA.T4, color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Premedicación",
       x="Premedicación",
       y="ENA T4",
       title="Violin Plots de ENA T4 por Premedicación") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = ENA.T4, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Premedicación",
       x="ENA T4",
       y="Frequencia",
       title="Histograma de ENA T4 por Premedicación") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
data_Rescate <- data.frame(group_Tapentadol %>% count(RESCATES.T0, sort = FALSE))
ggplot(data_Rescate, aes(x = "", y = n, fill = factor(RESCATES.T0))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Rescate",
       x=NULL,
       y=NULL,
       title="Proporciones de Rescate - Tapentadol T0") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_Rescate <- data.frame(group_Pregabalina %>% count(RESCATES.T0, sort = FALSE))
ggplot(data_Rescate, aes(x = "", y = n, fill = factor(RESCATES.T0))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Rescate",
       x=NULL,
       y=NULL,
       title="Proporciones de Rescate - Pregabalina T0") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_Rescate <- data.frame(group_Tapentadol %>% count(RESCATES.T1, sort = FALSE))
ggplot(data_Rescate, aes(x = "", y = n, fill = factor(RESCATES.T1))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Rescate",
       x=NULL,
       y=NULL,
       title="Proporciones de Rescate - Tapentadol T1") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_Rescate <- data.frame(group_Pregabalina %>% count(RESCATES.T1, sort = FALSE))
ggplot(data_Rescate, aes(x = "", y = n, fill = factor(RESCATES.T1))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Rescate",
       x=NULL,
       y=NULL,
       title="Proporciones de Rescate - Pregabalina T1") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_Rescate <- data.frame(group_Tapentadol %>% count(RESCATES.T2, sort = FALSE))
ggplot(data_Rescate, aes(x = "", y = n, fill = factor(RESCATES.T2))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Rescate",
       x=NULL,
       y=NULL,
       title="Proporciones de Rescate - Tapentadol T2") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_Rescate <- data.frame(group_Pregabalina %>% count(RESCATES.T2, sort = FALSE))
ggplot(data_Rescate, aes(x = "", y = n, fill = factor(RESCATES.T2))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Rescate",
       x=NULL,
       y=NULL,
       title="Proporciones de Rescate - Pregabalina T2") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_Rescate <- data.frame(group_Tapentadol %>% count(RESCATES.T3, sort = FALSE))
ggplot(data_Rescate, aes(x = "", y = n, fill = factor(RESCATES.T3))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Rescate",
       x=NULL,
       y=NULL,
       title="Proporciones de Rescate - Tapentadol T3") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_Rescate <- data.frame(group_Pregabalina %>% count(RESCATES.T3, sort = FALSE))
ggplot(data_Rescate, aes(x = "", y = n, fill = factor(RESCATES.T3))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Rescate",
       x=NULL,
       y=NULL,
       title="Proporciones de Rescate - Pregabalina T3") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_Rescate <- data.frame(group_Tapentadol %>% count(RESCATES.T4, sort = FALSE))
ggplot(data_Rescate, aes(x = "", y = n, fill = factor(RESCATES.T4))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Rescate",
       x=NULL,
       y=NULL,
       title="Proporciones de Rescate - Tapentadol T4") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_Rescate <- data.frame(group_Pregabalina %>% count(RESCATES.T4, sort = FALSE))
ggplot(data_Rescate, aes(x = "", y = n, fill = factor(RESCATES.T4))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Rescate",
       x=NULL,
       y=NULL,
       title="Proporciones de Rescate - Pregabalina T4") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Tapentadol$TOTAL.DE.MG)


## ----echo=FALSE------------------------
# Variance
var(group_Tapentadol$TOTAL.DE.MG)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Tapentadol$TOTAL.DE.MG)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Pregabalina$TOTAL.DE.MG)


## ----echo=FALSE------------------------
# Variance
var(group_Pregabalina$TOTAL.DE.MG)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Pregabalina$TOTAL.DE.MG)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=TOTAL.DE.MG, color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Premedicación",
       x="Premedicación",
       y="Total mg de Morfina",
       title="Violin Plots de Total mg de Morfina por Premedicación") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = TOTAL.DE.MG, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Premedicación",
       x="Total mg de Morfina",
       y="Frequencia",
       title="Histograma de Total mg de Morfina por Premedicación") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
data_Nausea <- data.frame(group_Tapentadol %>% count(NAUSEA.VÓMITO, sort = FALSE))
ggplot(data_Nausea, aes(x = "", y = n, fill = factor(NAUSEA.VÓMITO))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Presencia de Náusea/Vómito",
       x=NULL,
       y=NULL,
       title="Proporciones de Presencia de Náusea/Vómito - Tapentadol") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_Nausea <- data.frame(group_Pregabalina %>% count(NAUSEA.VÓMITO, sort = FALSE))
ggplot(data_Nausea, aes(x = "", y = n, fill = factor(NAUSEA.VÓMITO))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Presencia de Náusea/Vómito",
       x=NULL,
       y=NULL,
       title="Proporciones de Presencia de Náusea/Vómito - Tapentadol") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Prepare Data for T0
data_T0 <- evera_df[evera_df$Tiempo == 'T0',]
data_T0 <- table(data_T0[,1:2])
data_T0


## ----echo=FALSE------------------------
# Mosaicplot
p <- mosaicplot(data_T0,
  main = "Mosaic Plot T0 - Tapentadol vs Pregabalina",
  color = TRUE
)
p


## ----echo=FALSE------------------------
# Chi-Square Test for T0
result <- chisq.test(data_T0)
result


## ----echo=FALSE------------------------
# Prepare Data for T0
data_T1 <- evera_df[evera_df$Tiempo == 'T1',]
data_T1 <- table(data_T1[,1:2])
data_T1


## ----echo=FALSE------------------------
# Mosaicplot
p <- mosaicplot(data_T1,
  main = "Mosaic Plot T1 - Tapentadol vs Pregabalina",
  color = TRUE
)
p


## ----echo=FALSE------------------------
# Chi-Square Test for T2
result <- chisq.test(data_T1, simulate.p.value = TRUE)
result


## ----echo=FALSE------------------------
# Fisher's Exact Test for T2
result <- fisher.test(data_T1)
result


## ----echo=FALSE------------------------
# Prepare Data for T0
data_T2 <- evera_df[evera_df$Tiempo == 'T2',]
data_T2 <- table(data_T2[,1:2])
data_T2


## ----echo=FALSE------------------------
# Mosaicplot
mosaicplot(data_T2,
  main = "Mosaic Plot T2 - Tapentadol vs Pregabalina",
  color = TRUE
)



## ----echo=FALSE------------------------
# Chi-Square Test for T2
result <- chisq.test(data_T2, simulate.p.value = TRUE)
result


## ----echo=FALSE------------------------
# Prepare Data for T0
data_T3 <- evera_df[evera_df$Tiempo == 'T3',]
data_T3 <- table(data_T3[,1:2])
data_T3


## ----echo=FALSE------------------------
# Mosaicplot
mosaicplot(data_T3,
  main = "Mosaic Plot T3 - Tapentadol vs Pregabalina",
  color = TRUE
)



## ----echo=FALSE------------------------
# Chi-Square Test for T2
result <- chisq.test(data_T3, simulate.p.value = TRUE)
result


## ----echo=FALSE------------------------
# Prepare Data for T0
data_T4 <- evera_df[evera_df$Tiempo == 'T4',]
data_T4 <- table(data_T4[,1:2])
data_T4


## ----echo=FALSE------------------------
# Mosaicplot
mosaicplot(data_T4,
  main = "Mosaic Plot T4 - Tapentadol vs Pregabalina",
  color = TRUE
)



## ----echo=FALSE------------------------
# Chi-Square Test for T2
result <- chisq.test(data_T4, simulate.p.value = TRUE)
result


## ----echo=FALSE------------------------
ENA_df <- data.frame(
  c(rep(group_Tapentadol$GRUPO, 5), rep(group_Pregabalina$GRUPO, 5)),
  c(group_Tapentadol$ENA.T0, group_Tapentadol$ENA.T1, group_Tapentadol$ENA.T2, group_Tapentadol$ENA.T3, group_Tapentadol$ENA.T4, group_Pregabalina$ENA.T0, group_Pregabalina$ENA.T1, group_Pregabalina$ENA.T2, group_Pregabalina$ENA.T3, group_Pregabalina$ENA.T4),
  c(rep("T0", length(group_Tapentadol$ENA.T0)), rep("T1", length(group_Tapentadol$ENA.T1)), rep("T2", length(group_Tapentadol$ENA.T2)), rep("T3", length(group_Tapentadol$ENA.T3)), rep("T4", length(group_Tapentadol$ENA.T4)), rep("T0", length(group_Pregabalina$ENA.T0)), rep("T1", length(group_Pregabalina$ENA.T1)), rep("T2", length(group_Pregabalina$ENA.T2)), rep("T3", length(group_Pregabalina$ENA.T3)), rep("T4", length(group_Pregabalina$ENA.T4)))
)
colnames(ENA_df) <- c("Premed", "ENA", "Tiempo")
ENA_df


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- ENA_df %>%
  group_by(Tiempo) %>%
  t_test(ENA ~ Premed, paired=FALSE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")

stat.test$p.adj.signif[1] <- 'ns'
stat.test <- stat.test %>% add_xy_position(x = "Tiempo", dodge = 0.8)
stat.test


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- ENA_df[ENA_df$Tiempo!='T0',] %>%
  group_by(Tiempo) %>%
  wilcox_test(ENA ~ Premed, paired=FALSE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")

stat.test <- rbind(c('T0', 'ENA', 'Pregabalina', 'Tapentadol', '24', '23', NaN, NaN, NaN, 'ns'), stat.test) %>% add_xy_position(x = "Tiempo", dodge = 0.8)
stat.test


## ----echo=FALSE, fig.height=10, fig.width=10----
# Grouped Boxplots
bxp <- ggboxplot(
  ENA_df, x = "Tiempo", y = "ENA", 
  color = "Premed", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Premedicación",
       x="Tiempo",
       y="ENA",
       title="Comparación de ENA entre Premedicaciones a través del tiempo") +
  theme(plot.title = element_text(size=12))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Tiempo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
p <- ggplotly(bxp)
p


## ----echo=FALSE, fig.height=10, fig.width=10----
# Grouped Lineplots
lp <- ggline(
  ENA_df, x = "Tiempo", y = "ENA", add = "mean_sd", 
  color = "Premed", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Premedicación",
       x="Tiempo",
       y="ENA",
       title="Tendencias de ENA entre Premedicaciones a través del tiempo") +
  theme(plot.title = element_text(size=12))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Tiempo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )


## ----echo=FALSE------------------------
p <- ggplotly(lp)
p

