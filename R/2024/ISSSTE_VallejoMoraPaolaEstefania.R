## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("Data2.xlsx"))
data


## ----echo=FALSE------------------------

data$GRUPO[data$GRUPO == 1] <- 'Opioide Débil'
data$GRUPO[data$GRUPO == 2] <- 'AINE'
data$GRUPO[data$GRUPO == 3] <- 'AINE'


data$X2..SEXO.[data$X2..SEXO. == 1] <- 'Masculino'
data$X2..SEXO.[data$X2..SEXO. == 2] <- 'Femenino'

data$X13..VRS.TB.[data$X13..VRS.TB. == 1] <- 'Ausencia'
data$X13..VRS.TB.[data$X13..VRS.TB. == 2] <- 'Leve'
data$X13..VRS.TB.[data$X13..VRS.TB. == 3] <- 'Moderado'
data$X13..VRS.TB.[data$X13..VRS.TB. == 4] <- 'Severo'

data$X14..VRS.T0.[data$X14..VRS.T0. == 1] <- 'Ausencia'
data$X14..VRS.T0.[data$X14..VRS.T0. == 2] <- 'Leve'
data$X14..VRS.T0.[data$X14..VRS.T0. == 3] <- 'Moderado'
data$X14..VRS.T0.[data$X14..VRS.T0. == 4] <- 'Severo'

data$X15..VRS.T1.[data$X15..VRS.T1. == 1] <- 'Ausencia'
data$X15..VRS.T1.[data$X15..VRS.T1. == 2] <- 'Leve'
data$X15..VRS.T1.[data$X15..VRS.T1. == 3] <- 'Moderado'
data$X15..VRS.T1.[data$X15..VRS.T1. == 4] <- 'Severo'

data$X16..VRS.T2.[data$X16..VRS.T2. == 1] <- 'Ausencia'
data$X16..VRS.T2.[data$X16..VRS.T2. == 2] <- 'Leve'
data$X16..VRS.T2.[data$X16..VRS.T2. == 3] <- 'Moderado'
data$X16..VRS.T2.[data$X16..VRS.T2. == 4] <- 'Severo'

data$X17..VRS.T3.[data$X17..VRS.T3. == 1] <- 'Ausencia'
data$X17..VRS.T3.[data$X17..VRS.T3. == 2] <- 'Leve'
data$X17..VRS.T3.[data$X17..VRS.T3. == 3] <- 'Moderado'
data$X17..VRS.T3.[data$X17..VRS.T3. == 4] <- 'Severo'

data$X27..HIPERSENSIBILIDAD.[data$X27..HIPERSENSIBILIDAD. == 1] <- 'No'


data


## ----echo=FALSE------------------------
group_1 <- data[data$GRUPO == 'Opioide Débil',]
group_1


## ----echo=FALSE------------------------
group_2 <- data[data$GRUPO == 'AINE',]
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
# Mean & Quantiles
summary(group_1$X1..EDAD.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X1..EDAD.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X1..EDAD.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X1..EDAD.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X1..EDAD.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X1..EDAD.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X1..EDAD., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Edad",
       title="Violin Plots de Edad por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X1..EDAD., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Edad",
       y="Frequencia",
       title="Histograma de Edad por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_1 %>% count(X2..SEXO., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(X2..SEXO.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género - Opioide Débil") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_2 %>% count(X2..SEXO., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(X2..SEXO.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género - AINE") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X3..PESO.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X3..PESO.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X3..PESO.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X3..PESO.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X3..PESO.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X3..PESO.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X3..PESO., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Peso",
       title="Violin Plots de Peso por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X3..PESO., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Peso",
       y="Frequencia",
       title="Histograma de Género por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X4..IMC.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X4..IMC.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X4..IMC.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X4..IMC.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X4..IMC.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X4..IMC.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X4..IMC., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="IMC",
       title="Violin Plots de IMC por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X4..IMC., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = .05,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="IMC",
       y="Frequencia",
       title="Histograma de IMC por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_1 %>% count(X5..ASA., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(X5..ASA.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "ASA",
       x=NULL,
       y=NULL,
       title="Proporciones de ASA - Opioide Débil") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_2 %>% count(X5..ASA., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(X5..ASA.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "ASA",
       x=NULL,
       y=NULL,
       title="Proporciones de ASA - AINE") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X6..TIEMPO.CX.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X6..TIEMPO.CX.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X6..TIEMPO.CX.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X6..TIEMPO.CX.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X6..TIEMPO.CX.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X6..TIEMPO.CX.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X6..TIEMPO.CX., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Tiempo CX",
       title="Violin Plots de Tiempo CX por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X6..TIEMPO.CX., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 20,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Tiempo CX",
       y="Frequencia",
       title="Histograma de Tiemp CX por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X7..TIEMPO.ANEST.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X7..TIEMPO.ANEST.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X7..TIEMPO.ANEST.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X7..TIEMPO.ANEST.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X7..TIEMPO.ANEST.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X7..TIEMPO.ANEST.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X7..TIEMPO.ANEST., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Tiempo Anestesia",
       title="Violin Plots de Tiempo Anestesia por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X7..TIEMPO.ANEST., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 20,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Tiempo Anestesia",
       y="Frequencia",
       title="Histograma de Tiempo Anestesia por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X8..ENA.TB.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X8..ENA.TB.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X8..ENA.TB.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X8..ENA.TB.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X8..ENA.TB.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X8..ENA.TB.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X8..ENA.TB., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="ENA TB",
       title="Violin Plots de ENA TB por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X8..ENA.TB., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="ENA TB",
       y="Frequencia",
       title="Histograma de ENA TB por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X9..ENA.T0.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X9..ENA.T0.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X9..ENA.T0.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X9..ENA.T0.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X9..ENA.T0.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X9..ENA.T0.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X9..ENA.T0., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="ENA T0",
       title="Violin Plots de ENA T0 por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X9..ENA.T0., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="ENA T0",
       y="Frequencia",
       title="Histograma de ENA T0 por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X10..ENA.T1.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X10..ENA.T1.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X10..ENA.T1.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X10..ENA.T1.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X10..ENA.T1.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X10..ENA.T1.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X10..ENA.T1., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="ENA T1",
       title="Violin Plots de ENA T1 por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X10..ENA.T1., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="ENA T1",
       y="Frequencia",
       title="Histograma de ENA T1 por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X11..ENA.T2.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X11..ENA.T2.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X11..ENA.T2.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X11..ENA.T2.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X11..ENA.T2.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X11..ENA.T2.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X11..ENA.T2., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="ENA T2",
       title="Violin Plots de ENA T2 por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X11..ENA.T2., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="ENA T2",
       y="Frequencia",
       title="Histograma de ENA T2 por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X12..ENA.T3.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X12..ENA.T3.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X12..ENA.T3.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X12..ENA.T3.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X12..ENA.T3.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X12..ENA.T3.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X12..ENA.T3., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="ENA T3",
       title="Violin Plots de ENA T2 por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X12..ENA.T3., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="ENA T2",
       y="Frequencia",
       title="Histograma de ENA T3 por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_1 %>% count(X13..VRS.TB., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(X13..VRS.TB.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "VRS TB",
       x=NULL,
       y=NULL,
       title="Proporciones de VRS TB - Opioide Débil") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_2 %>% count(X13..VRS.TB., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(X13..VRS.TB.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "VRS TB",
       x=NULL,
       y=NULL,
       title="Proporciones de VRS TB - AINE") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
vrs_df <- data.frame(
  c( rep(group_1$GRUPO, 4), rep(group_2$GRUPO, 4) ),
  c( group_1$X14..VRS.T0., group_1$X15..VRS.T1., group_1$X16..VRS.T2., group_1$X17..VRS.T3.,
     group_2$X14..VRS.T0., group_2$X15..VRS.T1., group_2$X16..VRS.T2., group_2$X17..VRS.T3. ),
  c( rep("T0", length(group_1$X14..VRS.T0.)), rep("T1", length(group_1$X15..VRS.T1.)), rep("T2", length(group_1$X16..VRS.T2.)), rep("T3", length(group_1$X17..VRS.T3.)),
     rep("T0", length(group_2$X14..VRS.T0.)), rep("T1", length(group_2$X15..VRS.T1.)), rep("T2", length(group_2$X16..VRS.T2.)), rep("T3", length(group_2$X17..VRS.T3.)))
)
colnames(vrs_df) <- c("Grupo", "VRS", "Tiempo")
vrs_df


## ----echo=FALSE------------------------
# Tabla Contingencia
vrs_freqs <- data.frame(table(vrs_df))
vrs_freqs


## ----echo=FALSE, fig.height=10, fig.width=10----
# Grouped Bar Plots
ggplot(vrs_freqs,
       aes(x = Grupo,
           y = Freq,
           fill = VRS)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Tiempo) +
  labs(fill="VRS",
       x="Grupo",
       y="Frecuencia",
       title="Frecuencias Agrupadas de VRS por Grupo y Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X18..N.DOSIS.TB.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X18..N.DOSIS.TB.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X18..N.DOSIS.TB.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X18..N.DOSIS.TB.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X18..N.DOSIS.TB.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X18..N.DOSIS.TB.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X18..N.DOSIS.TB., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Rescate TB",
       title="Violin Plots de Dosis de Rescate TB por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X18..N.DOSIS.TB., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Rescate TB",
       y="Frequencia",
       title="Histograma de Dosis de Rescate TB por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X19..N.DOSIS.T0.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X19..N.DOSIS.T0.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X19..N.DOSIS.T0.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X19..N.DOSIS.T0.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X19..N.DOSIS.T0.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X19..N.DOSIS.T0.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X19..N.DOSIS.T0., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Rescate T0",
       title="Violin Plots de Dosis de Rescate T0 por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X19..N.DOSIS.T0., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Rescate T0",
       y="Frequencia",
       title="Histograma de Dosis de Rescate T0 por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X20..N.DOSIS.T1.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X20..N.DOSIS.T1.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X20..N.DOSIS.T1.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X20..N.DOSIS.T1.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X20..N.DOSIS.T1.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X20..N.DOSIS.T1.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X20..N.DOSIS.T1., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Rescate T1",
       title="Violin Plots de Dosis de Rescate T1 por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X20..N.DOSIS.T1., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Rescate T1",
       y="Frequencia",
       title="Histograma de Dosis de Rescate T1 por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X21..N.DOSIS.T2.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X21..N.DOSIS.T2.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X21..N.DOSIS.T2.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X21..N.DOSIS.T2.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X21..N.DOSIS.T2.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X21..N.DOSIS.T2.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X21..N.DOSIS.T2., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Rescate T2",
       title="Violin Plots de Dosis de Rescate T2 por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X21..N.DOSIS.T2., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Rescate T2",
       y="Frequencia",
       title="Histograma de Dosis de Rescate T2 por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X22..N.DOSIS.T3.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X22..N.DOSIS.T3.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X22..N.DOSIS.T3.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X22..N.DOSIS.T3.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X22..N.DOSIS.T3.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X22..N.DOSIS.T3.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X22..N.DOSIS.T3., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Rescate T3",
       title="Violin Plots de Dosis de Rescate T3 por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X22..N.DOSIS.T3., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Rescate T3",
       y="Frequencia",
       title="Histograma de Dosis de Rescate T3 por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X23..Mg.MORFINA.TB.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X23..Mg.MORFINA.TB.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X23..Mg.MORFINA.TB.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X23..Mg.MORFINA.TB.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X23..Mg.MORFINA.TB.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X23..Mg.MORFINA.TB.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X23..Mg.MORFINA.TB., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Morfina (mg) TB",
       title="Violin Plots de Dosis de Morfina TB por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X23..Mg.MORFINA.TB., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Morfina (mg) TB",
       y="Frequencia",
       title="Histograma de Dosis de Morfina TB por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X24...Mg.MORFINA.T0.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X24...Mg.MORFINA.T0.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X24...Mg.MORFINA.T0.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X24...Mg.MORFINA.T0.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X24...Mg.MORFINA.T0.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X24...Mg.MORFINA.T0.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X24...Mg.MORFINA.T0., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Morfina (mg) T0",
       title="Violin Plots de Dosis de Morfina T0 por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X24...Mg.MORFINA.T0., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Morfina (mg) T0",
       y="Frequencia",
       title="Histograma de Dosis de Morfina T0 por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X25..Mg.MORFINA.T1.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X25..Mg.MORFINA.T1.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X25..Mg.MORFINA.T1.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X25..Mg.MORFINA.T1.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X25..Mg.MORFINA.T1.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X25..Mg.MORFINA.T1.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X25..Mg.MORFINA.T1., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Morfina (mg) T1",
       title="Violin Plots de Dosis de Morfina T1 por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X25..Mg.MORFINA.T1., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Morfina (mg) T1",
       y="Frequencia",
       title="Histograma de Dosis de Morfina T1 por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_1$X26...Mg.MORFINA.T2.)


## ----echo=FALSE------------------------
# Variance
var(group_1$X26...Mg.MORFINA.T2.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_1$X26...Mg.MORFINA.T2.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_2$X26...Mg.MORFINA.T2.)


## ----echo=FALSE------------------------
# Variance
var(group_2$X26...Mg.MORFINA.T2.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_2$X26...Mg.MORFINA.T2.)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=X26...Mg.MORFINA.T2., color=GRUPO)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Morfina (mg) T2",
       title="Violin Plots de Dosis de Morfina T2 por Grupo") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = X26...Mg.MORFINA.T2., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Morfina (mg) T2",
       y="Frequencia",
       title="Histograma de Dosis de Morfina T2 por Grupo") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_1 %>% count(X27..HIPERSENSIBILIDAD., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(X27..HIPERSENSIBILIDAD.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Hipersensibilidad",
       x=NULL,
       y=NULL,
       title="Proporciones de Hipersensibilidad - Opioide Débil") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_ASA <- data.frame(group_2 %>% count(X27..HIPERSENSIBILIDAD., sort = FALSE))
ggplot(data_ASA, aes(x = "", y = n, fill = factor(X27..HIPERSENSIBILIDAD.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Hipersensibilidad",
       x=NULL,
       y=NULL,
       title="Proporciones de Hipersensibilidad - AINE") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Prepare Data for T0
data_T0 <- vrs_df[vrs_df$Tiempo == 'T0',]
data_T0 <- table(data_T0[,1:2])
data_T0


## ----echo=FALSE------------------------
# Mosaicplot
mosaicplot(data_T0,
  main = "Mosaic Plot T0 - Grupo 1 vs Grupo 2",
  color = TRUE
)



## ----echo=FALSE------------------------
# Chi-Square Test for T0
result <- chisq.test(data_T0, simulate.p.value=TRUE)
result


## ----echo=FALSE------------------------
# Fisher's Exact Test for T2
result <- fisher.test(data_T0)
result


## ----echo=FALSE------------------------
# Prepare Data for T0
data_T1 <- vrs_df[vrs_df$Tiempo == 'T1',]
data_T1 <- table(data_T1[,1:2])
data_T1


## ----echo=FALSE------------------------
# Mosaicplot
mosaicplot(data_T1,
  main = "Mosaic Plot T1 - Grupo 1 vs Grupo 2",
  color = TRUE
)



## ----echo=FALSE------------------------
# Chi-Square Test for T0
result <- chisq.test(data_T1, simulate.p.value=TRUE)
result


## ----echo=FALSE------------------------
# Prepare Data for T0
data_T2 <- vrs_df[vrs_df$Tiempo == 'T2',]
data_T2 <- table(data_T2[,1:2])
data_T2


## ----echo=FALSE------------------------
# Mosaicplot
mosaicplot(data_T2,
  main = "Mosaic Plot T2 - Grupo 1 vs Grupo 2",
  color = TRUE
)



## ----echo=FALSE------------------------
# Chi-Square Test for T0
result <- chisq.test(data_T2, simulate.p.value=TRUE)
result


## ----echo=FALSE------------------------
# Prepare Data for T0
data_T3 <- vrs_df[vrs_df$Tiempo == 'T3',]
data_T3 <- table(data_T3[,1:2])
data_T3


## ----echo=FALSE------------------------
# Mosaicplot
mosaicplot(data_T3,
  main = "Mosaic Plot T3 - Grupo 1 vs Grupo 2",
  color = TRUE
)



## ----echo=FALSE------------------------
# Chi-Square Test for T0
result <- chisq.test(data_T3, simulate.p.value=TRUE)
result


## ----echo=FALSE------------------------
ENA_df <- data.frame(
  c(rep(group_1$GRUPO, 4), rep(group_2$GRUPO, 4)),
  c(group_1$X9..ENA.T0., group_1$X10..ENA.T1., group_1$X11..ENA.T2., group_1$X12..ENA.T3.,
    group_2$X9..ENA.T0., group_2$X10..ENA.T1., group_2$X11..ENA.T2., group_2$X12..ENA.T3.),
  c( rep("T0", length(group_1$X9..ENA.T0.)), rep("T1", length(group_1$X10..ENA.T1.)), rep("T2", length(group_1$X11..ENA.T2.)), rep("T3", length(group_1$X12..ENA.T3.)),
     rep("T0", length(group_2$X9..ENA.T0.)), rep("T1", length(group_2$X10..ENA.T1.)), rep("T2", length(group_2$X11..ENA.T2.)), rep("T3", length(group_2$X12..ENA.T3.)))
)
colnames(ENA_df) <- c("Grupo", "ENA", "Tiempo")
ENA_df


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- ENA_df %>%
  group_by(Tiempo) %>%
  t_test(ENA ~ Grupo, paired=FALSE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")

#stat.test$p.adj.signif[1] <- 'ns'
stat.test


## ----echo=FALSE, fig.height=10, fig.width=10----
# Grouped Boxplots
bxp <- ggboxplot(
  ENA_df, x = "Tiempo", y = "ENA", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="ENA",
       title="Comparación de ENA entre Grupos  a través del tiempo") +
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
  ENA_df, x = "Tiempo", y = "ENA", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="ENA",
       title="Tendencias de ENA entre Grupos a través del tiempo") +
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
# Statistical Test <- Group by Times
stat.test <- ENA_df %>%
  group_by(Tiempo) %>%
  wilcox_test(ENA ~ Grupo, paired = FALSE) %>%  # Change to wilcox_test
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")

#stat.test$p.adj.signif[1] <- 'ns'
stat.test


## ----echo=FALSE, fig.height=10, fig.width=10----
# Grouped Boxplots
bxp <- ggboxplot(
  ENA_df, x = "Tiempo", y = "ENA", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="ENA",
       title="Comparación de ENA entre Grupos  a través del tiempo") +
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
  ENA_df, x = "Tiempo", y = "ENA", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="ENA",
       title="Tendencias de ENA entre Grupos a través del tiempo") +
  theme(plot.title = element_text(size=12))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Tiempo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )

