## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("LoalAngy_Data.xlsx"))


## ----echo=FALSE------------------------
data


## ----echo=FALSE------------------------
group_control = data[data$Grupo.Control..0..Experimental..1. == 0,]
group_control

## ----echo=FALSE------------------------
group_Experimental = data[data$Grupo.Control..0..Experimental..1. == 1,]
group_Experimental 


## ----echo=FALSE------------------------
data_groups <- data.frame(data %>% count(Grupo.Control..0..Experimental..1., sort = FALSE))
data_groups

## ----echo=FALSE------------------------
ggplot(data_groups, aes(x = "", y = n, fill = factor(Grupo.Control..0..Experimental..1.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Grupo",
       x=NULL,
       y=NULL,
       title="Proporciones entre Grupos: Control vs. Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = Edad.Años, fill = factor(Grupo.Control..0..Experimental..1.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 4,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Edad",
       y="Frequencia",
       title="Histograma de Edad por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = Peso.KG, fill = factor(Grupo.Control..0..Experimental..1.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 4,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Peso (Kg)",
       y="Frequencia",
       title="Histograma de Peso por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = Talla.CM, fill = factor(Grupo.Control..0..Experimental..1.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.025,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Talla (m)",
       y="Frequencia",
       title="Histograma de Talla por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = IMC, fill = factor(Grupo.Control..0..Experimental..1.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 3,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="IMC",
       y="Frequencia",
       title="Histograma de IMC por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
gender_control <- data.frame(group_control %>% count(Genero.F.1..M.2., sort = FALSE))
gender_control


## ----echo=FALSE------------------------
ggplot(gender_control, aes(x = "", y = n, fill = factor(Genero.F.1..M.2.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género del Grupo Control") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Femenino")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$Edad.Años)


## ----echo=FALSE------------------------
# Variance
var(group_control$Edad.Años)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$Edad.Años)


## ----echo=FALSE------------------------
# Histogram
ggplot(group_control, aes(x = factor(ASA..1.2.3.), fill = factor(ASA..1.2.3.))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Control") +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$Peso.KG)


## ----echo=FALSE------------------------
# Variance
var(group_control$Peso.KG)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$Peso.KG)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$Talla.CM)


## ----echo=FALSE------------------------
# Variance
var(group_control$Talla.CM)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$Talla.CM)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$IMC)


## ----echo=FALSE------------------------
# Variance
var(group_control$IMC)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$IMC)


## ----echo=FALSE------------------------
exp_control <- data.frame(group_control %>% count(Experiencia.del.Medico.R1.1..R2.2..R3.3..MB.4., sort = FALSE))
exp_control


## ----echo=FALSE------------------------
ggplot(exp_control, aes(x = "", y = n, fill = factor(Experiencia.del.Medico.R1.1..R2.2..R3.3..MB.4.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Experiencia del Médico - Grupo Control") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("R1", "R4")) +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
gender_Experimental <- data.frame(group_Experimental %>% count(Genero.F.1..M.2., sort = FALSE))
gender_Experimental


## ----echo=FALSE------------------------
ggplot(gender_Experimental, aes(x = "", y = n, fill = factor(Genero.F.1..M.2.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género del Grupo Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Femenino")) +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$Edad.Años)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$Edad.Años)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$Edad.Años)


## ----echo=FALSE------------------------
# Histogram
ggplot(group_Experimental, aes(x = factor(ASA..1.2.3.), fill = factor(ASA..1.2.3.))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Experimental") +
  theme(plot.title = element_text(size=12))



## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$Peso.KG)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$Peso.KG)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$Peso.KG)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$Talla.CM)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$Talla.CM)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$Talla.CM)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$IMC)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$IMC)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$IMC)


## ----echo=FALSE------------------------
exp_experimental <- data.frame(group_Experimental %>% count(Experiencia.del.Medico.R1.1..R2.2..R3.3..MB.4., sort = FALSE))
exp_experimental


## ----echo=FALSE------------------------
ggplot(exp_experimental, aes(x = "", y = n, fill = factor(Experiencia.del.Medico.R1.1..R2.2..R3.3..MB.4.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Experiencia del Médico - Grupo Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("R1", "R3", "R4")) +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
CPP_data <- data.frame(
  c(rep("Control", 2*length(group_control$Grupo.Control..0..Experimental..1.)), rep("Experimental", 2*length(group_Experimental$Grupo.Control..0..Experimental..1.))),
  c(group_control$T0.Cefalea.postpunción.a.las.12H.Si.1..No.0., group_control$T1.Cefalea.postpunción.a.las.24H.Si.1..No.0., group_Experimental$T0.Cefalea.postpunción.a.las.12H.Si.1..No.0.,group_Experimental$T1.Cefalea.postpunción.a.las.24H.Si.1..No.0.),
  c(rep(c("12h", "24h"), times = 2, each = length(group_control$T0.Cefalea.postpunción.a.las.12H.Si.1..No.0.)))
)
colnames(CPP_data) <- c("Group", "Value", "Time")
CPP_data$Value[CPP_data$Value == 0] <- "No"
CPP_data$Value[CPP_data$Value == 1] <- "Sí"
CPP_data


## ----echo=FALSE------------------------
# Tabla Contingencia
CPP_freqs <- data.frame(table(CPP_data))
CPP_freqs


## ----echo=FALSE------------------------
# Grouped Bar Plots
ggplot(CPP_freqs,
       aes(x = Group,
           y = Freq,
           fill = Value)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Time) +
  labs(fill="Valor de Cefalea Pospunción (CPP)",
       x="Tiempo",
       y="Frecuencia",
       title="Frecuencias Agrupadas de Cefalea Pospunción por Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=10))


## ----echo=FALSE------------------------
# CPP_12h Contingency Table
CPP_12h <- table(CPP_data[CPP_data$Time == "12h", c(1,2)])
CPP_12h

## ----echo=FALSE------------------------
# Fisher'x exact test
fisher.test(CPP_12h)


## ----echo=FALSE------------------------
# CPP_12h Contingency Table
CPP_24h <- table(CPP_data[CPP_data$Time == "24h", c(1,2)])
CPP_24h

## ----echo=FALSE------------------------
# Fisher'x exact test
fisher.test(CPP_24h)


## ----echo=FALSE------------------------
nausea_data <- data.frame(
  c(rep("Control", 2*length(group_control$Grupo.Control..0..Experimental..1.)), rep("Experimental", 2*length(group_Experimental$Grupo.Control..0..Experimental..1.))),
  c(group_control$Efectos.adversos..Nausea..a.las.12H.Si.1..No.0., group_control$Efectos.adversos..Nausea..a.las.24H.Si.1..No.0., group_Experimental$Efectos.adversos..Nausea..a.las.12H.Si.1..No.0., group_Experimental$Efectos.adversos..Nausea..a.las.24H.Si.1..No.0.),
  c(rep(c("12h", "24h"), times = 2, each = length(group_control$Efectos.adversos..Nausea..a.las.12H.Si.1..No.0.)))
)
colnames(nausea_data) <- c("Group", "Value", "Time")
nausea_data$Value[nausea_data$Value == 0] <- "No"
nausea_data$Value[nausea_data$Value == 1] <- "Sí"
nausea_data


## ----echo=FALSE------------------------
# Tabla Contingencia
nausea_freqs <- data.frame(table(nausea_data))
nausea_freqs


## ----echo=FALSE------------------------
# Grouped Bar Plots
ggplot(nausea_freqs,
       aes(x = Group,
           y = Freq,
           fill = Value)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Time) +
  labs(fill="Valor de Náusea",
       x="Tiempo",
       y="Frecuencia",
       title="Frecuencias Agrupadas de Náusea por Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# nausea_12h Contingency Table
nausea_12h <- table(nausea_data[nausea_data$Time == "12h", c(1,2)])
nausea_12h

## ----echo=FALSE------------------------
# Fisher'x exact test
fisher.test(nausea_12h)


## ----echo=FALSE------------------------
# Nausea_12h Contingency Table
nausea_24h <- table(nausea_data[nausea_data$Time == "24h", c(1,2)])
nausea_24h


## ----echo=FALSE------------------------
prurito_data <- data.frame(
  c(rep("Control", 2*length(group_control$Grupo.Control..0..Experimental..1.)), rep("Experimental", 2*length(group_Experimental$Grupo.Control..0..Experimental..1.))),
  c(group_control$Efectos.adversos..Prurito..a.las.12H.Si.1..No.0., group_control$Efectos.adversos..Prurito..a.las.24H.Si.1..No.0., group_Experimental$Efectos.adversos..Prurito..a.las.12H.Si.1..No.0., group_Experimental$Efectos.adversos..Prurito..a.las.24H.Si.1..No.0.),
  c(rep(c("12h", "24h"), times = 2, each = length(group_control$Efectos.adversos..Prurito..a.las.12H.Si.1..No.0.)))
)
colnames(prurito_data) <- c("Group", "Value", "Time")
prurito_data$Value[prurito_data$Value == 0] <- "No"
prurito_data$Value[prurito_data$Value == 1] <- "Sí"
prurito_data


## ----echo=FALSE------------------------
# Tabla Contingencia
prurito_freqs <- data.frame(table(prurito_data))
prurito_freqs


## ----echo=FALSE------------------------
# Grouped Bar Plots
ggplot(prurito_freqs,
       aes(x = Group,
           y = Freq,
           fill = Value)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Time) +
  labs(fill="Valor de Prurito",
       x="Tiempo",
       y="Frecuencia",
       title="Frecuencias Agrupadas de Prurito por Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# CPP_12h Contingency Table
prurito_12h <- table(prurito_data[prurito_data$Time == "12h", c(1,2)])
prurito_12h

## ----echo=FALSE------------------------
# Fisher'x exact test
fisher.test(prurito_12h)


## ----echo=FALSE------------------------
# CPP_12h Contingency Table
prurito_24h <- table(prurito_data[prurito_data$Time == "24h", c(1,2)])
prurito_24h


## ----echo=FALSE------------------------
urine_data <- data.frame(
  c(rep("Control", 2*length(group_control$Grupo.Control..0..Experimental..1.)), rep("Experimental", 2*length(group_Experimental$Grupo.Control..0..Experimental..1.))),
  c(group_control$Efectos.adversos..Retención.Urinaria..a.las.12H.Si.1..No.0., group_control$Efectos.adversos..Retención.Urinaria..a.las.24H.Si.1..No.0., group_Experimental$Efectos.adversos..Retención.Urinaria..a.las.12H.Si.1..No.0., group_Experimental$Efectos.adversos..Retención.Urinaria..a.las.24H.Si.1..No.0.),
  c(rep(c("12h", "24h"), times = 2, each = length(group_control$Efectos.adversos..Retención.Urinaria..a.las.12H.Si.1..No.0.)))
)
colnames(urine_data) <- c("Group", "Value", "Time")
prurito_data$Value[urine_data$Value == 0] <- "No"
prurito_data$Value[urine_data$Value == 1] <- "Sí"
urine_data


## ----echo=FALSE------------------------
# Tabla Contingencia
urine_freqs <- data.frame(table(urine_data))
urine_freqs


## ----echo=FALSE------------------------
# Grouped Bar Plots
ggplot(urine_freqs,
       aes(x = Group,
           y = Freq,
           fill = Value)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Time) +
  labs(fill="Valor de Orina",
       x="Tiempo",
       y="Frecuencia",
       title="Frecuencias Agrupadas de Orina por Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# CPP_12h Contingency Table
urine_12h <- table(urine_data[urine_data$Time == "12h", c(1,2)])
urine_12h

## ----echo=FALSE------------------------
# Fisher'x exact test
fisher.test(urine_12h)


## ----echo=FALSE------------------------
# CPP_12h Contingency Table
urine_24h <- table(urine_data[urine_data$Time == "24h", c(1,2)])
urine_24h

