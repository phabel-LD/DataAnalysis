## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("RosasZarateVictorManuel_Data.xlsx"))


## ----echo=FALSE------------------------
data


## ----echo=FALSE------------------------
group_control = data[data$Grupo..control..0..experimental..1... == 0,]
group_control

## ----echo=FALSE------------------------
group_Experimental = data[data$Grupo..control..0..experimental..1... == 1,]
group_Experimental 


## ----echo=FALSE------------------------
data_groups <- data.frame(data %>% count(Grupo..control..0..experimental..1..., sort = FALSE))
data_groups

## ----echo=FALSE------------------------
ggplot(data_groups, aes(x = "", y = n, fill = factor(Grupo..control..0..experimental..1...))) +
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
ggplot(data, aes(x = Edad..años., fill = factor(Grupo..control..0..experimental..1...))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 3,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Edad",
       y="Frequencia",
       title="Histograma de Edad por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = Tiempo.quirurgico..minutos., fill = factor(Grupo..control..0..experimental..1...))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Tiempo (min)",
       y="Frequencia",
       title="Histograma de Tiempo Quirúrgico por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
gender_control <- data.frame(group_control %>% count(Genero..Masculino..0..Femenino..1.., sort = FALSE))
gender_control


## ----echo=FALSE------------------------
ggplot(gender_control, aes(x = "", y = n, fill = factor(Genero..Masculino..0..Femenino..1..))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género del Grupo Control") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Masculino", "Femenino")) +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$Edad..años.)


## ----echo=FALSE------------------------
# Variance
var(group_control$Edad..años.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$Edad..años.)


## ----echo=FALSE------------------------
# Histogram
ggplot(group_control, aes(x = factor(ASA), fill = factor(ASA))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Control") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
rescate_ondansetron <- data.frame(group_control %>% count(Tramiento.de.rescate..ondansetron.Si...0..No..1.., sort = FALSE))
rescate_ondansetron


## ----echo=FALSE------------------------
ggplot(rescate_ondansetron, aes(x = "", y = n, fill = factor(Tramiento.de.rescate..ondansetron.Si...0..No..1..))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Rescate",
       x=NULL,
       y=NULL,
       title="Proporciones de Rescate con Ondansetron en Grupo Control") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Rescate", "No Rescate")) +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
rescate_propofol <- data.frame(group_control %>% count(Tramiento.de.rescate..Propofol.Si...0..No..1.., sort = FALSE))
rescate_propofol


## ----echo=FALSE------------------------
ggplot(rescate_propofol, aes(x = "", y = n, fill = factor(Tramiento.de.rescate..Propofol.Si...0..No..1..))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Rescate",
       x=NULL,
       y=NULL,
       title="Proporciones de Rescate con Propofol en Grupo Control") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Rescate", "No Rescate")) +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
alter_cadio <- data.frame(group_control %>% count(Alteraciones.electrocardiograficas...Si..0..No...1., sort = FALSE))
alter_cadio


## ----echo=FALSE------------------------
ggplot(alter_cadio, aes(x = "", y = n, fill = factor(Alteraciones.electrocardiograficas...Si..0..No...1.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Alteraciones",
       x=NULL,
       y=NULL,
       title="Proporciones de Alteraciones Cardiográficas en Grupo Control") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("No Alteración", "Alteración")) +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
gender_Experimental <- data.frame(group_Experimental %>% count(Genero..Masculino..0..Femenino..1.., sort = FALSE))
gender_Experimental


## ----echo=FALSE------------------------
ggplot(gender_Experimental, aes(x = "", y = n, fill = factor(Genero..Masculino..0..Femenino..1..))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género del Grupo Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Masculino", "Femenino")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$Edad..años.)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$Edad..años.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$Edad..años.)


## ----echo=FALSE------------------------
# Histogram
ggplot(group_Experimental, aes(x = factor(ASA), fill = factor(ASA))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Control") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
rescate_ondansetron <- data.frame(group_Experimental %>% count(Tramiento.de.rescate..ondansetron.Si...0..No..1.., sort = FALSE))
rescate_ondansetron


## ----echo=FALSE------------------------
ggplot(rescate_ondansetron, aes(x = "", y = n, fill = factor(Tramiento.de.rescate..ondansetron.Si...0..No..1..))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Rescate",
       x=NULL,
       y=NULL,
       title="Proporciones de Rescate con Ondansetron en Grupo Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("No Rescate", "Rescate")) +
  theme(plot.title = element_text(size=11))


## ----echo=FALSE------------------------
rescate_propofol <- data.frame(group_Experimental %>% count(Tramiento.de.rescate..Propofol.Si...0..No..1.., sort = FALSE))
rescate_propofol


## ----echo=FALSE------------------------
ggplot(rescate_propofol, aes(x = "", y = n, fill = factor(Tramiento.de.rescate..Propofol.Si...0..No..1..))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Rescate",
       x=NULL,
       y=NULL,
       title="Proporciones de Rescate con Propofol en Grupo Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("No Rescate", "Rescate")) +
  theme(plot.title = element_text(size=11))



## ----echo=FALSE------------------------
alter_cadio <- data.frame(group_Experimental %>% count(Alteraciones.electrocardiograficas...Si..0..No...1., sort = FALSE))
alter_cadio


## ----echo=FALSE------------------------
ggplot(alter_cadio, aes(x = "", y = n, fill = factor(Alteraciones.electrocardiograficas...Si..0..No...1.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Alteraciones",
       x=NULL,
       y=NULL,
       title="Proporciones de Alteraciones Cardiográficas en Grupo Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("No Alteración", "Alteración")) +
  theme(plot.title = element_text(size=11))


## ----echo=FALSE------------------------
nausea_data_t0 <- data.frame(
  c(group_control$T0..Presencia.de.nausea.y.vomito.en.postoperatorio.inmediato.Si.0..NO..1., group_Experimental$T0..Presencia.de.nausea.y.vomito.en.postoperatorio.inmediato.Si.0..NO..1.), c(rep("control", length(group_control$T0..Presencia.de.nausea.y.vomito.en.postoperatorio.inmediato.Si.0..NO..1.)), rep("experimental", length(group_Experimental$T0..Presencia.de.nausea.y.vomito.en.postoperatorio.inmediato.Si.0..NO..1.)))
)
colnames(nausea_data_t0) <- c("Valor", "Grupo")
nausea_data_t0$Valor[nausea_data_t0$Valor == 0] <- "Sí"
nausea_data_t0$Valor[nausea_data_t0$Valor == 1] <- "No"
nausea_data_t0

## ----echo=FALSE------------------------
# Contingency Table
nausea_table_t0 <- table(nausea_data_t0)
nausea_table_t0


## ----echo=FALSE------------------------
fisher.test(nausea_table_t0)


## ----echo=FALSE------------------------
nausea_data_t1 <- data.frame(
  c(group_control$T1..Presencia.de.nausea.y.vomito.a.los.60.minutos.Si.0..NO..1., group_Experimental$T1..Presencia.de.nausea.y.vomito.a.los.60.minutos.Si.0..NO..1.), c(rep("control", length(group_control$T1..Presencia.de.nausea.y.vomito.a.los.60.minutos.Si.0..NO..1.)), rep("experimental", length(group_Experimental$T1..Presencia.de.nausea.y.vomito.a.los.60.minutos.Si.0..NO..1.)))
)
colnames(nausea_data_t1) <- c("Valor", "Grupo")
nausea_data_t1$Valor[nausea_data_t1$Valor == 0] <- "Sí"
nausea_data_t1$Valor[nausea_data_t1$Valor == 1] <- "No"
nausea_data_t1

## ----echo=FALSE------------------------
# Contingency Table
nausea_table_t1 <- table(nausea_data_t1)
nausea_table_t1


## ----echo=FALSE------------------------
fisher.test(nausea_table_t1)


## ----echo=FALSE------------------------
nausea_data_t2 <- data.frame(
  c(group_control$T2..Presencia.de.nausea.y.vomito.120.minutos..Si.0..NO..1., group_Experimental$T2..Presencia.de.nausea.y.vomito.120.minutos..Si.0..NO..1.), c(rep("control", length(group_control$T2..Presencia.de.nausea.y.vomito.120.minutos..Si.0..NO..1.)), rep("experimental", length(group_Experimental$T2..Presencia.de.nausea.y.vomito.120.minutos..Si.0..NO..1.)))
)
colnames(nausea_data_t2) <- c("Valor", "Grupo")
nausea_data_t2$Valor[nausea_data_t2$Valor == 0] <- "Sí"
nausea_data_t2$Valor[nausea_data_t2$Valor == 1] <- "No"
nausea_data_t2

## ----echo=FALSE------------------------
# Contingency Table
nausea_table_t2 <- table(nausea_data_t2)
nausea_table_t2


## ----echo=FALSE------------------------
fisher.test(nausea_table_t1)


## ----echo=FALSE------------------------
# Dataframe
nausea_data <- data.frame(
  c(rep("control", 3*length(group_control$Grupo..control..0..experimental..1...)), rep("experimental", 3*length(group_Experimental$Grupo..control..0..experimental..1...))),
  c(group_control$T0..Presencia.de.nausea.y.vomito.en.postoperatorio.inmediato.Si.0..NO..1., group_control$T1..Presencia.de.nausea.y.vomito.a.los.60.minutos.Si.0..NO..1., group_control$T2..Presencia.de.nausea.y.vomito.120.minutos..Si.0..NO..1., group_Experimental$T0..Presencia.de.nausea.y.vomito.en.postoperatorio.inmediato.Si.0..NO..1., group_Experimental$T1..Presencia.de.nausea.y.vomito.a.los.60.minutos.Si.0..NO..1., group_Experimental$T2..Presencia.de.nausea.y.vomito.120.minutos..Si.0..NO..1.),
  c(rep(c("T0", "T1", "T2"), times = 2, each = length(group_control$T0..Presencia.de.nausea.y.vomito.en.postoperatorio.inmediato.Si.0..NO..1.)))
)

colnames(nausea_data) <- c("Group", "Value", "Time")
nausea_data$Value[nausea_data$Value == 0] <- "Sí"
nausea_data$Value[nausea_data$Value == 1] <- "No"
nausea_data


## ----echo=FALSE------------------------
# Dataframe
nausea_frequencies <- data.frame(table(nausea_data))
nausea_frequencies


## ----echo=FALSE------------------------
# Grouped Bar Plots
ggplot(nausea_frequencies,
       aes(x = Group,
           y = Freq,
           fill = Value)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Time) +
  labs(fill="Presencia de Náusea y Vómito",
       x="Tiempo",
       y="Frecuencia",
       title="Frecuencias Agrupadas de Náusea y Vómito por Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=12))
 

