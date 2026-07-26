## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("CharbelMorenoLimon_Data.xlsx"))


## ----echo=FALSE------------------------
data


## ----echo=FALSE------------------------
group_control = data[data$GRUPO..CONTROL.0...EXPERIMENTAL.1. == 0,]
group_control

## ----echo=FALSE------------------------
group_Experimental = data[data$GRUPO..CONTROL.0...EXPERIMENTAL.1. == 1,]
group_Experimental 


## ----echo=FALSE------------------------
data_groups <- data.frame(data %>% count(GRUPO..CONTROL.0...EXPERIMENTAL.1., sort = FALSE))
data_groups

## ----echo=FALSE------------------------
ggplot(data_groups, aes(x = "", y = n, fill = factor(GRUPO..CONTROL.0...EXPERIMENTAL.1.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Grupo",
       x=NULL,
       y=NULL,
       title="Proporciones entre Grupos") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=23))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = EDAD, fill = factor(GRUPO..CONTROL.0...EXPERIMENTAL.1.))) +
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
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = IMC, fill = factor(GRUPO..CONTROL.0...EXPERIMENTAL.1.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="IMC",
       y="Frequencia",
       title="Histograma de IMC por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = TALLA..MTS., fill = factor(GRUPO..CONTROL.0...EXPERIMENTAL.1.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.175,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Talla",
       y="Frequencia",
       title="Histograma de Talla por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
gender_control <- data.frame(group_control %>% count(SEXO..HOMBRE.0...MUJER.1., sort = FALSE))
gender_control


## ----echo=FALSE------------------------
ggplot(gender_control, aes(x = "", y = n, fill = factor(SEXO..HOMBRE.0...MUJER.1.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género del Grupo Control") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Masculino", "Femenino")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$EDAD)


## ----echo=FALSE------------------------
# Variance
var(group_control$EDAD)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$EDAD)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$TALLA..MTS.)


## ----echo=FALSE------------------------
# Variance
var(group_control$TALLA..MTS.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$TALLA..MTS.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$EVA.0.MINUTOS)


## ----echo=FALSE------------------------
# Variance
var(group_control$EVA.0.MINUTOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$EVA.0.MINUTOS)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$EVA.30.MINUTOS)


## ----echo=FALSE------------------------
# Variance
var(group_control$EVA.30.MINUTOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$EVA.30.MINUTOS)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$EVA.90.MINUTOS)


## ----echo=FALSE------------------------
# Variance
var(group_control$EVA.90.MINUTOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$EVA.90.MINUTOS)


## ----echo=FALSE------------------------
# Histogram
ggplot(group_control, aes(x = factor(ASA), fill = factor(ASA))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Control") +
  theme(plot.title = element_text(size=15))


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
gender_Experimental <- data.frame(group_Experimental %>% count(SEXO..HOMBRE.0...MUJER.1., sort = FALSE))
gender_Experimental


## ----echo=FALSE------------------------
ggplot(gender_Experimental, aes(x = "", y = n, fill = factor(SEXO..HOMBRE.0...MUJER.1.))) +
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
summary(group_Experimental$EDAD)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$EDAD)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$EDAD)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$TALLA..MTS.)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$TALLA..MTS.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$TALLA..MTS.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$EVA.0.MINUTOS)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$EVA.0.MINUTOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$EVA.0.MINUTOS)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$EVA.30.MINUTOS)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$EVA.30.MINUTOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$EVA.30.MINUTOS)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$EVA.90.MINUTOS)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$EVA.90.MINUTOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$EVA.90.MINUTOS)


## ----echo=FALSE------------------------
# Histogram
ggplot(group_Experimental, aes(x = factor(ASA), fill = factor(ASA))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Experimental") +
  theme(plot.title = element_text(size=12))



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
data_groups <- data.frame(data %>% count(EFECTOS.ADVERSOS..SI.0...NO.1., sort = FALSE))
data_groups

## ----echo=FALSE------------------------
ggplot(data_groups, aes(x = "", y = n, fill = factor(EFECTOS.ADVERSOS..SI.0...NO.1.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Grupo",
       x=NULL,
       y=NULL,
       title="Proporciones en Efecto Adversos") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Presentó Efecto Adverso")) +
  theme(plot.title = element_text(size=23))


## ----echo=FALSE------------------------
eva_data <- data.frame(
  c(rep("control", 3*length(group_control$GRUPO..CONTROL.0...EXPERIMENTAL.1.)), rep("experimental", 3*length(group_control$GRUPO..CONTROL.0...EXPERIMENTAL.1.))),
  c(group_control$EVA.0.MINUTOS, group_control$EVA.30.MINUTOS, group_control$EVA.90.MINUTOS, group_Experimental$EVA.0.MINUTOS, group_Experimental$EVA.30.MINUTOS, group_Experimental$EVA.90.MINUTOS),
  c(rep("t0", length(group_Experimental$EVA.0.MINUTOS)), rep("t30", length(group_Experimental$EVA.30.MINUTOS)), rep("t90", length(group_Experimental$EVA.90.MINUTOS)))
)
colnames(eva_data) <- c("Group", "EVA", "Time")
eva_data


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- eva_data %>%
  group_by(Time) %>%
  wilcox_test(EVA ~ Group) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test



## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  eva_data, x = "Time", y = "EVA", 
  color = "Group", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="EVA",
       title="Comparación de EVA entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=12))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Time", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )



## ----echo=FALSE------------------------
# Grouped Lineplots
lp <- ggline(
  eva_data, x = "Time", y = "EVA", add = "mean_sd", 
  color = "Group", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="EVA",
       title="Tendencias de EVA entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=12))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Time") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )

