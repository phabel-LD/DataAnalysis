## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("EvelinDiaz_data.xlsx"))


## ----echo=FALSE------------------------
data


## ----echo=FALSE------------------------
group_control = data[data$Grupo..0.Control..1.Exp. == 0,]
group_control

## ----echo=FALSE------------------------
group_Experimental = data[data$Grupo..0.Control..1.Exp. == 1,]
group_Experimental 


## ----echo=FALSE------------------------
data_groups <- data.frame(data %>% count(Grupo..0.Control..1.Exp., sort = FALSE))
data_groups

## ----echo=FALSE------------------------
ggplot(data_groups, aes(x = "", y = n, fill = factor(Grupo..0.Control..1.Exp.))) +
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
ggplot(data, aes(x = Edad..años., fill = factor(Grupo..0.Control..1.Exp.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 3,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Edad (años)",
       y="Frequencia",
       title="Histograma de Edad por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = IMC..normal.1..sobrepeso.2..obesidad.1.3., fill = factor(Grupo..0.Control..1.Exp.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.5,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="IMC",
       y="Frequencia",
       title="Histograma de IMC por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
gender_control <- data.frame(group_control %>% count(Género..0.Masc..1.Fem., sort = FALSE))
gender_control


## ----echo=FALSE------------------------
ggplot(gender_control, aes(x = "", y = n, fill = factor(Género..0.Masc..1.Fem.))) +
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
summary(group_control$Edad..años.)


## ----echo=FALSE------------------------
# Variance
var(group_control$Edad..años.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$Edad..años.)


## ----echo=FALSE------------------------
# Histogram
ggplot(group_control, aes(x = factor(ASA..1.2.3.4.5.6.), fill = factor(ASA..1.2.3.4.5.6.))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Control") +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$IMC..normal.1..sobrepeso.2..obesidad.1.3.)


## ----echo=FALSE------------------------
# Variance
var(group_control$IMC..normal.1..sobrepeso.2..obesidad.1.3.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$IMC..normal.1..sobrepeso.2..obesidad.1.3.)


## ----echo=FALSE------------------------
gender_Experimental <- data.frame(group_Experimental %>% count(Género..0.Masc..1.Fem., sort = FALSE))
gender_Experimental


## ----echo=FALSE------------------------
ggplot(gender_Experimental, aes(x = "", y = n, fill = factor(Género..0.Masc..1.Fem.))) +
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
ggplot(group_Experimental, aes(x = factor(ASA..1.2.3.4.5.6.), fill = factor(ASA..1.2.3.4.5.6.))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Experimental") +
  theme(plot.title = element_text(size=15))



## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$IMC..normal.1..sobrepeso.2..obesidad.1.3.)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$IMC..normal.1..sobrepeso.2..obesidad.1.3.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$IMC..normal.1..sobrepeso.2..obesidad.1.3.)


## ----echo=FALSE------------------------
# Control
control_satisfaction <- group_control$Satisfacción.URPA..Muy.bueno.1..bueno.2..Moderado.3..leve.4..Malo.5.
control_satisfaction


## ----echo=FALSE------------------------
summary(control_satisfaction)


## ----echo=FALSE------------------------
var(control_satisfaction)


## ----echo=FALSE------------------------
sd(control_satisfaction)


## ----echo=FALSE------------------------
# Experimental
experimental_satisfaction <- group_Experimental$Satisfacción.URPA..Muy.bueno.1..bueno.2..Moderado.3..leve.4..Malo.5.
experimental_satisfaction


## ----echo=FALSE------------------------
summary(experimental_satisfaction)


## ----echo=FALSE------------------------
var(experimental_satisfaction)


## ----echo=FALSE------------------------
sd(experimental_satisfaction)


## ----echo=FALSE------------------------
# Hypothesis Testing DataFrame
satisfaction_data <- data.frame(
  c(rep("control", length(group_control$Satisfacción.URPA..Muy.bueno.1..bueno.2..Moderado.3..leve.4..Malo.5.)), rep("experimental", length(group_Experimental$Satisfacción.URPA..Muy.bueno.1..bueno.2..Moderado.3..leve.4..Malo.5.))),
  c(group_control$Satisfacción.URPA..Muy.bueno.1..bueno.2..Moderado.3..leve.4..Malo.5., group_Experimental$Satisfacción.URPA..Muy.bueno.1..bueno.2..Moderado.3..leve.4..Malo.5.))

satisfaction_data$a[group_control$Rescate.2.Tramadol..mg. != "No"] <- "Rescate"
satisfaction_data$a[group_control$Rescate.2.Tramadol..mg. == "No"] <- "No Rescate"

colnames(satisfaction_data) <- c("grupo", "value", "rescate")


## ----echo=FALSE------------------------
# Statistical Test
stat.test <- satisfaction_data %>%
  group_by(rescate) %>%
  wilcox_test(value ~ grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots

bxp <- ggboxplot(
  satisfaction_data, x = "rescate", y = "value", 
  color = "grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Valor",
       y="Satisfacción",
       title="Comparación de Satisfacción entre Grupos dependiendo de Rescate") +
  theme(plot.title = element_text(size=10))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "rescate", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
ena_data <- data.frame(
  c(rep("control", 3*length(group_control$Grupo..0.Control..1.Exp.)), rep("experimental", 3*length(group_control$Grupo..0.Control..1.Exp.))),
  c(group_control$ENA.T0..1.10., group_control$ENA.T1..1.10., group_control$ENA.T2..1.10., group_Experimental$ENA.T0..1.10., group_Experimental$ENA.T1..1.10., group_Experimental$ENA.T2..1.10.),
  c(rep("t0", length(group_Experimental$ENA.T0..1.10.)), rep("t1", length(group_Experimental$ENA.T1..1.10.)), rep("t2", length(group_Experimental$ENA.T2..1.10.)))
)
colnames(ena_data) <- c("Grupo", "ENA", "Tiempo")
ena_data


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- ena_data %>%
  group_by(Tiempo) %>%
  wilcox_test(ENA ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test



## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  ena_data, x = "Tiempo", y = "ENA", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="ENA",
       title="Comparación de ENA entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=10))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Tiempo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
# Grouped Lineplots
lp <- ggline(
  ena_data, x = "Tiempo", y = "ENA", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="ENA",
       title="Tendencias de ENA entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=10))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Tiempo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )

