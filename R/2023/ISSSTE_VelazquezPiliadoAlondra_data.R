## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("VelazquezPiliadoAlondra_data.xlsx"))


## ----echo=FALSE------------------------
data


## ----echo=FALSE------------------------
group_control = data[data$Grupo_0Control_1Experimental == 0,]
group_control

## ----echo=FALSE------------------------
group_Experimental = data[data$Grupo_0Control_1Experimental == 1,]
group_Experimental 


## ----echo=FALSE------------------------
data_groups <- data.frame(data %>% count(Grupo_0Control_1Experimental, sort = FALSE))
data_groups

## ----echo=FALSE------------------------
ggplot(data_groups, aes(x = "", y = n, fill = factor(Grupo_0Control_1Experimental))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Grupo",
       x=NULL,
       y=NULL,
       title="Proporciones entre Grupos") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = Edad, fill = factor(Grupo_0Control_1Experimental))) +
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
ggplot(data, aes(x = as.numeric(Talla..m.), fill = factor(Grupo_0Control_1Experimental))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.175,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Talla (m)",
       y="Frequencia",
       title="Histograma de Talla por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = Peso..kg., fill = factor(Grupo_0Control_1Experimental))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 3,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Peso",
       y="Frequencia",
       title="Histograma de Peso (kg) por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = as.numeric(IMC..kg.m2.), fill = factor(Grupo_0Control_1Experimental))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="IMC",
       y="Frequencia",
       title="Histograma de IMC (kg/m2) por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = Duracion.de.Cirugia..min., fill = factor(Grupo_0Control_1Experimental))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 5,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Duración Cirugía (min)",
       y="Frequencia",
       title="Histograma de Duración de Cirugía (min) por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=13))

## ----echo=TRUE-------------------------
summary(data$Duracion.de.Cirugia..min.[!is.na(data$Duracion.de.Cirugia..min.)])


## ----echo=TRUE-------------------------
var(data$Duracion.de.Cirugia..min.[!is.na(data$Duracion.de.Cirugia..min.)])


## ----echo=TRUE-------------------------
sd(data$Duracion.de.Cirugia..min.[!is.na(data$Duracion.de.Cirugia..min.)])


## ----echo=FALSE------------------------
data_groups <- data.frame(data %>% count(Complicaciones_0No_1Si, sort = FALSE))
data_groups

## ----echo=FALSE------------------------
ggplot(data_groups, aes(x = "", y = n, fill = factor(Complicaciones_0No_1Si))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Complicaciones Posoperatorias",
       x=NULL,
       y=NULL,
       title="Complicaciones Posoperatorias en Total") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("No")) +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
gender_control <- data.frame(group_control %>% count(Sexo_1Masc_2Fem, sort = FALSE))
gender_control


## ----echo=FALSE------------------------
ggplot(gender_control, aes(x = "", y = n, fill = factor(Sexo_1Masc_2Fem))) +
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
summary(group_control$Edad)


## ----echo=FALSE------------------------
# Variance
var(group_control$Edad[!is.na(group_control$Edad)])


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$Edad[!is.na(group_control$Edad)])


## ----echo=FALSE------------------------
# Histogram
ggplot(group_control, aes(x = factor(ASA.123), fill = factor(ASA.123))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Control") +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(group_control$IMC..kg.m2.))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(group_control$IMC..kg.m2.)[!is.na(as.numeric(group_control$IMC..kg.m2.))])


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(group_control$IMC..kg.m2.)[!is.na(as.numeric(group_control$IMC..kg.m2.))])


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(group_control$Talla..m.))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(group_control$Talla..m.)[!is.na(as.numeric(group_control$Talla..m.))])


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(group_control$Talla..m.)[!is.na(as.numeric(group_control$Talla..m.))])


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(group_control$Peso..kg.))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(group_control$Peso..kg.)[!is.na(as.numeric(group_control$Peso..kg.))])


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(group_control$Peso..kg.)[!is.na(as.numeric(group_control$Peso..kg.))])


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(group_control$Presion.arterial.media..mmHg.))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(group_control$Presion.arterial.media..mmHg.)[!is.na(as.numeric(group_control$Presion.arterial.media..mmHg.))])


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(group_control$Presion.arterial.media..mmHg.)[!is.na(as.numeric(group_control$Presion.arterial.media..mmHg.))])


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(group_control$FC..lpm.))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(group_control$FC..lpm.)[!is.na(as.numeric(group_control$FC..lpm.))])


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(group_control$FC..lpm.)[!is.na(as.numeric(group_control$FC..lpm.))])


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(group_control$Duracion.de.anestesia..min.))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(group_control$Duracion.de.anestesia..min.)[!is.na(as.numeric(group_control$Duracion.de.anestesia..min.))])


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(group_control$Duracion.de.anestesia..min.)[!is.na(as.numeric(group_control$Duracion.de.anestesia..min.))])


## ----echo=FALSE------------------------
tos_control <- data.frame(group_control %>% count(MinogueTos_1234, sort = FALSE))
tos_control


## ----echo=FALSE------------------------
ggplot(tos_control, aes(x = "", y = n, fill = factor(MinogueTos_1234))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Grado",
       x=NULL,
       y=NULL,
       title="Proporciones de Reflejo Tusígeno (Minogue) - Grupo Control") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("1", "2", "3")) +
  theme(plot.title = element_text(size=10))


## ----echo=FALSE------------------------
gender_Experimental <- data.frame(group_Experimental %>% count(Sexo_1Masc_2Fem, sort = FALSE))
gender_Experimental


## ----echo=FALSE------------------------
ggplot(gender_Experimental, aes(x = "", y = n, fill = factor(Sexo_1Masc_2Fem))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género del Grupo Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Masculino", "Femenino")) +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$Edad)


## ----echo=FALSE------------------------
# Variance
var(group_control$Edad[!is.na(group_control$Edad)])


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$Edad[!is.na(group_control$Edad)])


## ----echo=FALSE------------------------
# Histogram
ggplot(group_Experimental, aes(x = factor(ASA.123), fill = factor(ASA.123))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Experimental") +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(group_Experimental$IMC..kg.m2.))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(group_Experimental$IMC..kg.m2.)[!is.na(as.numeric(group_Experimental$IMC..kg.m2.))])


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(group_Experimental$IMC..kg.m2.)[!is.na(as.numeric(group_Experimental$IMC..kg.m2.))])


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(group_Experimental$Talla..m.))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(group_Experimental$Talla..m.)[!is.na(as.numeric(group_Experimental$Talla..m.))])


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(group_Experimental$Talla..m.)[!is.na(as.numeric(group_Experimental$Talla..m.))])


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(group_Experimental$Peso..kg.))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(group_Experimental$Peso..kg.)[!is.na(as.numeric(group_Experimental$Peso..kg.))])


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(group_Experimental$Peso..kg.)[!is.na(as.numeric(group_Experimental$Peso..kg.))])


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(group_Experimental$Presion.arterial.media..mmHg.))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(group_Experimental$Presion.arterial.media..mmHg.)[!is.na(as.numeric(group_Experimental$Presion.arterial.media..mmHg.))])


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(group_Experimental$Presion.arterial.media..mmHg.)[!is.na(as.numeric(group_Experimental$Presion.arterial.media..mmHg.))])


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(group_Experimental$FC..lpm.))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(group_Experimental$FC..lpm.)[!is.na(as.numeric(group_Experimental$FC..lpm.))])


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(group_Experimental$FC..lpm.)[!is.na(as.numeric(group_Experimental$FC..lpm.))])


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(group_Experimental$Duracion.de.anestesia..min.))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(group_Experimental$Duracion.de.anestesia..min.)[!is.na(as.numeric(group_Experimental$Duracion.de.anestesia..min.))])


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(group_Experimental$Duracion.de.anestesia..min.)[!is.na(as.numeric(group_Experimental$Duracion.de.anestesia..min.))])


## ----echo=FALSE------------------------
tos_Experimental <- data.frame(group_Experimental %>% count(MinogueTos_1234, sort = FALSE))
tos_Experimental


## ----echo=FALSE------------------------
ggplot(tos_Experimental, aes(x = "", y = n, fill = factor(MinogueTos_1234))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Grado",
       x=NULL,
       y=NULL,
       title="Proporciones de Reflejo Tusígeno (Minogue) - Grupo Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("1", "2")) +
  theme(plot.title = element_text(size=10))


## ----echo=FALSE------------------------
PA_data <- data.frame(
  c(rep("control", length(group_control$Grupo_0Control_1Experimental)), rep("experimental", length(group_control$Grupo_0Control_1Experimental))),
  c(group_control$Presion.arterial.media..mmHg., group_Experimental$Presion.arterial.media..mmHg.)
)
colnames(PA_data) <- c("Grupo", "Valor")
PA_data <- PA_data[!is.na(PA_data$Valor),]
PA_data


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- PA_data %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  PA_data, x = "Grupo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Grupo",
       y="Presión Arterial (mmHg)",
       title="Comparación de Presión Arterial (mmHg) entre Grupos") +
  theme(plot.title = element_text(size=10))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Grupo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
# Grouped Lineplots
lp <- ggline(
  PA_data, x = "Grupo", y = "Valor", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Grupo",
       y="Presión Arterial (mmHg)",
       title="Tendencias de Presión Arterial (mmHg) entre Grupos") +
  theme(plot.title = element_text(size=13))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Grupo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )


## ----echo=FALSE------------------------
FC_data <- data.frame(
  c(rep("control", length(group_control$Grupo_0Control_1Experimental)), rep("experimental", length(group_control$Grupo_0Control_1Experimental))),
  c(group_control$FC..lpm., group_Experimental$FC..lpm.)
)
colnames(FC_data) <- c("Grupo", "Valor")
FC_data <- FC_data[!is.na(FC_data$Valor),]
FC_data


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- FC_data %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  FC_data, x = "Grupo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Grupo",
       y="Frecuencia Cardíaca (lat/min)",
       title="Comparación de Frecuencia Cardíaca (lat/min) entre Grupos") +
  theme(plot.title = element_text(size=10))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Grupo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
# Grouped Lineplots
lp <- ggline(
  FC_data, x = "Grupo", y = "Valor", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Grupo",
       y="Frecuencia Cardíaca (lat/min)",
       title="Tendencias de Frecuencia Cardíaca (lat/min) entre Grupos") +
  theme(plot.title = element_text(size=10))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Grupo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )


## ----echo=FALSE------------------------
DurAnest_data <- data.frame(
  c(rep("control", length(group_control$Grupo_0Control_1Experimental)), rep("experimental", length(group_control$Grupo_0Control_1Experimental))),
  c(group_control$Duracion.de.anestesia..min., group_Experimental$Duracion.de.anestesia..min.)
)
colnames(DurAnest_data) <- c("Grupo", "Valor")
DurAnest_data <- DurAnest_data[!is.na(DurAnest_data$Valor),]
DurAnest_data


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- DurAnest_data %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  DurAnest_data, x = "Grupo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Grupo",
       y="Duración Anestesia (min)",
       title="Comparación de Duración Anestesia (min) entre Grupos") +
  theme(plot.title = element_text(size=10))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Grupo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
# Grouped Lineplots
lp <- ggline(
  DurAnest_data, x = "Grupo", y = "Valor", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Grupo",
       y="Duración Anestesia (min)",
       title="Tendencias de Duración Anestesia (min) entre Grupos") +
  theme(plot.title = element_text(size=13))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Grupo")
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )


## ----echo=FALSE------------------------
Tos_data <- data.frame(
  c(rep("control", length(group_control$Grupo_0Control_1Experimental)), rep("experimental", length(group_control$Grupo_0Control_1Experimental))),
  c(group_control$MinogueTos_1234, group_Experimental$MinogueTos_1234)
)
colnames(Tos_data) <- c("Grupo", "Valor")
Tos_data <- Tos_data[!is.na(Tos_data$Valor),]
Tos_data


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- Tos_data %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  Tos_data, x = "Grupo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Grupo",
       y="Minogue Tos",
       title="Comparación de Minogue Tos entre Grupos") +
  theme(plot.title = element_text(size=10))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Grupo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
# Grouped Lineplots
lp <- ggline(
  Tos_data, x = "Grupo", y = "Valor", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Grupo",
       y="Duración Anestesia (min)",
       title="Tendencias de Minogue Tos entre Grupos") +
  theme(plot.title = element_text(size=15))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Grupo")
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )

