## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("BecerrilCastilloJaneth_Data.xlsx"))


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
       title="Proporciones entre Grupos: Control vs. Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = Edad, fill = factor(Grupo_0Control_1Experimental))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 5,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Edad",
       y="Frequencia",
       title="Histograma de Edad por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = Peso..kg., fill = factor(Grupo_0Control_1Experimental))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 5,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Peso (Kg)",
       y="Frequencia",
       title="Histograma de Peso por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = Talla..m., fill = factor(Grupo_0Control_1Experimental))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.05,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Talla (m)",
       y="Frequencia",
       title="Histograma de Talla por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = IMC, fill = factor(Grupo_0Control_1Experimental))) +
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
gender_control <- data.frame(group_control %>% count(Sexo_1Masculino_2Femenino, sort = FALSE))
gender_control


## ----echo=FALSE------------------------
ggplot(gender_control, aes(x = "", y = n, fill = factor(Sexo_1Masculino_2Femenino))) +
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
summary(group_control$Edad)


## ----echo=FALSE------------------------
# Variance
var(group_control$Edad)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$Edad)


## ----echo=FALSE------------------------
# Histogram
ggplot(group_control, aes(x = factor(ASA_123), fill = factor(ASA_123))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Control") +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$Peso..kg.)


## ----echo=FALSE------------------------
# Variance
var(group_control$Peso..kg.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$Peso..kg.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$Talla..m.)


## ----echo=FALSE------------------------
# Variance
var(group_control$Talla..m.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$Talla..m.)


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
gender_Experimental <- data.frame(group_Experimental %>% count(Sexo_1Masculino_2Femenino, sort = FALSE))
gender_Experimental


## ----echo=FALSE------------------------
ggplot(gender_Experimental, aes(x = "", y = n, fill = factor(Sexo_1Masculino_2Femenino))) +
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
summary(group_Experimental$Edad)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$Edad)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$Edad)


## ----echo=FALSE------------------------
# Histogram
ggplot(group_Experimental, aes(x = factor(ASA_123), fill = factor(ASA_123))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Experimental") +
  theme(plot.title = element_text(size=15))



## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$Peso..kg.)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$Peso..kg.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$Peso..kg.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$Talla..m.)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$Talla..m.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$Talla..m.)


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
summary(group_control$IPID)


## ----echo=FALSE------------------------
var(group_control$IPID)


## ----echo=FALSE------------------------
sd(group_control$IPID)


## ----echo=FALSE------------------------
summary(group_Experimental$IPID)


## ----echo=FALSE------------------------
var(group_Experimental$IPID)


## ----echo=FALSE------------------------
sd(group_Experimental$IPID)


## ----echo=FALSE------------------------
ipid_data <- data.frame(
  c(rep("control", length(group_control$IPID)), rep("experimental", length(group_Experimental$IPID))),
  c(group_control$IPID, group_Experimental$IPID)
)
colnames(ipid_data) <- c("Grupo", "Valor")
ipid_data


## ----echo=FALSE------------------------
# Statistical Test
stat.test <- ipid_data %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots

bxp <- ggboxplot(
  ipid_data, x = "Grupo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Valor",
       y="IPID",
       title="Comparación de IPID entre Grupos") +
  theme(plot.title = element_text(size=15))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Grupo", dodge = 1)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )



## ----echo=FALSE------------------------
spo2_data <- data.frame(
  c(rep("control", 3*length(group_control$T0.SpO2)), rep("experimental", 3*length(group_Experimental$T0.SpO2))),
  c(rep(c("t0", "t1", "t2"), times=2, each=length(group_control$T0.SpO2))),
  c(group_control$T0.SpO2, group_control$T1.SpO2, group_control$T2.SpO2, group_Experimental$T0.SpO2, group_Experimental$T1.SpO2, group_Experimental$T2.SpO2)
)
colnames(spo2_data) <- c("Grupo", "Tiempo", "SpO2")
spo2_data


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- spo2_data %>%
  group_by(Tiempo) %>%
  wilcox_test(SpO2 ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  spo2_data, x = "Tiempo", y = "SpO2", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="SpO2",
       title="Comparación de SpO2 entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=15))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Tiempo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
# Grouped Lineplots
lp <- ggline(
  spo2_data, x = "Tiempo", y = "SpO2", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="SpO2",
       title="Tendencias de SpO2 entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=15))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Tiempo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )

