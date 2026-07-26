## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("OrtizLopezGeorgina_Data.xlsx"))


## ----echo=FALSE------------------------
data


## ----echo=FALSE------------------------
group_control = data[data$GRUPO..0.1. == 0,]
group_control

## ----echo=FALSE------------------------
group_Experimental = data[data$GRUPO..0.1. == 1,]
group_Experimental 


## ----echo=FALSE------------------------
data_groups <- data.frame(data %>% count(GRUPO..0.1., sort = FALSE))
data_groups

## ----echo=FALSE------------------------
ggplot(data_groups, aes(x = "", y = n, fill = factor(GRUPO..0.1.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Grupo",
       x=NULL,
       y=NULL,
       title="Proporciones entre Grupos: Control vs. Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Dexmedetomidina Intravenosa", "Dexmedetomidina Intranasal")) +
  theme(plot.title = element_text(size=8))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = EDAD..años., fill = factor(GRUPO..0.1.))) +
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
  scale_fill_discrete(labels = c("Dexmedetomidina Intravenosa", "Dexmedetomidina Intranasal")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = PESO..Kg., fill = factor(GRUPO..0.1.))) +
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
  scale_fill_discrete(labels = c("Dexmedetomidina Intravenosa", "Dexmedetomidina Intranasal")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = TALLA..m., fill = factor(GRUPO..0.1.))) +
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
  scale_fill_discrete(labels = c("Dexmedetomidina Intravenosa", "Dexmedetomidina Intranasal")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = IMC, fill = factor(GRUPO..0.1.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 4,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="IMC",
       y="Frequencia",
       title="Histograma de IMC por Grupo") +
  scale_fill_discrete(labels = c("Dexmedetomidina Intravenosa", "Dexmedetomidina Intranasal")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
gender_control <- data.frame(group_control %>% count(GÉNERO..0..MASCULINO.1..FEMENINO., sort = FALSE))
gender_control


## ----echo=FALSE------------------------
ggplot(gender_control, aes(x = "", y = n, fill = factor(GÉNERO..0..MASCULINO.1..FEMENINO.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género del Grupo Dexmedetomidina Intravenosa") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Masculino", "Femenino")) +
  theme(plot.title = element_text(size=8))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$EDAD..años.)


## ----echo=FALSE------------------------
# Variance
var(group_control$EDAD..años.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$EDAD..años.)


## ----echo=FALSE------------------------
# Histogram
ggplot(group_control, aes(x = factor(ASA), fill = factor(ASA))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Dexmedetomidina Intravenosa") +
  theme(plot.title = element_text(size=8))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$PESO..Kg.)


## ----echo=FALSE------------------------
# Variance
var(group_control$PESO..Kg.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$PESO..Kg.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$TALLA..m.)


## ----echo=FALSE------------------------
# Variance
var(group_control$TALLA..m.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$TALLA..m.)


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
gender_Experimental <- data.frame(group_Experimental %>% count(GÉNERO..0..MASCULINO.1..FEMENINO., sort = FALSE))
gender_Experimental


## ----echo=FALSE------------------------
ggplot(gender_Experimental, aes(x = "", y = n, fill = factor(GÉNERO..0..MASCULINO.1..FEMENINO.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género del Grupo Dexmedetomidina Intranasal") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Masculino", "Femenino")) +
  theme(plot.title = element_text(size=8))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$EDAD..años.)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$EDAD..años.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$EDAD..años.)


## ----echo=FALSE------------------------
# Histogram
ggplot(group_Experimental, aes(x = factor(ASA), fill = factor(ASA))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Dexmedetomidina Nasal") +
  theme(plot.title = element_text(size=8))



## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$PESO..Kg.)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$PESO..Kg.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$PESO..Kg.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$TALLA..m.)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$TALLA..m.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$TALLA..m.)


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
# Control
control_FC <- group_control[,9:16]
control_FC


## ----echo=FALSE------------------------
summary(control_FC)


## ----echo=FALSE------------------------
var(control_FC$T0..FC..15.o.45.minutos.antes.del.colocar.el.medicamento.)
var(control_FC$T1.FC..al.inicio.del.procedimiento.qx.)
var(control_FC$T2.FC..Al.inicio.del.procedimiento.)
var(control_FC$T3.FC..60.min.después.)
var(control_FC$T4.FC..al.termino.del.procedimiento.)
var(control_FC$T5.FC..30.min.en.la.UCPA.)
var(control_FC$T6.FC..60.min.en.UCPA.)
var(control_FC$T7.FC..Al.alta.)


## ----echo=FALSE------------------------
sd(control_FC$T0..FC..15.o.45.minutos.antes.del.colocar.el.medicamento.)
sd(control_FC$T1.FC..al.inicio.del.procedimiento.qx.)
sd(control_FC$T2.FC..Al.inicio.del.procedimiento.)
sd(control_FC$T3.FC..60.min.después.)
sd(control_FC$T4.FC..al.termino.del.procedimiento.)
sd(control_FC$T5.FC..30.min.en.la.UCPA.)
sd(control_FC$T6.FC..60.min.en.UCPA.)
sd(control_FC$T7.FC..Al.alta.)


## ----echo=FALSE------------------------
# Experimental
experimental_FC <- group_Experimental[,9:16]
experimental_FC


## ----echo=FALSE------------------------
summary(experimental_FC)


## ----echo=FALSE------------------------
var(experimental_FC$T0..FC..15.o.45.minutos.antes.del.colocar.el.medicamento.)
var(experimental_FC$T1.FC..al.inicio.del.procedimiento.qx.)
var(experimental_FC$T2.FC..Al.inicio.del.procedimiento.)
var(experimental_FC$T3.FC..60.min.después.)
var(experimental_FC$T4.FC..al.termino.del.procedimiento.)
var(experimental_FC$T5.FC..30.min.en.la.UCPA.)
var(experimental_FC$T6.FC..60.min.en.UCPA.)
var(experimental_FC$T7.FC..Al.alta.)


## ----echo=FALSE------------------------
sd(experimental_FC$T0..FC..15.o.45.minutos.antes.del.colocar.el.medicamento.)
sd(experimental_FC$T1.FC..al.inicio.del.procedimiento.qx.)
sd(experimental_FC$T2.FC..Al.inicio.del.procedimiento.)
sd(experimental_FC$T3.FC..60.min.después.)
sd(experimental_FC$T4.FC..al.termino.del.procedimiento.)
sd(experimental_FC$T5.FC..30.min.en.la.UCPA.)
sd(experimental_FC$T6.FC..60.min.en.UCPA.)
sd(experimental_FC$T7.FC..Al.alta.)


## ----echo=FALSE------------------------
# Hypothesis Testing DataFrame

# Control Group
values_control <- c(group_control$T0..FC..15.o.45.minutos.antes.del.colocar.el.medicamento.,
            group_control$T1.FC..al.inicio.del.procedimiento.qx.,
            group_control$T2.FC..Al.inicio.del.procedimiento.,
            group_control$T3.FC..60.min.después.,
            group_control$T4.FC..al.termino.del.procedimiento.,
            group_control$T5.FC..30.min.en.la.UCPA.,
            group_control$T6.FC..60.min.en.UCPA.,
            group_control$T7.FC..Al.alta.)
groups_control <- rep("control", length(values_control))
times_control <- c(rep("t0", length(group_control$T0..FC..15.o.45.minutos.antes.del.colocar.el.medicamento.)),
           rep("t1", length(group_control$T1.FC..al.inicio.del.procedimiento.qx.)),
           rep("t2", length(group_control$T2.FC..Al.inicio.del.procedimiento.)),
           rep("t3", length(group_control$T3.FC..60.min.después.)),
           rep("t4", length(group_control$T4.FC..al.termino.del.procedimiento.)),
           rep("t5", length(group_control$T5.FC..30.min.en.la.UCPA.)),
           rep("t6", length(group_control$T6.FC..60.min.en.UCPA.)),
           rep("t7", length(group_control$T7.FC..Al.alta.)))

# Experimental Group
values_experimental <- c(group_Experimental$T0..FC..15.o.45.minutos.antes.del.colocar.el.medicamento.,
            group_Experimental$T1.FC..al.inicio.del.procedimiento.qx.,
            group_Experimental$T2.FC..Al.inicio.del.procedimiento.,
            group_Experimental$T3.FC..60.min.después.,
            group_Experimental$T4.FC..al.termino.del.procedimiento.,
            group_Experimental$T5.FC..30.min.en.la.UCPA.,
            group_Experimental$T6.FC..60.min.en.UCPA.,
            group_Experimental$T7.FC..Al.alta.)
groups_experimental <- rep("experimental", length(values_experimental))
times_experimental <- c(rep("t0", length(group_Experimental$T0..FC..15.o.45.minutos.antes.del.colocar.el.medicamento.)),
           rep("t1", length(group_Experimental$T1.FC..al.inicio.del.procedimiento.qx.)),
           rep("t2", length(group_Experimental$T2.FC..Al.inicio.del.procedimiento.)),
           rep("t3", length(group_Experimental$T3.FC..60.min.después.)),
           rep("t4", length(group_Experimental$T4.FC..al.termino.del.procedimiento.)),
           rep("t5", length(group_Experimental$T5.FC..30.min.en.la.UCPA.)),
           rep("t6", length(group_Experimental$T6.FC..60.min.en.UCPA.)),
           rep("t7", length(group_Experimental$T7.FC..Al.alta.)))

FC_data <- data.frame(c(values_control, values_experimental), c(groups_control, groups_experimental), c(times_control, times_experimental))
FC_data


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- FC_data %>%
  group_by(c.times_control..times_experimental.) %>%
  wilcox_test(c.values_control..values_experimental. ~ c.groups_control..groups_experimental.) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots

bxp <- ggboxplot(
  FC_data, x = "c.times_control..times_experimental.", y = "c.values_control..values_experimental.", 
  color = "c.groups_control..groups_experimental.", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="FC",
       title="Comparación de FC entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=15))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "c.times_control..times_experimental.", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )



## ----echo=FALSE------------------------
# Grouped Lineplots

lp <- ggline(
  FC_data, x = "c.times_control..times_experimental.", y = "c.values_control..values_experimental.", add = "mean_sd", 
  color = "c.groups_control..groups_experimental.", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="FC",
       title="Tendencias de FC entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=15))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "c.times_control..times_experimental.") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )


## ----echo=FALSE------------------------
# Control
control_PAM <- group_control[,17:24]
control_PAM

## ----echo=FALSE------------------------
# Experimental
experimental_PAM <- group_Experimental[,17:24]
experimental_PAM


## ----echo=FALSE------------------------
# Hypothesis Testing DataFrame

# Control Group
values_control <- c(group_control$T0..PAM..15.o.45.minutos.antes.del.colocar.el.medicamento.,
            group_control$T1.PAM...al.inicio.del.procedimiento.qx.,
            group_control$T2.PAM..Al.inicio.del.procedimiento.,
            group_control$T3.PAM..60.min.después.,
            group_control$T4.PAM..al.termino.del.procedimiento.,
            group_control$T5.PAM..30.min.en.la.UCPA.,
            group_control$T6.PAM..60.min.en.UCPA.,
            group_control$T7.PAM..Al.alta.)
groups_control <- rep("control", length(values_control))
times_control <- c(rep("t0", length(group_control$T0..PAM..15.o.45.minutos.antes.del.colocar.el.medicamento.)),
           rep("t1", length(group_control$T1.PAM...al.inicio.del.procedimiento.qx.)),
           rep("t2", length(group_control$T2.PAM..Al.inicio.del.procedimiento.)),
           rep("t3", length(group_control$T3.PAM..60.min.después.)),
           rep("t4", length(group_control$T4.PAM..al.termino.del.procedimiento.)),
           rep("t5", length(group_control$T5.PAM..30.min.en.la.UCPA.)),
           rep("t6", length(group_control$T6.PAM..60.min.en.UCPA.)),
           rep("t7", length(group_control$T7.PAM..Al.alta.)))

# Experimental Group
values_experimental <- c(group_Experimental$T0..PAM..15.o.45.minutos.antes.del.colocar.el.medicamento.,
            group_Experimental$T1.PAM...al.inicio.del.procedimiento.qx.,
            group_Experimental$T2.PAM..Al.inicio.del.procedimiento.,
            group_Experimental$T3.PAM..60.min.después.,
            group_Experimental$T4.PAM..al.termino.del.procedimiento.,
            group_Experimental$T5.PAM..30.min.en.la.UCPA.,
            group_Experimental$T6.PAM..60.min.en.UCPA.,
            group_Experimental$T7.PAM..Al.alta.)
groups_experimental <- rep("experimental", length(values_experimental))
times_experimental <- c(rep("t0", length(group_Experimental$T0..PAM..15.o.45.minutos.antes.del.colocar.el.medicamento.)),
           rep("t1", length(group_Experimental$T1.PAM...al.inicio.del.procedimiento.qx.)),
           rep("t2", length(group_Experimental$T2.PAM..Al.inicio.del.procedimiento.)),
           rep("t3", length(group_Experimental$T3.PAM..60.min.después.)),
           rep("t4", length(group_Experimental$T4.PAM..al.termino.del.procedimiento.)),
           rep("t5", length(group_Experimental$T5.PAM..30.min.en.la.UCPA.)),
           rep("t6", length(group_Experimental$T6.PAM..60.min.en.UCPA.)),
           rep("t7", length(group_Experimental$T7.PAM..Al.alta.)))

PAM_data <- data.frame(c(values_control, values_experimental), c(groups_control, groups_experimental), c(times_control, times_experimental))
PAM_data


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- PAM_data %>%
  group_by(c.times_control..times_experimental.) %>%
  wilcox_test(c.values_control..values_experimental. ~ c.groups_control..groups_experimental.) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test



## ----echo=FALSE------------------------
# Grouped Boxplots

bxp <- ggboxplot(
  PAM_data, x = "c.times_control..times_experimental.", y = "c.values_control..values_experimental.", 
  color = "c.groups_control..groups_experimental.", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="FC",
       title="Comparación de PAM entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=15))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "c.times_control..times_experimental.", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )



## ----echo=FALSE------------------------
# Grouped Lineplots
lp <- ggline(
  PAM_data, x = "c.times_control..times_experimental.", y = "c.values_control..values_experimental.", add = "mean_sd", 
  color = "c.groups_control..groups_experimental.", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="FC",
       title="Tendencias de PAM entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=15))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "c.times_control..times_experimental.") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )


## ----echo=FALSE------------------------
# Control
control_RAS <- group_control[,25:32]
control_RAS

## ----echo=FALSE------------------------
# Experimental
experimental_RAS <- group_Experimental[,25:32]
experimental_RAS


## ----echo=FALSE------------------------
# Hypothesis Testing DataFrame

# Control Group
values_control <- c(group_control$T0..RASS..15.o.45.minutos.antes.del.colocar.el.medicamento.,
            group_control$T1.RASS..al.inicio.del.procedimiento.qx.,
            group_control$T2.RASS..Al.inicio.del.procedimiento.,
            group_control$T3.RASS..60.min.después.,
            group_control$T4.RASS..al.termino.del.procedimiento.,
            group_control$T5.RASS..30.min.en.la.UCPA.,
            group_control$T6.RASS..60.min.en.UCPA.,
            group_control$T7.RASS..Al.alta.)
groups_control <- rep("control", length(values_control))
times_control <- c(rep("t0", length(group_control$T0..RASS..15.o.45.minutos.antes.del.colocar.el.medicamento.)),
           rep("t1", length(group_control$T1.RASS..al.inicio.del.procedimiento.qx.)),
           rep("t2", length(group_control$T2.RASS..Al.inicio.del.procedimiento.)),
           rep("t3", length(group_control$T3.RASS..60.min.después.)),
           rep("t4", length(group_control$T4.RASS..al.termino.del.procedimiento.)),
           rep("t5", length(group_control$T5.RASS..30.min.en.la.UCPA.)),
           rep("t6", length(group_control$T6.RASS..60.min.en.UCPA.)),
           rep("t7", length(group_control$T7.RASS..Al.alta.)))

# Experimental Group
values_experimental <- c(group_Experimental$T0..RASS..15.o.45.minutos.antes.del.colocar.el.medicamento.,
            group_Experimental$T1.RASS..al.inicio.del.procedimiento.qx.,
            group_Experimental$T2.RASS..Al.inicio.del.procedimiento.,
            group_Experimental$T3.RASS..60.min.después.,
            group_Experimental$T4.RASS..al.termino.del.procedimiento.,
            group_Experimental$T5.RASS..30.min.en.la.UCPA.,
            group_Experimental$T6.RASS..60.min.en.UCPA.,
            group_Experimental$T7.RASS..Al.alta.)
groups_experimental <- rep("experimental", length(values_experimental))
times_experimental <- c(rep("t0", length(group_Experimental$T0..RASS..15.o.45.minutos.antes.del.colocar.el.medicamento.)),
           rep("t1", length(group_Experimental$T1.RASS..al.inicio.del.procedimiento.qx.)),
           rep("t2", length(group_Experimental$T2.RASS..Al.inicio.del.procedimiento.)),
           rep("t3", length(group_Experimental$T3.RASS..60.min.después.)),
           rep("t4", length(group_Experimental$T4.RASS..al.termino.del.procedimiento.)),
           rep("t5", length(group_Experimental$T5.RASS..30.min.en.la.UCPA.)),
           rep("t6", length(group_Experimental$T6.RASS..60.min.en.UCPA.)),
           rep("t7", length(group_Experimental$T7.RASS..Al.alta.)))

RAS_data <- data.frame(c(values_control, values_experimental), c(groups_control, groups_experimental), c(times_control, times_experimental))
RAS_data


## ----echo=FALSE------------------------
# Dataframe
RAS_frequencies <- data.frame(table(RAS_data$c.values_control..values_experimental., RAS_data$c.groups_control..groups_experimental., RAS_data$c.times_control..times_experimental.))
RAS_frequencies


## ----echo=FALSE------------------------
# Grouped Bar Plots
ggplot(RAS_frequencies,
       aes(x = Var2,
           y = Freq,
           fill = Var1)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Var3) +
  labs(fill="Nivel RAS",
       x="Tiempo",
       y="Frecuencia",
       title="Frecuencias Agrupadas de RAS por Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Dataframe
RAS_contingency <- table(RAS_data$c.values_control..values_experimental., RAS_data$c.groups_control..groups_experimental., RAS_data$c.times_control..times_experimental.)
RAS_contingency


## ----echo=FALSE------------------------
# t0
chisq_0 <- chisq.test(RAS_contingency[,,1])
chisq_0


## ----echo=FALSE------------------------
# t1
chisq_1 <- chisq.test(RAS_contingency[,,2])
chisq_1


## ----echo=FALSE------------------------
# t2
chisq_2 <- chisq.test(RAS_contingency[,,3])
chisq_2


## ----echo=FALSE------------------------
# t3
chisq_3 <- chisq.test(RAS_contingency[,,4])
chisq_3


## ----echo=FALSE------------------------
# t4
chisq_4 <- chisq.test(RAS_contingency[,,5])
chisq_4


## ----echo=FALSE------------------------
# t5
chisq_5 <- chisq.test(RAS_contingency[,,6])
chisq_5


## ----echo=FALSE------------------------
# t6
chisq_6 <- chisq.test(RAS_contingency[,,7])
chisq_6


## ----echo=FALSE------------------------
# t7
chisq_7 <- chisq.test(RAS_contingency[,,8])
chisq_7

