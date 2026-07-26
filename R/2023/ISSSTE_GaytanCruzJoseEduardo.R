## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("GaytanCruzJoseEduardo_Data.xlsx"))


## ----echo=FALSE------------------------
data


## ----echo=FALSE------------------------
group_control = data[data$GRUPO..0.CONTROL.1.EXPERIMENTAL. == 0,]
group_control

## ----echo=FALSE------------------------
group_Experimental = data[data$GRUPO..0.CONTROL.1.EXPERIMENTAL. == 1,]
group_Experimental 


## ----echo=FALSE------------------------
data_groups <- data.frame(data %>% count(GRUPO..0.CONTROL.1.EXPERIMENTAL., sort = FALSE))
data_groups

## ----echo=FALSE------------------------
ggplot(data_groups, aes(x = "", y = n, fill = factor(GRUPO..0.CONTROL.1.EXPERIMENTAL.))) +
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
ggplot(data, aes(x = EDAD..AÑOS., fill = factor(GRUPO..0.CONTROL.1.EXPERIMENTAL.))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 2,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Edad",
       y="Frequencia",
       title="Histograma de Edad por Grupo") +
  scale_fill_discrete(labels = c("Control", "Experimental")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
summary(group_control$EDAD..AÑOS.)


## ----echo=FALSE------------------------
var(group_control$EDAD..AÑOS.)


## ----echo=FALSE------------------------
sd(group_control$EDAD..AÑOS.)


## ----echo=FALSE------------------------
summary(group_control$FC..T0.EXTUBACION.)


## ----echo=FALSE------------------------
var(group_control$FC..T0.EXTUBACION.)


## ----echo=FALSE------------------------
sd(group_control$FC..T0.EXTUBACION.)


## ----echo=FALSE------------------------
summary(group_control$FC..T1.LLEGAR.A.UCPA.)


## ----echo=FALSE------------------------
var(group_control$FC..T1.LLEGAR.A.UCPA.)


## ----echo=FALSE------------------------
sd(group_control$FC..T1.LLEGAR.A.UCPA.)


## ----echo=FALSE------------------------
summary(group_control$FC..T2.30.MIN.)


## ----echo=FALSE------------------------
var(group_control$FC..T2.30.MIN.)


## ----echo=FALSE------------------------
sd(group_control$FC..T2.30.MIN.)


## ----echo=FALSE------------------------
summary(group_control$FC..T3.60.MIN.)


## ----echo=FALSE------------------------
var(group_control$FC..T3.60.MIN.)


## ----echo=FALSE------------------------
sd(group_control$FC..T3.60.MIN.)


## ----echo=FALSE------------------------
summary(group_control$FC..T4.90.MIN.)


## ----echo=FALSE------------------------
var(group_control$FC..T4.90.MIN.)


## ----echo=FALSE------------------------
sd(group_control$FC..T4.90.MIN.)


## ----echo=FALSE------------------------
summary(group_control$TAM..T0.)


## ----echo=FALSE------------------------
var(group_control$TAM..T0.)


## ----echo=FALSE------------------------
sd(group_control$TAM..T0.)


## ----echo=FALSE------------------------
summary(group_control$TAM..T1.)


## ----echo=FALSE------------------------
var(group_control$TAM..T1.)


## ----echo=FALSE------------------------
sd(group_control$TAM..T1.)


## ----echo=FALSE------------------------
summary(group_control$TAM..T2.)


## ----echo=FALSE------------------------
var(group_control$TAM..T2.)


## ----echo=FALSE------------------------
sd(group_control$TAM..T2.)


## ----echo=FALSE------------------------
summary(group_control$TAM..T3.)


## ----echo=FALSE------------------------
var(group_control$TAM..T3.)


## ----echo=FALSE------------------------
sd(group_control$TAM..T3.)


## ----echo=FALSE------------------------
summary(group_control$TAM..T4.)


## ----echo=FALSE------------------------
var(group_control$TAM..T4.)


## ----echo=FALSE------------------------
sd(group_control$TAM..T4.)


## ----echo=FALSE------------------------
summary(group_control$ENA..0.10..T0)


## ----echo=FALSE------------------------
var(group_control$ENA..0.10..T0)


## ----echo=FALSE------------------------
sd(group_control$ENA..0.10..T0)


## ----echo=FALSE------------------------
summary(group_control$ENA..0.10..T1)


## ----echo=FALSE------------------------
var(group_control$ENA..0.10..T1)


## ----echo=FALSE------------------------
sd(group_control$ENA..0.10..T1)


## ----echo=FALSE------------------------
summary(group_control$ENA..0.10..T2)


## ----echo=FALSE------------------------
var(group_control$ENA..0.10..T2)


## ----echo=FALSE------------------------
sd(group_control$ENA..0.10..T2)


## ----echo=FALSE------------------------
summary(group_control$ENA..0.2..T3)


## ----echo=FALSE------------------------
var(group_control$ENA..0.2..T3)


## ----echo=FALSE------------------------
sd(group_control$ENA..0.2..T3)


## ----echo=FALSE------------------------
summary(group_control$ENA..0.10..T4)


## ----echo=FALSE------------------------
var(group_control$ENA..0.10..T4)


## ----echo=FALSE------------------------
sd(group_control$ENA..0.10..T4)


## ----echo=FALSE------------------------
summary(group_control$ASA..1.2.3.4.5.6.)


## ----echo=FALSE------------------------
var(group_control$ASA..1.2.3.4.5.6.)


## ----echo=FALSE------------------------
sd(group_control$ASA..1.2.3.4.5.6.)


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
summary(group_Experimental$EDAD..AÑOS.)


## ----echo=FALSE------------------------
var(group_Experimental$EDAD..AÑOS.)


## ----echo=FALSE------------------------
sd(group_Experimental$EDAD..AÑOS.)


## ----echo=FALSE------------------------
summary(group_Experimental$FC..T0.EXTUBACION.)


## ----echo=FALSE------------------------
var(group_Experimental$FC..T0.EXTUBACION.)


## ----echo=FALSE------------------------
sd(group_Experimental$FC..T0.EXTUBACION.)


## ----echo=FALSE------------------------
summary(group_Experimental$FC..T1.LLEGAR.A.UCPA.)


## ----echo=FALSE------------------------
var(group_Experimental$FC..T1.LLEGAR.A.UCPA.)


## ----echo=FALSE------------------------
sd(group_Experimental$FC..T1.LLEGAR.A.UCPA.)


## ----echo=FALSE------------------------
summary(group_Experimental$FC..T2.30.MIN.)


## ----echo=FALSE------------------------
var(group_Experimental$FC..T2.30.MIN.)


## ----echo=FALSE------------------------
sd(group_Experimental$FC..T2.30.MIN.)


## ----echo=FALSE------------------------
summary(group_Experimental$FC..T3.60.MIN.)


## ----echo=FALSE------------------------
var(group_Experimental$FC..T3.60.MIN.)


## ----echo=FALSE------------------------
sd(group_Experimental$FC..T3.60.MIN.)


## ----echo=FALSE------------------------
summary(group_Experimental$FC..T4.90.MIN.)


## ----echo=FALSE------------------------
var(group_Experimental$FC..T4.90.MIN.)


## ----echo=FALSE------------------------
sd(group_Experimental$FC..T4.90.MIN.)


## ----echo=FALSE------------------------
summary(group_Experimental$TAM..T0.)


## ----echo=FALSE------------------------
var(group_Experimental$TAM..T0.)


## ----echo=FALSE------------------------
sd(group_Experimental$TAM..T0.)


## ----echo=FALSE------------------------
summary(group_Experimental$TAM..T1.)


## ----echo=FALSE------------------------
var(group_Experimental$TAM..T1.)


## ----echo=FALSE------------------------
sd(group_Experimental$TAM..T1.)


## ----echo=FALSE------------------------
summary(group_Experimental$TAM..T2.)


## ----echo=FALSE------------------------
var(group_Experimental$TAM..T2.)


## ----echo=FALSE------------------------
sd(group_Experimental$TAM..T2.)


## ----echo=FALSE------------------------
summary(group_Experimental$TAM..T3.)


## ----echo=FALSE------------------------
var(group_Experimental$TAM..T3.)


## ----echo=FALSE------------------------
sd(group_Experimental$TAM..T3.)


## ----echo=FALSE------------------------
summary(group_Experimental$TAM..T4.)


## ----echo=FALSE------------------------
var(group_Experimental$TAM..T4.)


## ----echo=FALSE------------------------
sd(group_Experimental$TAM..T4.)


## ----echo=FALSE------------------------
summary(group_Experimental$ENA..0.10..T0)


## ----echo=FALSE------------------------
var(group_Experimental$ENA..0.10..T0)


## ----echo=FALSE------------------------
sd(group_Experimental$ENA..0.10..T0)


## ----echo=FALSE------------------------
summary(group_Experimental$ENA..0.10..T1)


## ----echo=FALSE------------------------
var(group_Experimental$ENA..0.10..T1)


## ----echo=FALSE------------------------
sd(group_Experimental$ENA..0.10..T1)


## ----echo=FALSE------------------------
summary(group_Experimental$ENA..0.10..T2)


## ----echo=FALSE------------------------
var(group_Experimental$ENA..0.10..T2)


## ----echo=FALSE------------------------
sd(group_Experimental$ENA..0.10..T2)


## ----echo=FALSE------------------------
summary(group_Experimental$ENA..0.2..T3)


## ----echo=FALSE------------------------
var(group_Experimental$ENA..0.2..T3)


## ----echo=FALSE------------------------
sd(group_Experimental$ENA..0.2..T3)


## ----echo=FALSE------------------------
summary(group_Experimental$ENA..0.10..T4)


## ----echo=FALSE------------------------
var(group_Experimental$ENA..0.10..T4)


## ----echo=FALSE------------------------
sd(group_Experimental$ENA..0.10..T4)


## ----echo=FALSE------------------------
summary(group_Experimental$ASA..1.2.3.4.5.6.)


## ----echo=FALSE------------------------
var(group_Experimental$ASA..1.2.3.4.5.6.)


## ----echo=FALSE------------------------
sd(group_Experimental$ASA..1.2.3.4.5.6.)


## ----echo=FALSE------------------------
# Histogram
ggplot(group_Experimental, aes(x = factor(ASA..1.2.3.4.5.6.), fill = factor(ASA..1.2.3.4.5.6.))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Control") +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
FC_data <- data.frame(
  c(rep("control", 5*length(group_control$GRUPO..0.CONTROL.1.EXPERIMENTAL.)), rep("experimental", 5*length(group_control$GRUPO..0.CONTROL.1.EXPERIMENTAL.))),
  
  c(group_control$FC..T0.EXTUBACION., group_control$FC..T1.LLEGAR.A.UCPA., group_control$FC..T2.30.MIN., group_control$FC..T3.60.MIN., group_control$FC..T4.90.MIN., group_Experimental$FC..T0.EXTUBACION., group_Experimental$FC..T1.LLEGAR.A.UCPA., group_Experimental$FC..T2.30.MIN., group_Experimental$FC..T3.60.MIN., group_Experimental$FC..T4.90.MIN.),
  
  c(rep("t0", length(group_Experimental$FC..T0.EXTUBACION.)), rep("t1", length(group_Experimental$FC..T1.LLEGAR.A.UCPA.)), rep("t2", length(group_Experimental$FC..T2.30.MIN.)), rep("t3", length(group_Experimental$FC..T3.60.MIN.)), rep("t4", length(group_Experimental$FC..T4.90.MIN.)))
)
colnames(FC_data) <- c("Grupo", "Valor", "Tiempo")
FC_data


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- FC_data %>%
  group_by(Tiempo) %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test



## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  FC_data, x = "Tiempo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="Frecuencia Cardíaca",
       title="Comparación de Frecuencia Cardíaca entre grupos a través del tiempo") +
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
  FC_data, x = "Tiempo", y = "Valor", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="Frecuencia Cardíaca",
       title="Tendencias de Frecuencia Cardíaca entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=10))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Tiempo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )


## ----echo=FALSE------------------------
TAM_data <- data.frame(
  c(rep("control", 5*length(group_control$GRUPO..0.CONTROL.1.EXPERIMENTAL.)), rep("experimental", 5*length(group_control$GRUPO..0.CONTROL.1.EXPERIMENTAL.))),
  
  c(group_control$TAM..T0., group_control$TAM..T1., group_control$TAM..T2., group_control$TAM..T3., group_control$TAM..T4., group_Experimental$TAM..T0., group_Experimental$TAM..T1., group_Experimental$TAM..T2., group_Experimental$TAM..T3., group_Experimental$TAM..T4.),
  
  c(rep("t0", length(group_Experimental$TAM..T0.)), rep("t1", length(group_Experimental$TAM..T1.)), rep("t2", length(group_Experimental$TAM..T2.)), rep("t3", length(group_Experimental$TAM..T3.)), rep("t4", length(group_Experimental$TAM..T4.)))
)
colnames(TAM_data) <- c("Grupo", "Valor", "Tiempo")
TAM_data


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- TAM_data %>%
  group_by(Tiempo) %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test



## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  TAM_data, x = "Tiempo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="TAM",
       title="Comparación de TAM entre grupos a través del tiempo") +
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
  FC_data, x = "Tiempo", y = "Valor", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="TAM",
       title="Tendencias de TAM entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=10))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Tiempo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )


## ----echo=FALSE------------------------
ENA_data <- data.frame(
  c(rep("control", 5*length(group_control$GRUPO..0.CONTROL.1.EXPERIMENTAL.)), rep("experimental", 5*length(group_control$GRUPO..0.CONTROL.1.EXPERIMENTAL.))),
  
  c(group_control$ENA..0.10..T0, group_control$ENA..0.10..T1, group_control$ENA..0.10..T2, group_control$ENA..0.2..T3, group_control$ENA..0.10..T4, group_Experimental$ENA..0.10..T0, group_Experimental$ENA..0.10..T1, group_Experimental$ENA..0.10..T2, group_Experimental$ENA..0.2..T3, group_Experimental$ENA..0.10..T4),
  
  c(rep("t0", length(group_Experimental$ENA..0.10..T0)), rep("t1", length(group_Experimental$ENA..0.10..T1)), rep("t2", length(group_Experimental$ENA..0.10..T2)), rep("t3", length(group_Experimental$ENA..0.2..T3)), rep("t4", length(group_Experimental$ENA..0.10..T4)))
)
colnames(ENA_data) <- c("Grupo", "Valor", "Tiempo")
ENA_data


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- ENA_data %>%
  group_by(Tiempo) %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test



## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  ENA_data, x = "Tiempo", y = "Valor", 
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
  ENA_data, x = "Tiempo", y = "Valor", add = "mean_sd", 
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

