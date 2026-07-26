## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("DiegoArmando_data.xlsx"))


## ----echo=FALSE------------------------
data


## ----echo=FALSE------------------------
group_control = data[data$GRUPO..SIN.MUSICOTERAPIA.0..CON.MUSICOTERAPIA..1 == 0,]
group_control


## ----echo=FALSE------------------------
group_Experimental = data[data$GRUPO..SIN.MUSICOTERAPIA.0..CON.MUSICOTERAPIA..1 == 1,]
group_Experimental 


## ----echo=FALSE------------------------
data_groups <- data.frame(data %>% count(GRUPO..SIN.MUSICOTERAPIA.0..CON.MUSICOTERAPIA..1, sort = FALSE))
data_groups

## ----echo=FALSE------------------------
ggplot(data_groups, aes(x = "", y = n, fill = factor(GRUPO..SIN.MUSICOTERAPIA.0..CON.MUSICOTERAPIA..1))) +
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
ggplot(data, aes(x = EDAD..AÑOS., fill = factor(GRUPO..SIN.MUSICOTERAPIA.0..CON.MUSICOTERAPIA..1))) +
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
tipo_anest <- data.frame(data %>% count(TIPO.DE.ANESTESIA..SUBARACNOIDEA...0..PERIDURAL...1..MIXTO...2, sort = FALSE))
tipo_anest


## ----echo=FALSE------------------------
ggplot(tipo_anest, aes(x = "", y = n, fill = factor(TIPO.DE.ANESTESIA..SUBARACNOIDEA...0..PERIDURAL...1..MIXTO...2))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Tipo de Anestesia",
       x=NULL,
       y=NULL,
       title="Proporciones de Tipo de Anestesia en total") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Mixto")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(data$EDAD..AÑOS.)


## ----echo=FALSE------------------------
# Variance
var(data$EDAD..AÑOS.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(data$EDAD..AÑOS.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(data$TensArtmmHG_T0,
          data$TensArtmmHG_T1,
          data$TensArtmmHG_T2,
          data$TensArtmmHG_T3,
          data$TensArtmmHG_T4,
          data$TensArtmmHG_T5,
          data$TensArtmmHG_T6,
          data$TensArtmmHG_T7,
          data$TensArtmmHG_T8))


## ----echo=FALSE------------------------
# Variance
var(c(data$TensArtmmHG_T0,
          data$TensArtmmHG_T1,
          data$TensArtmmHG_T2,
          data$TensArtmmHG_T3,
          data$TensArtmmHG_T4,
          data$TensArtmmHG_T5,
          data$TensArtmmHG_T6,
          data$TensArtmmHG_T7,
          data$TensArtmmHG_T8))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(data$TensArtmmHG_T0,
          data$TensArtmmHG_T1,
          data$TensArtmmHG_T2,
          data$TensArtmmHG_T3,
          data$TensArtmmHG_T4,
          data$TensArtmmHG_T5,
          data$TensArtmmHG_T6,
          data$TensArtmmHG_T7,
          data$TensArtmmHG_T8))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(data$FrecCard_T0_lat.min,
        data$FrecCard_T1_lat.min,
        data$FrecCard_T2_lat.min,
        data$FrecCard_T3_lat.min,
        data$FrecCard_T4_lat.min,
        data$FrecCard_T5_lat.min,
        data$FrecCard_T6_lat.min,
        data$FrecCard_T7_lat.min,
        data$FrecCard_T8_lat.min))


## ----echo=FALSE------------------------
# Variance
var(c(data$FrecCard_T0_lat.min,
        data$FrecCard_T1_lat.min,
        data$FrecCard_T2_lat.min,
        data$FrecCard_T3_lat.min,
        data$FrecCard_T4_lat.min,
        data$FrecCard_T5_lat.min,
        data$FrecCard_T6_lat.min,
        data$FrecCard_T7_lat.min,
        data$FrecCard_T8_lat.min))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(data$FrecCard_T0_lat.min,
        data$FrecCard_T1_lat.min,
        data$FrecCard_T2_lat.min,
        data$FrecCard_T3_lat.min,
        data$FrecCard_T4_lat.min,
        data$FrecCard_T5_lat.min,
        data$FrecCard_T6_lat.min,
        data$FrecCard_T7_lat.min,
        data$FrecCard_T8_lat.min))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(data$FrecResp_T0_lat.min,
        data$FrecResp_T1_lat.min,
        data$FrecResp_T2_lat.min,
        data$FrecResp_T3_lat.min,
        data$FrecResp_T4_lat.min,
        data$FrecResp_T5_lat.min,
        data$FrecResp_T6_lat.min,
        data$FrecResp_T7_lat.min,
        data$FrecResp_T8_lat.min))


## ----echo=FALSE------------------------
# Variance
var(c(data$FrecResp_T0_lat.min,
        data$FrecResp_T1_lat.min,
        data$FrecResp_T2_lat.min,
        data$FrecResp_T3_lat.min,
        data$FrecResp_T4_lat.min,
        data$FrecResp_T5_lat.min,
        data$FrecResp_T6_lat.min,
        data$FrecResp_T7_lat.min,
        data$FrecResp_T8_lat.min))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(data$FrecResp_T0_lat.min,
        data$FrecResp_T1_lat.min,
        data$FrecResp_T2_lat.min,
        data$FrecResp_T3_lat.min,
        data$FrecResp_T4_lat.min,
        data$FrecResp_T5_lat.min,
        data$FrecResp_T6_lat.min,
        data$FrecResp_T7_lat.min,
        data$FrecResp_T8_lat.min))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(data$Spo2_T0,
        data$Spo2_T1,
        data$Spo2_T2,
        data$Spo2_T3,
        data$Spo2_T4,
        data$Spo2_T5,
        data$Spo2_T6,
        data$Spo2_T7,
        data$Spo2_T8))


## ----echo=FALSE------------------------
# Variance
var(c(data$Spo2_T0,
        data$Spo2_T1,
        data$Spo2_T2,
        data$Spo2_T3,
        data$Spo2_T4,
        data$Spo2_T5,
        data$Spo2_T6,
        data$Spo2_T7,
        data$Spo2_T8))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(data$Spo2_T0,
        data$Spo2_T1,
        data$Spo2_T2,
        data$Spo2_T3,
        data$Spo2_T4,
        data$Spo2_T5,
        data$Spo2_T6,
        data$Spo2_T7,
        data$Spo2_T8))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(data$STAI_T0_A.E, data$STAI_T8_A.E))


## ----echo=FALSE------------------------
# Variance
var(c(data$STAI_T0_A.E, data$STAI_T8_A.E))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(data$STAI_T0_A.E, data$STAI_T8_A.E))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(data$STAI_T0_A.R, data$STAI_T8_A.R))


## ----echo=FALSE------------------------
# Variance
var(c(data$STAI_T0_A.R, data$STAI_T8_A.R))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(data$STAI_T0_A.R, data$STAI_T8_A.R))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(data$Ramsay_TO, data$Ramsay_T8))


## ----echo=FALSE------------------------
# Variance
var(c(data$Ramsay_TO, data$Ramsay_T8))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(data$Ramsay_TO, data$Ramsay_T8))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(data$ENA_TO, data$ENA_T8))


## ----echo=FALSE------------------------
# Variance
var(c(data$ENA_TO, data$ENA_T8))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(data$ENA_TO, data$ENA_T8))


## ----echo=FALSE------------------------
gender_control <- data.frame(group_control %>% count(GENERO..0...FEMENINO..1..MASCULINO, sort = FALSE))
gender_control


## ----echo=FALSE------------------------
ggplot(gender_control, aes(x = "", y = n, fill = factor(GENERO..0...FEMENINO..1..MASCULINO))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género del Grupo Control") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Femenino", "Masculino")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Histogram
ggplot(group_control, aes(x = STAI_T0_A.E, fill = factor(GENERO..0...FEMENINO..1..MASCULINO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 5,
                 alpha = 0.5) +
  labs(fill = "Género",
       x="STAI T0",
       y="Frequencia",
       title="Histograma de STAI A/E T0 por Género - Control") +
  scale_fill_discrete(labels = c("Femenino", "Masculino")) +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
# Histogram
ggplot(group_control, aes(x = STAI_T8_A.E, fill = factor(GENERO..0...FEMENINO..1..MASCULINO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 5,
                 alpha = 0.5) +
  labs(fill = "Género",
       x="STAI T8",
       y="Frequencia",
       title="Histograma de STAI A/E T8 por Género - Control") +
  scale_fill_discrete(labels = c("Femenino", "Masculino")) +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$EDAD..AÑOS.)


## ----echo=FALSE------------------------
# Variance
var(group_control$EDAD..AÑOS.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$EDAD..AÑOS.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_control$TensArtmmHG_T0,
          group_control$TensArtmmHG_T1,
          group_control$TensArtmmHG_T2,
          group_control$TensArtmmHG_T3,
          group_control$TensArtmmHG_T4,
          group_control$TensArtmmHG_T5,
          group_control$TensArtmmHG_T6,
          group_control$TensArtmmHG_T7,
          group_control$TensArtmmHG_T8))


## ----echo=FALSE------------------------
# Variance
var(c(group_control$TensArtmmHG_T0,
          group_control$TensArtmmHG_T1,
          group_control$TensArtmmHG_T2,
          group_control$TensArtmmHG_T3,
          group_control$TensArtmmHG_T4,
          group_control$TensArtmmHG_T5,
          group_control$TensArtmmHG_T6,
          group_control$TensArtmmHG_T7,
          group_control$TensArtmmHG_T8))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_control$TensArtmmHG_T0,
          group_control$TensArtmmHG_T1,
          group_control$TensArtmmHG_T2,
          group_control$TensArtmmHG_T3,
          group_control$TensArtmmHG_T4,
          group_control$TensArtmmHG_T5,
          group_control$TensArtmmHG_T6,
          group_control$TensArtmmHG_T7,
          group_control$TensArtmmHG_T8))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_control$FrecCard_T0_lat.min,
        group_control$FrecCard_T1_lat.min,
        group_control$FrecCard_T2_lat.min,
        group_control$FrecCard_T3_lat.min,
        group_control$FrecCard_T4_lat.min,
        group_control$FrecCard_T5_lat.min,
        group_control$FrecCard_T6_lat.min,
        group_control$FrecCard_T7_lat.min,
        group_control$FrecCard_T8_lat.min))


## ----echo=FALSE------------------------
# Variance
var(c(group_control$FrecCard_T0_lat.min,
        group_control$FrecCard_T1_lat.min,
        group_control$FrecCard_T2_lat.min,
        group_control$FrecCard_T3_lat.min,
        group_control$FrecCard_T4_lat.min,
        group_control$FrecCard_T5_lat.min,
        group_control$FrecCard_T6_lat.min,
        group_control$FrecCard_T7_lat.min,
        group_control$FrecCard_T8_lat.min))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_control$FrecCard_T0_lat.min,
        group_control$FrecCard_T1_lat.min,
        group_control$FrecCard_T2_lat.min,
        group_control$FrecCard_T3_lat.min,
        group_control$FrecCard_T4_lat.min,
        group_control$FrecCard_T5_lat.min,
        group_control$FrecCard_T6_lat.min,
        group_control$FrecCard_T7_lat.min,
        group_control$FrecCard_T8_lat.min))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_control$FrecResp_T0_lat.min,
        group_control$FrecResp_T1_lat.min,
        group_control$FrecResp_T2_lat.min,
        group_control$FrecResp_T3_lat.min,
        group_control$FrecResp_T4_lat.min,
        group_control$FrecResp_T5_lat.min,
        group_control$FrecResp_T6_lat.min,
        group_control$FrecResp_T7_lat.min,
        group_control$FrecResp_T8_lat.min))


## ----echo=FALSE------------------------
# Variance
var(c(group_control$FrecResp_T0_lat.min,
        group_control$FrecResp_T1_lat.min,
        group_control$FrecResp_T2_lat.min,
        group_control$FrecResp_T3_lat.min,
        group_control$FrecResp_T4_lat.min,
        group_control$FrecResp_T5_lat.min,
        group_control$FrecResp_T6_lat.min,
        group_control$FrecResp_T7_lat.min,
        group_control$FrecResp_T8_lat.min))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_control$FrecResp_T0_lat.min,
        group_control$FrecResp_T1_lat.min,
        group_control$FrecResp_T2_lat.min,
        group_control$FrecResp_T3_lat.min,
        group_control$FrecResp_T4_lat.min,
        group_control$FrecResp_T5_lat.min,
        group_control$FrecResp_T6_lat.min,
        group_control$FrecResp_T7_lat.min,
        group_control$FrecResp_T8_lat.min))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_control$Spo2_T0,
        group_control$Spo2_T1,
        group_control$Spo2_T2,
        group_control$Spo2_T3,
        group_control$Spo2_T4,
        group_control$Spo2_T5,
        group_control$Spo2_T6,
        group_control$Spo2_T7,
        group_control$Spo2_T8))


## ----echo=FALSE------------------------
# Variance
var(c(group_control$Spo2_T0,
        group_control$Spo2_T1,
        group_control$Spo2_T2,
        group_control$Spo2_T3,
        group_control$Spo2_T4,
        group_control$Spo2_T5,
        group_control$Spo2_T6,
        group_control$Spo2_T7,
        group_control$Spo2_T8))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_control$Spo2_T0,
        group_control$Spo2_T1,
        group_control$Spo2_T2,
        group_control$Spo2_T3,
        group_control$Spo2_T4,
        group_control$Spo2_T5,
        group_control$Spo2_T6,
        group_control$Spo2_T7,
        group_control$Spo2_T8))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_control$STAI_T0_A.E, group_control$STAI_T8_A.E))


## ----echo=FALSE------------------------
# Variance
var(c(group_control$STAI_T0_A.E, group_control$STAI_T8_A.E))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_control$STAI_T0_A.E, group_control$STAI_T8_A.E))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_control$STAI_T0_A.R, group_control$STAI_T8_A.R))


## ----echo=FALSE------------------------
# Variance
var(c(group_control$STAI_T0_A.R, group_control$STAI_T8_A.R))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_control$STAI_T0_A.R, group_control$STAI_T8_A.R))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_control$Ramsay_TO, group_control$Ramsay_T8))


## ----echo=FALSE------------------------
# Variance
var(c(group_control$Ramsay_TO, group_control$Ramsay_T8))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_control$Ramsay_TO, group_control$Ramsay_T8))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_control$ENA_TO, group_control$ENA_T8))


## ----echo=FALSE------------------------
# Variance
var(c(group_control$ENA_TO, group_control$ENA_T8))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_control$ENA_TO, group_control$ENA_T8))


## ----echo=FALSE------------------------
# Histogram
ggplot(group_control, aes(x = factor(ASA..1..ASA.1..2..ASA.2.), fill = factor(ASA..1..ASA.1..2..ASA.2.))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Control") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
gender_Experimental <- data.frame(group_Experimental %>% count(GENERO..0...FEMENINO..1..MASCULINO, sort = FALSE))
gender_Experimental


## ----echo=FALSE------------------------
ggplot(gender_Experimental, aes(x = "", y = n, fill = factor(GENERO..0...FEMENINO..1..MASCULINO))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género del Grupo Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Femenino", "Masculino")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Histogram
ggplot(group_Experimental, aes(x = STAI_T0_A.E, fill = factor(GENERO..0...FEMENINO..1..MASCULINO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 5,
                 alpha = 0.5) +
  labs(fill = "Género",
       x="STAI T0",
       y="Frequencia",
       title="Histograma de STAI A/E T0 por Género - Experimental") +
  scale_fill_discrete(labels = c("Femenino", "Masculino")) +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
# Histogram
ggplot(group_Experimental, aes(x = STAI_T8_A.E, fill = factor(GENERO..0...FEMENINO..1..MASCULINO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 5,
                 alpha = 0.5) +
  labs(fill = "Género",
       x="STAI T8",
       y="Frequencia",
       title="Histograma de STAI A/E T8 por Género - Experimental") +
  scale_fill_discrete(labels = c("Femenino", "Masculino")) +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$EDAD..AÑOS.)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$EDAD..AÑOS.)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$EDAD..AÑOS.)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_Experimental$TensArtmmHG_T0,
          group_Experimental$TensArtmmHG_T1,
          group_Experimental$TensArtmmHG_T2,
          group_Experimental$TensArtmmHG_T3,
          group_Experimental$TensArtmmHG_T4,
          group_Experimental$TensArtmmHG_T5,
          group_Experimental$TensArtmmHG_T6,
          group_Experimental$TensArtmmHG_T7,
          group_Experimental$TensArtmmHG_T8))


## ----echo=FALSE------------------------
# Variance
var(c(group_Experimental$TensArtmmHG_T0,
          group_Experimental$TensArtmmHG_T1,
          group_Experimental$TensArtmmHG_T2,
          group_Experimental$TensArtmmHG_T3,
          group_Experimental$TensArtmmHG_T4,
          group_Experimental$TensArtmmHG_T5,
          group_Experimental$TensArtmmHG_T6,
          group_Experimental$TensArtmmHG_T7,
          group_Experimental$TensArtmmHG_T8))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_Experimental$TensArtmmHG_T0,
          group_Experimental$TensArtmmHG_T1,
          group_Experimental$TensArtmmHG_T2,
          group_Experimental$TensArtmmHG_T3,
          group_Experimental$TensArtmmHG_T4,
          group_Experimental$TensArtmmHG_T5,
          group_Experimental$TensArtmmHG_T6,
          group_Experimental$TensArtmmHG_T7,
          group_Experimental$TensArtmmHG_T8))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_Experimental$FrecCard_T0_lat.min,
        group_Experimental$FrecCard_T1_lat.min,
        group_Experimental$FrecCard_T2_lat.min,
        group_Experimental$FrecCard_T3_lat.min,
        group_Experimental$FrecCard_T4_lat.min,
        group_Experimental$FrecCard_T5_lat.min,
        group_Experimental$FrecCard_T6_lat.min,
        group_Experimental$FrecCard_T7_lat.min,
        group_Experimental$FrecCard_T8_lat.min))


## ----echo=FALSE------------------------
# Variance
var(c(group_Experimental$FrecCard_T0_lat.min,
        group_Experimental$FrecCard_T1_lat.min,
        group_Experimental$FrecCard_T2_lat.min,
        group_Experimental$FrecCard_T3_lat.min,
        group_Experimental$FrecCard_T4_lat.min,
        group_Experimental$FrecCard_T5_lat.min,
        group_Experimental$FrecCard_T6_lat.min,
        group_Experimental$FrecCard_T7_lat.min,
        group_Experimental$FrecCard_T8_lat.min))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_Experimental$FrecCard_T0_lat.min,
        group_Experimental$FrecCard_T1_lat.min,
        group_Experimental$FrecCard_T2_lat.min,
        group_Experimental$FrecCard_T3_lat.min,
        group_Experimental$FrecCard_T4_lat.min,
        group_Experimental$FrecCard_T5_lat.min,
        group_Experimental$FrecCard_T6_lat.min,
        group_Experimental$FrecCard_T7_lat.min,
        group_Experimental$FrecCard_T8_lat.min))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_Experimental$FrecResp_T0_lat.min,
        group_Experimental$FrecResp_T1_lat.min,
        group_Experimental$FrecResp_T2_lat.min,
        group_Experimental$FrecResp_T3_lat.min,
        group_Experimental$FrecResp_T4_lat.min,
        group_Experimental$FrecResp_T5_lat.min,
        group_Experimental$FrecResp_T6_lat.min,
        group_Experimental$FrecResp_T7_lat.min,
        group_Experimental$FrecResp_T8_lat.min))


## ----echo=FALSE------------------------
# Variance
var(c(group_Experimental$FrecResp_T0_lat.min,
        group_Experimental$FrecResp_T1_lat.min,
        group_Experimental$FrecResp_T2_lat.min,
        group_Experimental$FrecResp_T3_lat.min,
        group_Experimental$FrecResp_T4_lat.min,
        group_Experimental$FrecResp_T5_lat.min,
        group_Experimental$FrecResp_T6_lat.min,
        group_Experimental$FrecResp_T7_lat.min,
        group_Experimental$FrecResp_T8_lat.min))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_Experimental$FrecResp_T0_lat.min,
        group_Experimental$FrecResp_T1_lat.min,
        group_Experimental$FrecResp_T2_lat.min,
        group_Experimental$FrecResp_T3_lat.min,
        group_Experimental$FrecResp_T4_lat.min,
        group_Experimental$FrecResp_T5_lat.min,
        group_Experimental$FrecResp_T6_lat.min,
        group_Experimental$FrecResp_T7_lat.min,
        group_Experimental$FrecResp_T8_lat.min))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_Experimental$Spo2_T0,
        group_Experimental$Spo2_T1,
        group_Experimental$Spo2_T2,
        group_Experimental$Spo2_T3,
        group_Experimental$Spo2_T4,
        group_Experimental$Spo2_T5,
        group_Experimental$Spo2_T6,
        group_Experimental$Spo2_T7,
        group_Experimental$Spo2_T8))


## ----echo=FALSE------------------------
# Variance
var(c(group_Experimental$Spo2_T0,
        group_Experimental$Spo2_T1,
        group_Experimental$Spo2_T2,
        group_Experimental$Spo2_T3,
        group_Experimental$Spo2_T4,
        group_Experimental$Spo2_T5,
        group_Experimental$Spo2_T6,
        group_Experimental$Spo2_T7,
        group_Experimental$Spo2_T8))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_Experimental$Spo2_T0,
        group_Experimental$Spo2_T1,
        group_Experimental$Spo2_T2,
        group_Experimental$Spo2_T3,
        group_Experimental$Spo2_T4,
        group_Experimental$Spo2_T5,
        group_Experimental$Spo2_T6,
        group_Experimental$Spo2_T7,
        group_Experimental$Spo2_T8))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_Experimental$STAI_T0_A.E, group_Experimental$STAI_T8_A.E))


## ----echo=FALSE------------------------
# Variance
var(c(group_Experimental$STAI_T0_A.E, group_Experimental$STAI_T8_A.E))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_Experimental$STAI_T0_A.E, group_Experimental$STAI_T8_A.E))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_Experimental$STAI_T0_A.R, group_Experimental$STAI_T8_A.R))


## ----echo=FALSE------------------------
# Variance
var(c(group_Experimental$STAI_T0_A.R, group_Experimental$STAI_T8_A.R))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_Experimental$STAI_T0_A.R, group_Experimental$STAI_T8_A.R))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_Experimental$Ramsay_TO, group_Experimental$Ramsay_T8))


## ----echo=FALSE------------------------
# Variance
var(c(group_Experimental$Ramsay_TO, group_Experimental$Ramsay_T8))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_Experimental$Ramsay_TO, group_Experimental$Ramsay_T8))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(c(group_Experimental$ENA_TO, group_Experimental$ENA_T8))


## ----echo=FALSE------------------------
# Variance
var(c(group_Experimental$ENA_TO, group_Experimental$ENA_T8))


## ----echo=FALSE------------------------
# Standar Deviation
sd(c(group_Experimental$ENA_TO, group_Experimental$ENA_T8))


## ----echo=FALSE------------------------
# Histogram
ggplot(group_Experimental, aes(x = factor(ASA..1..ASA.1..2..ASA.2.), fill = factor(ASA..1..ASA.1..2..ASA.2.))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Experimental") +
  theme(plot.title = element_text(size=12))



## ----echo=FALSE------------------------
# Hypothesis Testing DataFrame
TensArt_data <- data.frame(
  c(rep("control", 9*length(group_control$TensArtmmHG_T0)), rep("experimental", 9*length(group_Experimental$TensArtmmHG_T0))),
  c(group_control$TensArtmmHG_T0,
    group_control$TensArtmmHG_T1,
    group_control$TensArtmmHG_T2,
    group_control$TensArtmmHG_T3,
    group_control$TensArtmmHG_T4,
    group_control$TensArtmmHG_T5,
    group_control$TensArtmmHG_T6,
    group_control$TensArtmmHG_T7,
    group_control$TensArtmmHG_T8,
    group_Experimental$TensArtmmHG_T0,
    group_Experimental$TensArtmmHG_T1,
    group_Experimental$TensArtmmHG_T2,
    group_Experimental$TensArtmmHG_T3,
    group_Experimental$TensArtmmHG_T4,
    group_Experimental$TensArtmmHG_T5,
    group_Experimental$TensArtmmHG_T6,
    group_Experimental$TensArtmmHG_T7,
    group_Experimental$TensArtmmHG_T8),
  c(rep(c("T0", "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8"), times=2, each=length(group_control$TensArtmmHG_T0)))
)

colnames(TensArt_data) <- c("Grupo", "Valor", "Tiempo")
TensArt_data


## ----echo=FALSE------------------------
# Statistical Test
stat.test <- TensArt_data %>%
  group_by(Tiempo) %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  TensArt_data, x = "Tiempo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="Tensión Arterial (mmHg)",
       title="Comparación de Tensión Arterial (mmHg) entre Grupos a través del Tiempo") +
  theme(plot.title = element_text(size=8))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Tiempo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
# Grouped Lineplots
lp <- ggline(
  TensArt_data, x = "Tiempo", y = "Valor", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="Tensión Arterial (mmHg)",
       title="Tendencias de Tensión Arterial (mmHg) entre Grupos a través del Tiempo") +
  theme(plot.title = element_text(size=08))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Tiempo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )


## ----echo=FALSE------------------------
# Hypothesis Testing DataFrame
FrecCard_data <- data.frame(
  c(rep("control", 9*length(group_control$FrecCard_T0_lat.min)), rep("experimental", 9*length(group_Experimental$FrecCard_T0_lat.min))),
  c(group_control$FrecCard_T0_lat.min,
    group_control$FrecCard_T1_lat.min,
    group_control$FrecCard_T2_lat.min,
    group_control$FrecCard_T3_lat.min,
    group_control$FrecCard_T4_lat.min,
    group_control$FrecCard_T5_lat.min,
    group_control$FrecCard_T6_lat.min,
    group_control$FrecCard_T7_lat.min,
    group_control$FrecCard_T8_lat.min,
    group_Experimental$FrecCard_T0_lat.min,
    group_Experimental$FrecCard_T1_lat.min,
    group_Experimental$FrecCard_T2_lat.min,
    group_Experimental$FrecCard_T3_lat.min,
    group_Experimental$FrecCard_T4_lat.min,
    group_Experimental$FrecCard_T5_lat.min,
    group_Experimental$FrecCard_T6_lat.min,
    group_Experimental$FrecCard_T7_lat.min,
    group_Experimental$FrecCard_T8_lat.min),
  c(rep(c("T0", "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8"), times=2, each=length(group_control$FrecCard_T0_lat.min)))
)

colnames(FrecCard_data) <- c("Grupo", "Valor", "Tiempo")
FrecCard_data


## ----echo=FALSE------------------------
# Statistical Test
stat.test <- FrecCard_data %>%
  group_by(Tiempo) %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  FrecCard_data, x = "Tiempo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="Frecuencia Cardíaca (lats/min)",
       title="Comparación de Frecuencia Cardíaca entre Grupos a través del Tiempo") +
  theme(plot.title = element_text(size=08))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Tiempo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
# Grouped Lineplots
lp <- ggline(
  FrecCard_data, x = "Tiempo", y = "Valor", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="Frecuencia Cardíaca (lats/min)",
       title="Tendencias de Frecuencia Cardíaca (lats/min) entre Grupos a través del Tiempo") +
  theme(plot.title = element_text(size=06))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Tiempo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )


## ----echo=FALSE------------------------
# Hypothesis Testing DataFrame
FrecResp_data <- data.frame(
  c(rep("control", 9*length(group_control$FrecResp_T0_lat.min)), rep("experimental", 9*length(group_Experimental$FrecResp_T0_lat.min))),
  c(group_control$FrecResp_T0_lat.min,
    group_control$FrecResp_T1_lat.min,
    group_control$FrecResp_T2_lat.min,
    group_control$FrecResp_T3_lat.min,
    group_control$FrecResp_T4_lat.min,
    group_control$FrecResp_T5_lat.min,
    group_control$FrecResp_T6_lat.min,
    group_control$FrecResp_T7_lat.min,
    group_control$FrecResp_T8_lat.min,
    group_Experimental$FrecResp_T0_lat.min,
    group_Experimental$FrecResp_T1_lat.min,
    group_Experimental$FrecResp_T2_lat.min,
    group_Experimental$FrecResp_T3_lat.min,
    group_Experimental$FrecResp_T4_lat.min,
    group_Experimental$FrecResp_T5_lat.min,
    group_Experimental$FrecResp_T6_lat.min,
    group_Experimental$FrecResp_T7_lat.min,
    group_Experimental$FrecResp_T8_lat.min),
  c(rep(c("T0", "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8"), times=2, each=length(group_control$FrecResp_T0_lat.min)))
)

colnames(FrecResp_data) <- c("Grupo", "Valor", "Tiempo")
FrecResp_data


## ----echo=FALSE------------------------
# Statistical Test
stat.test <- FrecResp_data %>%
  group_by(Tiempo) %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  FrecCard_data, x = "Tiempo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="Frecuencia Respiratoria (lats/min)",
       title="Comparación de Frecuencia Respiratoria entre Grupos a través del Tiempo") +
  theme(plot.title = element_text(size=07))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Tiempo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
# Grouped Lineplots
lp <- ggline(
  FrecResp_data, x = "Tiempo", y = "Valor", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="Frecuencia Respiratoria (lats/min)",
       title="Tendencias de Frecuencia Respiratoria (lats/min) entre Grupos a través del Tiempo") +
  theme(plot.title = element_text(size=07))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Tiempo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )


## ----echo=FALSE------------------------
# Hypothesis Testing DataFrame
spo2_data <- data.frame(
  c(rep("control", 9*length(group_control$Spo2_T0)), rep("experimental", 9*length(group_Experimental$Spo2_T0))),
  c(group_control$Spo2_T0,
    group_control$Spo2_T1,
    group_control$Spo2_T2,
    group_control$Spo2_T3,
    group_control$Spo2_T4,
    group_control$Spo2_T5,
    group_control$Spo2_T6,
    group_control$Spo2_T7,
    group_control$Spo2_T8,
    group_Experimental$Spo2_T0,
    group_Experimental$Spo2_T1,
    group_Experimental$Spo2_T2,
    group_Experimental$Spo2_T3,
    group_Experimental$Spo2_T4,
    group_Experimental$Spo2_T5,
    group_Experimental$Spo2_T6,
    group_Experimental$Spo2_T7,
    group_Experimental$Spo2_T8),
  c(rep(c("T0", "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8"), times=2, each=length(group_control$Spo2_T0)))
)

colnames(spo2_data) <- c("Grupo", "Valor", "Tiempo")
spo2_data


## ----echo=FALSE------------------------
# Statistical Test
stat.test <- spo2_data %>%
  group_by(Tiempo) %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots

bxp <- ggboxplot(
  spo2_data, x = "Tiempo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="SpO2",
       title="Comparación de SpO2 entre Grupos a través del Tiempo") +
  theme(plot.title = element_text(size=08))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Tiempo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
# Grouped Lineplots
lp <- ggline(
  spo2_data, x = "Tiempo", y = "Valor", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="SpO2",
       title="Tendencias de SpO2 entre Grupos a través del Tiempo") +
  theme(plot.title = element_text(size=08))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Tiempo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )


## ----echo=FALSE------------------------
staiAE_data <- data.frame(
  c(rep("control", 2*length(group_control$GRUPO..SIN.MUSICOTERAPIA.0..CON.MUSICOTERAPIA..1)), rep("experimental", 2*length(group_control$GRUPO..SIN.MUSICOTERAPIA.0..CON.MUSICOTERAPIA..1))),
  c(group_control$STAI_T0_A.E, group_control$STAI_T8_A.E, group_Experimental$STAI_T0_A.E, group_Experimental$STAI_T8_A.E),
  c(rep(c("t0", "t8"), times=2, each=length(group_Experimental$STAI_T0_A.E)))
)
colnames(staiAE_data) <- c("Grupo", "Valor", "Tiempo")
staiAE_data


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- staiAE_data %>%
  group_by(Tiempo) %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  staiAE_data, x = "Tiempo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="STAI A/E",
       title="Comparación de STAI A/E entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=08))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Tiempo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
# Grouped Lineplots
lp <- ggline(
  staiAE_data, x = "Tiempo", y = "Valor", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="STAI A/E",
       title="Tendencias de STAI A/E entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=08))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Tiempo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )


## ----echo=FALSE------------------------
staiAR_data <- data.frame(
  c(rep("control", 2*length(group_control$GRUPO..SIN.MUSICOTERAPIA.0..CON.MUSICOTERAPIA..1)), rep("experimental", 2*length(group_control$GRUPO..SIN.MUSICOTERAPIA.0..CON.MUSICOTERAPIA..1))),
  c(group_control$STAI_T0_A.R, group_control$STAI_T8_A.R, group_Experimental$STAI_T0_A.R, group_Experimental$STAI_T8_A.R),
  c(rep(c("t0", "t8"), times=2, each=length(group_Experimental$STAI_T0_A.R)))
)
colnames(staiAR_data) <- c("Grupo", "Valor", "Tiempo")
staiAR_data


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- staiAR_data %>%
  group_by(Tiempo) %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  staiAR_data, x = "Tiempo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="STAI A/R",
       title="Comparación de STAI A/R entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=08))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Tiempo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
# Grouped Lineplots
lp <- ggline(
  staiAR_data, x = "Tiempo", y = "Valor", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="STAI A/R",
       title="Tendencias de STAI A/R entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=08))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Tiempo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )


## ----echo=FALSE------------------------
ramsay_data <- data.frame(
  c(rep("control", 2*length(group_control$GRUPO..SIN.MUSICOTERAPIA.0..CON.MUSICOTERAPIA..1)), rep("experimental", 2*length(group_control$GRUPO..SIN.MUSICOTERAPIA.0..CON.MUSICOTERAPIA..1))),
  c(group_control$Ramsay_TO, group_control$Ramsay_T8, group_Experimental$Ramsay_TO, group_Experimental$Ramsay_T8),
  c(rep(c("t0", "t8"), times=2, each=length(group_Experimental$Ramsay_TO)))
)
colnames(ramsay_data) <- c("Grupo", "Valor", "Tiempo")
ramsay_data


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
#stat.test <- ramsay_data %>%
#  group_by(Tiempo) %>%
#  wilcox_test(Valor ~ Grupo) %>%
#  adjust_pvalue(method = "bonferroni") %>%
#  add_significance("p.adj")
#stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  ramsay_data, x = "Tiempo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Tiempo",
       y="Ramsay",
       title="Comparación de Ramsay entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=08))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Tiempo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
# Grouped Lineplots
lp <- ggline(
  ramsay_data, x = "Tiempo", y = "Valor", add = "mean_sd", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")) +
  labs(fill="Grupo",
       x="Tiempo",
       y="Ramsay",
       title="Tendencias de Ramsay entre grupos a través del tiempo") +
  theme(plot.title = element_text(size=08))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Tiempo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )


## ----echo=FALSE------------------------
ena_data <- data.frame(
  c(rep("control", 2*length(group_control$GRUPO..SIN.MUSICOTERAPIA.0..CON.MUSICOTERAPIA..1)), rep("experimental", 2*length(group_control$GRUPO..SIN.MUSICOTERAPIA.0..CON.MUSICOTERAPIA..1))),
  c(group_control$ENA_TO, group_control$ENA_T8, group_Experimental$ENA_TO, group_Experimental$ENA_T8),
  c(rep(c("t0", "t8"), times=2, each=length(group_Experimental$ENA_TO)))
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
  theme(plot.title = element_text(size=08))

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
  theme(plot.title = element_text(size=08))

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Tiempo") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
  )

