## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(stringr)
library(ROCR)


## ----echo=TRUE-------------------------
data <- data.frame(read.csv("DataColangitis_PabelCarbajalCabrera.csv"))
data


## ----echo=TRUE-------------------------
# Clean numeric features
data$COMORBILIDADES <- gsub(" HAS", "HAS", data$COMORBILIDADES)
data$COMORBILIDADES <- gsub("HAS,DTII", "DTII,HAS", data$COMORBILIDADES)
data$COMORBILIDADES <- str_trim(data$COMORBILIDADES)
data$TEMPERATURA <- as.numeric(gsub(",", ".", data$TEMPERATURA))
data$LEUCOCITOS <- as.numeric(gsub(",", ".", data$LEUCOCITOS))
data$NEUTROFILOS <- as.numeric(gsub(",", ".", data$NEUTROFILOS))
data$LINFOCITOS <- as.numeric(gsub(",", ".", data$LINFOCITOS))
data$INR <- as.numeric(gsub(",", ".", data$INR))
data$CREATININA <- as.numeric(gsub(",", ".", data$CREATININA))
data$NLI <- as.numeric(gsub(",", ".", data$NLI))
data$PLI <- as.numeric(gsub(",", ".", data$PLI))
data$TIPO.DE.DRENAJE.REALIZADO <- gsub("CPRE12,04,2024", "CPRE", data$TIPO.DE.DRENAJE.REALIZADO)
data$TIPO.DE.DRENAJE.REALIZADO <- gsub("CPRE04,03,24", "CPRE", data$TIPO.DE.DRENAJE.REALIZADO)
data$DISTRES.RESPIRATORIO <- as.numeric(gsub(",", ".", data$DISTRES.RESPIRATORIO))
data$TIPO.DE.DRENAJE.REALIZADO <- str_trim(data$TIPO.DE.DRENAJE.REALIZADO)
data$ETIOLOGIA <- str_trim(data$ETIOLOGIA)

# Clean categorical features
data$SEXO[data$SEXO == 1] <- 'Male'
data$SEXO[data$SEXO == 2] <- 'Female'
data$INGRESO.A.UCI[data$INGRESO.A.UCI == 1] <- 'Yes'
data$INGRESO.A.UCI[data$INGRESO.A.UCI == 2] <- 'No'
data$MOTIVO.DE.EGRESO[data$MOTIVO.DE.EGRESO == 1] <- 'Improvement'
data$MOTIVO.DE.EGRESO[data$MOTIVO.DE.EGRESO == 2] <- 'Death'
data$MOTIVO.DE.EGRESO[data$MOTIVO.DE.EGRESO == 3] <- 'Change'
data$MOTIVO.DE.EGRESO[data$MOTIVO.DE.EGRESO == 4] <- 'Max. Benefit'
data$MOTIVO.DE.EGRESO[data$MOTIVO.DE.EGRESO == "ALTA VOLUNTARIA"] <- 'Voluntary'
data$SEVERIDAD.DE.COLANGITIS[data$SEVERIDAD.DE.COLANGITIS == 1] <- 'Light'
data$SEVERIDAD.DE.COLANGITIS[data$SEVERIDAD.DE.COLANGITIS == 2] <- 'Moderate'
data$SEVERIDAD.DE.COLANGITIS[data$SEVERIDAD.DE.COLANGITIS == 3] <- 'Serious'
data$DISTRES.RESPIRATORIO <- as.numeric(data$DISTRES.RESPIRATORIO)

# Check dataframe
data <- data[1:44,]
#data


## ----echo=TRUE-------------------------
group_colangitis1 = data[data$SEVERIDAD.DE.COLANGITIS == 'Light',]
group_colangitis1


## ----echo=TRUE-------------------------
group_colangitis2 = data[data$SEVERIDAD.DE.COLANGITIS == 'Moderate',]
group_colangitis2$COMORBILIDADES[group_colangitis2$COMORBILIDADES == ""] <- 0
group_colangitis2


## ----echo=TRUE-------------------------
group_colangitis3 = data[data$SEVERIDAD.DE.COLANGITIS == 'Serious',]
group_colangitis3


## ----echo=FALSE------------------------
data_groups <- data.frame(data %>% count(SEVERIDAD.DE.COLANGITIS, sort = FALSE))
ggplot(data_groups, aes(x = "", y = n, fill = factor(SEVERIDAD.DE.COLANGITIS
))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Severity",
       x=NULL,
       y=NULL,
       title="Proportions between Severity Groups") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis1$EDAD)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis1$EDAD)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis1$EDAD)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis2$EDAD)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis2$EDAD)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis2$EDAD)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis3$EDAD)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis3$EDAD)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis3$EDAD)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=SEVERIDAD.DE.COLANGITIS, y=EDAD, color=SEVERIDAD.DE.COLANGITIS)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Colangitis Severity",
       x="Severity",
       y="Age",
       title="Violin Plots of Age per Severity Group") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = EDAD, fill = factor(SEVERIDAD.DE.COLANGITIS))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Colangitis Severity",
       x="Age",
       y="Frequency",
       title="Histogram of Age per Severity Group") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_sexo <- data.frame(group_colangitis1 %>% count(SEXO, sort = FALSE))
ggplot(data_sexo, aes(x = "", y = n, fill = factor(SEXO))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Sex",
       x=NULL,
       y=NULL,
       title="Proportions of Sex - Light Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_sexo <- data.frame(group_colangitis2 %>% count(SEXO, sort = FALSE))
ggplot(data_sexo, aes(x = "", y = n, fill = factor(SEXO))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Sex",
       x=NULL,
       y=NULL,
       title="Proportions of Sex - Moderate Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_sexo <- data.frame(group_colangitis3 %>% count(SEXO, sort = FALSE))
ggplot(data_sexo, aes(x = "", y = n, fill = factor(SEXO))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Sex",
       x=NULL,
       y=NULL,
       title="Proportions of Sex - Serious Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_comorb <- data.frame(group_colangitis1 %>% count(COMORBILIDADES, sort = FALSE))
ggplot(data_comorb, aes(x = "", y = n, fill = factor(COMORBILIDADES))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Comorbilities",
       x=NULL,
       y=NULL,
       title="Proportions of Comorbilities - Light Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_comorb <- data.frame(group_colangitis2 %>% count(COMORBILIDADES, sort = FALSE))
ggplot(data_comorb, aes(x = "", y = n, fill = factor(COMORBILIDADES))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Comorbilities",
       x=NULL,
       y=NULL,
       title="Proportions of Comorbilities - Moderate Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_comorb <- data.frame(group_colangitis3 %>% count(COMORBILIDADES, sort = FALSE))
ggplot(data_comorb, aes(x = "", y = n, fill = factor(COMORBILIDADES))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Comorbilities",
       x=NULL,
       y=NULL,
       title="Proportions of Comorbilities - Serious Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis1$TEMPERATURA)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis1$TEMPERATURA)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis1$TEMPERATURA)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis2$TEMPERATURA)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis2$TEMPERATURA)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis2$TEMPERATURA)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis3$TEMPERATURA)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis3$TEMPERATURA)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis3$TEMPERATURA)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=SEVERIDAD.DE.COLANGITIS, y=TEMPERATURA, color=SEVERIDAD.DE.COLANGITIS)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Colangitis Severity",
       x="Severity",
       y="Temperature",
       title="Violin Plots of Temperature per Severity Group") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = TEMPERATURA, fill = factor(SEVERIDAD.DE.COLANGITIS))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.5,
                 alpha = 0.5) +
  labs(fill = "Colangitis Severity",
       x="Temperature",
       y="Frequency",
       title="Histogram of Temperature per Severity Group") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_glasgow <- data.frame(group_colangitis1 %>% count(ESCALA.DE.GLASGOW, sort = FALSE))
ggplot(data_glasgow, aes(x = "", y = n, fill = factor(ESCALA.DE.GLASGOW))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Glasgow Scale",
       x=NULL,
       y=NULL,
       title="Proportions of Glasgow Scale - Light Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_glasgow <- data.frame(group_colangitis2 %>% count(ESCALA.DE.GLASGOW, sort = FALSE))
ggplot(data_glasgow, aes(x = "", y = n, fill = factor(ESCALA.DE.GLASGOW))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Glasgow Scale",
       x=NULL,
       y=NULL,
       title="Proportions of Glasgow Scale - Moderate Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_glasgow <- data.frame(group_colangitis3 %>% count(ESCALA.DE.GLASGOW, sort = FALSE))
ggplot(data_glasgow, aes(x = "", y = n, fill = factor(ESCALA.DE.GLASGOW))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Glasgow Scale",
       x=NULL,
       y=NULL,
       title="Proportions of Glasgow Scale - Serious Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis1$LEUCOCITOS)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis1$LEUCOCITOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis1$LEUCOCITOS)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis2$LEUCOCITOS)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis2$LEUCOCITOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis2$LEUCOCITOS)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis3$LEUCOCITOS)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis3$LEUCOCITOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis3$LEUCOCITOS)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=SEVERIDAD.DE.COLANGITIS, y=LEUCOCITOS, color=SEVERIDAD.DE.COLANGITIS)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Colangitis Severity",
       x="Severity",
       y="Leucocites",
       title="Violin Plots of Leucocites per Severity Group") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = LEUCOCITOS, fill = factor(SEVERIDAD.DE.COLANGITIS))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Colangitis Severity",
       x="Leucocites",
       y="Frequency",
       title="Histogram of Leucocites per Severity Group") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis1$PLAQUETAS)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis1$PLAQUETAS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis1$PLAQUETAS)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis2$PLAQUETAS)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis2$PLAQUETAS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis2$PLAQUETAS)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis3$PLAQUETAS)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis3$PLAQUETAS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis3$PLAQUETAS)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=SEVERIDAD.DE.COLANGITIS, y=PLAQUETAS, color=SEVERIDAD.DE.COLANGITIS)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Colangitis Severity",
       x="Severity",
       y="Plaquetes",
       title="Violin Plots of Plaquetes per Severity Group") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = PLAQUETAS, fill = factor(SEVERIDAD.DE.COLANGITIS))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 50,
                 alpha = 0.5) +
  labs(fill = "Colangitis Severity",
       x="Plaquetes",
       y="Frequency",
       title="Histogram of Plaquetes per Severity Group") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis1$NEUTROFILOS)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis1$NEUTROFILOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis1$NEUTROFILOS)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis2$NEUTROFILOS)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis2$NEUTROFILOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis2$NEUTROFILOS)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis3$NEUTROFILOS)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis3$NEUTROFILOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis3$NEUTROFILOS)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=SEVERIDAD.DE.COLANGITIS, y=NEUTROFILOS, color=SEVERIDAD.DE.COLANGITIS)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Colangitis Severity",
       x="Severity",
       y="Neutrophiles",
       title="Violin Plots of Neutrophiles per Severity Group") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = NEUTROFILOS, fill = factor(SEVERIDAD.DE.COLANGITIS))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 5,
                 alpha = 0.5) +
  labs(fill = "Colangitis Severity",
       x="Neutrophiles",
       y="Frequency",
       title="Histogram of Neutrophiles per Severity Group") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis1$LINFOCITOS)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis1$LINFOCITOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis1$LINFOCITOS)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis2$LINFOCITOS)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis2$LINFOCITOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis2$LINFOCITOS)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis3$LINFOCITOS)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis3$LINFOCITOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis3$LINFOCITOS)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=SEVERIDAD.DE.COLANGITIS, y=LINFOCITOS, color=SEVERIDAD.DE.COLANGITIS)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Colangitis Severity",
       x="Severity",
       y="Lymphocites",
       title="Violin Plots of Lymphocites per Severity Group") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = LINFOCITOS, fill = factor(SEVERIDAD.DE.COLANGITIS))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 2,
                 alpha = 0.5) +
  labs(fill = "Colangitis Severity",
       x="Severity",
       y="Lymphocites",
       title="Histogram of Lymphocites per Severity Group") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis1$INR)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis1$INR)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis1$INR)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis2$INR)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis2$INR)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis2$INR)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis3$INR)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis3$INR)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis3$INR)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=SEVERIDAD.DE.COLANGITIS, y=INR, color=SEVERIDAD.DE.COLANGITIS)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Colangitis Severity",
       x="Severity",
       y="INR",
       title="Violin Plots of INR per Severity Group") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = INR, fill = factor(SEVERIDAD.DE.COLANGITIS))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Colangitis Severity",
       x="Severity",
       y="INR",
       title="Histograma of INR per Severity Group") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis1$CREATININA)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis1$CREATININA)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis1$CREATININA)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis2$CREATININA)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis2$CREATININA)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis2$CREATININA)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis3$CREATININA)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis3$CREATININA)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis3$CREATININA)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=SEVERIDAD.DE.COLANGITIS, y=CREATININA, color=SEVERIDAD.DE.COLANGITIS)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Colangitis Severity",
       x="Severity",
       y="Creatinine",
       title="Violin Plots of Creatinine per Severity Group") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = CREATININA, fill = factor(SEVERIDAD.DE.COLANGITIS))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Colangitis Severity",
       x="Severity",
       y="Creatinine",
       title="Histograma of Creatinine per Severity Group") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis1$DISTRES.RESPIRATORIO)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis1$DISTRES.RESPIRATORIO)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis1$DISTRES.RESPIRATORIO)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis2$DISTRES.RESPIRATORIO)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis2$DISTRES.RESPIRATORIO)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis2$DISTRES.RESPIRATORIO)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis3$DISTRES.RESPIRATORIO)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis3$DISTRES.RESPIRATORIO)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis3$DISTRES.RESPIRATORIO)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=SEVERIDAD.DE.COLANGITIS, y=DISTRES.RESPIRATORIO, color=SEVERIDAD.DE.COLANGITIS)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Colangitis Severity",
       x="Severity",
       y="Respiratory Distress",
       title="Violin Plots of Respiratory Distress per Severity Group") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = DISTRES.RESPIRATORIO, fill = factor(SEVERIDAD.DE.COLANGITIS))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 50,
                 alpha = 0.5) +
  labs(fill = "Colangitis Severity",
       x="Severity",
       y="Respiratory Distress",
       title="Histograma of Respiratory Distress per Severity Group") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_drenaje <- data.frame(group_colangitis1 %>% count(TIPO.DE.DRENAJE.REALIZADO, sort = FALSE))
ggplot(data_drenaje, aes(x = "", y = n, fill = factor(TIPO.DE.DRENAJE.REALIZADO))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Drainage Type",
       x=NULL,
       y=NULL,
       title="Proportions of Drainage Type - Light Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_drenaje <- data.frame(group_colangitis2 %>% count(TIPO.DE.DRENAJE.REALIZADO, sort = FALSE))
ggplot(data_drenaje, aes(x = "", y = n, fill = factor(TIPO.DE.DRENAJE.REALIZADO))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Drainage Type",
       x=NULL,
       y=NULL,
       title="Proportions of Drainage Type - Moderate Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_drenaje <- data.frame(group_colangitis3 %>% count(TIPO.DE.DRENAJE.REALIZADO, sort = FALSE))
ggplot(data_drenaje, aes(x = "", y = n, fill = factor(TIPO.DE.DRENAJE.REALIZADO))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Drainage Type",
       x=NULL,
       y=NULL,
       title="Proportions of Drainage Type - Serious Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_drenaje <- data.frame(group_colangitis1 %>% count(INGRESO.A.UCI, sort = FALSE))
ggplot(data_drenaje, aes(x = "", y = n, fill = factor(INGRESO.A.UCI))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "ICU Ingress",
       x=NULL,
       y=NULL,
       title="Proportions of ICU Ingress - Light Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_drenaje <- data.frame(group_colangitis2 %>% count(INGRESO.A.UCI, sort = FALSE))
ggplot(data_drenaje, aes(x = "", y = n, fill = factor(INGRESO.A.UCI))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "ICU Ingress",
       x=NULL,
       y=NULL,
       title="Proportions of ICU Ingress - Moderate Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_drenaje <- data.frame(group_colangitis3 %>% count(INGRESO.A.UCI, sort = FALSE))
ggplot(data_drenaje, aes(x = "", y = n, fill = factor(INGRESO.A.UCI))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "ICU Ingress",
       x=NULL,
       y=NULL,
       title="Proportions of ICU Ingress - Serious Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis1$DIAS.DE.ESTANCIA.INTRA.HOSPITALARIA)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis1$DIAS.DE.ESTANCIA.INTRA.HOSPITALARIA)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis1$DIAS.DE.ESTANCIA.INTRA.HOSPITALARIA)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis2$DIAS.DE.ESTANCIA.INTRA.HOSPITALARIA)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis2$DIAS.DE.ESTANCIA.INTRA.HOSPITALARIA)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis2$DIAS.DE.ESTANCIA.INTRA.HOSPITALARIA)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis3$DIAS.DE.ESTANCIA.INTRA.HOSPITALARIA)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis3$DIAS.DE.ESTANCIA.INTRA.HOSPITALARIA)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis3$DIAS.DE.ESTANCIA.INTRA.HOSPITALARIA)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=SEVERIDAD.DE.COLANGITIS, y=DIAS.DE.ESTANCIA.INTRA.HOSPITALARIA, color=SEVERIDAD.DE.COLANGITIS)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Colangitis Severity",
       x="Severity",
       y="Days in Hospital",
       title="Violin Plots of Days in Hospital") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = DIAS.DE.ESTANCIA.INTRA.HOSPITALARIA, fill = factor(SEVERIDAD.DE.COLANGITIS))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 5,
                 alpha = 0.5) +
  labs(fill = "Colangitis Severity",
       x="Severity",
       y="Days in Hospital",
       title="Histogram of Days in Hospital") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_drenaje <- data.frame(group_colangitis1 %>% count(MOTIVO.DE.EGRESO, sort = FALSE))
ggplot(data_drenaje, aes(x = "", y = n, fill = factor(MOTIVO.DE.EGRESO))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Discharge Motive",
       x=NULL,
       y=NULL,
       title="Proportions of Discharge Motive - Light Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_drenaje <- data.frame(group_colangitis2 %>% count(MOTIVO.DE.EGRESO, sort = FALSE))
ggplot(data_drenaje, aes(x = "", y = n, fill = factor(MOTIVO.DE.EGRESO))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Discharge Motive",
       x=NULL,
       y=NULL,
       title="Proportions of Discharge Motive - Moderate Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_drenaje <- data.frame(group_colangitis3 %>% count(MOTIVO.DE.EGRESO, sort = FALSE))
ggplot(data_drenaje, aes(x = "", y = n, fill = factor(MOTIVO.DE.EGRESO))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Discharge Motive",
       x=NULL,
       y=NULL,
       title="Proportions of Discharge Motive - Serious Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
data_drenaje <- data.frame(group_colangitis1 %>% count(ETIOLOGIA, sort = FALSE))
ggplot(data_drenaje, aes(x = "", y = n, fill = factor(ETIOLOGIA))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Etiology",
       x=NULL,
       y=NULL,
       title="Proportions of Etiology - Light Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12), legend.text=element_text(size=6), legend.key.size = unit(0.5, 'cm'))


## ----echo=FALSE------------------------
data_drenaje <- data.frame(group_colangitis2 %>% count(ETIOLOGIA, sort = FALSE))
ggplot(data_drenaje, aes(x = "", y = n, fill = factor(ETIOLOGIA))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Etiology",
       x=NULL,
       y=NULL,
       title="Proportions of Etiology - Moderate Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12), legend.key.size = unit(0.5, 'cm'))


## ----echo=FALSE------------------------
data_drenaje <- data.frame(group_colangitis3 %>% count(ETIOLOGIA, sort = FALSE))
ggplot(data_drenaje, aes(x = "", y = n, fill = factor(ETIOLOGIA))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Etiology",
       x=NULL,
       y=NULL,
       title="Proportions of Etiology - Serious Colangitis") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12), legend.text=element_text(size=4), legend.key.size = unit(0.5, 'cm'))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis1$NLI)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis1$NLI)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis1$NLI)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis2$NLI)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis2$NLI)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis2$NLI)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis3$NLI)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis3$NLI)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis3$NLI)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=SEVERIDAD.DE.COLANGITIS, y=NLI, color=SEVERIDAD.DE.COLANGITIS)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Colangitis Severity",
       x="Severity",
       y="NLI",
       title="Violin Plots of NLI per Severity Group") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = NLI, fill = factor(SEVERIDAD.DE.COLANGITIS))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 50,
                 alpha = 0.5) +
  labs(fill = "Colangitis Severity",
       x="Severity",
       y="NLI",
       title="Histogram of NLI per Severity Group") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis1$PLI)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis1$PLI)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis1$PLI)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis2$PLI)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis2$PLI)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis2$PLI)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_colangitis3$PLI)


## ----echo=FALSE------------------------
# Variance
var(group_colangitis3$PLI)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_colangitis3$PLI)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=SEVERIDAD.DE.COLANGITIS, y=PLI, color=SEVERIDAD.DE.COLANGITIS)) + 
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Colangitis Severity",
       x="Severity",
       y="PLI",
       title="Violin Plots of PLI per Severity Group") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = PLI, fill = factor(SEVERIDAD.DE.COLANGITIS))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 500,
                 alpha = 0.5) +
  labs(fill = "Colangitis Severity",
       x="Severity",
       y="PLI",
       title="Histogram of PLI per Severity Group") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Hypothesis Testing Dataframe

df_hypotest <- data.frame(
  c(rep(group_colangitis1$SEVERIDAD.DE.COLANGITIS, 2), rep(group_colangitis2$SEVERIDAD.DE.COLANGITIS, 2), rep(group_colangitis3$SEVERIDAD.DE.COLANGITIS, 2)),
  c(group_colangitis1$NLI, group_colangitis1$PLI, group_colangitis2$NLI, group_colangitis2$PLI, group_colangitis3$NLI, group_colangitis3$PLI),
  c(rep("NLI", length(group_colangitis1$NLI)), rep("PLI", length(group_colangitis1$PLI)), rep("NLI", length(group_colangitis2$NLI)), rep("PLI", length(group_colangitis2$PLI)), rep("NLI", length(group_colangitis3$NLI)), rep("PLI", length(group_colangitis3$PLI)))
)
colnames(df_hypotest) <- c("Severity", "Value", "Marker")
df_hypotest


## ----echo=TRUE-------------------------
# Hypothesis Testing
stat.test <- df_hypotest %>%
  group_by(Marker) %>%
  wilcox_test(Value ~ Severity) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test 


## ----echo=FALSE------------------------
# Grouped Boxplots

bxp <- ggboxplot(
  df_hypotest, x = "Marker", y = "Value", color = "Severity", palette = c("#00AFBB", "#E7B800", "#7FFF00")
  ) +
  labs(fill="Severity",
       x="Marker",
       y="Value",
       title="Markers Comparison across Severity Groups") +
  theme(plot.title = element_text(size=11))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Marker", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )



## ----echo=FALSE------------------------
# Prepare Dataframe
df_prediction <- data.frame(data$SEVERIDAD.DE.COLANGITIS, data$PLI)

# Set threshholds
thresholds <- seq(100, 350, by=10)

# ROCs & AUCs
for(threshold in thresholds){
  # Set thresholf for Classification: Grave = 1, No_Grave = 0 
  df_prediction$pred[df_prediction$data.PLI<=threshold] <- 1
  df_prediction$pred[df_prediction$data.PLI>threshold] <- 0
  # Reference Classification
  df_prediction$ref[df_prediction$data.SEVERIDAD.DE.COLANGITIS=="Serious"] <- 1
  df_prediction$ref[df_prediction$data.SEVERIDAD.DE.COLANGITIS!="Serious"] <- 0
  # Predictions
  pred <- prediction(predictions=df_prediction$pred, labels=df_prediction$ref)
  # AUC
  auc_ROCR <- performance(pred, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  # ROC Curve
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  plot(perf, avg='threshold', spread.estimate='stddev', colorize=TRUE, main = paste0("ROC of PLI: ",threshold," - AUC: ", auc_ROCR))
  abline(a = 0, b = 1)
}


