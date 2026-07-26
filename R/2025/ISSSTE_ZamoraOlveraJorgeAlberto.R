## ----echo=FALSE------------------------
# Libraries
library(readxl)
library(psych)
library(Hmisc)
library(stringr)
library(dplyr)
library(tidyr)

library(ggpubr)
library(rstatix)
library(gtsummary)

library(ggplot2)
library(plotly)
library(car)
library(babynames)
library(gapminder)


## ----echo=FALSE------------------------
x <- data.frame(read_excel("Data.xlsx"))
data <- setNames(data.frame(t(x[,-1])), x[,1])
#data


## ----echo=FALSE------------------------
data$EDAD <- as.numeric(data$EDAD)
data$PESO <- as.numeric(data$PESO)
data$TALLA <- as.numeric(data$TALLA)
data$IMC <- as.numeric(data$IMC)
data$DOSIS_DEXMEDETOMIDINA <- as.numeric(data$DOSIS_DEXMEDETOMIDINA)
data$DOSIS_OPIOIDE <- as.numeric(data$DOSIS_OPIOIDE)
data$FC_BASAL <- as.numeric(data$FC_BASAL)
data$TAM_BASAL <- as.numeric(data$TAM_BASAL)
data$SAT02_BASAL <- as.numeric(data$SAT02_BASAL)
data$FC_FINAL <- as.numeric(data$FC_FINAL)
data$TAM_FINAL <- as.numeric(data$TAM_FINAL)
data$SAT02_FINAL <- as.numeric(data$SAT02_FINAL)
#data


## ----echo=FALSE------------------------
# Grupo 1
group_1 <- data[data$GRUPO == "Control",]
group_1


## ----echo=FALSE------------------------
# Grupo 2
group_2 <- data[data$GRUPO == "Dexme",]
group_2


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(GRUPO) %>%
  summarise(n = n(), .groups = 'drop')

# Stacked barplot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = GRUPO)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", lwd = 0.5) +
  labs(title = "Proporciones de Grupos",
       fill = "Grupo",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE------------------------
psych::describe(group_1[sapply(group_1, class) == 'numeric'])


## ----echo=FALSE------------------------
Hmisc::describe(group_1[sapply(group_1, class) == 'numeric'])


## ----echo=FALSE------------------------
psych::describe(group_2[sapply(group_2, class) == 'numeric'])


## ----echo=FALSE------------------------
Hmisc::describe(group_2[sapply(group_2, class) == 'numeric'])


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=EDAD, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Edad",
       title="Violin Plots de Edad por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = EDAD, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Edad",
       y="Frequencia",
       title="Histograma de Edad por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(GRUPO, GENERO) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = GENERO)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = GRUPO, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Género por Grupo",
       fill = "Género",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=PESO, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Peso (kg)",
       title="Violin Plots de Peso (kg) por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = PESO, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Peso (kg)",
       y="Frequencia",
       title="Histograma de Peso (kg) por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=TALLA, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Talla (cm)",
       title="Violin Plots de Talla (cm) por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = TALLA, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Talla (cm)",
       y="Frequencia",
       title="Histograma de Talla (cm) por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=IMC, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="IMC",
       title="Violin Plots de IMC por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = IMC, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 5,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="IMC",
       y="Frequencia",
       title="Histograma de IMC por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(GRUPO, ASA) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = ASA)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = GRUPO, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de ASA por Grupo",
       fill = "ASA",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=6, fig.width=10----
# Categorical Test
p <- ggstatsplot::ggpiestats(
    data = data,
    x = ASA,
    y = GRUPO,
    type = "nonparametric",
    title = "Proporciones de ASA por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=DOSIS_DEXMEDETOMIDINA, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Dosis Dexmedetomidina (mcg)",
       title="Violin Plots de Dosis Dexmedetomidina (mcg) por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = DOSIS_DEXMEDETOMIDINA, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 5,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Dosis Dexmedetomidina (mcg)",
       y="Frequencia",
       title="Histograma de Dosis Dexmedetomidina (mcg) por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=DOSIS_OPIOIDE, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Dosis Opioide (mcg)",
       title="Violin Plots de Dosis Opioide (mcg) por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = DOSIS_OPIOIDE, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 50,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Dosis Dexmedetomidina (mcg)",
       y="Frequencia",
       title="Histograma de Dosis Opioide (mcg) por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=FC_BASAL, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="FC Basal",
       title="Violin Plots de FC Basal por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = FC_BASAL, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="FC Basal",
       y="Frequencia",
       title="Histograma de FC Basal por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=TAM_BASAL, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="TAM Basal",
       title="Violin Plots de TAM Basal por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = TAM_BASAL, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="TAM Basal",
       y="Frequencia",
       title="Histograma de TAM Basal por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=SAT02_BASAL, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="SAT O2 Basal",
       title="Violin Plots de SAT O2 Basal por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = SAT02_BASAL, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="SAT O2 Basal",
       y="Frequencia",
       title="Histograma de SAT O2 Basal por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(GRUPO, RAMSAY_BASAL) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = RAMSAY_BASAL)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = GRUPO, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Ramsay Basal por Grupo",
       fill = "RAMSAY_BASAL",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=6, fig.width=10----
# Categorical Test
p <- ggstatsplot::ggpiestats(
    data = data,
    x = RAMSAY_BASAL,
    y = GRUPO,
    type = "nonparametric",
    title = "Proporciones de Ramsay Basal por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=FC_FINAL, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="FC Basal",
       title="Violin Plots de FC Final por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = FC_FINAL, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="FC Basal",
       y="Frequencia",
       title="Histograma de FC Final por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(GRUPO, RAMSAY_FINAL) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = RAMSAY_FINAL)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = GRUPO, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Ramsay Final por Grupo",
       fill = "RAMSAY_BASAL",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=6, fig.width=10----
# Categorical Test
p <- ggstatsplot::ggpiestats(
    data = data,
    x = RAMSAY_FINAL,
    y = GRUPO,
    type = "nonparametric",
    title = "Proporciones de Ramsay Final por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=FC_FINAL, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="FC Basal",
       title="Violin Plots de FC Final por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = FC_FINAL, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="FC Basal",
       y="Frequencia",
       title="Histograma de FC Final por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=TAM_FINAL, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="TAM Final",
       title="Violin Plots de TAM Final por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = TAM_FINAL, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="TAM Final",
       y="Frequencia",
       title="Histograma de TAM Final por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=SAT02_FINAL, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="SAT O2 Final",
       title="Violin Plots de SAT O2 Final por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = SAT02_FINAL, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="SAT O2 Final",
       y="Frequencia",
       title="Histograma de SAT O2 Final por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(GRUPO, EFECTO_SECUNDARIO_TRANSOPERATORIO) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = EFECTO_SECUNDARIO_TRANSOPERATORIO)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = GRUPO, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Efecto Secundario Transoperatorio por Grupo",
       fill = "EFECTO_SECUNDARIO_TRANSOPERATORIO",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=6, fig.width=10----
# Categorical Test
p <- ggstatsplot::ggpiestats(
    data = data,
    x = EFECTO_SECUNDARIO_TRANSOPERATORIO,
    y = GRUPO,
    type = "nonparametric",
    title = "Proporciones de Efecto Secundario Transoperatorio por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(GRUPO, BRADICARDIA) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = BRADICARDIA)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = GRUPO, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Bradicardia por Grupo",
       fill = "BRADICARDIA",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(GRUPO, HIPOTENSION) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = HIPOTENSION)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = GRUPO, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Hipotensión por Grupo",
       fill = "HIPOTENSION",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(GRUPO, DEPRESION_RESPIRATORIA) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = DEPRESION_RESPIRATORIA)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = GRUPO, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Depresión Respiratoria por Grupo",
       fill = "DEPRESION_RESPIRATORIA",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(GRUPO, TRATAMIENTO) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = TRATAMIENTO)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = GRUPO, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Tratamiento por Grupo",
       fill = "TRATAMIENTO",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=6, fig.width=10----

p <- ggstatsplot::ggbetweenstats(
    data = data,
    x = GRUPO,
    y = DOSIS_OPIOIDE,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "Dosis",
    title = "Dosis de Opioide (mcg)"
)
p


## ----echo=FALSE, fig.height=6, fig.width=10----

p <- ggstatsplot::ggbetweenstats(
    data = data,
    x = GRUPO,
    y = FC_BASAL,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "Dosis",
    title = "FC Basal"
)
p


## ----echo=FALSE, fig.height=6, fig.width=10----

p <- ggstatsplot::ggbetweenstats(
    data = data,
    x = GRUPO,
    y = TAM_BASAL,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "Dosis",
    title = "TAM Basal"
)
p


## ----echo=FALSE, fig.height=6, fig.width=10----

p <- ggstatsplot::ggbetweenstats(
    data = data,
    x = GRUPO,
    y = SAT02_BASAL,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "SAT O2",
    title = "SAT O2 Basal"
)
p


## ----echo=FALSE, fig.height=6, fig.width=10----

p <- ggstatsplot::ggbetweenstats(
    data = data,
    x = GRUPO,
    y = FC_FINAL,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "FC",
    title = "FC_FINAL"
)
p


## ----echo=FALSE, fig.height=6, fig.width=10----

p <- ggstatsplot::ggbetweenstats(
    data = data,
    x = GRUPO,
    y = FC_FINAL,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "TAM",
    title = "TAM_FINAL"
)
p


## ----echo=FALSE, fig.height=6, fig.width=10----

p <- ggstatsplot::ggbetweenstats(
    data = data,
    x = GRUPO,
    y = SAT02_FINAL,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "SAT O2",
    title = "SAT O2 Final"
)
p

