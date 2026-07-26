## ----echo=FALSE------------------------
# Libraries
library(readxl)
library(psych)
library(Hmisc)
library(stringr)
library(dplyr)
library(tidyr)
library(plotly)

library(ggpubr)
library(rstatix)
library(gtsummary)

library(ggplot2)
library(plotly)
library(car)
library(babynames)
library(gapminder)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("Data.xlsx"))
#data


## ----echo=FALSE------------------------
data$Hemoglobina_Inicial <- as.numeric(sapply(strsplit(data$Hemoglobina_Inicial, " - "), `[`, 1))
data$Hematocrito_Inicial <- as.numeric(sapply(strsplit(data$Hematocrito_Inicial, " - "), `[`, 2))

data$Hemoglobina_Final <- as.numeric(sapply(strsplit(data$Hemoglobina_Final, " - "), `[`, 1))
data$Hematocrito_Final <- as.numeric(sapply(strsplit(data$Hematocrito_Final, " - "), `[`, 2))
#data


## ----echo=FALSE------------------------
# Grupo 1
group_1 <- data[data$Grupo == "A",]
#group_1


## ----echo=FALSE------------------------
# Grupo 2
group_2 <- data[data$Grupo == "B",]
#group_2


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(Grupo) %>%
  summarise(n = n(), .groups = 'drop')

# Stacked barplot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Grupo)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", lwd = 0.5) +
  labs(title = "Proporciones de Grupos",
       fill = "Grupo",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
#p
ggplotly(p)


## ----echo=FALSE------------------------
psych::describe(group_1[sapply(group_1, class) == 'numeric'])


## ----echo=FALSE------------------------
Hmisc::describe(group_1[sapply(group_1, class) == 'numeric'])


## ----echo=FALSE------------------------
psych::describe(group_2[sapply(group_2, class) == 'numeric'])


## ----echo=FALSE------------------------
Hmisc::describe(group_2[sapply(group_2, class) == 'numeric'])


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=Grupo, y=Edad, color=Grupo)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "GRUPO",
       x="GRUPO",
       y="Edad",
       title="Violin Plots de Edad por Grupo") +
    theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Edad, fill = factor(Grupo))) +
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
  theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(Grupo, Genero) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Genero)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Genero por Grupo",
       fill = "Genero",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
#p
ggplotly(p)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=Grupo, y=Peso, color=Grupo)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Peso",
       title="Violin Plots de Peso por Grupo") +
    theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Peso, fill = factor(Grupo))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Peso",
       y="Frequencia",
       title="Histograma de Peso por Grupo") +
  theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=Grupo, y=Hemoglobina_Inicial, color=Grupo)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Peso",
       title="Violin Plots de Hemoglobina Inicial por Grupo") +
    theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Hemoglobina_Inicial, fill = factor(Grupo))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 2,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Peso",
       y="Frequencia",
       title="Histograma de Hemoglobina Inicial por Grupo") +
  theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=Grupo, y=Hemoglobina_Final, color=Grupo)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Peso",
       title="Violin Plots de Hemoglobina Final por Grupo") +
    theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Hemoglobina_Final, fill = factor(Grupo))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 2,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Peso",
       y="Frequencia",
       title="Histograma de Hemoglobina Final por Grupo") +
  theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=Grupo, y=Hematocrito_Inicial, color=Grupo)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Peso",
       title="Violin Plots de Hematocrito Inicial por Grupo") +
    theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Hematocrito_Inicial, fill = factor(Grupo))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Peso",
       y="Frequencia",
       title="Histograma de Hematocrito Inicial por Grupo") +
  theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=Grupo, y=Hematocrito_Final, color=Grupo)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Peso",
       title="Violin Plots de Hematocrito Final por Grupo") +
    theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Hematocrito_Final, fill = factor(Grupo))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Peso",
       y="Frequencia",
       title="Histograma de Hematocrito Final por Grupo") +
  theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(Grupo, ASA) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = ASA)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Genero por Grupo",
       fill = "ASA",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
#p
ggplotly(p)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=Grupo, y=Tiempo.quirurgico..h., color=Grupo)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Tiempo Quirúrgico",
       title="Violin Plots de Tiempo Quirúrgico por Grupo") +
    theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Tiempo.quirurgico..h., fill = factor(Grupo))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.2,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Tiempo Quirúrgico",
       y="Frequencia",
       title="Histograma de Tiempo Quirúrgico por Grupo") +
  theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=Grupo, y=Tiempo.anestesico, color=Grupo)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Tiempo Anestésico",
       title="Violin Plots de Tiempo Anestésico por Grupo") +
    theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Tiempo.anestesico, fill = factor(Grupo))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.2,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Tiempo Anestésico",
       y="Frequencia",
       title="Histograma de Tiempo Anestésico por Grupo") +
  theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE, fig.width=8-----------
# Group
data_summary <- data %>%
  group_by(Grupo, Sistema.de.drenaje.postquirurgico) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Sistema.de.drenaje.postquirurgico)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Sistema de Drenaje Postquirúrgico por Grupo",
       fill = "Drenaje Postquirúrgico",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
#p
ggplotly(p)


## ----echo=FALSE, fig.width=8-----------
# Group
p <- ggstatsplot::ggpiestats(
    data = data,
    x = Sistema.de.drenaje.postquirurgico,
    y = Grupo,
    type = "nonparametric",
    title = "Proporciones de Sistema de Drenaje Postquirúrgico por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE, fig.width=8-----------
# Group
data_summary <- data %>%
  group_by(Grupo, Implante.protesico) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Implante.protesico)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Sistema de Implante Prostético por Grupo",
       fill = " Implante Prostético",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
#p
ggplotly(p)


## ----echo=FALSE, fig.width=8-----------
# Group
p <- ggstatsplot::ggpiestats(
    data = data,
    x = Implante.protesico,
    y = Grupo,
    type = "nonparametric",
    title = "Proporciones de Sistema de Implante Prostético por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=Grupo, y=Sangrado, color=Grupo)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Sangrado",
       title="Violin Plots de Sangrado por Grupo") +
    theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Sangrado, fill = factor(Grupo))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 100,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Sangrado",
       y="Frequencia",
       title="Histograma de Sangrado por Grupo") +
  theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE, fig.width=8-----------
# Group
data_summary <- data %>%
  group_by(Grupo, No..Paquetes.globulares) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = No..Paquetes.globulares)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de No. de Paquetes Globulares por Grupo",
       fill = " Implante Prostético",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
#p
ggplotly(p)


## ----echo=FALSE, fig.width=8-----------
# Group
p <- ggstatsplot::ggpiestats(
    data = data,
    x = No..Paquetes.globulares,
    y = Grupo,
    type = "nonparametric",
    title = "Proporciones de No. de Paquetes Globulares por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE, fig.width=8-----------
p <- ggstatsplot::ggbetweenstats(
    data = data,
    x = Grupo,
    y = Hemoglobina_Inicial,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "Hemoglobina Inicial",
    title = "Hemoglobina Inicial entre grupos"
  )
p


## ----echo=FALSE, fig.width=8-----------
p <- ggstatsplot::ggbetweenstats(
    data = data,
    x = Grupo,
    y = Hemoglobina_Final,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "Hemoglobina Final",
    title = "Hemoglobina Final entre grupos"
  )
p


## ----echo=FALSE, fig.width=8-----------
data$Hemoglobina_diff <- data$Hemoglobina_Final-data$Hemoglobina_Inicial
p <- ggstatsplot::ggbetweenstats(
    data = data,
    x = Grupo,
    y = Hemoglobina_diff,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "Hemoglobina Final -- Inicial",
    title = "Diferencia en Hemoglobina entre grupos"
  )
p


## ----echo=FALSE, fig.width=8-----------
p <- ggstatsplot::ggbetweenstats(
    data = data,
    x = Grupo,
    y = Hematocrito_Inicial,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "Hematocrito Inicial",
    title = "Hematocrito Inicial entre grupos"
  )
p


## ----echo=FALSE, fig.width=8-----------
p <- ggstatsplot::ggbetweenstats(
    data = data,
    x = Grupo,
    y = Hematocrito_Final,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "Hematocrito Final",
    title = "Hematocrito Final entre grupos"
  )
p


## ----echo=FALSE, fig.width=8-----------
data$Hematocrito_diff <- data$Hematocrito_Final-data$Hematocrito_Inicial
p <- ggstatsplot::ggbetweenstats(
    data = data,
    x = Grupo,
    y = Hematocrito_diff,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "Hematocrito Final -- Inicial",
    title = "Diferencia en Hematocrito entre grupos"
  )
p


## ----echo=FALSE, fig.width=8-----------
p <- ggstatsplot::ggbetweenstats(
    data = data,
    x = Grupo,
    y = Sangrado,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "Sangrado",
    title = "Diferencia en Sangrado entre grupos"
  )
p

