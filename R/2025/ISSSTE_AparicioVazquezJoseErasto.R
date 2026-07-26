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
data <- data.frame(read_excel("Data.xlsx"))
#data


## ----echo=FALSE------------------------
# Grupo 1
group_1 <- data[data$GRUPO == "Butilhioscina",]
#group_1


## ----echo=FALSE------------------------
# Grupo 2
group_2 <- data[data$GRUPO == "Control",]
#group_2


## ----echo=FALSE------------------------
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
p <- ggplot(data, aes(x=GRUPO, y=Edad, color=GRUPO)) + 
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
p <- ggplot(data, aes(x = Edad, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "GRUPO",
       x="Edad",
       y="Frequencia",
       title="Histograma de Edad por Grupo") +
  theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(GRUPO, Genero) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = Genero)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = GRUPO, y = n + 0.25, label = n),
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
p <- ggplot(data, aes(x=GRUPO, y=Peso, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "GRUPO",
       x="GRUPO",
       y="Peso",
       title="Violin Plots de Peso por Grupo") +
    theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Peso, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "GRUPO",
       x="Peso",
       y="Frequencia",
       title="Histograma de Peso por Grupo") +
  theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=Talla, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "GRUPO",
       x="GRUPO",
       y="Talla",
       title="Violin Plots de Talla por Grupo") +
    theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Talla, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = .2,
                 alpha = 0.5) +
  labs(fill = "GRUPO",
       x="Talla",
       y="Frequencia",
       title="Histograma de Talla por Grupo") +
  theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=IMC, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "GRUPO",
       x="GRUPO",
       y="IMC",
       title="Violin Plots de IMC por Grupo") +
    theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = IMC, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 5,
                 alpha = 0.5) +
  labs(fill = "GRUPO",
       x="IMC",
       y="Frequencia",
       title="Histograma de IMC por Grupo") +
  theme(plot.title = element_text(size=12))
#p
ggplotly(p)


## ----echo=FALSE------------------------
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
  labs(title = "Proporciones de Genero por Grupo",
       fill = "ASA",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("BRADICARDIA"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Counts
counts <- data_long %>%
  group_by(GRUPO, Tiempo, Valor) %>%
  summarise(n = n(), .groups = 'drop')
counts$Tiempo <- c("T0", "T1", "T2", "T2", "T3", "T3", "T4", "T4", "T0", "T1", "T1","T2", "T2", "T3", "T4")

# Plot
p <- ggplot(counts,
       aes(x = GRUPO,
           y = n,
           fill = Valor)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Tiempo) +
  labs(fill="Valor",
       x="GRUPO",
       y="Frecuencia",
       title="Frecuencias Agrupadas de Bradicardia por Grupo y Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=10))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Convert wide to long format
tiempos_str <- colnames(data %>% select(starts_with("BRADICARDIA")))

data_long <- data %>%
  pivot_longer(
    cols = tiempos_str,
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Plot with lapply
lapply(tiempos_str, function(tiempo_str) {
  data_subset <- data_long %>% filter(Tiempo == tiempo_str)
  
  p <- ggstatsplot::ggpiestats(
    data = data_subset,
    x = Valor,
    y = GRUPO,
    type = "nonparametric",
    title = paste("-", tiempo_str),
    pairwise.display = "significant",
    p.adjust.method = "holm"
  )
})


## ----echo=FALSE------------------------
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("ATROPINA"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Counts
counts <- data_long %>%
  group_by(GRUPO, Tiempo, Valor) %>%
  summarise(n = n(), .groups = 'drop')
counts$Tiempo <- c("T0", "T1", "T2", "T2", "T3", "T4", "T0", "T1", "T1","T2", "T2", "T3", "T4")

# Plot
p <- ggplot(counts,
       aes(x = GRUPO,
           y = n,
           fill = Valor)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Tiempo) +
  labs(fill="Valor",
       x="GRUPO",
       y="Frecuencia",
       title="Frecuencias Agrupadas de Uso de Atropina por Grupo y Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=10))
#p
ggplotly(p)


## ----echo=FALSE------------------------
# Convert wide to long format
tiempos_str <- colnames(data %>% select(starts_with("ATROPINA")))

data_long <- data %>%
  pivot_longer(
    cols = tiempos_str,
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Plot with lapply
lapply(tiempos_str, function(tiempo_str) {
  data_subset <- data_long %>% filter(Tiempo == tiempo_str)
  
  p <- ggstatsplot::ggpiestats(
    data = data_subset,
    x = Valor,
    y = GRUPO,
    type = "nonparametric",
    title = paste("-", tiempo_str),
    pairwise.display = "significant",
    p.adjust.method = "holm"
  )
})


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(GRUPO, ALERGIA) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = ALERGIA)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = GRUPO, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Alergia por Grupo",
       fill = "ALERGIA",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
#p
ggplotly(p)


## ----echo=FALSE------------------------
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
#p
ggplotly(p)

