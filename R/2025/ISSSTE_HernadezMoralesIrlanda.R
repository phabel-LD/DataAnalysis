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

library(ggplot2)
library(plotly)
library(car)
library(babynames)
library(gapminder)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("Data.xlsx"))
data <- data[1:22,]
data


## ----echo=FALSE------------------------

# Clean categorical features
data$GRUPO[data$GRUPO == 1] <- 'Buprenorfina'
data$GRUPO[data$GRUPO == 2] <- 'Bloqueo_ESP'

# Replace: 1->Si, 2->No
for(col in colnames(data)[14:49]){
  data[[col]][data[[col]] == 1] <- 'Si'
  data[[col]][data[[col]] == 2] <- 'No'
}
#data


## ----echo=FALSE------------------------
group_1 <- data[data$GRUPO == 'Buprenorfina',]
#group_1


## ----echo=FALSE------------------------
group_2 <- data[data$GRUPO == 'Bloqueo_ESP',]
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

p
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
p <- ggplot(data, aes(x=GRUPO, y=Edad..años., color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "GRUPO",
       x="GRUPO",
       y="Edad",
       title="Violin Plots de Edad por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Edad..años., fill = factor(GRUPO))) +
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
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(GRUPO, Género) %>%
  summarise(n = n(), .groups = 'drop')

# Stacked barplot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = Género)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", lwd = 0.5) +
  labs(title = "Proporciones de Género por Grupo",
       fill = "Género",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()

p
#ggplotly(p)


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(GRUPO, Género) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = Género)) +
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


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=Peso..kg., color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "GRUPO",
       x="GRUPO",
       y="Peso (kg)",
       title="Violin Plots de Peso (kg) por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Peso..kg., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "GRUPO",
       x="Peso (kg)",
       y="Frequencia",
       title="Histograma de Peso (kg) por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=Talla..cm., color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "GRUPO",
       x="GRUPO",
       y="Talla (cm)",
       title="Violin Plots de Talla (cm) por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Talla..cm., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "GRUPO",
       x="Talla (cm)",
       y="Frequencia",
       title="Histograma de Talla (cm) por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=GRUPO, y=Peso.Ideal..kg., color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "GRUPO",
       x="GRUPO",
       y="Peso Idead (kg)",
       title="Violin Plots de Peso Ideal (kg) por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Peso.Ideal..kg., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "GRUPO",
       x="Peso Ideal (kg)",
       y="Frequencia",
       title="Histograma de Peso Ideal (kg) por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(GRUPO, ASA) %>%
  summarise(n = n(), .groups = 'drop')

# Stacked barplot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = ASA)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", lwd = 0.5) +
  labs(title = "Proporciones de ASA por Grupo",
       fill = "ASA",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()

p
#ggplotly(p)


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
  labs(title = "Proporciones de ASA por Grupo",
       fill = "ASA",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=30, fig.width=10----
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("ENA"),
    names_to = "Tiempo",
    values_to = "ENA"
  )

# Plot
p <- ggplot(data_long, aes(x=GRUPO, y=ENA, fill=GRUPO)) +
  geom_violin(trim=FALSE, alpha=0.5) +
  geom_jitter(shape=16, position=position_jitter(0.1)) +
  geom_boxplot(width=0.1) +
  facet_wrap(~Tiempo, nrow = length(unique(data_long$Tiempo))) +
  labs(title = "ENA por Grupos en el Tiempo",
       x = "Grupo",
       y = "ENA") +
  theme_minimal()

p
#ggplotly(p, height = 2000, width = 800) %>%
#  layout(
#    title = list(
#      text = "ENA por Grupos en el Tiempo",
#      font = list(size = 30)
#    )
#  )


## ----echo=FALSE------------------------
# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = c("Rescate..0..mcg", "Rescate..1..mcg", "Rescate..2..mcg", "Rescate..3..mcg", "Rescate..4..mcg"),
    names_to = "Tiempo",
    values_to = "Rescate"
  )

# Counts
counts <- data_long %>%
  group_by(GRUPO, Tiempo, Rescate) %>%
  summarise(n = n(), .groups = 'drop')
counts$Tiempo <- c("T0", "T1", "T2", "T3", "T4", "T0", "T0", "T1", "T1", "T2", "T2", "T3", "T3", "T4", "T4")

# Plot
p <- ggplot(counts,
       aes(x = GRUPO,
           y = n,
           fill = Rescate)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Tiempo) +
  labs(fill="Rescate",
       x="GRUPO",
       y="Frecuencia",
       title="Frecuencias Agrupadas de Rescate por Grupo y Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=8))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Stages
tiempos_str <- c("Rescate..0..mcg", "Rescate..1..mcg", "Rescate..2..mcg", "Rescate..3..mcg", "Rescate..4..mcg")

# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = tiempos_str,
    names_to = "Tiempo",
    values_to = "Rescate"
  )

# Plot with lapply
lapply(tiempos_str, function(tiempo_str) {
  data_subset <- data_long %>% filter(Tiempo == tiempo_str)
  
  p <- ggstatsplot::ggbarstats(
    data = data_subset,
    x = Rescate,
    y = GRUPO,
    title = paste("-", tiempo_str),
    pairwise.display = "significant",
    p.adjust.method = "holm"
  )
})


## ----echo=FALSE------------------------
data$Rescate.total.Bupre..mcg. <- as.numeric(data$Rescate.total.Bupre..mcg.)
p <- ggplot(data, aes(x=GRUPO, y=Rescate.total.Bupre..mcg., color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "GRUPO",
       x="GRUPO",
       y="Rescate Total (mcg)",
       title="Violin Plots de Rescate Total (mcg) por Grupo") +
    theme(plot.title = element_text(size=8))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Rescate.total.Bupre..mcg., fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "GRUPO",
       x="Rescate total (mcg)",
       y="Frequencia",
       title="Histograma de Rescate total (mcg) por Grupo") +
  theme(plot.title = element_text(size=15))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = c("E..Adverso.Nausea.Vomito..0.", "E..Adverso.Nausea.Vomito..1.", "E..Adverso.Nausea.Vomito..2.", "E..Adverso.Nausea.Vomito..3.", "E..Adverso.Nausea.Vomito..4." ),
    names_to = "Tiempo",
    values_to = "Nausea"
  )

# Counts
counts <- data_long %>%
  group_by(GRUPO, Tiempo, Nausea) %>%
  summarise(n = n(), .groups = 'drop')
counts$Tiempo <- c("T0", "T1", "T2", "T3", "T4", "T4", "T0", "T1", "T2", "T3", "T4")

# Plot
p <- ggplot(counts,
       aes(x = GRUPO,
           y = n,
           fill = Nausea)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Tiempo) +
  labs(fill="Nausea",
       x="GRUPO",
       y="Frecuencia",
       title="Frecuencias Agrupadas de Nausea por Grupo y Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=8))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Stages
tiempos_str <- c("E..Adverso.Nausea.Vomito..0.", "E..Adverso.Nausea.Vomito..1.", "E..Adverso.Nausea.Vomito..2.", "E..Adverso.Nausea.Vomito..3.", "E..Adverso.Nausea.Vomito..4.")

# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = tiempos_str,
    names_to = "Tiempo",
    values_to = "Evento"
  )


# Plot with lapply
lapply(tiempos_str, function(tiempo_str) {
  data_subset <- data_long %>% filter(Tiempo == tiempo_str)
  
  p <- ggstatsplot::ggbarstats(
    data = data_subset,
    x = Evento,
    y = GRUPO,
    title = paste("-", tiempo_str),
    pairwise.display = "significant",
    p.adjust.method = "holm"
  )
})


## ----echo=FALSE------------------------
# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = c("Tx.E.A.Nausea.Vomito..0.", "Tx.E.A.Nausea.Vomito..1.", "Tx.E.A.Nausea.Vomito..2.", "Tx.E.A.Nausea.Vomito..3.", "Tx.E.A.Nausea.Vomito..4." ),
    names_to = "Tiempo",
    values_to = "Tratamiento_Nausea"
  )

# Counts
counts <- data_long %>%
  group_by(GRUPO, Tiempo, Tratamiento_Nausea) %>%
  summarise(n = n(), .groups = 'drop')
counts$Tiempo <- c("T0", "T1", "T2", "T3", "T4", "T4", "T0", "T1", "T2", "T3", "T4")

# Plot
p <- ggplot(counts,
       aes(x = GRUPO,
           y = n,
           fill = Tratamiento_Nausea)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Tiempo) +
  labs(fill="Tratamiento_Nausea",
       x="GRUPO",
       y="Frecuencia",
       title="Frecuencias Agrupadas de Tratamiento para Nausea por Grupo y Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=5))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Stages
tiempos_str <- c("Tx.E.A.Nausea.Vomito..0.", "Tx.E.A.Nausea.Vomito..1.", "Tx.E.A.Nausea.Vomito..2.", "Tx.E.A.Nausea.Vomito..3.", "Tx.E.A.Nausea.Vomito..4." )

# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = tiempos_str,
    names_to = "Tiempo",
    values_to = "Evento"
  )


# Plot with lapply
lapply(tiempos_str, function(tiempo_str) {
  data_subset <- data_long %>% filter(Tiempo == tiempo_str)
  
  p <- ggstatsplot::ggbarstats(
    data = data_subset,
    x = Evento,
    y = GRUPO,
    title = paste("-", tiempo_str),
    pairwise.display = "significant",
    p.adjust.method = "holm"
  )
})


## ----echo=FALSE------------------------
# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = c("E..Adverso.Hipotension..0.", "E..Adverso.Hipotension..1.", "E..Adverso.Hipotension..2.", "E..Adverso.Hipotension..3.", "E..Adverso.Hipotension..4." ),
    names_to = "Tiempo",
    values_to = "Hipotension"
  )

# Counts
counts <- data_long %>%
  group_by(GRUPO, Tiempo, Hipotension) %>%
  summarise(n = n(), .groups = 'drop')
counts$Tiempo <- c("T0", "T0", "T1", "T2", "T3", "T4", "T0", "T1", "T2", "T3", "T4")

# Plot
p <- ggplot(counts,
       aes(x = GRUPO,
           y = n,
           fill = Hipotension)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Tiempo) +
  labs(fill="Hipotension",
       x="GRUPO",
       y="Frecuencia",
       title="Frecuencias Agrupadas de Hipotension por Grupo y Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=6))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Stages
tiempos_str <- c("E..Adverso.Hipotension..0.", "E..Adverso.Hipotension..1.", "E..Adverso.Hipotension..2.", "E..Adverso.Hipotension..3.", "E..Adverso.Hipotension..4." )

# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = tiempos_str,
    names_to = "Tiempo",
    values_to = "Evento"
  )


# Plot with lapply
lapply(tiempos_str, function(tiempo_str) {
  data_subset <- data_long %>% filter(Tiempo == tiempo_str)
  
  p <- ggstatsplot::ggbarstats(
    data = data_subset,
    x = Evento,
    y = GRUPO,
    title = paste("-", tiempo_str),
    pairwise.display = "significant",
    p.adjust.method = "holm"
  )
})


## ----echo=FALSE------------------------
# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = c("Tx.EA.Hipotensión..0.", "Tx.EA.Hipotensión..1.", "Tx.EA.Hipotensión..2.", "Tx.EA.Hipotensión..3.", "Tx.EA.Hipotensión..4."),
    names_to = "Tiempo",
    values_to = "Tratamiento_Hipotension"
  )

# Counts
counts <- data_long %>%
  group_by(GRUPO, Tiempo, Tratamiento_Hipotension) %>%
  summarise(n = n(), .groups = 'drop')
counts$Tiempo <- c("T0", "T0", "T1", "T2", "T3", "T4", "T0", "T1", "T2", "T3", "T4")

# Plot
p <- ggplot(counts,
       aes(x = GRUPO,
           y = n,
           fill = Tratamiento_Hipotension)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Tiempo) +
  labs(fill="Tratamiento_Hipotension",
       x="GRUPO",
       y="Frecuencia",
       title="Frecuencias Agrupadas de Tratamiento para Hipotension por Grupo y Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=6))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Stages
tiempos_str <- c("Tx.EA.Hipotensión..0.", "Tx.EA.Hipotensión..1.", "Tx.EA.Hipotensión..2.", "Tx.EA.Hipotensión..3.", "Tx.EA.Hipotensión..4.")

# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = tiempos_str,
    names_to = "Tiempo",
    values_to = "Evento"
  )


# Plot with lapply
lapply(tiempos_str, function(tiempo_str) {
  data_subset <- data_long %>% filter(Tiempo == tiempo_str)
  
  p <- ggstatsplot::ggbarstats(
    data = data_subset,
    x = Evento,
    y = GRUPO,
    title = paste("-", tiempo_str),
    pairwise.display = "significant",
    p.adjust.method = "holm"
  )
})


## ----echo=FALSE------------------------
# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = c("E..Adv..Hipersensibilidad..0.", "E..Adv..Hipersensibilidad..1.", "E..Adv..Hipersensibilidad..2.", "E..Adv..Hipersensibilidad..3.", "E..Adv..Hipersensibilidad..4." ),
    names_to = "Tiempo",
    values_to = "Hipersensibilidad"
  )

# Counts
counts <- data_long %>%
  group_by(GRUPO, Tiempo, Hipersensibilidad) %>%
  summarise(n = n(), .groups = 'drop')
counts$Tiempo <- c("T0", "T1", "T2", "T3", "T4", "T0", "T1", "T2", "T3", "T4")

# Plot
p <- ggplot(counts,
       aes(x = GRUPO,
           y = n,
           fill = Hipersensibilidad)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Tiempo) +
  labs(fill="Hipersensibilidad",
       x="GRUPO",
       y="Frecuencia",
       title="Frecuencias Agrupadas de Hipersensibilidad por Grupo y Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=6))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Stages
tiempos_str <- c("E..Adv..Hipersensibilidad..0.", "E..Adv..Hipersensibilidad..1.", "E..Adv..Hipersensibilidad..2.", "E..Adv..Hipersensibilidad..3.", "E..Adv..Hipersensibilidad..4.")

# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = tiempos_str,
    names_to = "Tiempo",
    values_to = "Evento"
  )


# Plot with lapply
lapply(tiempos_str, function(tiempo_str) {
  data_subset <- data_long %>% filter(Tiempo == tiempo_str)
  
  p <- ggstatsplot::ggbarstats(
    data = data_subset,
    x = Evento,
    y = GRUPO,
    title = paste("-", tiempo_str),
    pairwise.display = "significant",
    p.adjust.method = "holm"
  )
})


## ----echo=FALSE------------------------
# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = c("Tx.EA.Hipersensibilidad..0.", "Tx.EA.Hipersensibilidad..1.", "Tx.EA.Hipersensibilidad..2.", "Tx.EA.Hipersensibilidad..3.", "Tx.EA.Hipersensibilidad..4." ),
    names_to = "Tiempo",
    values_to = "Tratamiento_Hipersensibilidad"
  )

# Counts
counts <- data_long %>%
  group_by(GRUPO, Tiempo, Tratamiento_Hipersensibilidad) %>%
  summarise(n = n(), .groups = 'drop')
counts$Tiempo <- c("T0", "T1", "T2", "T3", "T4", "T0", "T1", "T2", "T3", "T4")

# Plot
p <- ggplot(counts,
       aes(x = GRUPO,
           y = n,
           fill = Tratamiento_Hipersensibilidad)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ Tiempo) +
  labs(fill="Tratamiento_Hipersensibilidad",
       x="GRUPO",
       y="Frecuencia",
       title="Frecuencias Agrupadas de Tratamiento para Hipersensibilidad por Grupo y Tiempos") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size=6))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Stages
tiempos_str <- c("Tx.EA.Hipersensibilidad..0.", "Tx.EA.Hipersensibilidad..1.", "Tx.EA.Hipersensibilidad..2.", "Tx.EA.Hipersensibilidad..3.", "Tx.EA.Hipersensibilidad..4.")

# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = tiempos_str,
    names_to = "Tiempo",
    values_to = "Evento"
)


# Plot with lapply
lapply(tiempos_str, function(tiempo_str) {
  data_subset <- data_long %>% filter(Tiempo == tiempo_str)
  
  p <- ggstatsplot::ggbarstats(
    data = data_subset,
    x = Evento,
    y = GRUPO,
    title = paste("-", tiempo_str),
    pairwise.display = "significant",
    p.adjust.method = "holm"
  )
})


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = c("ENA..0.", "ENA..1.", "ENA..2.", "ENA..3.", "ENA..4."),
    names_to = "Tiempo",
    values_to = "Valor"
  )

p <- ggstatsplot::ggwithinstats(
  data=ggstats_df,
  x=Tiempo,
  y=Valor,
  type = "nonparametric",
  pairwise.display = "significant",
  p.adjust.method = "holm",
  digits="signif",
  xlab="Tiempos",
  ylab="ENA",
  title="Comparación de ENA por Tiempo - Grupo 1"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = c("ENA..0.", "ENA..1.", "ENA..2.", "ENA..3.", "ENA..4."),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Plot
p <- ggstatsplot::ggwithinstats(
  data=ggstats_df,
  x=Tiempo,
  y=Valor,
  type = "nonparametric",
  pairwise.display = "significant",
  p.adjust.method = "BH",
  digits="signif",
  xlab="Tiempos",
  ylab="ENA",
  title="Comparación de ENA por Tiempo - Grupo 1"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
ggstats_df <- group_2 %>%
  pivot_longer(
    cols = c("ENA..0.", "ENA..1.", "ENA..2.", "ENA..3.", "ENA..4."),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Plot
p <- ggplot(ggstats_df, aes(x=Tiempo, y=Valor, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "GRUPO",
       x="Tiempo",
       y="ENA",
       title="Violin Plots de ENA por Tiempo - Grupo 2") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(ggstats_df, aes(x = Valor, fill = factor(Tiempo))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Tiempo",
       x="ENA",
       y="Frequencia",
       title="Histograma de ENA por Tiempo - Grupo 2") +
  theme(plot.title = element_text(size=8))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=30----
# Group
ggstats_df <- data %>%
  pivot_longer(
    cols = c("ENA..0.", "ENA..1.", "ENA..2.", "ENA..3.", "ENA..4."),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Plot
p <- ggplot(ggstats_df, aes(x=Tiempo, y=Valor, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5, position=position_dodge(width=0.0)) +
    geom_jitter(shape=16, position=position_jitter(width=0.1)) +
    geom_boxplot(width=0, position=position_dodge(width=0.0)) +
    labs(
      fill = "GRUPO",
      x="Tiempo",
      y="ENA",
      title="Violin Plots de ENA por Grupo en el Tiempo"
    ) +
    theme(plot.title = element_text(size=8))
p
#ggplotly(p)


## ----echo=FALSE------------------------
library(dplyr)
library(survival)
library(survminer)


## ----echo=FALSE------------------------
# Survival-Time Dataframe: 0,30,60,90,180 min
survival_df <- data[c("GRUPO", "Rescate..0..mcg", "Rescate..1..mcg", "Rescate..2..mcg", "Rescate..3..mcg", "Rescate..4..mcg")]
colnames(survival_df) <- c("GRUPO", 0, 30, 60, 90, 180)

# Time points
time_points <- c(0, 30, 60, 90, 180)

# Function to extract first event time and status
extract_event_info <- function(row) {
  # Loop over time points
  for (i in seq_along(time_points)) {
    time_point <- time_points[i]
    # Check if event occurred at this time point
    if (row[[as.character(time_point)]] == "Si") {
      return(list(time = time_point, event = 1))
    }
  }
  # If no event, censored at the last time point
  return(list(time = max(time_points), event = 0))
}

# Apply the function to each row
long_format_list <- lapply(1:nrow(survival_df), function(i) {
  row <- survival_df[i, ]
  event_info <- extract_event_info(row)
  data.frame(
    GRUPO = row$GRUPO,
    time = event_info$time,
    event = event_info$event
  )
})

# Combine into a dataframe
surv_long_df <- do.call(rbind, long_format_list)

# Run Kaplan-Meier 
km_fit <- survfit(Surv(time, event) ~ GRUPO,
                  data = surv_long_df)

# Plot
p <- ggsurvplot(km_fit,
                data = surv_long_df,
                pval = TRUE,
                conf.int = TRUE,
                risk.table = TRUE,
                risk.table.height = 0.3,
                title = "Curvas Kaplan-Meier & Log-Rank Test P-val por Grupo - Analgesia sin Rescate")
# Customize title size
p$plot <- p$plot + theme(
  plot.title = element_text(size = 6, face = "bold")
)
# Display the plot
p


## ----echo=FALSE------------------------
# Survival-Time Dataframe: 0,30,60,90,180 min
survival_df <- data[c("GRUPO", "ENA..0.", "ENA..1.", "ENA..2.", "ENA..3.", "ENA..4.")]
colnames(survival_df) <- c("GRUPO", 0, 30, 60, 90, 180)
threshhold <- 3
survival_df[survival_df[,2] >= threshhold, 2] <- "Si"
survival_df[survival_df[,2] < threshhold, 2] <- "No"
survival_df[survival_df[,3] >= threshhold, 2] <- "Si"
survival_df[survival_df[,3] < threshhold, 2] <- "No"
survival_df[survival_df[,4] >= threshhold, 2] <- "Si"
survival_df[survival_df[,4] < threshhold, 2] <- "No"
survival_df[survival_df[,5] >= threshhold, 2] <- "Si"
survival_df[survival_df[,5] < threshhold, 2] <- "No"
survival_df[survival_df[,6] >= threshhold, 2] <- "Si"
survival_df[survival_df[,6] < threshhold, 2] <- "No"

# Time points
time_points <- c(0, 30, 60, 90, 180)

# Function to extract first event time and status
extract_event_info <- function(row) {
  # Loop over time points
  for (i in seq_along(time_points)) {
    time_point <- time_points[i]
    # Check if event occurred at this time point
    if (row[[as.character(time_point)]] == "Si") {
      return(list(time = time_point, event = 1))
    }
  }
  # If no event, censored at the last time point
  return(list(time = max(time_points), event = 0))
}

# Apply the function to each row
long_format_list <- lapply(1:nrow(survival_df), function(i) {
  row <- survival_df[i, ]
  event_info <- extract_event_info(row)
  data.frame(
    GRUPO = row$GRUPO,
    time = event_info$time,
    event = event_info$event
  )
})

# Combine into a dataframe
surv_long_df <- do.call(rbind, long_format_list)

# Run Kaplan-Meier 
km_fit <- survfit(Surv(time, event) ~ GRUPO,
                  data = surv_long_df)

# Plot
p <- ggsurvplot(km_fit,
                data = surv_long_df,
                pval = TRUE,
                conf.int = TRUE,
                risk.table = TRUE,
                risk.table.height = 0.3,
                title = "Curvas Kaplan-Meier & Log-Rank Test P-val por Grupo - Nausea")
# Customize title size
p$plot <- p$plot + theme(
  plot.title = element_text(size = 6, face = "bold")
)
# Display the plot
p


## ----echo=FALSE------------------------
# Survival-Time Dataframe: 0,30,60,90,180 min
survival_df <- data[c("GRUPO", "E..Adverso.Nausea.Vomito..0.", "E..Adverso.Nausea.Vomito..1.", "E..Adverso.Nausea.Vomito..2.", "E..Adverso.Nausea.Vomito..3.", "E..Adverso.Nausea.Vomito..4.")]
colnames(survival_df) <- c("GRUPO", 0, 30, 60, 90, 180)

# Time points
time_points <- c(0, 30, 60, 90, 180)

# Function to extract first event time and status
extract_event_info <- function(row) {
  # Loop over time points
  for (i in seq_along(time_points)) {
    time_point <- time_points[i]
    # Check if event occurred at this time point
    if (row[[as.character(time_point)]] == "Si") {
      return(list(time = time_point, event = 1))
    }
  }
  # If no event, censored at the last time point
  return(list(time = max(time_points), event = 0))
}

# Apply the function to each row
long_format_list <- lapply(1:nrow(survival_df), function(i) {
  row <- survival_df[i, ]
  event_info <- extract_event_info(row)
  data.frame(
    GRUPO = row$GRUPO,
    time = event_info$time,
    event = event_info$event
  )
})

# Combine into a dataframe
surv_long_df <- do.call(rbind, long_format_list)

# Run Kaplan-Meier 
km_fit <- survfit(Surv(time, event) ~ GRUPO,
                  data = surv_long_df)

# Plot
p <- ggsurvplot(km_fit,
                data = surv_long_df,
                pval = TRUE,
                conf.int = TRUE,
                risk.table = TRUE,
                risk.table.height = 0.3,
                title = "Curvas Kaplan-Meier & Log-Rank Test P-val por Grupo - Nausea")
# Customize title size
p$plot <- p$plot + theme(
  plot.title = element_text(size = 6, face = "bold")
)
# Display the plot
p


## ----echo=FALSE------------------------
# Survival-Time Dataframe: 0,30,60,90,180 min
survival_df <- data[c("GRUPO", "E..Adverso.Hipotension..0.", "E..Adverso.Hipotension..1.", "E..Adverso.Hipotension..2.", "E..Adverso.Hipotension..3.", "E..Adverso.Hipotension..4.")]
colnames(survival_df) <- c("GRUPO", 0, 30, 60, 90, 180)

# Time points
time_points <- c(0, 30, 60, 90, 180)

# Function to extract first event time and status
extract_event_info <- function(row) {
  # Loop over time points
  for (i in seq_along(time_points)) {
    time_point <- time_points[i]
    # Check if event occurred at this time point
    if (row[[as.character(time_point)]] == "Si") {
      return(list(time = time_point, event = 1))
    }
  }
  # If no event, censored at the last time point
  return(list(time = max(time_points), event = 0))
}

# Apply the function to each row
long_format_list <- lapply(1:nrow(survival_df), function(i) {
  row <- survival_df[i, ]
  event_info <- extract_event_info(row)
  data.frame(
    GRUPO = row$GRUPO,
    time = event_info$time,
    event = event_info$event
  )
})

# Combine into a dataframe
surv_long_df <- do.call(rbind, long_format_list)

# Run Kaplan-Meier 
km_fit <- survfit(Surv(time, event) ~ GRUPO,
                  data = surv_long_df)

# Plot
p <- ggsurvplot(km_fit,
                data = surv_long_df,
                pval = TRUE,
                conf.int = TRUE,
                risk.table = TRUE,
                risk.table.height = 0.3,
                title = "Curvas Kaplan-Meier & Log-Rank Test P-val por Grupo - Hipotensión")
# Customize title size
p$plot <- p$plot + theme(
  plot.title = element_text(size = 6, face = "bold")
)
# Display the plot
p

