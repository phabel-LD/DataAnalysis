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
# Masculino: 1 / Femenino:2
data$Género[data$Género == 1] <- "Masculino"
data$Género[data$Género == 2] <- "Femenino"
# IMC: 1: <18,5 / 2: 18,5-24,9 / 3: 24.9 -29,9 / 4:  IMC >30
# Rescate: 0: No / 1:Si
data$Rescate[data$Rescate == 0] <- "No"
data$Rescate[data$Rescate == 1] <- "Sí"
data$IMC <- as.factor(data$IMC)
data$ASA <- as.factor(data$ASA)
data$Complicaciones[data$Complicaciones == 0] <- "No"
data$Complicaciones[data$Complicaciones == 1] <- "Sí"
data$Satisfacción <- as.factor(data$Satisfacción)
#data


## ----echo=FALSE------------------------
# Grupo 1
group_1 <- data[data$Grupo == "A",]
group_1


## ----echo=FALSE------------------------
# Grupo 2
group_2 <- data[data$Grupo == "B",]
group_2


## ----echo=FALSE, fig.height=8, fig.width=10----
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
       y = "Frecuencias") +
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
p <- ggplot(data, aes(x=Grupo, y=Edad..Años., color=Grupo)) + 
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
p <- ggplot(data, aes(x = Edad..Años., fill = factor(Grupo))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Edad",
       y="Frecuencias",
       title="Histograma de Edad por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(Grupo, Género) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Género)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Género por Grupo",
       fill = "Género",
       x = NULL,
       y = "Frecuencias") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=Grupo, y=Peso..kg., color=Grupo)) + 
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
p <- ggplot(data, aes(x = Peso..kg., fill = factor(Grupo))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Peso (kg)",
       y="Frecuencias",
       title="Histograma de Peso (kg) por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=Grupo, y=Talla..m., color=Grupo)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Talla (m)",
       title="Violin Plots de Talla (m) por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = Talla..m., fill = factor(Grupo))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.1,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Talla (m)",
       y="Frecuencias",
       title="Histograma de Talla (m) por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(Grupo, IMC) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = IMC)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de IMC por Grupo",
       fill = "IMC",
       x = NULL,
       y = "Frecuencias") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
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
  labs(title = "Proporciones de ASA por Grupo",
       fill = "ASA",
       x = NULL,
       y = "Frecuencias") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=6, fig.width=10----
# Categorical Test
p <- ggstatsplot::ggpiestats(
    data = data,
    x = ASA,
    y = Grupo,
    type = "nonparametric",
    title = "Proporciones de ASA por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE, fig.height=6, fig.width=8----
# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = starts_with("ENA"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Loop through each Tiempo and generate individual plots
for (tiempo in unique(data_long$Tiempo)) {
  subset_data <- filter(data_long, Tiempo == tiempo)
  
  p <- ggplot(subset_data, aes(x=Grupo, y=Valor, fill=Grupo)) +
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3) +
    geom_boxplot(width=0.1, outlier.shape=NA, alpha=0.7) +
    labs(title = paste(tiempo),
         x = "Grupo",
         y = "ENA") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust=0.5)
    )
  print(p)
}


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(Grupo, Rescate) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Rescate)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Rescate por Grupo",
       fill = "Rescate",
       x = NULL,
       y = "Frecuencias") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=6, fig.width=10----
# Categorical Test
p <- ggstatsplot::ggpiestats(
    data = data,
    x = Rescate,
    y = Grupo,
    type = "nonparametric",
    title = "Proporciones de Rescate por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=Grupo, y=Dosis.Rescate, color=Grupo)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Dosis de Rescate",
       title="Violin Plots de Dosis de Rescate por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = Dosis.Rescate, fill = factor(Grupo))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 100,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Dosis de Rescate",
       y="Frecuencias",
       title="Histograma de Dosis de Rescate por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(Grupo, Complicaciones) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Complicaciones)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Complicaciones por Grupo",
       fill = "Complicaciones",
       x = NULL,
       y = "Frecuencias") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(Grupo, Satisfacción) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Satisfacción)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Satisfacción por Grupo",
       fill = "Satisfacción",
       x = NULL,
       y = "Frecuencias") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=6, fig.width=10----
# Categorical Test
p <- ggstatsplot::ggpiestats(
    data = data,
    x = Satisfacción,
    y = Grupo,
    type = "nonparametric",
    title = "Proporciones de Satisfacción por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = starts_with("ENA"),
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
  title="Comparación de ENA - Grupo A"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = starts_with("ENA"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

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
  title="Comparación de ENA - Grupo A"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_2 %>%
  pivot_longer(
    cols = starts_with("ENA"),
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
  title="Comparación de ENA - Grupo B"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_2 %>%
  pivot_longer(
    cols = starts_with("ENA"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

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
  title="Comparación de ENA - Grupo B"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("ENA"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Plot
for (tiempo in unique(data_long$Tiempo)) {
  subset_data <- filter(data_long, Tiempo == tiempo)
  p_subset <- ggstatsplot::ggbetweenstats(
    data = subset_data,
    x = Grupo,
    y = Valor,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "Holm",
    digits = "signif",
    xlab = "Grupos",
    ylab = "ENA",
    title = tiempo
  )
  print(p_subset)
}


## ----echo=FALSE, fig.height=8, fig.width=10----
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("ENA"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Plot
for (tiempo in unique(data_long$Tiempo)) {
  subset_data <- filter(data_long, Tiempo == tiempo)
  p_subset <- ggstatsplot::ggbetweenstats(
    data = subset_data,
    x = Grupo,
    y = Valor,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "ENA",
    title = tiempo
  )
  print(p_subset)
}

