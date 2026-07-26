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
data$Rescate[data$Rescate == 0] <- "No"
data$Rescate[data$Rescate == 1] <- "Sí"
data$Adversos[data$Adversos == 0] <- "No"
data$Adversos[data$Adversos == 1] <- "Sí"
#data


## ----echo=FALSE------------------------
# Grupo 1
group_1 <- data[data$Grupo == "A",]
#group_1


## ----echo=FALSE------------------------
# Grupo 2
group_2 <- data[data$Grupo == "B",]
#group_2


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
p <- ggplot(data, aes(x=Grupo, y=Edad, color=Grupo)) + 
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


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = Edad, fill = factor(Grupo))) +
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


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(Grupo, Sexo) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Sexo)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Sexo por Grupo",
       fill = "Sexo",
       x = NULL,
       y = "Frequencia") +
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
       y = "Frequencia") +
  theme_minimal()
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
       title="Violin Plots de Talla por Grupo") +
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
       y="Frequencia",
       title="Histograma de Talla (m) por Grupo") +
  theme(plot.title = element_text(size=12))
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
       y="Frequencia",
       title="Histograma de Peso (kg) por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=Grupo, y=IMC, color=Grupo)) + 
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
p <- ggplot(data, aes(x = IMC, fill = factor(Grupo))) +
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


## ----echo=FALSE, fig.height=6, fig.width=8----
# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = starts_with("Dolor"),
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
       y = "Frequencia") +
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
# Group
data_summary <- data %>%
  group_by(Grupo, Adversos) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Adversos)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Adversos por Grupo",
       fill = "Adversos",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=6, fig.width=10----
# Categorical Test
p <- ggstatsplot::ggpiestats(
    data = data,
    x = Adversos,
    y = Grupo,
    type = "nonparametric",
    title = "Proporciones de Adversos por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = starts_with("Dolor"),
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
  title="Comparación de Dolor Agudo Postoperatorio - Grupo A"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = starts_with("Dolor"),
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
  title="Comparación de Dolor Agudo Postoperatorio - Grupo A"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_2 %>%
  pivot_longer(
    cols = starts_with("Dolor"),
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
  title="Comparación de Dolor Agudo Postoperatorio - Grupo B"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_2 %>%
  pivot_longer(
    cols = starts_with("Dolor"),
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
  title="Comparación de Dolor Agudo Postoperatorio - Grupo B"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("Dolor"),
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
    cols = starts_with("Dolor"),
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

