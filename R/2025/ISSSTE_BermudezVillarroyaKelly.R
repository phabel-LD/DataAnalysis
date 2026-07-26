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
data$PESO.IDEAL <- as.numeric(data$PESO.IDEAL)
#data


## ----echo=FALSE------------------------
# Grupo 1
group_1 <- data[data$GRUPO == "GRUPO A LIDOCACINA NEBULIZADA",]
#group_1


## ----echo=FALSE------------------------
# Grupo 2
group_2 <- data[data$GRUPO == "GRUPO B LIDOCAINA PERFUSION",]
#group_2


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
    labs(fill = "GRUPO",
       x="GRUPO",
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
                 binwidth = 5,
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
  group_by(GRUPO, GENERO) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = GENERO)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = GRUPO, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Genero por Grupo",
       fill = "Género",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
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


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=PESO.KG, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "GRUPO",
       x="GRUPO",
       y="Peso",
       title="Violin Plots de Peso (Kg) por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = PESO.KG, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "GRUPO",
       x="Peso",
       y="Frequencia",
       title="Histograma de Peso (Kg) por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=TALLA.CM, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "GRUPO",
       x="GRUPO",
       y="Talla",
       title="Violin Plots de Talla (cm) por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = TALLA.CM, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "GRUPO",
       x="Talla",
       y="Frequencia",
       title="Histograma de Talla (cm) por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
p <- ggplot(data, aes(x=GRUPO, y=PESO.IDEAL, color=GRUPO)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "GRUPO",
       x="GRUPO",
       y="Peso Ideal",
       title="Violin Plots de Peso Ideal por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Histogram
p <- ggplot(data, aes(x = PESO.IDEAL, fill = factor(GRUPO))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 5,
                 alpha = 0.5) +
  labs(fill = "GRUPO",
       x="Peso Ideal",
       y="Frequencia",
       title="Histograma de Peso Ideal por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(GRUPO, ESTABILIDADA.HEMODINAMICA) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = ESTABILIDADA.HEMODINAMICA)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = GRUPO, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Estabilidad Hemodinámica por Grupo",
       fill = "Estabilidad Hemodinámica",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=6, fig.width=8----
# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = starts_with("TAM"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Loop through each Tiempo and generate individual plots
for (tiempo in unique(data_long$Tiempo)) {
  subset_data <- filter(data_long, Tiempo == tiempo)
  
  p <- ggplot(subset_data, aes(x=GRUPO, y=Valor, fill=GRUPO)) +
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3) +
    geom_boxplot(width=0.1, outlier.shape=NA, alpha=0.7) +
    labs(title = paste(tiempo),
         x = "Grupo",
         y = "TAM") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust=0.5)
    )
  print(p)
}


## ----echo=FALSE, fig.height=6, fig.width=8----
# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = starts_with("FC"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Loop through each Tiempo and generate individual plots
for (tiempo in unique(data_long$Tiempo)) {
  subset_data <- filter(data_long, Tiempo == tiempo)
  
  p <- ggplot(subset_data, aes(x=GRUPO, y=Valor, fill=GRUPO)) +
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3) +
    geom_boxplot(width=0.1, outlier.shape=NA, alpha=0.7) +
    labs(title = paste(tiempo),
         x = "Grupo",
         y = "FC") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust=0.5)
    )
  print(p)
}


## ----echo=FALSE, fig.height=6, fig.width=8----
# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = starts_with("SAT"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Loop through each Tiempo and generate individual plots
for (tiempo in unique(data_long$Tiempo)) {
  subset_data <- filter(data_long, Tiempo == tiempo)
  
  p <- ggplot(subset_data, aes(x=GRUPO, y=Valor, fill=GRUPO)) +
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3) +
    geom_boxplot(width=0.1, outlier.shape=NA, alpha=0.7) +
    labs(title = paste(tiempo),
         x = "Grupo",
         y = "SATO2") +
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
  group_by(GRUPO, COMPLICACIONES) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = COMPLICACIONES)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = GRUPO, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Complicaciones por Grupo",
       fill = "Complicaciones",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
p <- ggstatsplot::ggpiestats(
    data = data,
    x = COMPLICACIONES,
    y = GRUPO,
    type = "nonparametric",
    title = "Proporciones de Complicaciones por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
data_summary <- data %>%
  group_by(GRUPO, EFECTOS.ADVERSOS) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = GRUPO, y = n, fill = EFECTOS.ADVERSOS)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = GRUPO, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Efectos Adversos por Grupo",
       fill = "Efectos Adversos",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
# Group
p <- ggstatsplot::ggpiestats(
    data = data,
    x = EFECTOS.ADVERSOS,
    y = GRUPO,
    type = "nonparametric",
    title = "Proporciones de Efectos Adversos por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = starts_with("TAM"),
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
  ylab="TAM",
  title="Comparación de TAM por Tiempo - Grupo 1"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = starts_with("TAM"),
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
  ylab="TAM",
  title="Comparación de TAM por Tiempo - Grupo 1"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_2 %>%
  pivot_longer(
    cols = starts_with("TAM"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

p <- ggstatsplot::ggwithinstats(
  data=ggstats_df,
  x=Tiempo,
  y=Valor,
  type = "parametric",
  pairwise.display = "significant",
  p.adjust.method = "holm",
  digits="signif",
  xlab="Tiempos",
  ylab="TAM",
  title="Comparación de TAM por Tiempo - Grupo 1"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_2 %>%
  pivot_longer(
    cols = starts_with("TAM"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

p <- ggstatsplot::ggwithinstats(
  data=ggstats_df,
  x=Tiempo,
  y=Valor,
  type = "parametric",
  pairwise.display = "significant",
  p.adjust.method = "BH",
  digits="signif",
  xlab="Tiempos",
  ylab="TAM",
  title="Comparación de TAM por Tiempo - Grupo 1"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("TAM"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Plot
for (tiempo in unique(data_long$Tiempo)) {
  subset_data <- filter(data_long, Tiempo == tiempo)
  p_subset <- ggstatsplot::ggbetweenstats(
    data = subset_data,
    x = GRUPO,
    y = Valor,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "Holm",
    digits = "signif",
    xlab = "Grupos",
    ylab = "TAM",
    title = tiempo
  )
  print(p_subset)
}


## ----echo=FALSE, fig.height=8, fig.width=10----
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("TAM"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Plot
for (tiempo in unique(data_long$Tiempo)) {
  subset_data <- filter(data_long, Tiempo == tiempo)
  p_subset <- ggstatsplot::ggbetweenstats(
    data = subset_data,
    x = GRUPO,
    y = Valor,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "TAM",
    title = tiempo
  )
  print(p_subset)
}


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = starts_with("FC"),
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
  ylab="FC",
  title="Comparación de FC por Tiempo - Grupo 1"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = starts_with("FC"),
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
  xlab="Tiempos",
  ylab="FC",
  title="Comparación de FC por Tiempo - Grupo 1"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_2 %>%
  pivot_longer(
    cols = starts_with("FC"),
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
  ylab="FC",
  title="Comparación de FC por Tiempo - Grupo 2"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_2 %>%
  pivot_longer(
    cols = starts_with("FC"),
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
  xlab="Tiempos",
  ylab="FC",
  title="Comparación de FC por Tiempo - Grupo 2"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("FC"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Plot
for (tiempo in unique(data_long$Tiempo)) {
  subset_data <- filter(data_long, Tiempo == tiempo)
  p_subset <- ggstatsplot::ggbetweenstats(
    data = subset_data,
    x = GRUPO,
    y = Valor,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "Holm",
    digits = "signif",
    xlab = "Grupos",
    ylab = "FC",
    title = tiempo
  )
  print(p_subset)
}


## ----echo=FALSE, fig.height=8, fig.width=10----
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("FC"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Plot
for (tiempo in unique(data_long$Tiempo)) {
  subset_data <- filter(data_long, Tiempo == tiempo)
  p_subset <- ggstatsplot::ggbetweenstats(
    data = subset_data,
    x = GRUPO,
    y = Valor,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "FC",
    title = tiempo
  )
  print(p_subset)
}


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = starts_with("SAT"),
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
  ylab="SAT O2",
  title="Comparación de SAT O2 por Tiempo - Grupo 1"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = starts_with("SAT"),
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
  xlab="Tiempos",
  ylab="SAT O2",
  title="Comparación de SAT O2 por Tiempo - Grupo 1"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_2 %>%
  pivot_longer(
    cols = starts_with("SAT"),
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
  ylab="SAT O2",
  title="Comparación de SAT O2 por Tiempo - Grupo 2"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_2 %>%
  pivot_longer(
    cols = starts_with("SAT"),
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
  xlab="Tiempos",
  ylab="SAT O2",
  title="Comparación de SAT O2 por Tiempo - Grupo 2"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("SAT"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Plot
for (tiempo in unique(data_long$Tiempo)) {
  subset_data <- filter(data_long, Tiempo == tiempo)
  p_subset <- ggstatsplot::ggbetweenstats(
    data = subset_data,
    x = GRUPO,
    y = Valor,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "Holm",
    digits = "signif",
    xlab = "Grupos",
    ylab = "SAT O2",
    title = tiempo
  )
  print(p_subset)
}


## ----echo=FALSE, fig.height=8, fig.width=10----
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("SAT"),
    names_to = "Tiempo",
    values_to = "Valor"
  )

# Plot
for (tiempo in unique(data_long$Tiempo)) {
  subset_data <- filter(data_long, Tiempo == tiempo)
  p_subset <- ggstatsplot::ggbetweenstats(
    data = subset_data,
    x = GRUPO,
    y = Valor,
    type = "nonparametric",
    pairwise.display = "significant",
    p.adjust.method = "BH",
    digits = "signif",
    xlab = "Grupos",
    ylab = "SAT O2",
    title = tiempo
  )
  print(p_subset)
}

