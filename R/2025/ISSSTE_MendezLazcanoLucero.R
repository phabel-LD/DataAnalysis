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
group_1 <- data.frame(read_excel("Data_Control.xlsx"))
#group_1


## ----echo=FALSE------------------------
group_2 <- data.frame(read_excel("Data_Experimental.xlsx"))
#group_2


## ----echo=FALSE------------------------
# Dataset Completo
data <- rbind(group_1, group_2)
#data


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


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=Grupo, y=Edad, color=Grupo)) + 
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


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Edad, fill = factor(Grupo))) +
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


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(Grupo, Sexo) %>%
  summarise(n = n(), .groups = 'drop')

# Stacked barplot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Sexo)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", lwd = 0.5) +
  labs(title = "Proporciones de Sexo por Grupo",
       fill = "Sexo",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()

p
#ggplotly(p)


## ----echo=FALSE------------------------
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


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=Grupo, y=Peso, color=Grupo)) + 
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
       x="Peso (kg)",
       y="Frequencia",
       title="Histograma de Peso (kg) por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=Grupo, y=Talla, color=Grupo)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Frequencia",
       title="Violin Plots de Talla (m) por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Talla, fill = factor(Grupo))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.2,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Talla (m)",
       y="Frequencia",
       title="Histograma de Talla (m) por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE------------------------
p <- ggplot(data, aes(x=Grupo, y=Peso_Corregido, color=Grupo)) + 
    geom_violin(trim=FALSE, alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1)) +
    geom_boxplot(width=0.1) +
    labs(fill = "Grupo",
       x="Grupo",
       y="Peso_Corregido (kg)",
       title="Violin Plots de Peso_Corregido (kg) por Grupo") +
    theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Histogram
p <- ggplot(data, aes(x = Peso_Corregido, fill = factor(Grupo))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Peso_Corregido (kg)",
       y="Frequencia",
       title="Histograma de Peso_Corregido (kg) por Grupo") +
  theme(plot.title = element_text(size=12))
p
#ggplotly(p)


## ----echo=FALSE------------------------
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


## ----echo=FALSE------------------------
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


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(Grupo, ASA) %>%
  summarise(n = n(), .groups = 'drop')

# Stacked barplot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = ASA)) +
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
  group_by(Grupo, Sonda) %>%
  summarise(n = n(), .groups = 'drop')

# Stacked barplot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Sonda)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", lwd = 0.5) +
  labs(title = "Proporciones de Sonda por Grupo",
       fill = "Sonda",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()

p
#ggplotly(p)


## ----echo=FALSE, fig.width=8-----------
# Group
data_summary <- data %>%
  group_by(Grupo, Complicacion_Colocacion) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Complicacion_Colocacion)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Complicacion de Colocacion por Grupo",
       fill = "Complicacion_Colocacion",
       x = NULL,
       y = "Frequencia") +
  theme(plot.title = element_text(size=6)) +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.width=8-----------
# Group
data_summary <- data %>%
  group_by(Grupo, Complicacion_Colocacion) %>%
  summarise(n = n(), .groups = 'drop')

# Stacked barplot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Complicacion_Colocacion)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", lwd = 0.5) +
  labs(title = "Proporciones de Complicacion de Colocacion por Grupo",
       fill = "Complicacion_Colocacion",
       x = NULL,
       y = "Frequencia") +
  theme(plot.title = element_text(size=5)) +
  theme_minimal()

p
#ggplotly(p)


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(Grupo, Complicacion_Colocacion) %>%
  summarise(n = n(), .groups = 'drop')

p <- ggstatsplot::ggpiestats(
    data = data,
    x = Complicacion_Colocacion,
    y = Grupo,
    type = "nonparametric",
    title = "Proporciones de Complicacion_Colocacion por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(Grupo, Epistaxis) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Epistaxis)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Epistaxis por Grupo",
       fill = "Epistaxis",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Stacked barplot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Epistaxis)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", lwd = 0.5) +
  labs(title = "Proporciones de Epistaxis por Grupo",
       fill = "Epistaxis",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()

p
#ggplotly(p)


## ----echo=FALSE------------------------
# Group
p <- ggstatsplot::ggpiestats(
    data = data,
    x = Epistaxis,
    y = Grupo,
    type = "nonparametric",
    title = "Proporciones de Epistaxis por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(Grupo, Laringoespasmo) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Laringoespasmo)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Laringoespasmo por Grupo",
       fill = "Laringoespasmo",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(Grupo, Vomito) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Vomito)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Vomito por Grupo",
       fill = "Vomito",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(Grupo, Tratamiento) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Tratamiento)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Tratamiento por Grupo",
       fill = "Tratamiento",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Group
p <- ggstatsplot::ggpiestats(
    data = data,
    x = Tratamiento,
    y = Grupo,
    type = "nonparametric",
    title = "Proporciones de Tratamiento por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE, fig.height=6, fig.width=8----
# Convert wide to long format
data_long <- data %>%
  pivot_longer(
    cols = starts_with("ETCO2_"),
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
    labs(title = paste("ETCO2 en", tiempo),
         x = "Grupo",
         y = "ETCO2") +
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
    cols = starts_with("SPO2_"),
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
    labs(title = paste("SPO2 en", tiempo),
         x = "Grupo",
         y = "SPO2") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust=0.5)
    )
  print(p)
}


## ----echo=FALSE, fig.width=8-----------
# Group
data_summary <- data %>%
  group_by(Grupo, Complicaciones_Respiratorias) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Complicaciones_Respiratorias)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Complicaciones_Respiratorias por Grupo",
       fill = "Complicaciones_Respiratorias",
       x = NULL,
       y = "Frequencia") +
  theme(plot.title = element_text(size=5)) +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Group
p <- ggstatsplot::ggpiestats(
    data = data,
    x = Complicaciones_Respiratorias,
    y = Grupo,
    type = "nonparametric",
    title = "Proporciones de Complicaciones_Respiratorias por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(Grupo, Hipercapnia) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Hipercapnia)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Hipercapnia por Grupo",
       fill = "Hipercapnia",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Group
p <- ggstatsplot::ggpiestats(
    data = data,
    x = Hipercapnia,
    y = Grupo,
    type = "nonparametric",
    title = "Proporciones de Hipercapnia por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(Grupo, SPO2...80.) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = SPO2...80.)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de SPO2 80% por Grupo",
       fill = "SPO2 80%",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(Grupo, AUMENTO_O2) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = AUMENTO_O2)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Aumento de O2 80% por Grupo",
       fill = "Aumento de O2",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Group
p <- ggstatsplot::ggpiestats(
    data = data,
    x = AUMENTO_O2,
    y = Grupo,
    type = "nonparametric",
    title = "Proporciones de Aumento de O2 por Grupo",
    pairwise.display = "significant",
    p.adjust.method = "holm"
)
p


## ----echo=FALSE, fig.width=10----------
# Group
data_summary <- data %>%
  group_by(Grupo, Ventilacion_Mascarilla) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Ventilacion_Mascarilla)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Ventilacion con Mascarilla 80% por Grupo",
       fill = "Ventilacion con Mascarilla",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE------------------------
# Group
data_summary <- data %>%
  group_by(Grupo, Manejo_Via_Aerea) %>%
  summarise(n = n(), .groups = 'drop')

# Plot
p <- ggplot(data_summary, aes(x = Grupo, y = n, fill = Manejo_Via_Aerea)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", lwd = 0.5) +
  geom_text(aes(x = Grupo, y = n + 0.25, label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  labs(title = "Proporciones de Manejo de la Via Aerea por Grupo",
       fill = "Manejo de la Via Aerea",
       x = NULL,
       y = "Frequencia") +
  theme_minimal()
p
#ggplotly(p)


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = starts_with("ETCO2"),
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
  ylab="ETCO2",
  title="Comparación de ETCO2 por Tiempo - Grupo 1"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = starts_with("ETCO2"),
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
  ylab="ETCO2",
  title="Comparación de ETCO2 por Tiempo - Grupo 1"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_2 %>%
  pivot_longer(
    cols = starts_with("ETCO2"),
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
  ylab="ETCO2",
  title="Comparación de ETCO2 por Tiempo - Grupo 2"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_2 %>%
  pivot_longer(
    cols = starts_with("ETCO2"),
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
  ylab="ETCO2",
  title="Comparación de ETCO2 por Tiempo - Grupo 2"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("ETCO2"),
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
    ylab = "ETCO2",
    title = tiempo
  )
  print(p_subset)
}


## ----echo=FALSE, fig.height=8, fig.width=10----
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("ETCO2"),
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
    ylab = "ETCO2",
    title = tiempo
  )
  print(p_subset)
}


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = starts_with("SPO2_"),
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
  ylab="ETCO2",
  title="Comparación de ETCO2 por Tiempo - Grupo 1"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_1 %>%
  pivot_longer(
    cols = starts_with("SPO2_"),
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
  ylab="ETCO2",
  title="Comparación de ETCO2 por Tiempo - Grupo 1"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_2 %>%
  pivot_longer(
    cols = starts_with("SPO2_"),
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
  ylab="ETCO2",
  title="Comparación de ETCO2 por Tiempo - Grupo 2"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
ggstats_df <- group_2 %>%
  pivot_longer(
    cols = starts_with("SPO2_"),
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
  ylab="ETCO2",
  title="Comparación de ETCO2 por Tiempo - Grupo 2"
)
p


## ----echo=FALSE, fig.height=8, fig.width=10----
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("SPO2_"),
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
    ylab = "SPO2",
    title = tiempo
  )
  print(p_subset)
}


## ----echo=FALSE, fig.height=8, fig.width=10----
# Convert wide to long format
data_long <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("SPO2_"),
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
    ylab = "SPO2",
    title = tiempo
  )
  print(p_subset)
}


## ----echo=FALSE------------------------
cor_df <- group_1[c("IMC", "ETCO2_T0_Inicial", "ETCO2_T1_10min", "ETCO2_T2_20min", "ETCO2_T3_30min")]
psych::pairs.panels(cor_df, lm=TRUE)


## ----echo=FALSE------------------------
cor_df <- group_2[c("IMC", "ETCO2_T0_Inicial", "ETCO2_T1_10min", "ETCO2_T2_20min", "ETCO2_T3_30min")]
psych::pairs.panels(cor_df, lm=TRUE)

