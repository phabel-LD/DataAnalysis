## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(car)
library(babynames)
library(gapminder)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("Data.xlsx"))
data


## ----echo=FALSE------------------------

# Clean categorical features
data$SEXO[data$SEXO == 1] <- 'Masculino'
data$SEXO[data$SEXO == 2] <- 'Femenino'

data$COMPLICACIONES[data$COMPLICACIONES == 0] <- 'No'
data$COMPLICACIONES[data$COMPLICACIONES == 1] <- 'Hematoma/Sangrado'

data$TRATAMIENTO[is.na(data$TRATAMIENTO)] <- 'No'
data$TRATAMIENTO[data$TRATAMIENTO == 1] <- 'Compresión con Banda Elástica'

data


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(data$EDAD)


## ----echo=FALSE------------------------
# Variance
var(data$EDAD)


## ----echo=FALSE------------------------
# Standar Deviation
sd(data$EDAD)


## ----echo=FALSE, fig.height=6----------
p <- ggplot(data, aes(x='', y=EDAD)) + 
    geom_violin(trim=FALSE, color="violet", alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1), color="violet") +
    geom_boxplot(width=0.1, color="violet") +
    labs(x="Pacientes",
       y="Edad (años)",
       title="Violin Plot de Edad") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE, fig.height=6----------
# Histogram
p <- ggplot(data, aes(x = EDAD)) +
  geom_histogram(colour = "violet",
                 fill = "violet",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 5,
                 alpha = 0.25) +
  labs(
       x="Pacientes",
       y="Edad",
       title="Histograma de Edad") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE, fig.height=6----------
data_sexo <- data.frame(data %>% count(SEXO, sort = FALSE))
p <- ggplot(data_sexo, aes(x = "", y = n, fill = factor(SEXO))) +
  geom_col(color = "violet") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Sexo",
       x=NULL,
       y=NULL,
       title="Proporciones de Sexo") +
  scale_fill_manual(values=c("violet", "white")) +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE, fig.height=6----------
data_sexo <- data.frame(data %>% count(ASA, sort = FALSE))
p <- ggplot(data_sexo, aes(x = "", y = n, fill = factor(ASA))) +
  geom_col(color = "violet") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "ASA",
       x=NULL,
       y=NULL,
       title="Proporciones de ASA") +
  scale_fill_manual(values=c("violet", "white")) +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))

p


## ----echo=FALSE, fig.height=6----------
data_sexo <- data.frame(data %>% count(COMPLICACIONES, sort = FALSE))
p <- ggplot(data_sexo, aes(x = "", y = n, fill = factor(COMPLICACIONES))) +
  geom_col(color = "violet") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Complicaciones",
       x=NULL,
       y=NULL,
       title="Proporciones de Complicaciones") +
  scale_fill_manual(values=c("violet", "white")) +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
plot_ly(data_sexo, labels = ~COMPLICACIONES, values = ~n, type = 'pie',
        marker = list(
          colors = c("Yes" = "violet", "No" = "white"),
          line = list(
            color = 'violet',   # Border color
            width = 2           # Border width
          )
        ),
        textinfo = 'label+percent',
        hoverinfo = 'label+percent+value') %>%
  layout(title = 'Proporciones de Complicaciones')


## ----echo=FALSE, fig.height=6----------
data_sexo <- data.frame(data %>% count(TRATAMIENTO, sort = FALSE))
p <- ggplot(data_sexo, aes(x = "", y = n, fill = factor(TRATAMIENTO))) +
  geom_col(color = "violet") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Tratamiento",
       x=NULL,
       y=NULL,
       title="Proporciones de Tratamiento") +
  scale_fill_manual(values=c("violet", "white")) +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(data$TI.1)


## ----echo=FALSE------------------------
# Variance
var(data$TI.1)


## ----echo=FALSE------------------------
# Standar Deviation
sd(data$TI.1)


## ----echo=FALSE, fig.height=6----------
p <- ggplot(data, aes(x='', y=TI.1)) + 
    geom_violin(trim=FALSE, color="violet", alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1), color="violet") +
    geom_boxplot(width=0.1, color="violet") +
    labs(x="Pacientes",
       y="Concentración de Lactato Sérico [mmol]",
       title="Violin Plot de Concentración de Lactato Sérico [mmol] T0") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE, fig.height=6----------
# Histogram
p <- ggplot(data, aes(x = TI.1)) +
  geom_histogram(colour = "violet",
                 fill = "violet",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.1,
                 alpha = 0.25) +
  labs(
       y="Pacientes",
       x="Concentración de Lactato Sérico [mmol]",
       title="Histograma de Concentración de Lactato Sérico [mmol] T0") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(data$TI.2)


## ----echo=FALSE------------------------
# Variance
var(data$TI.2)


## ----echo=FALSE------------------------
# Standar Deviation
sd(data$TI.2)


## ----echo=FALSE, fig.height=6----------
p <- ggplot(data, aes(x='', y=TI.2)) + 
    geom_violin(trim=FALSE, color="violet", alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1), color="violet") +
    geom_boxplot(width=0.1, color="violet") +
    labs(x="Pacientes",
       y="Concentración de Lactato Sérico [mmol]",
       title="Violin Plot de Concentración de Lactato Sérico [mmol] T1") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE, fig.height=6----------
# Histogram
p <- ggplot(data, aes(x = TI.2)) +
  geom_histogram(colour = "violet",
                 fill = "violet",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.1,
                 alpha = 0.25) +
  labs(
       y="Pacientes",
       x="Concentración de Lactato Sérico [mmol]",
       title="Histograma de Concentración de Lactato Sérico [mmol] T1") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(data$TI.3)


## ----echo=FALSE------------------------
# Variance
var(data$TI.3)


## ----echo=FALSE------------------------
# Standar Deviation
sd(data$TI.3)


## ----echo=FALSE, fig.height=6----------
p <- ggplot(data, aes(x='', y=TI.3)) + 
    geom_violin(trim=FALSE, color="violet", alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1), color="violet") +
    geom_boxplot(width=0.1, color="violet") +
    labs(x="Pacientes",
       y="Concentración de Lactato Sérico [mmol]",
       title="Violin Plot de Concentración de Lactato Sérico [mmol] T2") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE, fig.height=6----------
# Histogram
p <- ggplot(data, aes(x = TI.3)) +
  geom_histogram(colour = "violet",
                 fill = "violet",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.1,
                 alpha = 0.25) +
  labs(
       y="Pacientes",
       x="Concentración de Lactato Sérico [mmol]",
       title="Histograma de Concentración de Lactato Sérico [mmol] T2") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(data$TI.4)


## ----echo=FALSE------------------------
# Variance
var(data$TI.4)


## ----echo=FALSE------------------------
# Standar Deviation
sd(data$TI.4)


## ----echo=FALSE, fig.height=6----------
p <- ggplot(data, aes(x='', y=TI.4)) + 
    geom_violin(trim=FALSE, color="violet", alpha=0.5) +
    geom_jitter(shape=16, position=position_jitter(0.1), color="violet") +
    geom_boxplot(width=0.1, color="violet") +
    labs(x="Pacientes",
       y="Concentración de Lactato Sérico [mmol]",
       title="Violin Plot de Concentración de Lactato Sérico [mmol] T3") +
    theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE, fig.height=6----------
# Histogram
p <- ggplot(data, aes(x = TI.4)) +
  geom_histogram(colour = "violet",
                 fill = "violet",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.1,
                 alpha = 0.25) +
  labs(
       y="Pacientes",
       x="Concentración de Lactato Sérico [mmol]",
       title="Histograma de Concentración de Lactato Sérico [mmol] T3") +
  theme(plot.title = element_text(size=12))
p


## ----echo=FALSE------------------------
ggplotly(p)


## ----echo=FALSE, fig.height=6, fig.width=7----
ggstats_df <- data.frame(
  c(rep("T0 (Sin Isq.)", length(data$TI.1)), rep("T1 (30 min)", length(data$TI.2)), rep("T2 (60 min)", length(data$TI.3)), rep("T3 (Fin Isq.)", length(data$TI.4))),
  c(data$TI.1, data$TI.2, data$TI.3, data$TI.4)
)
colnames(ggstats_df) <- c("Tiempo", "Valor")
print(ggstats_df)

p <- ggstatsplot::ggwithinstats(
  data=ggstats_df,
  x=Tiempo,
  y=Valor,
  type = "nonparametric",
  pairwise.display = "significant",
  p.adjust.method = "holm",
  digits="signif",
  xlab="Tiempos de Isquemia",
  ylab="Valor Sérico de Lactato [mmol/L]",
  title="Comparación de Valor Sérico de Lactato [mmol/L] por Tiempo de Isquemia",
)
p


## ----echo=FALSE, fig.height=6, fig.width=7----
ggstats_df <- data.frame(
  c(rep("T0 (Sin Isq.)", length(data$TI.1)), rep("T1 (30 min)", length(data$TI.2)), rep("T2 (60 min)", length(data$TI.3)), rep("T3 (Fin Isq.)", length(data$TI.4))),
  c(data$TI.1, data$TI.2, data$TI.3, data$TI.4)
)
colnames(ggstats_df) <- c("Tiempo", "Valor")

p <- ggstatsplot::ggwithinstats(
  data=ggstats_df,
  x=Tiempo,
  y=Valor,
  type = "nonparametric",
  pairwise.display = "significant",
  p.adjust.method = "BH",
  digits="signif",
  xlab="Tiempos de Isquemia",
  ylab="Valor Sérico de Lactato [mmol/L]",
  title="Comparación de Valor Sérico de Lactato [mmol/L] por Tiempo de Isquemia"
)
p

