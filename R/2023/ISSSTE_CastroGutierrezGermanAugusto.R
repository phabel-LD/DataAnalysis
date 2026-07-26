## ----echo=FALSE------------------------
# Libraries
library(ggpubr)
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)


## ----echo=FALSE------------------------
data <- data.frame(read_excel("CastroGutierrezGermanAugusto_Data.xlsx"))


## ----echo=FALSE------------------------
data


## ----echo=FALSE------------------------
group_control = data[data$GRUPO.0.1 == 1,]
group_control

## ----echo=FALSE------------------------
group_Experimental = data[data$GRUPO.0.1 == 0,]
group_Experimental 


## ----echo=FALSE------------------------
data_groups <- data.frame(data %>% count(GRUPO.0.1, sort = FALSE))
data_groups

## ----echo=FALSE------------------------
ggplot(data_groups, aes(x = "", y = n, fill = factor(GRUPO.0.1))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Grupo",
       x=NULL,
       y=NULL,
       title="Proporciones entre Grupos: Control vs. Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Experimental", "Control")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = EDAD.AÑOS, fill = factor(GRUPO.0.1))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 4,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Edad (años)",
       y="Frequencia",
       title="Histograma de Edad (años) por Grupo") +
  scale_fill_discrete(labels = c("Experimental", "Control")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = PESO..KG, fill = factor(GRUPO.0.1))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 4,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Peso (Kg)",
       y="Frequencia",
       title="Histograma de Peso por Grupo") +
  scale_fill_discrete(labels = c("Experimental", "Control")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = as.numeric(TALLA.CM), fill = factor(GRUPO.0.1))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.0175,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Talla (m)",
       y="Frequencia",
       title="Histograma de Talla (m) por Grupo") +
  scale_fill_discrete(labels = c("Experimental", "Control")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = as.numeric(IMC), fill = factor(GRUPO.0.1))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 2,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="IMC",
       y="Frequencia",
       title="Histograma de IMC por Grupo") +
  scale_fill_discrete(labels = c("Experimental", "Control")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = as.numeric(DOSIS.DE.ROCURONIO.MG), fill = factor(GRUPO.0.1))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 2,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Dosis Rocuronio (mg)",
       y="Frequencia",
       title="Histograma de Dosis de Rocuronio por Grupo") +
  scale_fill_discrete(labels = c("Experimental", "Control")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(data, aes(x = as.numeric(DOSIS.DE.DEXAMETASONA.MG), fill = factor(GRUPO.0.1))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.5,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Dosis Rocuronio (mg)",
       y="Frequencia",
       title="Histograma de Dosis de Dexametasona por Grupo") +
  scale_fill_discrete(labels = c("Experimental", "Control")) +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
gender_control <- data.frame(group_control %>% count(GENERO..F.1..M.2., sort = FALSE))
gender_control


## ----echo=FALSE------------------------
ggplot(gender_control, aes(x = "", y = n, fill = factor(GENERO..F.1..M.2.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género del Grupo Control") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Femenino", "Masculino")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$EDAD.AÑOS)


## ----echo=FALSE------------------------
# Variance
var(group_control$EDAD.AÑOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$EDAD.AÑOS)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$DOSIS.DE.ROCURONIO.MG)


## ----echo=FALSE------------------------
# Variance
var(group_control$DOSIS.DE.ROCURONIO.MG)


## ----echo=FALSE------------------------
# SD
sd(group_control$DOSIS.DE.ROCURONIO.MG)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$DOSIS.DE.DEXAMETASONA.MG)


## ----echo=FALSE------------------------
# Variance
var(group_control$DOSIS.DE.DEXAMETASONA.MG)


## ----echo=FALSE------------------------
# SD
sd(group_control$DOSIS.DE.DEXAMETASONA.MG)


## ----echo=FALSE------------------------
# Histogram
ggplot(group_control, aes(x = factor(ASA.1.2.3.), fill = factor(ASA.1.2.3.))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Control") +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_control$PESO..KG)


## ----echo=FALSE------------------------
# Variance
var(group_control$PESO..KG)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_control$PESO..KG)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(data$TALLA.CM))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(data$TALLA.CM))


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(data$TALLA.CM))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(data$IMC))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(data$IMC))


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(data$IMC))


## ----echo=FALSE------------------------
# Histogram
ggplot(group_control, aes(x = as.numeric(TOF.BASAL), fill = factor(GRUPO.0.1))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.5,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Dosis Rocuronio (mg)",
       y="Frequencia",
       title="Histograma de TOF basal - Control ") +
  scale_fill_discrete(labels = c("Control")) +
  theme(plot.title = element_text(size=20))


## ----echo=FALSE------------------------
# Histogram
ggplot(group_control, aes(x = T.MIN..INICIO.DE.ACCION, fill = factor(GRUPO.0.1))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.5,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Tiempo de Inicio de Acción (min)",
       y="Frequencia",
       title="Histograma de Tiempo de Inicio de Acción - Control") +
  scale_fill_discrete(labels = c("Control")) +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
relaj_control <- data.frame(group_control %>% count(RELAJACION.RESIDUAL...Si1....no.2., sort = FALSE))
relaj_control


## ----echo=FALSE------------------------
ggplot(relaj_control, aes(x = "", y = n, fill = factor(RELAJACION.RESIDUAL...Si1....no.2.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Relajación Residual",
       x=NULL,
       y=NULL,
       title="Proporciones de Relajación Residual del Grupo Control") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("No")) +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
sugamadex_control <- data.frame(group_control %>% count(SUGAMADEX..SI.1...NO.2., sort = FALSE))
sugamadex_control


## ----echo=FALSE------------------------
ggplot(sugamadex_control, aes(x = "", y = n, fill = factor(SUGAMADEX..SI.1...NO.2.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Sugamadex",
       x=NULL,
       y=NULL,
       title="Proporciones de Sugamadex del Grupo Control") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("No")) +
  theme(plot.title = element_text(size=15))


## ----echo=FALSE------------------------
gender_Experimental <- data.frame(group_Experimental %>% count(GENERO..F.1..M.2., sort = FALSE))
gender_Experimental


## ----echo=FALSE------------------------
ggplot(gender_Experimental, aes(x = "", y = n, fill = factor(GENERO..F.1..M.2.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Género",
       x=NULL,
       y=NULL,
       title="Proporciones de Género del Grupo Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Femenino", "Masculino")) +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$EDAD.AÑOS)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$EDAD.AÑOS)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$EDAD.AÑOS)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$DOSIS.DE.ROCURONIO.MG)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$DOSIS.DE.ROCURONIO.MG)


## ----echo=FALSE------------------------
# SD
sd(group_Experimental$DOSIS.DE.ROCURONIO.MG)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$DOSIS.DE.DEXAMETASONA.MG)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$DOSIS.DE.DEXAMETASONA.MG)


## ----echo=FALSE------------------------
# SD
sd(group_Experimental$DOSIS.DE.DEXAMETASONA.MG)


## ----echo=FALSE------------------------
# Histogram
ggplot(group_Experimental, aes(x = factor(ASA.1.2.3.), fill = factor(ASA.1.2.3.))) +
  geom_bar() +
  labs(fill = "Grado ASA",
       x="Grado ASA",
       y="Frecuencia",
       title="Frecuencias de Grado ASA del Grupo Experimental") +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(group_Experimental$PESO..KG)


## ----echo=FALSE------------------------
# Variance
var(group_Experimental$PESO..KG)


## ----echo=FALSE------------------------
# Standar Deviation
sd(group_Experimental$PESO..KG)


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(group_Experimental$TALLA.CM))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(group_Experimental$TALLA.CM))


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(group_Experimental$TALLA.CM))


## ----echo=FALSE------------------------
# Mean & Quantiles
summary(as.numeric(group_Experimental$IMC))


## ----echo=FALSE------------------------
# Variance
var(as.numeric(group_Experimental$IMC))


## ----echo=FALSE------------------------
# Standar Deviation
sd(as.numeric(group_Experimental$IMC))


## ----echo=FALSE------------------------
# Histogram
ggplot(group_Experimental, aes(x = as.numeric(TOF.BASAL), fill = factor(GRUPO.0.1))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.25,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Dosis Rocuronio (mg)",
       y="Frequencia",
       title="Histograma de TOF basal - Experimental") +
  scale_fill_discrete(labels = c("Experimental")) +
  theme(plot.title = element_text(size=13))


## ----echo=FALSE------------------------
# Histogram
ggplot(group_Experimental, aes(x = T.MIN..INICIO.DE.ACCION, fill = factor(GRUPO.0.1))) +
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity",
                 binwidth = 0.5,
                 alpha = 0.5) +
  labs(fill = "Grupo",
       x="Tiempo de Inicio de Acción (min)",
       y="Frequencia",
       title="Histograma de Tiempo de Inicio de Acción - Experimental") +
  scale_fill_discrete(labels = c("Control")) +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
relaj_Experimental <- data.frame(group_Experimental %>% count(RELAJACION.RESIDUAL...Si1....no.2., sort = FALSE))
relaj_Experimental


## ----echo=FALSE------------------------
ggplot(relaj_Experimental, aes(x = "", y = n, fill = factor(RELAJACION.RESIDUAL...Si1....no.2.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Relajación Residual",
       x=NULL,
       y=NULL,
       title="Proporciones de Relajación Residual del Grupo Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Si", "No")) +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
sugamadex_Experimental <- data.frame(group_Experimental %>% count(SUGAMADEX..SI.1...NO.2., sort = FALSE))
sugamadex_Experimental


## ----echo=FALSE------------------------
ggplot(sugamadex_Experimental, aes(x = "", y = n, fill = factor(SUGAMADEX..SI.1...NO.2.))) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Sugamadex",
       x=NULL,
       y=NULL,
       title="Proporciones de Sugamadex del Grupo Experimental") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Si", "No")) +
  theme(plot.title = element_text(size=12))


## ----echo=FALSE------------------------
neuromusc_data <- data.frame(
  c(rep("control", length(group_control$GRUPO.0.1)), rep("experimental", length(group_control$GRUPO.0.1))),
  c(group_control$DURACION.DEL.BLOQUEO.NEUROMUSCULAR..MIN..25., group_Experimental$DURACION.DEL.BLOQUEO.NEUROMUSCULAR..MIN..25.)
)
colnames(neuromusc_data) <- c("Grupo", "Valor")
neuromusc_data


## ----echo=FALSE------------------------
summary(group_control$DURACION.DEL.BLOQUEO.NEUROMUSCULAR..MIN..25.)


## ----echo=FALSE------------------------
var(group_control$DURACION.DEL.BLOQUEO.NEUROMUSCULAR..MIN..25.)


## ----echo=FALSE------------------------
sd(group_control$DURACION.DEL.BLOQUEO.NEUROMUSCULAR..MIN..25.)


## ----echo=FALSE------------------------
summary(group_Experimental$DURACION.DEL.BLOQUEO.NEUROMUSCULAR..MIN..25.)


## ----echo=FALSE------------------------
var(group_Experimental$DURACION.DEL.BLOQUEO.NEUROMUSCULAR..MIN..25.)


## ----echo=FALSE------------------------
sd(group_Experimental$DURACION.DEL.BLOQUEO.NEUROMUSCULAR..MIN..25.)


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- neuromusc_data %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  neuromusc_data, x = "Grupo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Grupo",
       y="Duración Bloqueo Neuromuscular",
       title="Comparación de Duración del Bloqueo Neuromuscular entre Grupos") +
  theme(plot.title = element_text(size=10))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Grupo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )


## ----echo=FALSE------------------------
recup_data <- data.frame(
  c(rep("control", length(group_control$GRUPO.0.1)), rep("experimental", length(group_control$GRUPO.0.1))),
  c(group_control$RECUPERACION.ESPONTANEA..MIN., group_Experimental$RECUPERACION.ESPONTANEA..MIN.)
)
colnames(recup_data) <- c("Grupo", "Valor")
# Remove NAs
recup_data <- recup_data[!is.na(as.numeric(recup_data$Valor)),]
recup_data$Valor <- as.numeric(recup_data$Valor)
recup_data


## ----echo=FALSE------------------------
summary(na.omit(as.numeric(group_control$RECUPERACION.ESPONTANEA..MIN.)))


## ----echo=FALSE------------------------
var(na.omit(as.numeric(group_control$RECUPERACION.ESPONTANEA..MIN.)))


## ----echo=FALSE------------------------
sd(na.omit(as.numeric(group_control$RECUPERACION.ESPONTANEA..MIN.)))


## ----echo=FALSE------------------------
summary(na.omit(as.numeric(group_Experimental$RECUPERACION.ESPONTANEA..MIN.)))


## ----echo=FALSE------------------------
var(na.omit(as.numeric(group_Experimental$RECUPERACION.ESPONTANEA..MIN.)))


## ----echo=FALSE------------------------
sd(na.omit(as.numeric(group_Experimental$RECUPERACION.ESPONTANEA..MIN.)))


## ----echo=FALSE------------------------
# Statistical Test <- Group by Times
stat.test <- recup_data %>%
  wilcox_test(Valor ~ Grupo) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


## ----echo=FALSE------------------------
# Grouped Boxplots
bxp <- ggboxplot(
  recup_data, x = "Grupo", y = "Valor", 
  color = "Grupo", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(fill="Grupo",
       x="Grupo",
       y="Recuperación Espontánea (min)",
       title="Comparación de Recuperación Espontánea (min) entre Grupos") +
  theme(plot.title = element_text(size=10))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Grupo", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0
  )

