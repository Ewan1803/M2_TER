library(lme4)
library(ez)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(coin)
library(reshape2)

# Lecture des données
donnees <- read_excel("C:\\Users\\ewanm\\OneDrive\\Documents\\M2\\TER\\Motimus\\data\\first_results_ewan\\PE.xlsx")

# Restructurer les données de format large à format long
donnees_long <- pivot_longer(donnees,
                             cols = -Participant,
                             names_to = "Condition",
                             values_to = "PE")

# Conversion de la colonne en numérique si nécessaire
donnees_long$PE <- as.numeric(as.character(donnees_long$PE))

# Renommer les sessions
donnees <- donnees %>%
  mutate(ses = recode(ses, "music" = "Musique", "control" = "Contrôle", "audiobook" = "Audiobook"))

# Calcul des moyennes par condition
resultats <- donnees_long %>%
  group_by(Condition) %>%
  summarise(Moyenne_PE = mean(PE, na.rm = TRUE)) %>%
  mutate(Moyenne_PE = round(Moyenne_PE, 4))

# Affichage des résultats
print(resultats)

############ Visualisations graphiques #############


ggplot(donnees_long) +
  aes(x = Condition, y = PE, fill = Condition) +  # Ajouter 'fill = ses' pour des couleurs différentes par condition
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +  # Utiliser une palette de couleurs prédéfinie
  labs(x = "Condition", 
       y = "Perception de l'effort", 
       fill = "Condition",
       caption = "10 : Effort intense\n0 : Aucun effort") + # Renommer le titre de la légende
  ggthemes::theme_solarized() +
  theme(plot.caption = element_text(size = 12, face = "bold"))

############# ANOVA #################

# Utilisation de la fonction ezANOVA
anova_rep <- ezANOVA(data = donnees_long, dv = .(PE), wid = .(Participant), within = .(Condition), detailed = FALSE)
anova_rep

############ Vérification de la normalité multivariée des résidus ############

# Modèle mixte avec effets aléatoires pour chaque sujet
mod <- lmer(PE ~ Condition + (1|Participant), data = donnees_long)

# Extraction des résidus
residus <- residuals(mod)

# Test de normalité sur les résidus
shapiro_test_result <- shapiro.test(residus)
print(shapiro_test_result)


####Pas de normalité donc Friedman######
# Test de Friedman

friedman_data <- dcast(donnees_long, Participant ~ Condition, value.var = "PE")
friedman_data_matrix <- as.matrix(friedman_data[, -1])  # Exclure la colonne des participants
friedman_result_base <- friedman.test(friedman_data_matrix)
print(friedman_result_base)
