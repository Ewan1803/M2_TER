library(lme4)
library(ez)
library(readxl)
library(dplyr)
library(ggplot2)

# Lecture des données
donnees <- read_excel("C:\\Users\\ewanm\\OneDrive\\Documents\\M2\\TER\\Motimus\\data\\first_results_ewan\\time_to_exhaustion.xlsx")

# Renommer les sessions
donnees <- donnees %>%
  mutate(ses = recode(ses, "music" = "Musique", "control" = "Contrôle", "audiobook" = "Audiobook"))

# Calcul des moyennes par condition
resultats <- donnees %>%
  group_by(ses) %>%
  summarise(Moyenne_Durée = mean(time, na.rm = TRUE))

# Affichage des résultats
print(resultats)


############Visualisations graphiques##############

ggplot(donnees) +
  aes(x = ses, y = time, fill = ses) +  # Ajouter 'fill = ses' pour des couleurs différentes par condition
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +  # Utiliser une palette de couleurs prédéfinie
  labs(x = "Condition", 
       y = "Temps d'effort jusqu'à épuisement (en s)", 
       fill = "Condition") +  # Renommer le titre de la légende
  ggthemes::theme_solarized() +
  theme(plot.title = element_text(hjust = 0.5, vjust = -1.5, face = "bold"))


#############ANOVA#################

# Utilisation de la fonction aov() pour l'ANOVA à mesures répétées
#modele_anova_repeated <- aov(time ~ ses + Error(sub/ses), data = donnees)

# Résumé du modèle ANOVA à mesures répétées
#summary(modele_anova_repeated)

#Utilisation de la fonction ezANOVA
anova_rep <- ezANOVA(data = donnees, dv = .(time), wid = .(sub), within = .(ses), detailed = FALSE)
anova_rep


############Vérification de la normalité multivariée des résidus############

# Modèle mixte avec effets aléatoires pour chaque sujet
mod <- lmer(time ~ ses + (1|sub), data = donnees)

# Extraction des résidus
residus <- residuals(mod)

# Test de normalité sur les résidus
shapiro_test_result <- shapiro.test(residus)
print(shapiro_test_result)

