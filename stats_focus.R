library(lme4)
library(ez)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(grid)

# Lecture des données
donnees <- read_excel("C:\\Users\\ewanm\\OneDrive\\Documents\\M2\\TER\\Motimus\\data\\first_results_ewan\\Focus.xlsx")

# Restructurer les données de format large à format long
donnees_long <- pivot_longer(donnees,
                             cols = -Participant,
                             names_to = "Condition",
                             values_to = "Focus")

# Conversion de la colonne en numérique si nécessaire
donnees_long$Focus <- as.numeric(as.character(donnees_long$Focus))

# Renommer les sessions
donnees <- donnees %>%
  mutate(ses = recode(ses, "music" = "Musique", "control" = "Contrôle", "audiobook" = "Audiobook"))

# Calcul des moyennes par condition 
resultats <- donnees_long %>%
  group_by(Condition) %>%
  summarise(Moyenne_Focus = mean(Focus, na.rm = TRUE)) %>%
  mutate(Moyenne_Focus = round(Moyenne_Focus, 4))

# Affichage des résultats
print(resultats)



############ Visualisations graphiques ##############

ggplot(donnees_long) +
  aes(x = Condition, y = Focus, fill = Condition) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Condition", 
       y = "Niveau de focus attentionnel", 
       fill = "Condition",
       caption = "10 : dissociatif\n-10 : associatif") +  # Ajout d'une légende explicative comme sous-titre
  ggthemes::theme_solarized() +
  theme(plot.caption = element_text(size = 12, face = "bold"))

############# ANOVA #################

# Utilisation de la fonction ezANOVA
anova_rep <- ezANOVA(data = donnees_long, dv = .(Focus), wid = .(Participant), within = .(Condition), detailed = FALSE)
anova_rep

############ Vérification de la normalité multivariée des résidus ############

# Modèle mixte avec effets aléatoires pour chaque sujet
mod <- lmer(Focus ~ Condition + (1|Participant), data = donnees_long)

# Extraction des résidus
residus <- residuals(mod)

# Test de normalité sur les résidus
shapiro_test_result <- shapiro.test(residus)
print(shapiro_test_result)
