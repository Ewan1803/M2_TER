library(lme4)
library(ez)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(reshape2)

# Lecture des données
donnees <- read_excel("C:\\Users\\ewanm\\OneDrive\\Documents\\M2\\TER\\Motimus\\data\\first_results_ewan\\real_d_value.xlsx")

# Assurez-vous que dlpfc est bien numérique
donnees$dlpfc <- as.numeric(donnees$dlpfc)

# Renommer les sessions
donnees <- donnees %>%
  mutate(ses = recode(ses, "music" = "Musique", "control" = "Contrôle", "audiobook" = "Audiobook"))

# Calcul des moyennes par condition
resultats <- donnees %>%  
  group_by(ses) %>%
  summarise(Moyenne_delta = mean(dlpfc, na.rm = TRUE))

# Affichage des résultats
print(resultats)

############Visualisations graphiques##############

ggplot(donnees) +
  aes(x = ses, y = dlpfc, fill = ses) +  # Ajouter 'fill = ses' pour des couleurs différentes par condition
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette = "Set3") +  # Utiliser une palette de couleurs prédéfinie
  labs(x = "Condition", 
       y = "Apparition du déclin de l'oxygénation (en s)", 
       fill = "Condition") +  # Renommer le titre de la légende
  ggthemes::theme_solarized() +
  theme(plot.title = element_text(hjust = 0.5, vjust = -1.5, face = "bold"))


#############ANOVA#################

# Utilisation de la fonction aov() pour l'ANOVA à mesures répétées
#modele_anova_repeated <- aov(time ~ ses + Error(sub/ses), data = donnees_filtrees)

# Résumé du modèle ANOVA à mesures répétées
#summary(modele_anova_repeated)

#Utilisation de la fonction ezANOVA
anova_rep <- ezANOVA(data = donnees, dv = .(dlpfc), wid = .(sub), within = .(ses), detailed = FALSE)
anova_rep


############Vérification de la normalité multivariée des résidus############

# Modèle mixte avec effets aléatoires pour chaque sujet
mod <- lmer(dlpfc ~ ses + (1|sub), data = donnees)

# Extraction des résidus
residus <- residuals(mod)

# Test de normalité sur les résidus
shapiro_test_result <- shapiro.test(residus)
print(shapiro_test_result)

#Pas de normalité donc Friedman
friedman_data <- dcast(donnees, sub ~ ses, value.var = "dlpfc")
friedman_data_matrix <- as.matrix(friedman_data[, -1])  # Exclure la colonne des participants
friedman_result_base <- friedman.test(friedman_data_matrix)
print(friedman_result_base)

