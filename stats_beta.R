library(lme4)
library(ez)
library(readxl)
library(dplyr)
library(ggplot2)

# Lecture des données
donnees <- read_excel("C:\\Users\\ewanm\\OneDrive\\Documents\\M2\\TER\\Motimus\\data\\first_results_ewan\\beta_value.xlsx")


# Filtrage des données pour exclure le participant 23 et calcul des moyennes par condition
resultats <- donnees %>%
  filter(sub != 23) %>%  # Assurez-vous que 'Participant' est le nom correct de la colonne
  group_by(ses) %>%
  summarise(Moyenne_beta = mean(occipital_slope, na.rm = TRUE))

# Affichage des résultats
print(resultats)

# Filtrage pour exclure le participant 23
donnees_filtrees <- donnees %>%
  filter(sub != 23)

# Imputation simple avec la médiane, par exemple
donnees_filtrees$occipital_slope <- ifelse(is.na(donnees_filtrees$occipital_slope),
                                        mean(donnees_filtrees$occipital_slope, na.rm = TRUE),
                                        donnees_filtrees$occipital_slope)

############Visualisations graphiques##############


ggplot(donnees_filtrees) +
  aes(x = ses, y = occipital_slope) +
  geom_boxplot(fill = "#46B0B4") +
  labs(x = "Condition", 
       y = "Activation moyenne du cortex occipital") +
  ggthemes::theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5, vjust = -1.5, face = "bold"))


#############ANOVA#################

# Utilisation de la fonction aov() pour l'ANOVA à mesures répétées
#modele_anova_repeated <- aov(time ~ ses + Error(sub/ses), data = donnees_filtrees)

# Résumé du modèle ANOVA à mesures répétées
#summary(modele_anova_repeated)

#Utilisation de la fonction ezANOVA
anova_rep <- ezANOVA(data = donnees_filtrees, dv = .(occipital_slope), wid = .(sub), within = .(ses), detailed = FALSE)
anova_rep


############Vérification de la normalité multivariée des résidus############

# Modèle mixte avec effets aléatoires pour chaque sujet
mod <- lmer(occipital_slope ~ ses + (1|sub), data = donnees_filtrees)

# Extraction des résidus
residus <- residuals(mod)

# Test de normalité sur les résidus
shapiro_test_result <- shapiro.test(residus)
print(shapiro_test_result)

####Pas de normalité donc Friedman######
# Test de Friedman

friedman_data <- dcast(donnees_filtrees, sub ~ ses, value.var = "occipital_slope")
friedman_data_matrix <- as.matrix(friedman_data[, -1])  # Exclure la colonne des participants
friedman_result_base <- friedman.test(friedman_data_matrix)
print(friedman_result_base)

