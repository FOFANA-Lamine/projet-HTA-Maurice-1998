
#============================
writeLines(c(
  paste(rep("=", 80), collapse = ""),
  "                 Contexte",
  paste(rep("=", 80), collapse = ""),
  "",
  "ÉTUDE : Enquête sur l'hypertension artérielle à l'Île Maurice (1998)",
  "OBJECTIF : Analyser la prévalence de l'HTA et identifier les facteurs de risque",
  "POPULATION : 402 habitants adultes de l'Île Maurice",
  "MÉTHODE : Enquête transversale avec questionnaire et mesures cliniques",
  "PROTOCOLE : 3 mesures successives de tension artérielle",
  "",
  "Variables clés :",
  "────────────────────────────────────────────────────",
  "• TASM  = Moyenne tension systolique (TAS2+TAS3)/2",
  "• TADM  = Moyenne tension diastolique (TAD2+TAD3)/2",
  "• HTA   = Hypertension (1 si TASM>140 & TADM>90 OU HTACONNU='oui')",
  "• IMC   = Poids(kg) / Taille(m)²",
  "• cIMC  = normal(<25), surpoids(25-30), obèse(>30)",
  "• SEXE  = H (homme), F (femme)",
  "• ETHNIE= Hindou, Musulman, Créole, Chinois",
  "• AGE   = Âge en années",
  "• SEDENT= 1 (actif, marche >5 miles/semaine), 0 (sédentaire)",
  "────────────────────────────────────────────────────"
  ))



#--------------------------------

# 1. importer les données 
data=read.table("HTA4.txt", dec=",",sep="\t",header=TRUE)
# data
#
mean(data$HTACONNU)
#. Déclaration des variables qualitatives comme facteurs
data$HTACONNU <-as.factor(data$HTACONNU) 
mean(data$HTACONNU)
levels(data$HTACONNU) <- c("non", "oui")
#
mean(data$SEXE)
data$SEXE<-as.factor(data$SEXE)
mean(data$SEXE)
levels(data$SEXE)<-c("non","oui")
data$ETHNIE2<-factor(data$ETHNIE, labels=c('Hindou','Musulman','Creole','Chinois'))
table(data$ETHNIE,data$ETHNIE2)
data$TASM <- (data$TAS2+ data$TAS3)/2
data$TADM <- (data$TAD2+ data$TAD3)/2
table(data$TASM)
table(data$TADM)

data$HTAhnorm <- (data$TASM>140)*(data$TADM>90)*1
##nombre d'HTA horsnorme
sum(data$HTAhnorm)
data$HTA<- data$HTAhnorm | (data$HTACONNU=="oui")
data$HTA <- data$HTA*1
table(data$HTA)



# 
data$IMC <- data$POIDS / ((data$TAILLE/100)^2)  # Taille en mètres



data$cIMC <- ifelse(data$IMC < 25, "normal",
                    ifelse(data$IMC >= 25 & data$IMC < 30, "surpoids", "obese"))



# Convertir en facteur ordonné 
data$cIMC <- ordered(data$cIMC, levels = c("normal", "surpoids", "obese"))


#  Création de la variable ETHNIE2 
data$ETHNIE2 <- factor(data$ETHNIE, labels = c("Hindou", "Musulman", "Creole", "Chinois"))

# Vérification par tableau croisé
table(data$ETHNIE, data$ETHNIE2)

# Création de la variable HTAsev (hypertension sévère)
data$HTAsev <- (data$TASM > 180) * (data$TADM > 110) * 1
sum(data$HTAsev)





# ==================== Analyse conjointe de deux variables  ====================

# Croisement de deux variables qualitatives (Exemple : ETHNIE2 et HTA)
# (a) Tableau de contingence
tab <- table(data$ETHNIE2, data$HTA)
print("Tableau de contingence ETHNIE2 x HTA:")
print(tab)

# (b) Effectifs marginaux
print("Tableau avec effectifs marginaux:")
print(addmargins(tab))

# (c) Pourcentages conditionnels
# En ligne (proportions par ethnie)
print("Pourcentages par ethnie (en ligne):")
print(round(prop.table(tab, 1) * 100, 1))


# (d) Test du chi-deux d'indépendance
print("Test du chi-deux d'indépendance:")
print(summary(tab))



# Problème d'effectifs théoriques pour les Chinois (1 seul individu)
# On crée une variable ETHNIE3 sans les Chinois

data$ETHNIE <- as.integer(data$ETHNIE)
levels(data$ETHNIE) <-c(1,2, 3, 4)
levels(data$ETHNIE)
ethnie_indices <-  which(data$ETHNIE==4) # Exclure les Chinois (code 4)


ETHNIE3 <-data$ETHNIE[-ethnie_indices]    
ETHNIE3 <- factor(ETHNIE3, labels=c('Hindou','Musulman','Creole'))

# Première table : ETHNIE3 × HTA (sans les Chinois)
t1 <- table(ETHNIE3, data$HTA[-ethnie_indices])


# Deuxième table : ETHNIE3 × cIMC (sans les Chinois)
t2 <- table(ETHNIE3, data$cIMC[-ethnie_indices])


# Tableau sans les Chinois
print("Tableau sans les Chinois:")
print(t1)

print("Test du chi-deux sans les Chinois:")
print(summary(t1))

#
addmargins(t2)



# ==================== Analyse croisée SEXE × HTA ====================

# 1. Stat descriptives
tab_sexe_hta <- table(data$SEXE, data$HTA)
cat("Tableau de contingence SEXE × HTA:\n")
print(tab_sexe_hta)

cat("\nTableau avec effectifs marginaux:\n")
print(addmargins(tab_sexe_hta))


cat("\nPourcentages par sexe (en ligne):\n")
pourcentages_ligne <- round(prop.table(tab_sexe_hta, 1) * 100, 1)
print(pourcentages_ligne)

cat("\nPourcentages par statut HTA (en colonne):\n")
pourcentages_colonne <- round(prop.table(tab_sexe_hta, 2) * 100, 1)
print(pourcentages_colonne)

# 2. Test statistique
cat("Test du chi-deux d'indépendance:\n")
result = summary(tab_sexe_hta)
print(result)   




# 3. Interprétation et conclusion

if (result$p.value < 0.05) {
  cat("\nCONCLUSION: Il existe une association statistiquement significative",
      "entre le sexe et l'hypertension (p =", 
      format.pval(result$p.value, digits = 3), ").\n")
  cat("Les femmes et les hommes ne sont pas atteints de la même façon",
      "par l'hypertension.\n")
} else {
  cat("\nCONCLUSION: Il n'existe pas d'association statistiquement significative",
      "entre le sexe et l'hypertension (p =", 
      format.pval(result$p.value, digits = 3), ").\n")
  cat("Les femmes et les hommes sont atteints de façon similaire",
      "par l'hypertension.\n")
}


#====================  Comparaison de deux moyennes ================


#pourl'effectifdechacundesgroupes
table(data$SEXE)


levels(data$SEXE) <-c("F", "H")
levels(data$SEXE)


## boites à moustaches( va quantitatives)

boxplot(TASM ~ SEXE, 
        data = data,
        main = "Boxplot de la TAS moyenne en fonction du Sexe\n(241 Femmes et 161 Hommes)",
        xlab = "Sexe",
        ylab = "TAS moyenne (mmHg)",
        col = c("lightpink", "lightblue"),
        ylim = c(80, 200),  
        notch = TRUE,       
        outline = TRUE)    



# 
moyennes <- tapply(data$TASM, data$SEXE, mean, na.rm = TRUE)
points(x = 1:2, y = moyennes, col = "red", pch = 18, cex = 1.5)

legend("topright", 
       legend = paste(c("Femmes", "Hommes"), ":", round(moyennes, 1), "mmHg"),
       col = "red", 
       pch = 18,
       title = "Tension moyenne",
       cex = 0.9)


grid()



# les statistiques pour l'interprétation
stats_F <- summary(data$TASM[data$SEXE == "F"])
stats_H <- summary(data$TASM[data$SEXE == "H"])


writeLines(c(
  
  "============= Interpretation du graphe ==============",
  "",
  "1. LA BOÎTE (rectangle coloré) :",
  "   - Elle contient la MOITIÉ des personnes du groupe",
  "   - Plus la boîte est haute → plus la tension est élevée",
  "   - Plus la boîte est grande → plus les tensions sont variées",
  "",
  "2. LA LIGNE À L'INTÉRIEUR DE LA BOÎTE :",
  "   - C'est la tension TYPIQUE (la médiane) du groupe",
  "   - 50% des personnes ont une tension AU-DESSUS de cette ligne",
  "   - 50% des personnes ont une tension EN-DESSOUS de cette ligne",
  "",
  "3. LES « MOUSTACHES » (les lignes qui sortent) :",
  "   - Elles montrent l'étendue des tensions « habituelles »",
  "   - Elles vont du minimum au maximum, sauf les valeurs extrêmes",
  "   - Plus les moustaches sont longues → plus les tensions sont dispersées",
  "",
  "4. LES POINTS ROUGES :",
  "   - Ce sont les MOYENNES de chaque groupe",
  "   - Elles nous donnent la tension MOYENNE des personnes",
  "",
  "5. LES POINTS ISOLÉS :",
  "   - Ce sont des personnes avec des tensions TRÈS ÉLEVÉES ou TRÈS BAISSES",
  "   - On les appelle les « valeurs extrêmes »",
  ""
))



writeLines(c(
  "========== Observations =========",
  "",
  paste("Observations pour les femmes :"),
  paste("  • Tension typique :", round(stats_F[3], 1), "mmHg (médiane)"),
  paste("  • Tension moyenne :", round(moyennes["F"], 1), "mmHg"),
  paste("  • 50% des femmes ont entre", round(stats_F[2], 1), "et", round(stats_F[5], 1), "mmHg"),
  paste("  • La moitié des femmes a plus de", round(stats_F[3], 1), "mmHg"),
  "",
  paste("Observations pour les hommes :"),
  paste("  • Tension typique :", round(stats_H[3], 1), "mmHg (médiane)"),
  paste("  • Tension moyenne :", round(moyennes["H"], 1), "mmHg"),
  paste("  • 50% des hommes ont entre", round(stats_H[2], 1), "et", round(stats_H[5], 1), "mmHg"),
  paste("  • La moitié des hommes a plus de", round(stats_H[3], 1), "mmHg")
))




writeLines(c(
  "================== CONCLUSION ==================",
  "",
  "Synthèse visuelle :",
  "• La boîte des hommes est positionnée plus haut que celle des femmes",
  "• Les points rouges (moyennes) confirment cet écart",
  "",
  "Comparaison quantitative :",
  sprintf("• Écart moyen : %.1f mmHg en faveur des %s", 
          abs(moyennes["H"] - moyennes["F"]),
          ifelse(moyennes["H"] > moyennes["F"], "hommes", "femmes")),
  sprintf("• Médianes : %.1f mmHg (F) vs %.1f mmHg (H)", 
          stats_F[3], stats_H[3]),
  "",
  "Interpretation :",
  ifelse(abs(moyennes["H"] - moyennes["F"]) > 5,
         "• Différence cliniquement notable (>5 mmHg)",
         "• Différence modérée (≤5 mmHg)"),
  ifelse(IQR(data$TASM[data$SEXE == "F"], na.rm = TRUE) > 
           IQR(data$TASM[data$SEXE == "H"], na.rm = TRUE),
         "• Variabilité plus importante chez les femmes",
         "• Variabilité plus importante chez les hommes"),
  "",
  "Conclusion globale :",
  sprintf("Les hommes présentent une tension artérielle systolique"),
  sprintf("moyenne supérieure de %.1f mmHg par rapport aux femmes.", 
          abs(moyennes["H"] - moyennes["F"])),
  "Cette différence visuelle sur le boxplot suggère une influence",
  "du sexe sur la tension artérielle."
))


#=================  

# les moyennes dans chacun des échantillons
t.test(data$TASM~data$SEXE)

# Alternative
# t.test(data$TASM[which(data$SEXE=='F')], data$TASM[which(data$SEXE=='H')], conf.level=0.95)


# Calcul de l'écart-type de TASM chez les femmes
sd_F <- sd(data$TASM[which(data$SEXE=='F')], na.rm = TRUE)

#Alternative
# sd(data$TASM[data$SEXE == 'F'], na.rm = TRUE)

sd_H <- sd(data$TASM[data$SEXE == 'H'], na.rm = TRUE)



writeLines(c(
  "================== Ecart-Type de TASM ==================",
  "",
  "Variabilité autour de la moyenne :",
  sprintf("• Femmes : écart-type = %.1f mmHg", sd_F),
  sprintf("• Hommes : écart-type = %.1f mmHg", sd_H),
  "",
  "Interpretation :",
  "L'écart-type mesure la dispersion des valeurs.",
  "Plus il est grand, plus les tensions sont variables.",
  "",
  "Comparaison :",
  ifelse(sd_F > sd_H,
         sprintf("• Les femmes ont une variabilité plus importante (+%.1f mmHg)", 
                 sd_F - sd_H),
         sprintf("• Les hommes ont une variabilité plus importante (+%.1f mmHg)", 
                 sd_H - sd_F)),
  ""
))


#


#  IMC normal vs surpoids+obèse

# =================== Analyse comparative ===================

#  HTA vs non-HTA
# Calcul des statistiques
tas_hta <- data$TASM[data$HTA == 1]
tas_non_hta <- data$TASM[data$HTA == 0]

moy_hta <- mean(tas_hta, na.rm = TRUE)
moy_non_hta <- mean(tas_non_hta, na.rm = TRUE)
sd_hta <- sd(tas_hta, na.rm = TRUE)
sd_non_hta <- sd(tas_non_hta, na.rm = TRUE)
n_hta <- sum(!is.na(tas_hta))
n_non_hta <- sum(!is.na(tas_non_hta))

# Test t
test_hta <- t.test(TASM ~ HTA, data = data)

cat(sprintf("HTA+ (n=%d) : %.2f (sd=%.2f)\n", n_hta, moy_hta, sd_hta))
cat(sprintf("HTA- (n=%d) : %.2f (sd=%.2f)\n", n_non_hta, moy_non_hta, sd_non_hta))
cat(sprintf("Test t : t=%.2f, p=%s\n", 
            test_hta$statistic, 
            format.pval(test_hta$p.value, digits = 3)))
cat(sprintf("Différence : %.2f mmHg\n", moy_hta - moy_non_hta))


# Résultat attendu significatif
# Car la variable HTA a été construite à partir de la TASM
# HTA = 1 si TASM > 140 mmHg ou TADM > 90 mmHg ou HTACONNU = "oui"
# Donc par définition, les hypertendus ont des TASM plus élevées


#
# Création d'une variable binaire IMC
data$IMC_cat <- ifelse(data$cIMC == "normal", "normal", "surpoids_obese")

# Calcul des statistiques
tas_normal <- data$TASM[data$IMC_cat == "normal"]
tas_surpoids_obese <- data$TASM[data$IMC_cat == "surpoids_obese"]


moy_normal <- mean(tas_normal, na.rm = TRUE)
moy_surpoids_obese <- mean(tas_surpoids_obese, na.rm = TRUE)

sd_normal <- sd(tas_normal, na.rm = TRUE)
sd_surpoids_obese <- sd(tas_surpoids_obese, na.rm = TRUE)

n_normal <- sum(!is.na(tas_normal))
n_surpoids_obese <- sum(!is.na(tas_surpoids_obese))

# Test t
test_imc_binaire <- t.test(TASM ~ IMC_cat, data = data)

cat(sprintf("IMC normal (n=%d) : %.2f (sd=%.2f)\n", n_normal, moy_normal, sd_normal))
cat(sprintf("Surpoids+obèse (n=%d) : %.2f (sd=%.2f)\n", n_surpoids_obese, moy_surpoids_obese, sd_surpoids_obese))
cat(sprintf("Test t : t=%.2f, p=%s\n", 
            test_imc_binaire$statistic, 
            format.pval(test_imc_binaire$p.value, digits = 3)))
cat(sprintf("Différence : %.2f mmHg\n", moy_surpoids_obese - moy_normal))

cat("Hypothèse :")
cat(ifelse(test_imc_binaire$p.value < 0.05, 
           "→ Différence significative (p < 0.05) (IMC n'est pas lié à tension artérielle)",
           "→ Différence non significative (p ≥ 0.05) (IMC lié à tension artérielle)"))




# ================== Comparaison de deux moyennes appariées ==============

mean(data$TAS1)
mean(data$TAS2)

t.test(data$TAS1, data$TAS2,paired=TRUE)

D<-data$TAS1- data$TAS2
t.test(D)



# Analyse effet Blouse Blanche entre TAS1 et TAS2

# Calcul des moyennes
moy_tas1 <- mean(data$TAS1, na.rm = TRUE)
moy_tas2 <- mean(data$TAS2, na.rm = TRUE)

# Test t apparié
test_apparie <- t.test(data$TAS1, data$TAS2, paired = TRUE)


cat("Effet Blouse Blanche : Comparaison TAS1 vs TAS2\n")
cat(sprintf("TAS1 (première mesure): %.1f mmHg\n", moy_tas1))
cat(sprintf("TAS2 (deuxième mesure) : %.1f mmHg\n", moy_tas2))
cat(sprintf("Différence moyenne : %.1f mmHg\n", moy_tas1 - moy_tas2))
cat(sprintf("Test t apparié : t(%.0f) = %.2f, p = %s\n",
            test_apparie$parameter,
            test_apparie$statistic,
            ifelse(test_apparie$p.value < 0.001, 
                   "< 0.001", 
                   sprintf("%.3f", test_apparie$p.value))))
cat(sprintf("IC 95%% différence : [%.1f ; %.1f] mmHg\n",
            test_apparie$conf.int[1], test_apparie$conf.int[2]))




writeLines(c(
  "",
  paste(rep("═", 50), collapse = ""),
  "        Analyse effet Blouse Blanche",
  paste(rep("═", 50), collapse = ""),
  "",
  "Interpretation :",
  paste(rep("─", 35), collapse = ""),
  "• Première mesure (TAS1) > Deuxième mesure (TAS2)",
  sprintf("• Écart moyen : %.1f mmHg", moy_tas1 - moy_tas2),
  "• Phénomène « blouse blanche » confirmé",
  "• Diminution tensionnelle significative",
  "",
  "Conclusion :",
  paste(rep("─", 35), collapse = ""),
  sprintf("La tension diminue significativement entre les deux"),
  sprintf("premières mesures (%.1f → %.1f mmHg, Δ = %.1f mmHg, p < 0.001).",
          moy_tas1, moy_tas2, moy_tas1 - moy_tas2),
  sprintf("Cet effet « blouse blanche » est cliniquement observable."),
  paste(rep("═", 50), collapse = "")
))



# Est-ce que l’on constate un effet blouse blanche entre la TAD2 et la TAD3 ?

# On fera paraillement comme ci-dessus 

# Calcul des moyennes
moy_tad2 <- mean(data$TAD2, na.rm = TRUE)
moy_tad3 <- mean(data$TAD3, na.rm = TRUE)
difference_tad <- moy_tad2 - moy_tad3

# Test t apparié
test_tad <- t.test(data$TAD2, data$TAD3, paired = TRUE)


writeLines(c(
  paste(rep("═", 60), collapse = ""),
  "      Analyse effet Blouse Blanche : TAD2 vs TAD3",
  paste(rep("═", 60), collapse = ""),
  "",
  "Stat descriptives :",
  paste(rep("─", 35), collapse = ""),
  sprintf("• TAD2 (2ème mesure) : %.1f mmHg", moy_tad2),
  sprintf("• TAD3 (3ème mesure) : %.1f mmHg", moy_tad3),
  sprintf("• Différence moyenne : %.1f mmHg", difference_tad),
  "",
  "Test statistiques :",
  paste(rep("─", 35), collapse = ""),
  sprintf("Test t apparié : t(%.0f) = %.2f", 
          test_tad$parameter, test_tad$statistic),
  sprintf("p-value = %s", 
          ifelse(test_tad$p.value < 0.001, 
                 "< 0.001", 
                 sprintf("%.3f", test_tad$p.value))),
  sprintf("IC 95%% : [%.2f ; %.2f] mmHg", 
          test_tad$conf.int[1], test_tad$conf.int[2]),
  "",
  "Interpretation :",
  paste(rep("─", 35), collapse = ""),
  if(test_tad$p.value < 0.05) {
    c("→ Effet « blouse blanche » SIGNIFICATIF",
      sprintf("→ TAD2 > TAD3 (différence = %.1f mmHg)", difference_tad),
      "→ La tension diastolique diminue entre les mesures")
  } else {
    c("→ Effet « blouse blanche » NON SIGNIFICATIF",
      "→ Pas de différence significative entre TAD2 et TAD3",
      "→ La tension diastolique reste stable entre les mesures")
  },
  "",
  "Contexte :",
  paste(rep("─", 35), collapse = ""),
  "• Comparaison : mesure 2 (TAD2) vs mesure 3 (TAD3)",
  "• Test apparié : mêmes patients pour les deux mesures",
  "• Seuil de significativité : α = 0.05",
  "",
  "Conclusion :",
  paste(rep("─", 35), collapse = ""),
  if(test_tad$p.value < 0.05) {
    sprintf("On observe un effet « blouse blanche » significatif pour la TAD entre la 2ème (%.1f mmHg) et la 3ème mesure (%.1f mmHg) avec une diminution moyenne de %.1f mmHg (p %s).",
            moy_tad2, moy_tad3, abs(difference_tad),
            ifelse(test_tad$p.value < 0.001, "< 0.001", 
                   sprintf("= %.3f", test_tad$p.value)))
  } else {
    sprintf("Pas d'effet « blouse blanche » significatif pour la TAD entre la 2ème (%.1f mmHg) et la 3ème mesure (%.1f mmHg) : différence non significative de %.1f mmHg (p = %.3f).",
            moy_tad2, moy_tad3, abs(difference_tad), test_tad$p.value)
  },
  paste(rep("═", 60), collapse = "")
))


#================= Analyse complète ===============


cat("\n")
writeLines(c(
  paste(rep("=", 70), collapse = ""),
  "                 Analyse complète de notre jeu de données",
  paste(rep("=", 70), collapse = ""),
  ""
))

cat("1. Description des individus\n")
cat(paste(rep("-", 40), collapse = ""), "\n")

# Description IMC
writeLines(c(
  "Indice de masse corporelle (IMC) :",
  sprintf("   • Moyenne : %.1f kg/m²", mean(data$IMC, na.rm = TRUE)),
  sprintf("   • Écart-type : %.1f kg/m²", sd(data$IMC, na.rm = TRUE)),
  sprintf("   • Min : %.1f | Max : %.1f kg/m²", 
          min(data$IMC, na.rm = TRUE), max(data$IMC, na.rm = TRUE)),
  ""
))

# Description groupe IMC
cat("Répartition statut pondéral :\n")
tab_cIMC <- table(data$cIMC)
for(i in 1:length(tab_cIMC)) {
  pourcent <- tab_cIMC[i]/ sum(tab_cIMC)*100
  cat(sprintf("   • %s : %d (%.1f%%)\n", 
              names(tab_cIMC)[i], tab_cIMC[i], pourcent))
}
cat("\n")


cat("Répartition ethniquen :\n")
tab_ethnie <- table(data$ETHNIE2)
for(i in 1:length(tab_ethnie)) {
  pourcent <- tab_ethnie[i]/sum(tab_ethnie)*100
  cat(sprintf("   • %s : %d (%.1f%%)\n", 
              names(tab_ethnie)[i], tab_ethnie[i], pourcent))
}
cat("\n\n")


cat("2. Lien Pondéral statut - Sexe\n")
cat(paste(rep("-", 40), collapse = ""), "\n")

tab_sexe_cIMC <- table(data$SEXE, data$cIMC)
test_chi2_sexe_imc <- chisq.test(tab_sexe_cIMC, correct = TRUE)

writeLines(c(
  "Tableau croisé :",
  sprintf("   • Test du chi-deux : χ²(%.0f) = %.2f, p = %s",
          test_chi2_sexe_imc$parameter, test_chi2_sexe_imc$statistic,
          ifelse(test_chi2_sexe_imc$p.value < 0.001, "<0.001", 
                 sprintf("%.3f", test_chi2_sexe_imc$p.value))),
  "",
  "Pourcentages par sexe :"
))

# Pourcentages par sexe
pourcentages <- prop.table(tab_sexe_cIMC, 1) * 100
for(sexe in rownames(pourcentages)) {
  cat(sprintf("   • %s : ", ifelse(sexe == "F", "Femmes", "Hommes")))
  cat(sprintf("%s=%.1f%%, ", names(pourcentages[sexe,]), pourcentages[sexe,]))
  cat("\n")
}

cat("\n")
if(test_chi2_sexe_imc$p.value < 0.05) {
  cat(" ssociation SIGNIFICATIVE entre sexe et statut pondéral\n")
} else {
  cat(" Pas d'association significative entre sexe et statut pondéral\n")
}
cat("\n\n")


cat("3. IMC vs Activité physique (SEDENT=1 : marche > 5 miles/semaine)\n")
cat(paste(rep("-", 40), collapse = ""), "\n")

# Création d'une variable binaire pour SEDENT
# Note: SEDENT=1 signifie "non sédentaire" (marche > 5 miles/semaine)
data$ACTIF <- ifelse(data$SEDENT == 1, "Actif", "Sédentaire")

imc_actif <- data$IMC[data$ACTIF == "Actif"]
imc_sedentaire <- data$IMC[data$ACTIF == "Sédentaire"]

test_imc_sedent <- t.test(imc_actif, imc_sedentaire)

writeLines(c(
  sprintf("Comparaison IMC :"),
  sprintf("   • Actifs (marche > 5 miles/semaine, n=%d) : %.1f ± %.1f kg/m²", 
          length(na.omit(imc_actif)), mean(imc_actif, na.rm = TRUE), sd(imc_actif, na.rm = TRUE)),
  sprintf("   • Sédentaires (n=%d) : %.1f ± %.1f kg/m²",
          length(na.omit(imc_sedentaire)), mean(imc_sedentaire, na.rm = TRUE), sd(imc_sedentaire, na.rm = TRUE)),
  "",
  sprintf("test statistique :"),
  sprintf("   • Test t : t = %.2f, p = %s",
          test_imc_sedent$statistic,
          ifelse(test_imc_sedent$p.value < 0.001, "<0.001", 
                 sprintf("%.3f", test_imc_sedent$p.value))),
  sprintf("   • Différence : %.1f kg/m²", 
          mean(imc_actif, na.rm = TRUE) - mean(imc_sedentaire, na.rm = TRUE))
))

cat("\n")
if(test_imc_sedent$p.value < 0.05) {
  if(mean(imc_actif, na.rm = TRUE) < mean(imc_sedentaire, na.rm = TRUE)) {
    cat(" Les personnes actives ont un IMC SIGNIFICATIVEMENT PLUS BAS\n")
  } else {
    cat(" Les personnes actives ont un IMC SIGNIFICATIVEMENT PLUS ÉLEVÉ\n")
  }
} else {
  cat(" Pas de différence significative d'IMC selon l'activité physique\n")
}
cat("\n\n")


cat("4. Intervalle de confiance\n")
cat(paste(rep("-", 40), collapse = ""), "\n")

# IC pour proportion HTAhnorm
p_htahnorm <- mean(data$HTAhnorm, na.rm = TRUE)
n_total <- sum(!is.na(data$HTAhnorm))
ic_prop <- prop.test(sum(data$HTAhnorm, na.rm = TRUE), n_total)

# IC pour moyenne IMC
ic_imc <- t.test(data$IMC)

writeLines(c(
  "Intervalle de confiance pour HTAhnorm :",
  sprintf("   • Proportion observée : %.1f%% (n=%d/%d)", 
          p_htahnorm*100, sum(data$HTAhnorm, na.rm = TRUE), n_total),
  sprintf("   • IC 95%% : [%.1f%% ; %.1f%%]", 
          ic_prop$conf.int[1]*100, ic_prop$conf.int[2]*100),
  sprintf("   • Hypothèse : échantillon aléatoire représentatif"),
  "",
  "Intervalle de confiance pour l'IMC :",
  sprintf("   • Moyenne observée : %.1f kg/m²", mean(data$IMC, na.rm = TRUE)),
  sprintf("   • IC 95%% : [%.1f ; %.1f] kg/m²", 
          ic_imc$conf.int[1], ic_imc$conf.int[2]),
  sprintf("   • Hypothèse : distribution normale ou n suffisamment grand")
))
cat("\n\n")


cat("5. Reherche de facteurs de risque de  l'HTA\n")
cat(paste(rep("-", 40), collapse = ""), "\n")

# Exemple avec SEDENT
tab_sedent_hta <- table(data$ACTIF, data$HTA)
test_sedent_hta <- chisq.test(tab_sedent_hta)

writeLines(c(
  "Exemple : HTA vs Activité physique",
  sprintf("   • Test du chi-deux : χ²(%.0f) = %.2f, p = %s",
          test_sedent_hta$parameter, test_sedent_hta$statistic,
          ifelse(test_sedent_hta$p.value < 0.001, "<0.001", 
                 sprintf("%.3f", test_sedent_hta$p.value)))
))
cat("\n\n")


# 6. Liens ÂGE-TASM, IMC-TADM

cat("6. Analyse des corrélations\n")
cat(paste(rep("-", 40), collapse = ""), "\n")

# Corrélation Âge - TASM
cor_age_tasm <- cor(data$AGE, data$TASM, use = "complete.obs")
print(cor_age_tasm)
test_cor_age <- cor.test(data$AGE, data$TASM)
print(test_cor_age)

# Corrélation IMC - TADM
cor_imc_tadm <- cor(data$IMC, data$TADM, use = "complete.obs")
test_cor_imc <- cor.test(data$IMC, data$TADM)

writeLines(c(
  "Corrrélation ÂGE - TASM :",
  sprintf("   • Coefficient de corrélation : r = %.3f", cor_age_tasm),
  sprintf("   • Test : t = %.2f, p = %s",
          test_cor_age$statistic,
          ifelse(test_cor_age$p.value < 0.001, "<0.001", 
                 sprintf("%.3f", test_cor_age$p.value))),
  
  sprintf("   • Interprétation : La TASM %s avec l'âge", 
          ifelse(cor_age_tasm > 0, "augmente", "diminue")),
  "",
  " Corrélation IMC - TADM :",
  sprintf("   • Coefficient de corrélation : r = %.3f", cor_imc_tadm),
  sprintf("   • Test : t = %.2f, p = %s",
          test_cor_imc$statistic,
          ifelse(test_cor_imc$p.value < 0.001, "<0.001", 
                 sprintf("%.3f", test_cor_imc$p.value))),
  sprintf("   • Interprétation : La TADM  %s avec l'IMC", 
          ifelse(cor_imc_tadm > 0, "augmente", "diminue"))
))
cat("\n\n")


#

cat(paste(rep("=", 50), collapse = ""), "\n\n")
cat("7. Recherche des facteurs modifiants la TASM\n")
cat(paste(rep("=", 50), collapse = ""), "\n\n")

# A. Facteurs qualitatifs (ANOVA ou test t)
cat("A. Facteurs qualitatifs influencant la TASM\n")
cat(paste(rep("-", 40), collapse = ""), "\n")

cat("1. Ethnie (ANOVA) :\n")
anova_ethnie <- aov(TASM ~ ETHNIE2, data = data)
summary_ethnie <- summary(anova_ethnie)
p_ethnie <- summary_ethnie[[1]]$"Pr(>F)"[1]

cat(sprintf("   • F(%d, %d) = %.2f, p = %s\n",
            summary_ethnie[[1]]$Df[1],
            summary_ethnie[[1]]$Df[2],
            summary_ethnie[[1]]$"F value"[1],
            ifelse(p_ethnie < 0.001, "<0.001", sprintf("%.3f", p_ethnie))))

# Moyennes par ethnie
moyennes_ethnie <- tapply(data$TASM, data$ETHNIE2, mean, na.rm = TRUE)
for(i in 1:length(moyennes_ethnie)) {
  cat(sprintf("   • %s : %.1f mmHg\n", names(moyennes_ethnie)[i], moyennes_ethnie[i]))
}
cat("\n")

# 2. Profession
cat("2. Pofession (test ANOVA) :\n")
anova_prof <- aov(TASM ~ as.factor(PROFES), data = data)
summary_prof <- summary(anova_prof)
p_prof <- summary_prof[[1]]$"Pr(>F)"[1]

cat(sprintf("   • F(%d, %d) = %.2f, p = %s\n",
            summary_prof[[1]]$Df[1],
            summary_prof[[1]]$Df[2],
            summary_prof[[1]]$"F value"[1],
            ifelse(p_prof < 0.001, "<0.001", sprintf("%.3f", p_prof))))
cat("\n")

# 3. Domicile (urbain/rural) - test t
cat("3. Domicille (test t) :\n")

# Supposons DOMICILE=1 urbain, DOMICILE=0 rural
test_domicile <- t.test(TASM ~ DOMICILE, data = data)
cat(sprintf("   • Urbain vs Rural : t = %.2f, p = %s\n",
            test_domicile$statistic,
            ifelse(test_domicile$p.value < 0.001, "<0.001", 
                   sprintf("%.3f", test_domicile$p.value))))
cat(sprintf("   • Différence : %.1f mmHg\n",
            diff(test_domicile$estimate)))
cat("\n")


# B. Facteurs quantitatifs (corrélations)
cat("B. Facteurs quantitatifs corrélés à la TASM\n")
cat(paste(rep("-", 40), collapse = ""), "\n")

cat("1. Âge :\n")
cor_age <- cor.test(data$AGE, data$TASM, use = "complete.obs")
cat(sprintf("   • r = %.3f, t = %.2f, p = %s\n",
            cor_age$estimate,
            cor_age$statistic,
            ifelse(cor_age$p.value < 0.001, "<0.001", 
                   sprintf("%.3f", cor_age$p.value))))
cat(sprintf("   • La TASM %s de %.1f mmHg par décennie\n",
            ifelse(cor_age$estimate > 0, "augmente", "diminue"),
            abs(cor_age$estimate) * 10 * sd(data$TASM, na.rm = TRUE) / sd(data$AGE, na.rm = TRUE)))
cat("\n")


cat("2. IMC :\n")
cor_imc <- cor.test(data$IMC, data$TASM, use = "complete.obs")
cat(sprintf("   • r = %.3f, t = %.2f, p = %s\n",
            cor_imc$estimate,
            cor_imc$statistic,
            ifelse(cor_imc$p.value < 0.001, "<0.001", 
                   sprintf("%.3f", cor_imc$p.value))))
cat("\n")

#
cat("3. Poids :\n")
cor_poids <- cor.test(data$POIDS, data$TASM, use = "complete.obs")
cat(sprintf("   • r = %.3f, t = %.2f, p = %s\n",
            cor_poids$estimate,
            cor_poids$statistic,
            ifelse(cor_poids$p.value < 0.001, "<0.001", 
                   sprintf("%.3f", cor_poids$p.value))))
cat("\n")



cat("\nC. Synthèse des facteurs significatifs\n")
cat(paste(rep("-", 40), collapse = ""), "\n")

writeLines(c(
  " Facteurs significatifs (p < 0.05) :",
  if(p_ethnie < 0.05) "   • Ethnie",
  if(cor_age$p.value < 0.05) sprintf("   • Âge (r = %.2f)", cor_age$estimate),
  if(cor_imc$p.value < 0.05) sprintf("   • IMC (r = %.2f)", cor_imc$estimate),
  "",
  " Hiérachie d'influence :",
  "   1. Âge (le facteur le plus influent)",
  "   2. IMC",
  "   3. Ethnie",
  "   4. Sexe",
  "",
  " Implications cliniques :",
  "   • Surveiller particulièrement les patients âgés",
  "   • Contrôler le poids pour réduire la tension",
  "   • Adapter la prévention selon l'ethnie"
))

cat(paste(rep("=", 50), collapse = ""), "\n")


cat("\n")
writeLines(c(
  paste(rep("=", 70), collapse = ""),
  "                     Fin de l'analyse",
  paste(rep("=", 70), collapse = ""),
  ""
))







