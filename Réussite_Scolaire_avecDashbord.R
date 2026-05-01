#-------------------------------------------------------------------------------
# Installer + charger les packages
#-------------------------------------------------------------------------------
packages <- c(
  "rcompanion",
  "corrplot",
  "ggplot2",
  "patchwork",
  "broom",
  "dplyr",
  "pscl",
  "randomForest",
  "tidyr",
  "caret",
  "scales",
  "rpart.plot",
  "lubridate",
  "rpart",
  "car",
  "shiny"
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 1. Fonction pour analyse exploratoire
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 1.1 Fonction pour voir les valeurs abérantes
#-------------------------------------------------------------------------------
# 1.1.1 Résumé des variables numériques
#-------------------------------------------------------------------------------
resume_num <- function(table) {
  # Sélection des colonnes numériques
  num_data <- table[sapply(table, is.numeric)]
  
  # Construction du résumé
  res <- data.frame(
    Min = sapply(num_data, function(v) round(min(v, na.rm = TRUE), 1)),
    Moyenne = sapply(num_data, function(v) round(mean(v, na.rm = TRUE), 1)),
    Max = sapply(num_data, function(v) round(max(v, na.rm = TRUE), 1)),
    NAs = sapply(num_data, function(v) sum(is.na(v))),
    
    zeros = sapply(num_data, function(v) {
      round(sum(v == 0, na.rm = TRUE) / sum(!is.na(v)) * 100, 1)
    }),
    
    stringsAsFactors = FALSE
  )
  
  return(res)
}
#-------------------------------------------------------------------------------
# 1.1.2 Résumé des variables qualitatives
#-------------------------------------------------------------------------------
resume_quali <- function(df) {
  
  for (var in names(df)) {
    
    if (is.factor(df[[var]]) | is.character(df[[var]])) {
      
      cat("\n-------------------------\n")
      cat("Variable :", var, "\n")
      
      x <- as.character(df[[var]])
      
      # Nombre de NA et Chaînes vides
      nas <- sum(is.na(x) | trimws(x) == "")
      
      # Modalités visibles avec guillemets
      modalities <- unique(x)
      modalities_affichage <- paste0('"', modalities, '"')
      
      cat("Modalités :", paste(modalities_affichage, collapse = ", "), "\n")
      cat("Valeurs manquantes :", nas, "\n")
    }
  }
}
#-------------------------------------------------------------------------------
# 1.2 Fonction distribution
#-------------------------------------------------------------------------------
# 1.2.1 Histogrammes - variables numériques
#-------------------------------------------------------------------------------
histplot <- function(data, var_names, bins = 30, labels) {
  
  plots <- lapply(var_names, function(var_name) {
    
    ggplot(data, aes(x = .data[[var_name]])) +
      geom_histogram(bins = bins, fill = "steelblue", color = "white") +
      labs(
        title = paste("Distribution de", labels[[var_name]]),
        x = labels[[var_name]],
        y = "Fréquence"
      ) +
      theme_minimal()
  })
  
  patchwork::wrap_plots(plots)
}

#-------------------------------------------------------------------------------
# 1.2.2 Barplots - variables qualitatives
#-------------------------------------------------------------------------------
barplot_quali <- function(data, var_names, labels, labels_levels, ncol = 2) {
  
  library(ggplot2)
  library(patchwork)
  
  plots <- lapply(var_names, function(var_name) {
    
    var_label <- ifelse(var_name %in% names(labels), labels[[var_name]], var_name)
    
    ggplot(data, aes(x = .data[[var_name]])) +
      geom_bar(fill = "steelblue") +
      stat_count(
        aes(label = after_stat(count)), 
        geom = "text", 
        vjust = -0.1
      ) +
      scale_x_discrete(labels = labels_levels) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
        title = paste("Distribution de fréquences -", var_label),
        x = var_label,
        y = "Fréquence"
      ) +
      theme_minimal()
  })
  
  patchwork::wrap_plots(plots, ncol = ncol)
}
#-------------------------------------------------------------------------------
# 1.3 Fonction corrélation
#-------------------------------------------------------------------------------
# 1.3.1 Corrélation des variables numériques
#-------------------------------------------------------------------------------
correlation <- function(data, vars, labels) {
  
  require(corrplot)
  
  vars_df <- data[, vars, drop = FALSE]
  cor_matrix <- cor(vars_df, use = "complete.obs")
  
  colnames(cor_matrix) <- labels[colnames(cor_matrix)]
  rownames(cor_matrix) <- labels[rownames(cor_matrix)]
  
  col <- colorRampPalette(c("red", "green", "darkblue"))(200)
  
  corrplot::corrplot(
    cor_matrix,
    method = "color",
    type = "upper",
    tl.col = "black",
    tl.srt = 45,
    col = col,
    addCoef.col = "black",
    number.cex = 0.8,
    number.digits = 3,
    diag = TRUE,
    mar = c(0, 0, 1, 0),
    is.corr = TRUE
  )
}
#-------------------------------------------------------------------------------
# 1.3.2 Corrélation des variables qualitatives - V de Cramér
#-------------------------------------------------------------------------------
cramer <- function(data, vars, labels) {
  
  require(rcompanion)
  require(corrplot)
  
  df <- data[, vars, drop = FALSE]
  df[] <- lapply(df, as.factor)
  
  n <- ncol(df)
  mat <- matrix(NA, nrow = n, ncol = n)
  colnames(mat) <- colnames(df)
  rownames(mat) <- colnames(df)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        mat[i, j] <- 1
      } else {
        tab <- table(df[[i]], df[[j]])
        mat[i, j] <- suppressWarnings(rcompanion::cramerV(tab))
      }
    }
  }
  
  colnames(mat) <- labels[colnames(mat)]
  rownames(mat) <- labels[rownames(mat)]
  
  col <- colorRampPalette(c("green", "yellow", "red"))(200)
  
  corrplot::corrplot(
    mat,
    method = "color",
    type = "upper",
    tl.col = "black",
    tl.srt = 45,
    col = col,
    addCoef.col = "black",
    number.cex = 0.75,
    number.digits = 3,
    diag = TRUE,
    mar = c(0, 0, 2, 0),
    is.corr = FALSE,
    col.lim = c(0, 1)
  )
}
#-------------------------------------------------------------------------------
# 2. Fonction pour la modélisation
#-------------------------------------------------------------------------------
#------------------------------------------------
# 2.1 Fonction regression logistique
#------------------------------------------------
# 2.1.1 Fonction pour vérifier la linéarité du logit
#------------------------------------------------
verif_logit <- function(data, vars, modele, ncol = 2, labels) {
  
  library(ggplot2)
  library(patchwork)
  
  p <- predict(modele, newdata = data, type = "response")
  
  p[p == 0] <- 1e-6
  p[p == 1] <- 1 - 1e-6
  
  logit <- log(p / (1 - p))
  
  plots <- lapply(vars, function(v) {
    
    df <- data.frame(
      x = data[[v]],
      logit = logit
    )
    
    ggplot(df, aes(x = x, y = logit)) +
      geom_point(alpha = 0.4) +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      geom_smooth(
        method = "gam",
        formula = y ~ s(x, k = 4),
        color = "blue",
        se = FALSE,
        linewidth = 0.5
      ) +
      labs(
        title = paste("Vérification du logit -", labels[[v]]),
        x = labels[[v]],
        y = "Logit"
      ) +
      theme_minimal()
  })
  
  patchwork::wrap_plots(plots, ncol = ncol)
}

#-------------------------------------------------
# 2.1.2 Fonction qui affiche un graphique des VIF
#-------------------------------------------------
plot_vif <- function(modele, labels) {
  
  library(car)
  library(ggplot2)
  library(dplyr)
  library(tibble)
  
  vif_values <- car::vif(modele)
  
  vif_df <- as.data.frame(vif_values) %>%
    rownames_to_column("Variable")
  
  # VIF ajusté
  if ("GVIF^(1/(2*Df))" %in% colnames(vif_df)) {
    vif_df <- vif_df %>%
      mutate(VIF_ajuste = `GVIF^(1/(2*Df))`)
  } else {
    vif_df <- vif_df %>%
      mutate(VIF_ajuste = GVIF)
  }
  
  # Traduction variables
  vif_df$Variable_fr <- sapply(vif_df$Variable, function(v) {
    if (v %in% names(labels)) {
      labels[[v]]
    } else {
      v
    }
  })
  
  ggplot(vif_df, aes(
    y = reorder(Variable_fr, VIF_ajuste),
    x = VIF_ajuste
  )) +
    geom_col() +
    geom_vline(xintercept = 5, linetype = "dashed") +
    labs(
      title = "Multicolinéarité des variables",
      x = "VIF ajusté",
      y = "Variables"
    ) +
    coord_cartesian(xlim = c(0, 10)) +
    theme_minimal(base_size = 14)
}
#-------------------------------------------------
# 2.1.3 Fonction qui affiche un graphique des p-values et des odds ratios
#-------------------------------------------------
logit_graph <- function(modele, labels) {
  
  library(broom)
  library(ggplot2)
  library(dplyr)
  library(pscl)
  
  coef_data <- broom::tidy(modele) %>%
    filter(term != "(Intercept)")
  
  pseudo_r2 <- pscl::pR2(modele)["McFadden"]
  
  coef_data <- coef_data %>%
    mutate(
      odds_ratio = exp(estimate),
      signif = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".",
        TRUE ~ ""
      ),
      term_fr = sapply(term, function(v) {
        if (v %in% names(labels)) {
          labels[[v]]
        } else {
          v
        }
      })
    )
  
  ggplot(coef_data, aes(
    x = reorder(term_fr, -p.value),
    y = -log10(p.value),
    fill = p.value < 0.05
  )) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02")) +
    geom_text(
      aes(label = paste0(signif, "  OR=", round(odds_ratio, 2))),
      hjust = -0.1,
      size = 4,
      color = "black"
    ) +
    labs(
      title = "Résultats du modèle logistique",
      subtitle = paste0("Pseudo R² de McFadden = ", round(pseudo_r2, 3)),
      x = "Variables explicatives",
      y = expression(-log[10](p-value))
    ) +
    theme_minimal(base_size = 14)
}
#-------------------------------------------------------------------------------
# 2.2 Fonction arbre de décision
#-------------------------------------------------------------------------------
# 2.2.1 Fonction qui affiche l'abre de décision
#-------------------------------------------------------------------------------
plot_tree_structure <- function(modele) {
  
  library(rpart.plot)
  
  rpart.plot(
    modele,
    type = 2,
    extra = 104,
    fallen.leaves = TRUE,
    cex = 0.8
  )
}
#-------------------------------------------------------------------------------
# 2.2.2 Fonction qui affiche un graph de l'importance des variables
#-------------------------------------------------------------------------------
tree_graph <- function(modele, labels, var_name = NULL) {
  
  library(ggplot2)
  library(dplyr)
  library(shiny)
  
  validate(
    need(!is.null(modele$variable.importance),
         "Aucune importance de variable disponible pour ce modèle.")
  )
  
  imp_data <- data.frame(
    variable = names(modele$variable.importance),
    Importance = as.numeric(modele$variable.importance)
  ) %>%
    arrange(desc(Importance))
  
  # Traduction des variables
  imp_data$Variable_fr <- sapply(imp_data$variable, function(v) {
    if (v %in% names(labels)) {
      labels[[v]]
    } else {
      v
    }
  })
  
  titre_modele <- ifelse(
    is.null(var_name),
    "Importance des variables - Arbre de décision",
    paste("Importance des variables pour", var_name)
  )
  
  ggplot(imp_data, aes(
    x = reorder(Variable_fr, Importance),
    y = Importance,
    fill = Importance
  )) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_fill_gradient(low = "#d95f02", high = "#1b9e77") +
    labs(
      title = titre_modele,
      x = "Variables explicatives",
      y = "Importance"
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold"))
}
#-------------------------------------------------------------------------------
# 2.3 Fonction random forest
#-------------------------------------------------------------------------------
# 2.3.1 Fonction qui affiche un graph de l'importance des variables
# et le taux d'erreur OOB d'un modèle Random Forest de classification
#------------------------------------------------------------
rf_graph <- function(modele, labels, var_name = NULL) {
  
  library(ggplot2)
  library(dplyr)
  library(randomForest)
  library(shiny)
  
  validate(
    need(!is.null(modele$err.rate),
         "Erreur OOB non disponible pour ce modèle."),
    need(!is.null(importance(modele)),
         "Importance des variables non disponible.")
  )
  
  oob_error <- tail(modele$err.rate[, "OOB"], 1)
  
  imp_data <- as.data.frame(importance(modele))
  imp_data$variable <- rownames(imp_data)
  
  if ("MeanDecreaseGini" %in% colnames(imp_data)) {
    imp_data$Importance <- imp_data$MeanDecreaseGini
  } else if ("MeanDecreaseAccuracy" %in% colnames(imp_data)) {
    imp_data$Importance <- imp_data$MeanDecreaseAccuracy
  } else {
    validate("Aucune mesure d'importance reconnue.")
  }
  
  imp_data <- imp_data %>% arrange(desc(Importance))
  
  # Traduction des variables
  imp_data$Variable_fr <- sapply(imp_data$variable, function(v) {
    if (v %in% names(labels)) {
      labels[[v]]
    } else {
      v
    }
  })
  
  titre_modele <- ifelse(
    is.null(var_name),
    "Importance des variables - Random Forest",
    paste("Importance des variables pour", var_name)
  )
  
  ggplot(imp_data, aes(
    x = reorder(Variable_fr, Importance),
    y = Importance,
    fill = Importance
  )) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_fill_gradient(low = "#d95f02", high = "#1b9e77") +
    labs(
      title = titre_modele,
      subtitle = paste0("Erreur OOB = ", round(oob_error, 3)),
      x = "Variables explicatives",
      y = "Importance"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray30")
    )
}
#-------------------------------------------------------------------------------
# 2.4 Fonction pour évaluer et comparer des modèles de classification
#-------------------------------------------------------------------------------
evaluer_modeles <- function(models, data_test, variable_cible = "Success",
                            positive_class = "Failure") {
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(caret)
  
  validate(
    need(length(models) > 0, "Aucun modèle fourni.")
  )
  
  evaluer <- function(modele, data_test, variable_cible, nom_modele, positive_class) {
    
    if ("glm" %in% class(modele)) {
      proba <- predict(modele, newdata = data_test, type = "response")
      pred <- ifelse(proba >= 0.5, "Success", "Failure")
    } else {
      pred <- predict(modele, newdata = data_test, type = "class")
    }
    
    pred <- factor(pred, levels = levels(data_test[[variable_cible]]))
    
    cm <- confusionMatrix(pred, data_test[[variable_cible]], positive = positive_class)
    
    accuracy  <- as.numeric(cm$overall["Accuracy"])
    precision <- as.numeric(cm$byClass["Pos Pred Value"])
    recall    <- as.numeric(cm$byClass["Sensitivity"])
    
    f1 <- ifelse((precision + recall) == 0,
                 NA,
                 2 * (precision * recall) / (precision + recall))
    
    data.frame(
      Modele = nom_modele,
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall,
      F1 = f1
    )
  }
  
  res <- do.call(rbind, lapply(models, function(m) {
    evaluer(m$modele, data_test, variable_cible, m$nom, positive_class)
  }))
  
  res_long <- res %>%
    pivot_longer(cols = c("Accuracy", "Precision", "Recall", "F1"),
                 names_to = "Metrique",
                 values_to = "Valeur")
  
  ggplot(res_long, aes(x = Metrique, y = Valeur, fill = Modele)) +
    geom_col(position = position_dodge(0.9)) +
    geom_text(
      aes(label = round(Valeur, 3)),
      position = position_dodge(0.9),
      vjust = -0.3,
      size = 3.5
    ) +
    labs(
      title = "Comparaison des modèles",
      x = "Métrique",
      y = "Valeur"
    ) +
    ylim(0, 1.1) +
    theme_minimal()
}
#-------------------------------------------------------------------------------
# 3. Fonction pour analyse final
#-------------------------------------------------------------------------------
# 3.1 Fonction qui génère des graphiques pour comparer les variables qualitatives
# selon les résultats extrêmes (max/min)
#-------------------------------------------------------------------------------
extreme_plot_quali <- function(data, vars, labels, labels_levels,
                               score_var = "Exam_Score", ncol = 2) {
  
  library(ggplot2)
  library(dplyr)
  library(patchwork)
  library(scales)
  library(shiny)
  
  validate(
    need(length(vars) > 0, "Aucune variable sélectionnée.")
  )
  
  data_extreme <- data %>%
    mutate(
      Groupe = case_when(
        .data[[score_var]] >= 95 ~ "Élevé (≥95)",
        .data[[score_var]] < 60 ~ "Faible (<60)",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Groupe))
  
  plots <- lapply(vars, function(v) {
    
    var_label <- ifelse(v %in% names(labels), labels[[v]], v)
    
    ggplot(data_extreme, aes(x = .data[[v]], fill = Groupe)) +
      geom_bar(position = "fill") +
      scale_x_discrete(labels = labels_levels) +
      scale_y_continuous(labels = percent) +
      labs(
        title = paste("Résultats extrêmes selon", var_label),
        x = var_label,
        y = "Proportion"
      ) +
      theme_minimal()
  })
  
  patchwork::wrap_plots(plots, ncol = ncol)
}
#-------------------------------------------------------------------------------
# 3.2 Fonction qui génère des graphiques pour comparer les variables quantitatives
# selon les résultats extrêmes (max/min)
#-------------------------------------------------------------------------------
extreme_plot_num <- function(data, vars, labels, score_var = "Exam_Score", ncol = 2) {
  
  library(ggplot2)
  library(dplyr)
  library(patchwork)
  library(shiny)
  
  validate(
    need(length(vars) > 0, "Aucune variable sélectionnée.")
  )
  
  data_extreme <- data %>%
    mutate(
      Groupe = case_when(
        .data[[score_var]] >= 95 ~ "Élevé (≥95)",
        .data[[score_var]] < 60 ~ "Faible (<60)",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Groupe))
  
  plots <- lapply(vars, function(v) {
    
    var_label <- ifelse(v %in% names(labels), labels[[v]], v)
    
    ggplot(data_extreme, aes(x = Groupe, y = .data[[v]], fill = Groupe)) +
      geom_boxplot() +
      labs(
        title = paste("Résultats extrêmes selon", var_label),
        x = "Groupe",
        y = var_label
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  patchwork::wrap_plots(plots, ncol = ncol)
}
#-------------------------------------------------------------------------------
# 4. Fonction Shiny pour la visualisation et l’analyse des données et des modèles
#-------------------------------------------------------------------------------
dashboard <- function(data, data_test, data_train, modele_logit, modele_tree, modele_rf) {
  
  library(shiny)
  library(ggplot2)
  library(patchwork)
  
  # Traduction des variables numériques
  labels_num_fr <- c(
    Sleep_Hours = "Heures de sommeil",
    Physical_Activity = "Activité physique",
    Hours_Studied = "Heures d'étude",
    Attendance = "Assiduité",
    Previous_Scores = "Résultats antérieurs",
    Exam_Score = "Score à l'examen (cible)"
  )
  
  # Traduction des variables qualitatives
  labels_quali_fr <- c(
    Motivation_Level = "Niveau de motivation",
    Gender = "Genre",
    Access_to_Resources = "Accès aux ressources",
    Parental_Education_Level = "Niveau d'éducation des parents",
    Parental_Involvement = "Implication parentale",
    Success = "Réussite (cible)"
  )
  
  # Traduction pour toutes les variables
  labels_tous_fr <- c(labels_num_fr, labels_quali_fr)
  
  # Traduction des variables pour les modèles
  labels_modeles_fr <- c(
    labels_quali_fr,
    labels_num_fr,
    Motivation_LevelMedium = "Niveau de motivation moyen",
    Motivation_LevelHigh = "Niveau de motivation élevé",
    GenderMale = "Genre masculin",
    Access_to_ResourcesMedium = "Accès aux ressources moyen",
    Access_to_ResourcesHigh = "Accès aux ressources élevé",
    Parental_Education_LevelCollege = "Niveau d'éducation des parents (cégep)",
    Parental_Education_LevelPostgraduate = "Niveau d'éducation des parents (universitaire)",
    Parental_InvolvementMedium = "Implication parentale moyenne",
    Parental_InvolvementHigh = "Implication parentale élevée"
  )
  # Traduction pour les classes
  labels_levels_fr <- c(
    "Low" = "Faible",
    "Medium" = "Moyen",
    "High" = "Élevé",
    "Male" = "Homme",
    "Female" = "Femme",
    "Failure" = "Échec",
    "Success" = "Réussite",
    "High School" = "Secondaire",
    "College" = "Cégep",
    "Postgraduate" = "Universitaire"
  )
  
  models <- list(
    list(nom = "Régression logistique", modele = modele_logit),
    list(nom = "Arbre de décision", modele = modele_tree),
    list(nom = "Random Forest", modele = modele_rf)
  )
  
  # Interface utilisateur (UI) du dashboard Shiny
  ui <- fluidPage(
    
    titlePanel("Dashboard réussite scolaire"),
    
    tabsetPanel(
      
      navbarMenu(
        "Exploration des données",
        
        tabPanel(
          "Variables numériques",
          
          fluidRow(
            column(
              width = 8,
              checkboxGroupInput(
                inputId = "variables_explicatives_num",
                label = "Variables numériques :",
                choices = setNames(names(labels_num_fr), labels_num_fr),
                selected = names(labels_num_fr),
                inline = TRUE
              )
            ),
            column(
              width = 4,
              sliderInput(
                inputId = "bins",
                label = "Nombre de classes",
                min = 5,
                max = 50,
                value = 30
              )
            )
          ),
          
          fluidRow(
            column(
              width = 6,
              h4("Distribution"),
              plotOutput("hist_plot", height = "500px")
            ),
            column(
              width = 6,
              h4("Corrélation"),
              plotOutput("correlation_num", height = "500px")
            )
          )
        ),
        
        tabPanel(
          "Variables qualitatives",
          
          fluidRow(
            column(
              width = 12,
              checkboxGroupInput(
                inputId = "variables_explicatives_quali",
                label = "Variables qualitatives :",
                choices = setNames(names(labels_quali_fr), labels_quali_fr),
                selected = names(labels_quali_fr),
                inline = TRUE
              )
            )
          ),
          
          fluidRow(
            column(
              width = 6,
              h4("Distribution"),
              plotOutput("barplot_quali", height = "500px")
            ),
            column(
              width = 6,
              h4("Association (V de Cramér)"),
              plotOutput("cramer_quali", height = "600px")
            )
          )
        )
      ),
      
      navbarMenu(
        "Modélisation",
        
        tabPanel(
          "Régression logistique",
          
          h3("Régression logistique"),
          
          tabsetPanel(
            
            tabPanel(
              "Linéarité",
              
              fluidRow(
                column(
                  width = 12,
                  checkboxGroupInput(
                    inputId = "variables_logit",
                    label = "Variables numériques pour la vérification du logit :",
                    choices = setNames(names(labels_num_fr), labels_num_fr),
                    selected = setdiff(names(labels_num_fr), "Exam_Score"),
                    inline = TRUE
                  )
                )
              ),
              
              fluidRow(
                column(
                  width = 12,
                  h4("Linéarité du logit"),
                  plotOutput("verif_logit_plot", height = "650px")
                )
              )
            ),
            
            tabPanel(
              "VIF",
              fluidRow(
                column(
                  width = 12,
                  h4("Multicolinéarité (VIF)"),
                  plotOutput("vif_plot", height = "650px")
                )
              )
            ),
            
            tabPanel(
              "Résultats du modèle",
              fluidRow(
                column(
                  width = 12,
                  h4("Résultats du modèle logistique"),
                  plotOutput("logit_graph", height = "650px")
                )
              )
            )
          )
        ),
        
        tabPanel(
          "Arbre de décision",
          
          h3("Arbre de décision"),
          
          tabsetPanel(
            
            tabPanel("Structure de l'arbre",
              fluidRow(
                column(
                  width = 12,
                  h4("Structure de l'arbre de décision"),
                  plotOutput("arbre_decision_plot", height = "650px")
                )
              )
            ),
            
            tabPanel("Résultats du modèle",
              fluidRow(
                column(
                  width = 12,
                  h4("Importance des variables"),
                  plotOutput("tree_graph", height = "650px")
                )
              )
            )
          )
        ),
        
        tabPanel(
          "Random Forest",
          
          h3("Random Forest"),
          
          fluidRow(
            column(
              width = 12,
              h4("Importance des variables et erreur OOB"),
              plotOutput("rf_graph", height = "650px")
            )
          )
        ),
        
        tabPanel(
          "Comparaison des modèles",
          
          h3("Comparaison des performances réelles"),
          
          fluidRow(
            column(
              width = 12,
              h4("Accuracy, Precision, Recall et F1"),
              plotOutput("eval_models", height = "650px")
            )
          )
        )
      ),
      
      navbarMenu(
        "Résultats extrêmes",
        
        tabPanel(
          "Extrêmes numériques",
          
          h3("Analyse des résultats extrêmes - variables numériques"),
          
          fluidRow(
            column(
              width = 12,
              checkboxGroupInput(
                inputId = "vars_extreme_num",
                label = "Variables numériques à comparer :",
                choices = setNames(
                  setdiff(names(labels_num_fr), "Exam_Score"),
                  labels_num_fr[setdiff(names(labels_num_fr), "Exam_Score")]
                ),
                selected = setdiff(names(labels_num_fr), "Exam_Score"),
                inline = TRUE
              )
            )
          ),
          
          fluidRow(
            column(
              width = 12,
              plotOutput("extreme_num", height = "650px")
            )
          )
        ),
        
        tabPanel(
          "Extrêmes qualitatives",
          
          h3("Analyse des résultats extrêmes - variables qualitatives"),
          
          fluidRow(
            column(
              width = 12,
              checkboxGroupInput(
                inputId = "vars_extreme_quali",
                label = "Variables qualitatives à comparer :",
                choices = setNames(
                  setdiff(names(labels_quali_fr), "Success"),
                  labels_quali_fr[setdiff(names(labels_quali_fr), "Success")]
                ),
                selected = setdiff(names(labels_quali_fr), "Success"),
                inline = TRUE
              )
            )
          ),
          
          fluidRow(
            column(
              width = 12,
              plotOutput("extreme_quali", height = "650px")
            )
          )
        )
      )
    )
  )
  
  # Logique serveur du dashboard Shiny
  server <- function(input, output) {
    
    output$hist_plot <- renderPlot({
      req(input$variables_explicatives_num)
      histplot(
        data = data,
        var_names = input$variables_explicatives_num,
        bins = input$bins,
        labels = labels_num_fr
      )
    })
    
    output$barplot_quali <- renderPlot({
      req(input$variables_explicatives_quali)
      barplot_quali(
        data = data,
        var_names = input$variables_explicatives_quali,
        ncol = 2,
        labels = labels_quali_fr,
        labels_levels = labels_levels_fr
      )
    })
    
    output$correlation_num <- renderPlot({
      req(input$variables_explicatives_num)
      correlation(
        data = data,
        vars = input$variables_explicatives_num,
        labels = labels_num_fr
      )
    })
    
    output$cramer_quali <- renderPlot({
      req(input$variables_explicatives_quali)
      cramer(
        data = data,
        vars = input$variables_explicatives_quali,
        labels = labels_quali_fr
      )
    })
    
    output$verif_logit_plot <- renderPlot({
      req(input$variables_logit)
      verif_logit(
        data = data_train,
        vars = input$variables_logit,
        modele = modele_logit,
        ncol = 2,
        labels = labels_num_fr
      )
    })
    
    output$logit_graph <- renderPlot({
      logit_graph(
        modele = modele_logit,
        labels = labels_modeles_fr
      )
    })
    
    output$vif_plot <- renderPlot({
      plot_vif(
        modele_logit,
        labels = labels_tous_fr
      )
    })
    
    output$tree_graph <- renderPlot({
      tree_graph(
        modele = modele_tree,
        labels = labels_tous_fr
      )
    })
    
    output$arbre_decision_plot <- renderPlot({
      plot_tree_structure(modele_tree)
    })
    
    output$rf_graph <- renderPlot({
      rf_graph(
        modele = modele_rf,
        labels = labels_tous_fr)
    })
    
    output$eval_models <- renderPlot({
      evaluer_modeles(
        models = models,
        data_test = data_test,
        variable_cible = "Success",
        positive_class = "Failure"
      )
    })
    
    output$extreme_num <- renderPlot({
      req(input$vars_extreme_num)
      extreme_plot_num(
        data = data,
        vars = input$vars_extreme_num,
        labels = labels_num_fr
      )
    })
    
    output$extreme_quali <- renderPlot({
      req(input$vars_extreme_quali)
      extreme_plot_quali(
        data = data,
        vars = input$vars_extreme_quali,
        labels = labels_quali_fr,
        labels_levels = labels_levels_fr
      )
    })
  }
  shinyApp(ui, server)
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Préparation des données
#-------------------------------------------------------------------------------
# Importer les données depuis le fichier csv
data <- read.csv("C:/Users/laure/Documents/TELUQ/3_année_2025-2026/StudentPerformanceFactors.csv", header = TRUE, sep = ",")

# Garder les colonnes sélectionnées seulement
data <- data %>%
  select(
    Parental_Involvement,
    Parental_Education_Level,
    Access_to_Resources,
    Gender,
    Sleep_Hours,
    Motivation_Level,
    Physical_Activity,
    Hours_Studied,
    Attendance,
    Previous_Scores,
    Exam_Score
  )

# Création d’une variable binaire Success/Failure à partir de Exam_Score (seuil = 60)
data$Success <- factor(
  ifelse(data$Exam_Score >= 60, "Success", "Failure")
)
#-------------------------------------------------------------------------------
# Vérification des données
#-------------------------------------------------------------------------------
# Vérifier le type des données
str(data)

# Vérifier les valeurs abérantes/manquantes
resume_num(data)
resume_quali(data)

# Supprimer les lignes avec valeur vide de Parental_Education_Level
data <- data[trimws(data$Parental_Education_Level) != "", ]

# Mettre les variables qualitatives en factor
data$Motivation_Level <- factor(data$Motivation_Level,levels = c("Low", "Medium", "High"))
data$Access_to_Resources <- factor(data$Access_to_Resources,levels = c("Low", "Medium", "High"))
data$Parental_Involvement <- factor(data$Parental_Involvement,levels = c("Low", "Medium", "High"))
data$Parental_Education_Level <- factor(data$Parental_Education_Level,levels = c("High School", "College", "Postgraduate"))
data$Gender <- as.factor(data$Gender)
#-------------------------------------------------------------------------------
# Préparation des données train/test
#-------------------------------------------------------------------------------
set.seed(123)  # reproductibilité

# Split stratifié 80% train / 20% test
train_index <- createDataPartition(data$Success, p = 0.8, list = FALSE)

data_train <- data[train_index, ]
data_test  <- data[-train_index, ]
#-------------------------------------------------------------------------------
# Modèle logistique
#-------------------------------------------------------------------------------
modele_logit <- glm(
  Success ~ Sleep_Hours + Physical_Activity + Hours_Studied + 
    Attendance + Previous_Scores +
    Motivation_Level + Gender + Access_to_Resources +
    Parental_Education_Level + Parental_Involvement,
  data = data_train,
  family = binomial
)
#-------------------------------------------------------------------------------
# Modèle arbre de décision
#-------------------------------------------------------------------------------
modele_tree <- rpart(
  Success ~ Sleep_Hours + Physical_Activity + Hours_Studied +
    Attendance + Previous_Scores +
    Motivation_Level + Gender + Access_to_Resources +
    Parental_Education_Level + Parental_Involvement,
  data = data_train,
  method = "class"
)
#-------------------------------------------------------------------------------
# Modèle Random Forest
#-------------------------------------------------------------------------------
modele_rf <- randomForest(
  Success ~ Sleep_Hours + Physical_Activity + Hours_Studied +
    Attendance + Previous_Scores +
    Motivation_Level + Gender + Access_to_Resources +
    Parental_Education_Level + Parental_Involvement,
  data = data_train,
  importance = TRUE
)
#-------------------------------------------------------------------------------
# Lancement du dashboard Shiny
#-------------------------------------------------------------------------------
dashboard(
  data = data,
  data_test = data_test,
  data_train = data_train,
  modele_logit = modele_logit,
  modele_tree = modele_tree,
  modele_rf = modele_rf
)
