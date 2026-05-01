Modélisation de la réussite scolaire
Description du projet

Ce projet vise à modéliser et prédire la réussite scolaire à partir du jeu de données Student Exam Performance Dataset (Kaggle).
Les données incluent des variables académiques, comportementales et socio-démographiques décrivant le profil des étudiants.

L’objectif est de :

comparer différents modèles de classification
évaluer leur performance
identifier les facteurs influençant la réussite académique

Trois hypothèses guident l’analyse :

H1 : la forêt aléatoire est le modèle le plus performant
H2 : les variables liées à l’environnement scolaire ont plus d’impact que les variables socio-démographiques
H3 : les performances extrêmes sont associées à des profils spécifiques d’étudiants

Outils utilisés

Le projet a été réalisé avec :

R et RStudio pour l’analyse
Shiny pour la visualisation interactive des données

Un tableau de bord a été développé afin de faciliter :

l’exploration des données
la visualisation des relations entre variables
l’interprétation des résultats

Méthodologie
1. Prétraitement des données
Sélection des variables pertinentes (académiques, comportementales et socio-démographiques)
Création d’une variable cible binaire :
Success (≥ 60)
Failure (< 60)
Nettoyage des données (suppression des valeurs manquantes)
Transformation des variables qualitatives en facteurs

2. Analyse exploratoire

Une analyse exploratoire a été réalisée afin de :

observer la distribution des variables
analyser les relations entre variables (corrélations)
examiner la répartition des variables qualitatives

Cette étape permet de mieux comprendre les données avant la modélisation.

3. Séparation des données

Les données ont été divisées en :

80 % pour l’entraînement
20 % pour le test

Une séparation stratifiée a été utilisée afin de conserver la proportion de réussite/échec.

4. Modélisation

Trois modèles de classification ont été utilisés :

Régression logistique
Arbre de décision
Forêt aléatoire

Chaque modèle vise à prédire la réussite scolaire à partir des variables explicatives.

5. Évaluation des modèles

Les modèles ont été comparés à l’aide de plusieurs métriques :

Accuracy
Précision
Recall
Score F1

L’évaluation est réalisée sur des données non vues (jeu de test).

6. Analyse complémentaire

Une analyse des résultats extrêmes a été effectuée afin d’identifier :

les profils associés à une forte réussite (≥ 95)
les profils associés à l’échec (< 60)

Cette étape permet de mieux comprendre les caractéristiques des étudiants selon leur performance.
