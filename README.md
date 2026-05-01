# Modélisation de la réussite scolaire

## Description du projet
Ce projet a pour objectif d’analyser et de prédire la réussite scolaire à partir du jeu de données *Student Exam Performance Dataset* (Kaggle).  

Les données incluent des variables académiques, comportementales et socio-démographiques décrivant le profil des étudiants. L’objectif principal est de comparer différents modèles de classification afin d’identifier celui qui permet le mieux de prédire la réussite, tout en mettant en évidence les facteurs les plus influents.

Trois hypothèses guident l’analyse :
- **H1** : la forêt aléatoire est le modèle le plus performant  
- **H2** : les variables liées à l’environnement scolaire ont plus d’impact que les variables socio-démographiques  
- **H3** : les performances extrêmes sont associées à des profils spécifiques d’étudiants  

---

## Méthodologie (résumé)
Le projet suit les principales étapes suivantes :
- Prétraitement et nettoyage des données  
- Analyse exploratoire  
- Séparation des données (entraînement / test)  
- Modélisation (régression logistique, arbre de décision, forêt aléatoire)  
- Évaluation et comparaison des modèles  
- Analyse complémentaire des profils extrêmes  

---

## Résumé des résultats
Les résultats montrent que la **régression logistique** est le modèle le plus performant dans ce contexte, notamment grâce à une meilleure capacité à détecter les cas d’échec malgré un déséquilibre important des classes.

L’analyse met en évidence que l’**engagement de l’étudiant** (heures d’étude, assiduité) constitue le facteur le plus déterminant de la réussite scolaire, devant les variables socio-démographiques.

**Voir le rapport complet pour plus de détails**

---
## Source des données

Kaggle. (s. d.). Student exam performance dataset analysis.
https://www.kaggle.com/datasets/grandmaster07/student-exam-performance-dataset-analysis

**Voir le rapport complet pour les références détaillées.**
