# Éditeur de Texte avec OCaml

### Projet de Programmation Fonctionnelle

## Présentation
Ce projet consiste en la réalisation d'un éditeur de texte graphique interactif développé en OCaml. L'objectif principal était de manipuler des structures de données persistantes pour gérer efficacement le contenu d'un fichier, la navigation du curseur et l'édition dynamique (insertion/suppression). Le programme s'appuie sur la bibliothèque Graphics pour le rendu visuel et propose une gestion avancée des déplacements pour offrir une expérience utilisateur fluide.

## Architecture Technique
L'implémentation repose sur l'utilisation de Zippers, une structure de données permettant de simuler des modifications locales de manière efficace dans un contexte fonctionnel:
- **Type Line** : Un zipper de caractères représentant une ligne unique, où le curseur sépare les éléments précédents (ordre inversé) des éléments suivants.
- **Type Buffer** : Un zipper de lignes représentant l'intégralité du document. Cette structure permet d'effectuer des opérations d'insertion ou de déplacement en un temps constant $O(1)$ à la position courante. 

## Fonctionnalités Implémentées
### Gestion de la navigation
Le moteur de l'éditeur gère les déplacements classiques avec des comportements optimisés:
- **Déplacement horizontal** : Navigation caractère par caractère. Le passage au début d'une ligne entraîne automatiquement le repositionnement à la fin de la ligne précédente.
- **Déplacement vertical** : Navigation entre les lignes tout en tentant de conserver la position relative du curseur (colonne) entre la ligne de départ et la ligne cible.

### Édition et Modification
- **Insertion dynamique** : Ajout de caractères à la position courante.
- **Gestion des retours à la ligne** : Support du saut de ligne avec transfert du texte situé après le curseur vers la nouvelle ligne créée.
- **Suppression intelligente** :
 - **Backspace** : Suppression à gauche du curseur avec jonction de lignes si le curseur est en début de ligne.
 - **Delete** : Suppression du caractère courant (à droite) avec remontée du contenu de la ligne suivante si nécessaire.

## Défis Algorithmiques
- **Calcul de position** : Implémentation de fonctions récursives pour déterminer la position réelle du curseur et la longueur des lignes afin de synchroniser l'affichage graphique avec la structure interne.
- **Réconciliation d'historiques** : Gestion précise du champ pos dans les zippers lors des opérations de suppression complexe (jonction de deux lignes) pour maintenir la cohérence de l'état du buffer.

## Installation et Utilisation
Le projet inclut un Makefile pour automatiser la compilation sous Linux :

#### Compilation du projet :
make all

#### Exécution
./test

## Commandes de l'éditeur
- **Touches fléchées / CTRL+Q,S,D,Z** : Navigation du curseur.
- **Entrée** : Nouvelle ligne.
- **Suppr / Retour arrière** : Suppression de texte.
- **Échap** : Quitter le programme
