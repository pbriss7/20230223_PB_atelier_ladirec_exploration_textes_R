#### Exploration des données textuelles avec R - 23 février 2023 ----

## Note importante
# Une ligne précédée d'un ou de plusieurs croisillons # ne sera pas exécutée par l'interpréteur
# Pour exécuter une instruction, placez votre curseur au début de la ligne et appuyez sur 'COMMAND' + 'RETURN' (raccourci Mac) ou 'CTRL' + 'RETURN' (raccourci Windows)
# Pour obtenir de l'aide sur une fonction, exécutez-la précédée d'un point d'interrogation. Par exemple:

?paste()


#### Préparation de l'environnement de travail ----

# Installation des extensions dont nous aurons besoin (si elles ne sont pas déjà installées)
if(!"stringr" %in% rownames(installed.packages())) {install.packages("stringr")}
if(!"readxl" %in% rownames(installed.packages())) {install.packages("readxl")}
if(!"dplyr" %in% rownames(installed.packages())) {install.packages("dplyr")}
if(!"ggplot2" %in% rownames(installed.packages())) {install.packages("ggplot2")}
if(!"quanteda" %in% rownames(installed.packages())) {install.packages("quanteda")}
if(!"quanteda.textplots" %in% rownames(installed.packages())) {install.packages("quanteda.textplots")}
if(!"quanteda.textstats" %in% rownames(installed.packages())) {install.packages("quanteda.textstats")}
if(!"lsa" %in% rownames(installed.packages())) {install.packages("lsa")}

# Activons les extensions (i.e. chargeons en mémoire toutes les fonctions qu'elles contiennent)
library(readxl)               # Extension pour l'importation de fichiers Excel
library(stringr)              # Extension pour la manipulation des chaînes de caractères
library(dplyr)                # Extension pour la manipulation des structures des tableaux de données
library(ggplot2)              # Extension pour la production de graphiques de haute qualité
library(quanteda)             # Extension pour le forage textuel
library(quanteda.textplots)   # Extension pour le forage textuel
library(quanteda.textstats)   # Extension pour l'analyse des collocations
library(lsa)                  # Extension offrant un antidictionnaire élaboré

##### Importation des données -----

# Lecture des données et assignation à une variable 
xyz <- readxl::read_excel("donnees/XYZ-2015-2022-table-20230205JV.xlsx", sheet = 1)


##### Examen de la structure -----
str(xyz)


# On peut observer les détails de cette structure une information à la fois
# Nom des colonnes
colnames(xyz)


# Nombre de lignes et de colonnes dans le tableau
dim(xyz)


# Nombre de lignes
nrow(xyz)


# Regardons de quel type sont les valeurs de la colonne `ISSN (numérique)`
# Note: on doit utiliser les guillemets pour appeler le vecteur-colonne parce qu'il contient des caractères accentués et une espace. Nous corrigerons cela plus loin.
class(xyz$`ISSN (numérique)`)


# On veut savoir si et combien il y a des cellules sans contenu ("" ou NA)
sapply(xyz, function(x) sum(is.na(x)))
sapply(xyz, function(x) sum(x == ""))


# Facultatif: on peut créer deux colonnes sans contenu vérifier à nouveau si R les repère
xyz$test <- NA
xyz$test2 <- ""


sapply(xyz, function(x) sum(is.na(x)))
sapply(xyz, function(x) sum(x == ""))


# On supprime ces colonnes inutiles
xyz[, c("test", "test2")] <- NULL


# Vérification
xyz

#### Prétraitement des données ----
# Objectifs: 
# 1. résoudre les problèmes que pourraient poser la manipulation et l'analyse éventuelle des données (noms de colonnes, types de données, classes d'objets, NA, encodage, etc.);
# 2. réduire les dimensions de l'objet en fonction des tâches à exécuter (ex.: ne conserver que les colonnes/lignes nécessaires);
# 3. rendre le tableau aussi lisible que possible.

# Problème 1: les noms de colonnes contiennent des accents et des espaces.
# Problème 2: les observations ne sont pas pourvues d'identifiants uniques.
# Problème 3: certaines colonnes semblent n'apporter aucune information essentielle. À vérifier. Éliminer si inutiles.
# Problème 4: on aimerait avoir une colonne réservée aux années.
# Problème 5: on voudrait que les valeurs numériques soient considérées comme des nombres, non comme des chaines de caractères.
# Problème 6: dans la colonne `texte`, les sauts de paragraphes du texte original sont indiqués par "\n".


##### Problème 1. Renommer les colonnes -----
colnames(xyz) <- c("periodique", "titre", "auteur",
                   "numero", "date", "theme", "uri",
                   "editeur", "issn_imp", "issn_num",
                   "citation", "mention_legale", "texte")


##### Problème 2. Créer un identifiant unique. On peut utiliser les numéros de ligne -----
xyz$doc_id <- 1:nrow(xyz)


##### Problème 3. Repérer les colonnes inutiles et les éliminer -----
# La première colonne semble contenir une information redondante. Vérifions:
unique(xyz[ , "periodique"])  # ou table(xyz$periodique)
unique(xyz[, "numero"])       # ou table(xyz$numero)
unique(xyz[, "theme"])        # ou table(xyz$theme)


### Exercice: poursuivez la vérification avec les colonnes `uri`, `editeur`, `issn` (x2), `mention_legale`. ###
# unique(...[, ...])


# Créons un vecteur avec les colonnes inutiles (on utilise pour cela la fonction de concaténation c( ) )
colonnes_a_supprimer <- c("periodique", "editeur", "issn_imp", "issn_num", "mention_legale", "uri", "citation")


# On élimine l'ensemble des colonnes inutiles d'un seul coup.
xyz[, colonnes_a_supprimer] <- NULL


# Problème 4. On veut créer une colonne contenant les années. Il faut les extraire de la colonne `date`.
xyz$annee <- stringr::str_extract(xyz$date, "[0-9]+")


### Exercice: éliminez la colonne `date`, qui ne sert plus à rien. ###
# xyz$... <- ...


##### Problème 5. Cette colonne `annee`, de même que la colonne `numero`, devraient être de type `numeric`, non de type `chr` -----
xyz$numero <- as.integer(xyz$numero)      # La fonction as.integer() force la conversion du type chararcter en type integer
# xyz$... <- ...(xyz$...)


##### 6. Remplacer un symbole dans une longue chaine de caractères (un texte) -----
# Observons tout d'abord un texte en particulier
xyz$texte[1]


# Remplaçons par une espace simple le symbole `\n`
xyz$texte <- gsub(pattern = "\n", replacement = " ", x = xyz$texte, fixed = TRUE)


#### Exercice: vérifiez un texte pris au hasard pour voir s'il reste des scories
# xyz$...


# Enfin, par souci de lisibilité, réordonnons la séquence des colonnes
xyz <- xyz[, c("doc_id", "auteur", "titre", "numero", "annee", "theme", "texte")]


#### EXPLORATION 1: LES MÉTADONNÉES ----
# La fonction `table( )` permet d'observer la fréquence des modalités d'un ou de plusieurs champs 
distrib_annuelle <- table(xyz$annee)


# Quelle est la moyenne de cette distribution?
mean(distrib_annuelle)


### Exercice: créez une table pour observer la distribution des thèmes ###
# distrib_themes <- ...


# Pour ordonner cette table, on peut utiliser la fonction `sort( )`, auquel on passe l'argument `decreasing = TRUE`
# distrib_themes_ord <- sort(distrib_themes, decreasing = TRUE)


### Exercice: trouvez les noms des 10 principaux contributeurs de la revue ###
# distrib_auteurs <- ...
# distrib_auteurs_ord <- ...


# Réponse à la question ci-dessus (j'en profite pour vous montrer trois syntaxes différentes produisant le même résultat): 

# 1. Syntaxe étendue (petite perte de mémoire, mais plus explicite: le résultat de chaque opération est emmagasiné dans une variable)
distrib_auteurs <- table(xyz$auteur)
distrib_auteurs_ord <- sort(distrib_auteurs, decreasing = TRUE)
head(distrib_auteurs_ord, n = 10)


# 2. Syntaxe condensée, par enchâssement des fonctions. Lire de l'intérieur vers l'extérieur
head(sort(table(xyz$auteur), decreasing = TRUE), 10)


# 3. Syntaxe enchainée (pipe), qui se lit de gauche à droite
xyz$auteur |> table() |> sort(decreasing = TRUE) |> head(10)


##### Tests de corrélation -----
# Une tâche importante dans l'AED est de comprendre s'il existe des corrélations entre des variables
# Les corrélations se calculent sur des variables numériques

# EXEMPLE avec un jeu de données disponible en tout temps dans l'extension de base, `mtcars`
# Ce jeu de données présente différentes marques et modèles de voitures et leurs attributs techniques
# Il permet notamment de connaître la distance au "gallon" parcourue par différentes marques selon le volume de leurs moteurs

mtcars # Dans le jeu de données, les noms de lignes (rownames) correspondent aux marques.

# On voudrait savoir s'il y a une corrélation, positive ou négative, entre le volume du moteur (disp) et la distance/gallon que peut parcourir une voiture
# Expliquer variable indépendante / dépendante
plot(mtcars$disp, mtcars$mpg)


# Cette corrélation est confirmée par la p-value
# Ajouter quelques informations sur la corrélation - et +, sur la force de la corrélation et la p-value
cor.test(mtcars$disp, mtcars$mpg)


# Pour effectuer des tests de corrélation sur des données textuelles, il faut au préalable se demander ce qu'on souhaite mesurer et générer des données numériques qui serviront au calcul.

##### Exemple -----
# On pourrait vouloir vérifier s'il y a une corrélation, positive ou négative, entre la longueur des titres et la longueur des textes.
# Le test de corrélation est un test statistique qui mesure l'interdépendance ou l'association entre des paires de valeurs (deux variables).
# Nous allons utiliser le coefficient de corrélation appelé tau de Kendall (compris entre -1 et +1).
# Une corrélation positive est marquée par un tau positif, et inversement pour une corrélation négative. Plus le nombre s'éloigne de 0, plus la corrélation est forte.
# Référence: https://academic.oup.com/biomet/article/30/1-2/81/176907 

# Pour faire ce test, il faut créer deux variables numériques qui seront mises en relation. 
# Pour aller au plus simple, nous allons calculer le nombre de caractères des titres (première variable) et le nombre de caractères des textes (2e variable).
# Cette opération sera faite avec la fonction nchar().

xyz$ncharTitre <- nchar(xyz$titre)
xyz$ncharTexte <- nchar(xyz$texte)


# Observons le résultat dans la table
xyz[, c("titre", "texte", "ncharTitre", "ncharTexte")]

cor.test(xyz$ncharTitre, xyz$ncharTexte, method = "kendall")
# La mesure de corrélation, `tau`, est positive, mais très proche de zéro, ce qui dénote une corrélation très faible.


# On peut projeter ces données dans un diagramme à points
ggplot(xyz, aes(x=ncharTitre, y=ncharTexte))+
  geom_jitter()+
  geom_smooth()

#### Enrichissement des données ----
# On peut tirer profit des données numériques ajoutées (longueur des textes) pour créer un nouveau champ
# Par exemple, on pourrait souhaiter classer les textes selon qu'ils sont "courts", "moyens" ou "longs"
# Le type d'objet que nous créerons est dit "catégorique" (ou 'factor')
 

percentiles <- unclass(summary(xyz$ncharTexte))                  # On transforme le sommaire statistique, un objet de type `table`, en simple vecteur avec unclass()

xyz$longueur_texte_cat <- factor(                                # Création de valeurs catégorielles fondées sur les modalités d'une autre colonne
  ifelse(xyz$ncharTexte < percentiles["1st Qu."], "court",
         ifelse(xyz$ncharTexte > percentiles["3rd Qu."], "long", "moyen")),
  levels = c("court", "moyen", "long")                           # La fonction `factor()` possède un argument, `levels=` qui permet de déterminer l'ordre des catégories
)


# On peut maintenant observer les proportions
ggplot(xyz, aes(x = longueur_texte_cat))+                        
  geom_bar(stat = "count")

table(xyz$longueur_texte_cat)



#### EXPLORATION 2 -- LES DONNÉES TEXTUELLES ----
# Pour aller plus loin dans l'analyse des données textuelles, nous devrons transformer le tableau de données en matrice documents-mots
# Une matrice est un objet qui ressemble beaucoup à un tableau de données, à ceci près qu'il n'est composé que d'un seul type de données
# Chaque ligne de notre matrice représentera un document et chaque colonne, un mot unique du vocabulaire complet de tous les textes assemblés en "corpus"
# Ce type de matrices peut être pondérée pour accorder plus de poids à certains mots, elle peut être normalisée pour éviter les biais de longueur
# De plus, les matrices permettent des calculs vectoriels, donc très rapides.

# Pour transformer notre tableau en matrice documents-mots (ou 'document-feature matrix'), nous allons utiliser l'extension Quanteda
# Quanteda offre une multitude de fonctions utiles pour l'ADT. Sa syntaxe est cohérente et la documentation est abondante.
# Si on ne veut pas avoir à écrire ses propres fonctions (de tokénisation, de transformation, etc.), Quanteda est un bon compromis

# La transformation se fait en trois grandes étapes, chacune visant un objectif précis et permettant de faire des opérations à la volée
# La première étape est la création d'un corpus avec la fonction corpus(). On y précise la colonne des identifiants uniques et la colonne contenant le texte à analyser
# La deuxième étape est la tokénisation des textes du corpus. On peut à cette étape se servir d'un antidictionnaire pour éliminer les mots fonctionnels
# La troisième étape est le passage entre cet objet "tokens" à la dfm (document-feature matrix).

# Les métadonnées suivent par défaut chacune des transformations et permettront, une fois la dfm construite, de filtrer les documents en ligne avec les métadonnées

# Pour plus d'information sur chacune des fonctions, consultez la documentation. Exemple:

?quanteda::corpus()

#### Création de la matrice documents-mots ----
xyz_corp <- quanteda::corpus(xyz, docid_field = "doc_id", text_field = "texte")   # Les arguments de la fonction corpus() permettent de préciser les champs du tableau correspondant aux identifiants et aux textes
xyz_toks <- tokens(xyz_corp, 
                   remove_punct = TRUE,                                           # On supprime à la volée la ponctuation
                   remove_symbols = TRUE,                                         # On supprime à la volée les symboles
                   remove_numbers = FALSE,                                        # On pourrait supprimer à la volée les nombres
                   remove_separators = TRUE) |>                                   # On supprime les blancs laissés par la tokénisation
  tokens_split(separator = "'", valuetype = "fixed")                              # On force la tokénisation à partir de l'apostrophe

xyz_dfm <- dfm(xyz_toks) |>                                                       # On transforme l'objet tokens en dfm
  dfm_remove(lsa::stopwords_fr) |>                                                # On élimine toutes les colonnes correspondant à un mot fonctionnel (toujours inspecter l'antidictionnaire)
  dfm_trim(min_docfreq = 0.15,                                                    # Un mot doit être présent dans au moins 15% des documents (élimination des hapax)
           max_docfreq = 0.7,                                                     # Un mot ne doit pas être présent dans plus de 70% des documents du corpus
           docfreq_type = "prop")                                                 # Cet argument permet de préciser que les valeurs indiquées dans les arguments précédents sont des proportions


### Exercice: modifiez les seuils et voyez l'effet sur les dimensions de l'objet!
# Pour voir les dimensions de l'objet, vous n'avez qu'à l'apeller ainsi:
xyz_dfm

# Vous pouvez également accéder en tout temps aux métadonnées
docvars(xyz_dfm)


#### Exploration du vocabulaire ----
##### Examen des vocabulaires en fonction d'années ou de thèmes -----
xyz_dfm_2015 <- dfm_subset(xyz_dfm, subset = annee == 2015)
xyz_dfm_2021 <- dfm_subset(xyz_dfm, subset = annee == 2021)

xyz_dfm_th_jardin <- dfm_subset(xyz_dfm, subset = theme == "Jardin : un enfer de morceaux de paradis")
xyz_dfm_th_YOLO <- dfm_subset(xyz_dfm, subset = theme == "YOLO (You Only Live Once) : hardis, téméraires, écervelés, aventureux, fonceurs, délurés")


# Comparaison des vocabulaires de numéros thématiques
set.seed(100)
textplot_wordcloud(xyz_dfm_th_jardin)
set.seed(100)
textplot_wordcloud(xyz_dfm_th_YOLO)


# On pourrait également comparer les vocabulaires d'auteurs ayant offert chacun 3 contributions à XYZ
set.seed(100)
dfm_subset(xyz_dfm, auteur %in% c("Edem Awumey", "J.D. Kurtness")) |> 
  textplot_wordcloud(comparison = TRUE)


#### Exercice: choisissez deux numéros thématiques et comparez les vocabulaires de chacun


#### Analyse de cooccurrences ----
# L'analyse des cooccurrences consiste à évaluer la présence de deux ou plusieurs mots dans une fenêtre (contexte) de mots donnée
# Elle montre les mots qui sont les plus souvent associés, la force d'attraction qui les lie
# La cooccurrence peut évaluer la présence/absence de deux termes dans un contexte donné, ou mesurer la fréquence.
# La fonction fcm() (fcm pour feature-context matrix) prend en entrée un objet 'tokens' qu'on peut ou non filtrer selon la direction de l'exploration

cooccurrence_matrice <- xyz_toks |>                                   # Envoi dans la chaîne d'opérations l'objet 'tokens' créé auparavant
  tokens_remove(lsa::stopwords_fr) |>                                 # Retrait des mots fonctionnels avec l'antidictionnaire lsa
  tokens_subset(annee == 2021) |>                                     # Utilisation des mots présents seulement dans les documents de l'année 2021
  fcm(context = "window", window = 10, tri = FALSE)                   # On choisit ici une fenêtre contextuelle de 10 mots

# On repère ici couples de mots ayant un coefficient de cooccurrence égal ou supérieur à 30
principaux_mots <- names(topfeatures(cooccurrence_matrice, 30))

# La matrice de cooccurrence est réduite à ses principaux mots, et projetée sous la forme d'un graphique
set.seed(100)
textplot_network(fcm_select(cooccurrence_matrice, principaux_mots), min_freq = 0.8) # On augmente encore le seuil pour rendre le graphique plus facile à interpréter