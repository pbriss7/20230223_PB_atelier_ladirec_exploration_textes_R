knitr::opts_chunk$set(warning = FALSE, message = FALSE)



# Création d'un vecteur comprenant trois titres de romans
romans <- c("Illusions perdues", "Le Dernier jour d'un condamné", "La Débâcle")

# Impression d'une chaine de caractères et d'une fonction qui choisit au hasard l'un des trois éléments du vecteur romans
cat("Mon roman préféré est ", sample(x = romans, size = 1), ".", sep = "")


?paste()
?str()
?nchar()



# Création d'objets à éliminer
a=1
b=3
c="Virginie"
d="Théodore"

# Élimination d'une sélection d'éléments
rm(list = setdiff(ls(), c("d", "c")))

# Élimination de tout le contenu de l'environnement:
rm(list = ls()) 




# Installation des extensions dont nous aurons besoin (si elles ne sont pas déjà installées)
if(!"stringr" %in% rownames(installed.packages())) {install.packages("stringr")}
if(!"readxl" %in% rownames(installed.packages())) {install.packages("readxl")}
if(!"dplyr" %in% rownames(installed.packages())) {install.packages("dplyr")}
if(!"ggplot2" %in% rownames(installed.packages())) {install.packages("ggplot2")}
if(!"quanteda" %in% rownames(installed.packages())) {install.packages("quanteda")}
if(!"quanteda.textplots" %in% rownames(installed.packages())) {install.packages("quanteda.textplots")}
if(!"quanteda.textstats" %in% rownames(installed.packages())) {install.packages("quanteda.textstats")}
if(!"lsa" %in% rownames(installed.packages())) {install.packages("lsa")}
if(!"janitor" %in% rownames(installed.packages())) {install.packages("janitor")}

# Activation des extensions
library(readxl)               # Extension pour l'importation de fichiers Excel
library(stringr)              # Extension pour la manipulation des chaînes de caractères
library(dplyr)                # Extension pour la manipulation des structures des tableaux de données
library(ggplot2)              # Extension pour la production de graphiques de haute qualité
library(quanteda)             # Extension pour le forage textuel
library(quanteda.textplots)   # Extension pour le forage textuel
library(quanteda.textstats)   # Extension pour l'analyse des collocations
library(lsa)                  # Extension offrant un antidictionnaire élaboré
library(janitor)              # Extension qui offre des fonctions pour le "nettoyage" de chaines de caractères



# Lecture des données et assignation à une variable
# Les deux points suivis de / indiquent à R qu'il doit remonter d'un niveau pour trouver un dossier appelé `donnees`

xyz <- readxl::read_excel("../donnees/XYZ-2015-2022-table-20230205JV.xlsx", sheet = 1)



# Structure de l'objet
str(xyz)

# Nom des colonnes
colnames(xyz)

# Nombre de colonnes
ncol(xyz)

# Nom des lignes
rownames(xyz)

# Nombre de lignes
nrow(xyz)

# Nombre de lignes et de colonnes dans le tableau
dim(xyz)

# Type d'objet
class(xyz)

# Type de données d'un vecteur du tableau
class(xyz$`ISSN (numérique)`)

# La première fonction renvoie, pour chaque colonne du tableau, la somme des valeurs correspondant à NA.
sapply(xyz, function(x) sum(is.na(x)))

# Cette deuxième fonction renvoie, pour chacune des colonnes du tableau, la somme des valeurs correspondant à une chaîne de caractères vide.
sapply(xyz, function(x) sum(x == ""))


# Simple test: on peut créer deux colonnes sans contenu, puis vérifier à nouveau si R repère ces valeurs nulles
xyz$test <- NA
xyz$test2 <- ""


sapply(xyz, function(x) sum(is.na(x)))
sapply(xyz, function(x) sum(x == ""))


# On supprime ainsi des colonnes devenues inutiles
xyz[, c("test", "test2")] <- NULL


# Vérification que les colonnes ont bien disparu
colnames(xyz)




# On assigne aux noms de colonnes un vecteur comprenant, pour chaque colonne, le nom choisi, dans le bon ordre.
colnames(xyz) <- c("periodique", "titre", "auteur",
                   "numero", "date", "theme", "uri",
                   "editeur", "issn_imp", "issn_num",
                   "citation", "mention_legale", "texte")




# Nous allons créer une petite fonction maison qui va générer des identifiants uniques composé d'une chaine de caractères commençant par le nom de l'auteur du texte, suivi du numéro de la ligne du document dans le tableau. Ces éléments seront concaténés. La fonction recourra ensuite à la fonction `make_clean_names()` de l'extension janitor pour assurer un résultat propre et uniforme.

faire_unique_id_fonction <- function(vecteur1, vecteur2){
  valeurs_concatenees <- paste(vecteur1, vecteur2, sep = "_")
  valeurs_concatenees <- janitor::make_clean_names(valeurs_concatenees)
  return(valeurs_concatenees)
}

# Nous passons à notre fonction les deux vecteurs qui forment le matériau de nos doc_id, et emmagasinons le résultat dans une nouvelle colonne du tableau appelée `doc_id`.
xyz$doc_id <- faire_unique_id_fonction(vecteur1 = xyz$auteur, vecteur2 = 1:nrow(xyz))




# La première colonne semble contenir une information redondante. Vérifions:
unique(xyz[ , "periodique"])  # ou table(xyz$periodique)
unique(xyz[, "numero"])       # ou table(xyz$numero)
unique(xyz[, "theme"])        # ou table(xyz$theme)


# Exercice: poursuivez la vérification avec les colonnes `uri`, `editeur`, `issn` (x2), `mention_legale`. ###
# unique(...[, ...])


# Solution au problème no 3

# On crée un vecteur avec les colonnes inutiles
colonnes_a_supprimer <- c("periodique", "editeur", "issn_imp", "issn_num", "mention_legale", "uri", "citation")


# On élimine l'ensemble des colonnes inutiles d'un seul coup.
xyz[, colonnes_a_supprimer] <- NULL



# Il faut extraire les années de la colonne `date`. On utilise pour cela l'extension stringr.

xyz$annee <- stringr::str_extract(xyz$date, "[0-9]+")


### Exercice: éliminez la colonne `date`, qui ne sert plus à rien. ###
# xyz$... <- ...



# La fonction as.integer() force la conversion du type chararcter en type integer.

xyz$numero <- as.integer(xyz$numero)      

# Faites la même opération pour la colonne appelée `numero`:

# xyz$... <- ...(xyz$...)




# Observons tout d'abord un texte en particulier
# xyz$texte[1]


# Remplaçons par une espace simple le symbole `\n`
xyz$texte <- gsub(pattern = "\n",
                  replacement = " ",
                  x = xyz$texte,
                  fixed = TRUE) # On indique ici à R de ne pas lire le "pattern" comme une                                       expression régulière, mais littéralement


### Exercice: vérifiez un texte pris au hasard pour voir s'il reste des scories
# xyz$...



xyz <- xyz[, c("doc_id", "auteur", "titre", "numero", "annee", "theme", "texte")]



# La fonction `table( )` permet d'observer le nombre de modalités uniques d'un vecteur 
distrib_annuelle <- table(xyz$annee)

# Observons le résultat en appelant les 10 premiers éléments de L'objet `table`
distrib_annuelle[1:10]

# Quelle est la moyenne de cette distribution?
mean(distrib_annuelle)

### Exercice: créez une table pour observer la distribution des thèmes ###
# distrib_themes <- ...



sort(distrib_annuelle, decreasing = TRUE)

# distrib_themes_ord <- sort(distrib_themes, decreasing = TRUE)


### Exercice: trouvez les noms des 10 principaux contributeurs de la revue ###
# distrib_auteurs <- ...
# distrib_auteurs_ord <- ...



# 1. Syntaxe étendue -- plus explicite: le résultat de chaque opération est emmagasiné dans des conteneurs indépendants, sollicitant davantage la mémoire
distrib_auteurs <- table(xyz$auteur)
distrib_auteurs_ord <- sort(distrib_auteurs, decreasing = TRUE)
head(distrib_auteurs_ord, n = 10)


# 2. Syntaxe condensée: enchâssement des fonctions -- lire de l'intérieur vers l'extérieur
head(sort(table(xyz$auteur), decreasing = TRUE), 10)


# 3. Syntaxe enchainée (pipe), qui se lit de gauche à droite
xyz$auteur |> table() |> sort(decreasing = TRUE) |> head(10)




mtcars[1:3, 1:3] # Dans le jeu de données, les noms de lignes (rownames) correspondent aux marques

# Dans ce graphique simple, on pose la variable dépendante en y et l'indépendante en x
plot(mtcars$disp, mtcars$mpg)



cor.test(mtcars$disp, mtcars$mpg, method = "kendall")



# Dans R, la plupart des fonctions de base sont vectorisées, c'est-à-dire que si on lui donne en entrée un vecteur, la fonction effectuera l'opération demandée sur chacun de ses éléments. D'où la grande simplicité des instructions suivantes:

# Nombre total de caractères pour chaque titre
xyz$ncharTitre <- nchar(xyz$titre)

# Nombre total de caractères pour chaque texte
xyz$ncharTexte <- nchar(xyz$texte)


# Observons le résultat dans la table
xyz[, c("ncharTitre", "ncharTexte")]




cor.test(xyz$ncharTitre, xyz$ncharTexte, method = "kendall")




ggplot(xyz, aes(x=ncharTitre, y=ncharTexte))+
  geom_jitter()+
  geom_smooth()



# On peut tout d'abord observer le sommaire statistique et s'en inspirer
summary(xyz$ncharTexte)

# Création de valeurs catégorielles fondées sur les modalités d'une autre colonne
# La fonction `factor()` possède un argument, `levels=` qui permet de déterminer l'ordre des catégories
xyz$longueur_texte_cat <- factor(                                
  ifelse(xyz$ncharTexte < 5000, "court",
         ifelse(xyz$ncharTexte >10000, "long", "moyen")),
  levels = c("court", "moyen", "long")                        
)

# Exercice: observez la distribution de cette variable avec la fonction table().



ggplot(xyz, aes(x = longueur_texte_cat))+                        
  geom_bar(stat = "count")



?quanteda::corpus()



xyz_corp <- quanteda::corpus(xyz, docid_field = "doc_id", text_field = "texte")   

head(xyz_corp, 2)


xyz_toks <- tokens(xyz_corp, 
                   remove_punct = TRUE,              # On supprime à la volée la ponctuation
                   remove_symbols = TRUE,            # On supprime à la volée les symboles
                   remove_numbers = FALSE,           # On pourrait supprimer à la volée les nombres
                   remove_separators = TRUE) |>      # On supprime les blancs laissés par la tokénisation
  tokens_split(separator = "'", valuetype = "fixed") # On force la tokénisation à partir de l'apostrophe

head(xyz_toks, 2)



# On transforme l'objet tokens en dfm
xyz_dfm <- dfm(xyz_toks) |>
   # Retrait des mots fonctionnels avec l'antidictionnaire lsa (inspecter le dictionnaire!)
  dfm_remove(lsa::stopwords_fr) |>  
  # Un mot doit être présent dans au moins 15% des documents (élimination des hapax)
  dfm_trim(min_docfreq = 0.15,
  # Un mot ne doit pas être présent dans plus de 70% des documents du corpus
           max_docfreq = 0.7, 
  # L'argument suivant précise que la valeur indiquée dans min_docfreq= est une proportion
           docfreq_type = "prop")          

head(xyz_dfm, 2)

### Exercice: modifiez les seuils et voyez l'effet sur les dimensions de l'objet!
# Pour voir les dimensions de l'objet, vous n'avez qu'à l'appeler ainsi:

xyz_dfm



          
xyz_dfm_rioux <- dfm_subset(xyz_dfm, subset = auteur == "Hélène Rioux")
xyz_dfm_dorais <- dfm_subset(xyz_dfm, subset = auteur == "David Dorais")

set.seed(100)
textplot_wordcloud(xyz_dfm_rioux, max_words = 200)
set.seed(100)
textplot_wordcloud(xyz_dfm_dorais, max_words = 200)





dfm_comp_auteurs <- xyz_corp |> 
  
  corpus_subset(auteur %in% c("Hélène Rioux", "David Dorais")) |> 
  
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = FALSE,
         remove_separators = TRUE) |>     
  tokens_split(separator = "'", valuetype = "fixed") |> 
  
  dfm()|>                
  
  dfm_remove(lsa::stopwords_fr) |>        
  
  dfm_trim(min_docfreq = 0.15,            
           max_docfreq = 0.7,           
           docfreq_type = "prop")


dfm_comp_auteurs_groupes <- dfm_group(dfm_comp_auteurs, dfm_comp_auteurs$auteur)

textplot_wordcloud(dfm_comp_auteurs_groupes, comparison = TRUE, max_words = 300,
                   color = c("blue", "red"))




xyz_dfm_th_jardin <- dfm_subset(xyz_dfm, subset = theme == "Jardin : un enfer de morceaux de paradis")

xyz_dfm_th_YOLO <- dfm_subset(xyz_dfm, subset = theme == "YOLO (You Only Live Once) : hardis, téméraires, écervelés, aventureux, fonceurs, délurés")


# Comparaison des vocabulaires de numéros thématiques
set.seed(100)
textplot_wordcloud(xyz_dfm_th_jardin)
set.seed(100)
textplot_wordcloud(xyz_dfm_th_YOLO)



#### Exercice: choisissez deux autres numéros thématiques et comparez les vocabulaires. Vous pouvez utiliser la voie simple ou reprendre le bloc d'instructions enchainées ci-dessus.


  # On fournit en entrée l'objet tokens créé auparavant
xyz_fcm <- xyz_toks |>
  # On ne retient que les tokens trouvés dans les textes d'un auteur
  tokens_subset(subset = auteur == "David Dorais") |> 
  # Retrait des mots fonctionnels avec l'antidictionnaire lsa
  tokens_remove(lsa::stopwords_fr) |>
  # On choisit ici une fenêtre contextuelle de 10 mots
  fcm(context = "window", window = 10, tri = FALSE)


# On repère ici couples de mots ayant un coefficient de cooccurrence égal ou supérieur à 30
principaux_mots <- names(topfeatures(xyz_fcm, 30))


# La matrice de cooccurrence est réduite à ses principaux mots, et projetée sous la forme d'un diagramme de réseau
set.seed(100)
textplot_network(fcm_select(xyz_fcm, principaux_mots), min_freq = 0.7)
# On augmente encore le seuil pour rendre le graphique plus facile à interpréter





# Conversion de l'objet fcm en simple matrice
xyz_cooc_matrice <- convert(xyz_fcm, "matrix")

# On peut observer la structure de cet objet. On voit que les noms de lignes et de colonnes correspondent aux mots des textes
str(xyz_cooc_matrice)

# On peut donc indexer la matrice avec les mots à observer. La valeur renvoyée correspondra au nombre de fois que ces mots cooccurrent dans la fenêtre de 10 mots.
xyz_cooc_matrice["jeune", "femme"]

xyz_corp |> 

  corpus_subset(subset = auteur == "David Dorais") |> 
  
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE, 
         remove_numbers = FALSE, 
         remove_separators = TRUE) |>
  
  # tokens_remove(lsa::stopwords_fr) |>
  
  kwic(pattern = phrase("jeunes? femmes?"), 
       window = 5, 
       valuetype = "regex") |> head(10)


