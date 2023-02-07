# Exploration

library(xlsx)
library(data.table)
library(ggplot2)
library(stringr)
library(parallel)

xyz <- readxl::read_excel("~/Downloads/XYZ-2015-2022-table-20230205JV.xlsx", 1)
setDT(xyz)
principaux_auteurs <- (
  xyz[, .N, by=c("auteur")]
  [order(N, decreasing = T)]
  [1:21]
  )

ggplot2::ggplot(principaux_auteurs, aes(x = reorder(auteur, N), y=factor(N)))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_classic()

# Longueur des titres
xyz[, `:=`(long_titre = nchar(titre),
           annee = as.integer(str_extract(date, '[0-9]{4}')))]

ggplot(xyz, aes(x = annee, y=long_titre))+
  geom_jitter()+
  geom_smooth(method=lm)


# Longueur des textes (caractères)
xyz[, `:=`(long_nouvelle = nchar(texte))]
summary(xyz$long_nouvelle)


# =============================================== Calcul du nombre de mots

# Liste vide pour recevoir la liste des mots de chaque texte
words_l <- vector(mode = "list", length = nrow(xyz))

# Fonction de calcul des mots pour un texte
words_count_f <- function(x){
  words_v <- strsplit(x, "\\W") |> unlist()
  not_blanks_v <- which(words_v != "")
  words_v <- words_v[not_blanks_v]
  return(length(words_v))
}

# Application de cette fonction à tous les textes de la table de départ (parallélisation de la boucle)
n_words <- mclapply(
  xyz$texte, words_count_f,
  mc.cores = detectCores()
) |> unlist()

xyz$long_nouvelle <- n_words


ggplot(xyz, aes(x=annee, y=long_nouvelle))+
  geom_point()

ggplot(xyz, aes(x=annee, y=long_nouvelle))+
  geom_jitter()

long_moyenne_annuelle <- xyz[ , .(moyenne=mean(long_nouvelle)), "annee"]
ggplot(long_moyenne_annuelle, aes(x=annee, y=moyenne))+
  geom_bar(stat="identity")

ggplot(xyz, aes(x=annee))+
  geom_bar(stat = "count")
