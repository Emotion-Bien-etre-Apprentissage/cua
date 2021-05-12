# Load library 
library(limer)
library(tidyverse)
library(lubridate)
library(rio)
#install.packages("devtools")
#devtools::install_github("andrewheiss/limer")


# Setup API details
options(lime_api = 'https://survey.competences-emotionnelles.ch/admin/remotecontrol')
options(lime_username = 'nbr_low') #Compte limité
options(lime_password = '82BBdJyTjqzz')

# Do stuff with LimeSurvey API
get_session_key()  # Log in
responses <- get_responses(198632, sResponseType = "short")  # Get results from survey



# Release session key
release_session_key()

# Mise à jour des noms des variables PAS ENCORE BON
d <- responses

# Mise à jour des variables selon une syntaxe de type q1_1, tri et adaptation des variables meta
d <- d %>% 
  rename_with(~ gsub('[[:punct:]]$', '', .x)) %>% 
  rename_with(~ gsub('[[:punct:]]', '', .x)) %>% #on peut mettre un _ à la place de rien
  select(!c("lastpage","seed","startdate","submitdate",)) %>% 
  rename(lan = startlanguage, dat = datestamp)

# on enlève les majuscules
names(d) <- tolower(names(d))

# Mise à jour du format des dates avec lubridate
d$dat <- ymd_hms(d$dat)

# Sortie Excel si souhaité
export(d,"sortie.xlsx")




### Premières analyses

#load library

library(psych)


# first descriptions


summary(d)
head(d)
describe(d)
names(d)


# recode variables
d$pro <- as.factor(d$pro)
d$sex <- as.factor(d$sex)
d$can <- as.factor(d$can)

summary(d)

rep.df <- d[9:28]
clar.df <- d[29:48]
rep.df <- d[9:48]

describe(rep.df)
describe(clar.df)

boxplot(rep.df, las=2, notch = T)
boxplot(clar.df, las=2, notch = T)

# problème: r1a1 = j'explicite 
# r1a2 = item 2 

# item c1 => démontrer au lieu de témoigner
# item c2 => évaluer ce que on on veut évaluer, éviter parasites, "barrières"
# item c3 => (((dé)montrer )) attester au lieu de témoigner
# item d2 => grace à mes observations des apprenants
# items d5 => mon enseignement (en fonction de / en tenant compte de chaque apprenant) plutôt que gestes professionnels
















#### KO
## describe likert
#Covert values to factors
library(likert)
rep.df= factor(rep.df, levels = c("1", "2", "3", "4"), ordered = TRUE)

likert(rep.df)

rep.df <- lapply(d, factor, levels = 1:4)

# Create a likert object
ex_1_likert = likert(d)

# Figure 2

plo0t(d, ordered = FALSE, group.order = names(ex_1[2:5]))

##############################
#          Code NBR          #
#    reproduire code PGA     #
##############################

##############
# variante 1 #
##############

# Préparer un dataframe
d3 <- d %>%
  select(starts_with("r1") & ends_with("1"))

# Préparer les vecteurs
listing <- names(d3)
plot_list <- vector(mode = "list", length = 20)

# Génération des 20 plots 1 à 1
for (i in 1:20) {
  p <- d3 %>% 
    ggplot() +
    aes(x=factor(0)) +
    aes_string(y=listing[[i]]) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red")
  plot_list[[i]] <- p
}

##############
# variante 2 #
##############

# je veux 20 boxplots côte à côte donc
# x doit être "items" et y en a 20 à regrouper
# y doit être "valeurs" et y

# préparation du dataframe avec pivot en long
d4 <- d %>% 
  select(starts_with("r1") & ends_with("1")) %>% 
  pivot_longer(cols=everything(), names_to ="prout", values_to = "valeurs")

# variable en x en facteur
d4$prout <- as_factor(d4$prout)

# plot
q <- d4 %>% 
  ggplot() +
  aes(x=fct_rev(fct_reorder(prout,valeurs,.fun="median")), y=valeurs, group=prout) +
  geom_boxplot() +
  stat_summary(fun = mean, color = "red", aes(group = 1), geom = "line") +
  theme(axis.text.x = element_text(angle = 50))

