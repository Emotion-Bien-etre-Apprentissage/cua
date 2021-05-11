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

describe(rep.df)
describe(clar.df)

boxplot(rep.df, las=2, notch = T)
boxplot(clar.df, las=2, notch = T)














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

plot(d, ordered = FALSE, group.order = names(ex_1[2:5]))


