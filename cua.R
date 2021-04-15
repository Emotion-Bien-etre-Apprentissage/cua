# Load library
library(limer)
library(tidyverse)
library(lubridate)
library(rio)
#ad test phil

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
