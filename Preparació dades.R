# Carreguem la llibreria necessària:
library(readr)
library(dplyr)

#--- Carreguem fitxer csv ------------------------------------------------------

# Carreguem el contingut del fitxer a la taula BRP:
fpn <- read_csv("female_play_net.csv")

head(fpn)
summary(fpn)

# Seleccionem només les variables que ens interessen pel projecte:
nou_fpn <- fpn %>% 
  select(value_eur, club_name, league_id, league_name, club_position, overall, potential, age, wage_eur, release_clause_eur, nation_team_id)

summary(nou_fpn)

#--- Arreglem variable value_eur -----------------------------------------------

str(nou_fpn$value_eur)

# Moneda
# Creem una variable anomenada moneda on indicarem la unitat monetària de la variable value_eur:
nou_fpn$moneda <- ifelse(grepl("£", nou_fpn$value_eur), "£", "€")
# Eliminem el símbol £ de la variable value_eur:
nou_fpn$value_eur <- gsub("£", "", nou_fpn$value_eur)
# Convertim els valors en lliures de la variable value_eur a Euros (els valors en Euros no els modifiquem):
nou_fpn$value_eur[nou_fpn$moneda == "£"] <- as.character(as.numeric(nou_fpn$value_eur[nou_fpn$moneda == "£"]) * 1/0.85)

# Valors notació científica
nou_fpn$value_eur <- gsub(",", ".", nou_fpn$value_eur)
# Indiquem que ho substitueixi tot pel buit, excepte els números i les excepcions descrites:
nou_fpn$value_eur <- gsub("[^0-9\\.eE-]", "", nou_fpn$value_eur)

# Transformem a numèrica
nou_fpn$value_eur <- as.numeric(nou_fpn$value_eur)
str(nou_fpn$value_eur)
summary(nou_fpn$value_eur)

#--- Normalitzem de formats en variables categòriques --------------------------

table(nou_fpn$club_name)
table(nou_fpn$league_name)
table(nou_fpn$club_position)

# Eliminem l'espai i la W, indicant que ens referim a la W que es troba al final del nom:
nou_fpn$club_name <- gsub(" W$", "", nou_fpn$club_name)
table (nou_fpn$club_name)

sum(!is.na(nou_fpn$club_name))
sum(!is.na(nou_fpn$league_name))
sum(!is.na(nou_fpn$club_position))

#--- Inconsistències en variables quantitatives --------------------------------

# Tot ok

#--- Valors absents ------------------------------------------------------------

fpn_ple <- nou_fpn[!is.na(nou_fpn$league_name), ]
sapply(fpn_ple, function(x) sum(is.na(x)))

# Imputació absents value_eur
# Fem la imputació:
fpn_ple_imputat <- fpn_ple %>%
  # Agrupem les observacions per equip:
  group_by(club_name) %>%
  # Fem la imputació calculant la mediana sense els valors absents (na.rm = TRUE):
  mutate(value_eur = ifelse(is.na(value_eur), median(value_eur, na.rm = TRUE), value_eur))
# Comprovem
sum(is.na(fpn_ple_imputat$value_eur))

# Realese_clause_euro
nrow(fpn_ple_imputat)
# Fem la imputació:
fpn_ple_imputat1 <- fpn_ple_imputat %>%
  # Agrupem les observacions per equip:
  group_by(club_name) %>%
  # Fem la imputació calculant la mediana sense els valors absents (na.rm = TRUE):
  mutate(release_clause_eur = ifelse(is.na(release_clause_eur), median(release_clause_eur, na.rm = TRUE), release_clause_eur))
# Comprovem
sum(is.na(fpn_ple_imputat1$release_clause_eur))

#--- EXportem csv --------------------------------------------------------------

summary(fpn_ple_imputat1)

write.csv2(fpn_ple_imputat1, "futbolista.csv", 
           row.names = FALSE, 
           na = "")
