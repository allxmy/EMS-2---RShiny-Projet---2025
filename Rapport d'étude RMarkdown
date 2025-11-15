---
title: "Étude statistique des DPE en Savoie (73) et Haute-Savoie (74)"
geometry: margin=1in
output:
  word_document: default
  html_document: default
  pdf_document: default
fontsize: 11pt
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE
)

```
```{r reduce_whitespace, include=FALSE}
hook_output <- knitr::knit_hooks$get("output")
knitr::knit_hooks$set(output = function(x, options) {
  # Supprimer les lignes vides excessives
  x <- gsub("\n{2,}", "\n", x)
  hook_output(x, options)
})
```

# Introduction

L'objectif de ce rapport est de mener une **étude statistique** à partir
des données publiques de l'ADEME concernant les *Diagnostics de
Performance Énergétique (DPE)*.\
Nous concentrons notre analyse sur deux départements alpins : la
**Savoie (73)** et la **Haute-Savoie (74)**.\
Ces territoires sont caractérisés par des conditions climatiques
exigeantes, notamment en termes de chauffage, ce qui justifie une
analyse de la performance énergétique des bâtiments.

Nous chercherons à répondre à **deux problématiques** :

**1.** Comment les étiquettes énergétiques (DPE et GES) varient-elles
selon la période de construction des bâtiments ?\
**2.** Les consommations énergétiques globales diffèrent-elles selon le
type de bâtiment et certaines caractéristiques structurelles (ex :
hauteur sous plafond) ?

Pour cela, nous allons charger les données, les nettoyer, puis les
analyser via plusieurs visualisations.

------------------------------------------------------------------------

# 1. Chargement et préparation des données

Dans cette partie, nous allons :

- appeler l'API de l'ADEME\
- récupérer un échantillon de 10 000 lignes\
- filtrer les départements 73 et 74\
- sélectionner uniquement les variables utiles\
- mettre en forme les données pour permettre les graphiques

```{r}
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(purrr)

# ============================
# 1. RÉCUPÉRATION DES DONNÉES
# ============================

base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines"

params <- list(
  size = 10000,
  select = "etiquette_ges,etiquette_dpe,type_batiment,annee_construction,periode_construction,hauteur_sous_plafond,conso_5_usages_ep,coordonnee_cartographique_x_ban,coordonnee_cartographique_y_ban",
  qs = "code_departement_ban:(73 OR 74)"
)

req <- GET(base_url, query = params)
raw <- content(req, as = "parsed")$results

# ============================
# 2. NETTOYAGE ROBUSTE
# ============================

dpe <- raw %>%
  # remplacer NULL → NA
  lapply(function(x) { 
    x[sapply(x, is.null)] <- NA
    x 
  }) %>% 
  # convertir chaque entrée en data.frame
  lapply(as.data.frame, stringsAsFactors = FALSE) %>% 
  # empiler
  bind_rows()

# ============================
# 3. CORRECTION DES COLONNES LISTES
# ============================

dpe <- dpe %>%
  mutate(across(
    where(is.list),
    ~ sapply(., function(x) ifelse(is.null(x), NA, as.character(x)))
  ))

# ============================
# 4. MISE EN FORME FINALE
# ============================

dpe <- dpe %>%
  mutate(
    etiquette_dpe = factor(etiquette_dpe, levels = LETTERS[1:7]),
    etiquette_ges = factor(etiquette_ges, levels = LETTERS[1:7]),
    type_batiment = factor(type_batiment),
    conso_5_usages_ep = suppressWarnings(as.numeric(conso_5_usages_ep)),
    hauteur_sous_plafond = suppressWarnings(as.numeric(hauteur_sous_plafond)),
    annee_construction = suppressWarnings(as.numeric(annee_construction))
  )

head(dpe)
```

------------------------------------------------------------------------

# 2. Analyse 1 — Lien entre période de construction et étiquettes DPE

```{r, fig.height=4, fig.width=6}
ggplot(dpe, aes(x = periode_construction, fill = etiquette_dpe)) +
  geom_bar(position = "fill") +
  labs(
    title = "Répartition des étiquettes DPE par période de construction",
    x = "Période de construction",
    y = "Proportion"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

------------------------------------------------------------------------

# 3. Analyse 2 — Étiquette GES selon la période de construction

```{r, fig.height=4, fig.width=6}
ggplot(dpe, aes(x = periode_construction, fill = etiquette_ges)) +
  geom_bar(position = "fill") +
  labs(
    title = "Étiquette GES en fonction de la période de construction",
    x = "Période de construction",
    y = "Proportion"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

------------------------------------------------------------------------

# 4. Analyse 3 — Effet de la hauteur sous plafond sur la consommation

```{r, fig.height=4, fig.width=6}
ggplot(dpe, aes(x = hauteur_sous_plafond, y = conso_5_usages_ep)) +
  geom_point(alpha = 0.3) +
  geom_smooth(color = "blue") +
  labs(
    title = "Relation entre hauteur sous plafond et consommation",
    x = "Hauteur sous plafond (m)",
    y = "Consommation (kWh/an)"
  )
```

------------------------------------------------------------------------

# Conclusion

Cette étude met en évidence plusieurs tendances majeures concernant les
bâtiments des départements 73 et 74 :

- La **période de construction** influence fortement les performances énergétiques.  
- Les bâtiments récents sont **nettement mieux classés** en DPE et GES.  
- La **hauteur sous plafond** est un facteur explicatif important de la consommation énergétique.
