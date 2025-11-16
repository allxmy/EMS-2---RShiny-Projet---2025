---
title: "Documentation technique — Application Shiny (SAE_DPE)"
output:
  word_document:
    toc: true
    toc_depth: '2'
  html_document:
    toc: true
    toc_depth: 2
    number_sections: true
---

# 1. Schéma d'architecture

```mermaid
flowchart TD
  subgraph App
    A[app.R]
    UI[UI (shiny UI + shinydashboard)]
    SV[Server (reactive, renderPlot, renderLeaflet,...)]
  end
  subgraph Resources
    W[www/identification.png]
    DATA[Données (CSV / API via httr)]
  end
  subgraph Outputs
    PLOTS[ggplot2 plots]
    MAP[Leaflet map]
    TABLE[DT tables]
    EXPORTS[Exports PNG]
  end

  A --> UI
  A --> SV
  SV --> PLOTS
  SV --> MAP
  SV --> TABLE
  SV --> EXPORTS
  SV --> DATA
  UI --> W
```

L’application utilise un fichier unique `app.R` intégrant l’UI et le serveur.

# 2. Installation locale

## Pré-requis système

- R (>= 4.0 recommandé)
- Rtools (Windows) ou outils de compilation Linux (`build-essential`)
- Dépendances pour `sf` :  
  **Ubuntu :**
  ```bash
  sudo apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev
  ```
  **macOS :**
  ```bash
  brew install gdal proj geos
  ```

## Installation des packages R

```r
install.packages(c("shiny","shinydashboard","httr","jsonlite","leaflet",
                   "ggplot2","dplyr","sf","DT","gridExtra"))
```

### Lancement de l'application

```r
setwd("/chemin/vers/le/dossier/qui/contient/SAE_DPE")
shiny::runApp("SAE_DPE")
```

# 3. Packages nécessaires (extraits de app.R)

- shiny<br> 
- shinydashboard<br>  
- httr<br>  
- jsonlite<br> 
- leaflet <br>
- ggplot2<br> 
- dplyr<br> 
- sf<br> 
- DT<br> 
- gridExtra

---

