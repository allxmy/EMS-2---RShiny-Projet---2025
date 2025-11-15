# app.R - Shiny app complète avec image de connexion, thème, export, modélisation et charte CSS

library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sf)
library(DT)
library(gridExtra)
library(rsconnect)

# Fix pour shinyapps.io
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Empêcher l'installation de versions trop récentes de units/terra
packageVersion("units")
packageVersion("terra")

VALID_USER <- "admin"
VALID_PASS <- "1234"

# Fonction pour récupérer les données
get_data <- function() {
  base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines"
  params <- list(
    size = 10000,
    select = paste(
      "etiquette_ges",
      "etiquette_dpe",
      "type_batiment",
      "annee_construction",
      "periode_construction",
      "hauteur_sous_plafond",
      "conso_5_usages_ep",
      "coordonnee_cartographique_x_ban",
      "coordonnee_cartographique_y_ban",
      sep = ","
    ),
    qs = "code_departement_ban:(73 OR 74)"
  )
  
  res <- GET(base_url, query = params)
  if (status_code(res) == 200) {
    df <- content(res, as = "parsed", simplifyVector = TRUE)$results
    df$coordonnee_cartographique_x_ban <- as.numeric(df$coordonnee_cartographique_x_ban)
    df$coordonnee_cartographique_y_ban <- as.numeric(df$coordonnee_cartographique_y_ban)
    df
  } else {
    NULL
  }
}

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "SAE - RSHINY",
    tags$li(
      class = "dropdown",
      selectInput("theme_select", "Choisir thème :", 
                  choices = c("blue", "purple", "green", "red", "yellow"), 
                  selected = "blue")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "home", icon = icon("home")),
      menuItem("Carte", tabName = "map", icon = icon("map")),
      menuItem("Visualisation", tabName = "viz", icon = icon("chart-bar")),
      menuItem("Modélisation", tabName = "model", icon = icon("chart-line"))
    ),
    hr(),
    h4("Filtres"),
    selectInput("filter_dpe", "DPE", choices = c("Tous", LETTERS[1:7]), selected = "Tous"),
    selectInput("filter_ges", "GES", choices = c("Tous", LETTERS[1:7]), selected = "Tous"),
    selectInput("filter_type", "Type bâtiment", choices = c("Tous"), selected = "Tous")
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Charte visuelle */
        body { font-family: Arial, sans-serif; background-color: #f8f9fa; }
        h2 { color: #007BFF; }
        .box { border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); padding: 15px; background-color: white; }
      "))
    ),
    tabItems(
      tabItem(tabName = "home",
              h2("Prévisualisation des données"),
              downloadButton("export_csv", "Exporter CSV"),
              DTOutput("preview_dt")),
      tabItem(tabName = "map",
              h2("Répartition des DPE"),
              leafletOutput("mapPlot", height = "700px")),
      tabItem(tabName = "viz",
              h2("Visualisation des données"),
              fluidRow(
                column(9,
                       plotOutput("top_plots", height = "500px"),
                       hr(),
                       plotOutput("corr_plot", height = "400px")
                ),
                column(3,
                       selectInput("corr_x", "Variable X (quantitative)", choices = NULL),
                       selectInput("corr_y", "Variable Y (quantitative)", choices = NULL),
                       downloadButton("export_corr", "Exporter Corrélation PNG"),
                       hr(),
                       radioButtons("select_plot_export", "Graphique à exporter :", 
                                    choices = c("Distribution DPE", "Distribution GES", "Type bâtiment", "Consommation moyenne"),
                                    selected = "Distribution DPE"),
                       downloadButton("export_top_plot", "Exporter graphique PNG")
                )
              )),
      tabItem(tabName = "model",
              h2("Modélisation : Régression linéaire"),
              fluidRow(
                column(3,
                       selectInput("lm_x", "Variable X (prédicteur)", choices = NULL),
                       selectInput("lm_y", "Variable Y (réponse)", choices = NULL),
                       downloadButton("export_lm_plot", "Exporter graphique régression PNG")
                ),
                column(9,
                       plotOutput("lm_plot", height = "400px"),
                       verbatimTextOutput("lm_out")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(logged = FALSE, data = NULL)
  
  # Charger les données dès le lancement
  observe({
    if(is.null(rv$data)){
      df <- get_data()
      if(is.null(df) || nrow(df) == 0){
        showNotification("Erreur: pas de données chargées depuis l'API.", type = "error")
      } else {
        rv$data <- df
      }
    }
  })
  
  # Modal de connexion avec image
  show_login_modal <- function() {
    showModal(modalDialog(
      title = "Connexion",
      tags$div(
        style = "text-align:center; margin-bottom:15px;",
        tags$img(src = "identification.png", style = "max-width:80%; height:auto; border-radius:8px; box-shadow:0 2px 5px rgba(0,0,0,0.2);")
      ),
      textInput("user", "Identifiant"),
      passwordInput("passwd", "Mot de passe"),
      footer = actionButton("login", "Se connecter"),  # Seul le bouton Se connecter
      easyClose = FALSE, size = "s"
    ))
  }
  
  observe({ if (!rv$logged) show_login_modal() })
  
  observeEvent(input$login, {
    req(input$user, input$passwd)
    if (identical(input$user, VALID_USER) && identical(input$passwd, VALID_PASS)) {
      rv$logged <- TRUE
      removeModal()
      showNotification("Connexion réussie !", type = "message")
    } else {
      showNotification("Identifiant ou mot de passe incorrect.", type = "error")
    }
  })
  
  # Changement dynamique du thème
  observe({
    req(input$theme_select)
    session$sendCustomMessage("changeSkin", input$theme_select)
  })
  
  # Mise à jour filtres et variables numériques
  observe({
    req(rv$data)
    updateSelectInput(session, "filter_type", choices = c("Tous", unique(rv$data$type_batiment)))
    numeric_cols <- names(rv$data)[sapply(rv$data, is.numeric)]
    updateSelectInput(session, "corr_x", choices = numeric_cols)
    updateSelectInput(session, "corr_y", choices = numeric_cols)
    updateSelectInput(session, "lm_x", choices = numeric_cols)
    updateSelectInput(session, "lm_y", choices = numeric_cols)
  })
  
  # Données filtrées
  filtered_data <- reactive({
    req(rv$data)
    df <- rv$data
    if(input$filter_dpe != "Tous") df <- df[df$etiquette_dpe == input$filter_dpe, ]
    if(input$filter_ges != "Tous") df <- df[df$etiquette_ges == input$filter_ges, ]
    if(input$filter_type != "Tous") df <- df[df$type_batiment == input$filter_type, ]
    df
  })
  
  # Accueil
  output$preview_dt <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), options = list(pageLength=20, scrollX=TRUE, searching=FALSE))
  })
  output$export_csv <- downloadHandler(
    filename = function() { "donnees.csv" },
    content = function(file) { write.csv(filtered_data(), file, row.names=FALSE) }
  )
  
  # Carte
  output$mapPlot <- renderLeaflet({
    req(filtered_data())
    df <- filtered_data() %>% filter(!is.na(coordonnee_cartographique_x_ban), !is.na(coordonnee_cartographique_y_ban))
    df_sf <- st_as_sf(df, coords=c("coordonnee_cartographique_x_ban","coordonnee_cartographique_y_ban"), crs=2154) %>% st_transform(4326)
    coords <- st_coordinates(df_sf)
    df_sf$lon <- coords[,1]; df_sf$lat <- coords[,2]
    dpe_colors <- colorFactor(palette=c("#0E920D","#7FFF00","yellow","#FFA500","#BE6918","red","#6F060C"), levels=c("A","B","C","D","E","F","G"))
    
    leaflet(df_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng=mean(df_sf$lon), lat=mean(df_sf$lat), zoom=10) %>%
      addCircleMarkers(
        lng=~lon, lat=~lat, radius=4, fillColor=~dpe_colors(etiquette_dpe),
        color="black", weight=0.5, fillOpacity=0.8,
        popup=~paste0("<b>DPE:</b>",etiquette_dpe,"<br>",
                      "<b>GES:</b>",etiquette_ges,"<br>",
                      "<b>Type:</b>",type_batiment,"<br>")
      ) %>%
      addLegend("bottomright", pal=dpe_colors, values=df_sf$etiquette_dpe, title="DPE", opacity=1)
  })
  
  # Graphiques principaux
  output$top_plots <- renderPlot({
    req(filtered_data())
    df <- filtered_data()
    p1 <- ggplot(df, aes(x=etiquette_dpe, fill=etiquette_dpe)) + geom_bar() + labs(title="Distribution DPE") + theme_minimal()
    p2 <- ggplot(df, aes(x=etiquette_ges, fill=etiquette_ges)) + geom_bar() + labs(title="Distribution GES") + theme_minimal()
    p3 <- ggplot(df, aes(x=type_batiment, fill=type_batiment)) + geom_bar() + labs(title="Type bâtiment") + theme_minimal() + theme(axis.text.x=element_text(angle=45,hjust=1))
    df_avg <- df %>% group_by(etiquette_dpe) %>% summarise(avg_conso=mean(conso_5_usages_ep, na.rm=TRUE))
    p4 <- ggplot(df_avg, aes(x=etiquette_dpe, y=avg_conso, fill=etiquette_dpe)) + geom_bar(stat="identity") + labs(title="Consommation moyenne") + theme_minimal()
    grid.arrange(p1,p2,p3,p4, ncol=2)
  })
  
  # Export graphique parmi les 4
  output$export_top_plot <- downloadHandler(
    filename = function() { paste0(input$select_plot_export, ".png") },
    content = function(file){
      df <- filtered_data()
      p <- switch(input$select_plot_export,
                  "Distribution DPE" = ggplot(df, aes(x=etiquette_dpe, fill=etiquette_dpe)) + geom_bar() + labs(title="Distribution DPE") + theme_minimal(),
                  "Distribution GES" = ggplot(df, aes(x=etiquette_ges, fill=etiquette_ges)) + geom_bar() + labs(title="Distribution GES") + theme_minimal(),
                  "Type bâtiment" = ggplot(df, aes(x=type_batiment, fill=type_batiment)) + geom_bar() + labs(title="Type bâtiment") + theme_minimal() + theme(axis.text.x=element_text(angle=45,hjust=1)),
                  "Consommation moyenne" = {
                    df_avg <- df %>% group_by(etiquette_dpe) %>% summarise(avg_conso=mean(conso_5_usages_ep, na.rm=TRUE))
                    ggplot(df_avg, aes(x=etiquette_dpe, y=avg_conso, fill=etiquette_dpe)) + geom_bar(stat="identity") + labs(title="Consommation moyenne") + theme_minimal()
                  }
      )
      ggsave(file, p, width=8, height=6)
    }
  )
  
  # Graphique corrélation
  output$corr_plot <- renderPlot({
    req(filtered_data(), input$corr_x, input$corr_y)
    df <- filtered_data()
    ggplot(df, aes_string(x=input$corr_x, y=input$corr_y)) +
      geom_point(color="#007BFF") +
      labs(title=paste("Corrélation", input$corr_x, "vs", input$corr_y)) +
      theme_minimal()
  })
  output$export_corr <- downloadHandler(
    filename = function() { "correlation.png" },
    content = function(file) {
      df <- filtered_data()
      p <- ggplot(df, aes_string(x=input$corr_x, y=input$corr_y)) +
        geom_point(color="#007BFF") +
        labs(title=paste("Corrélation", input$corr_x, "vs", input$corr_y)) +
        theme_minimal()
      ggsave(file, p, width=8, height=6)
    }
  )
  
  # Modélisation régression linéaire
  lm_model <- reactive({
    req(filtered_data(), input$lm_x, input$lm_y)
    df <- filtered_data() %>% select(all_of(c(input$lm_x, input$lm_y))) %>% na.omit()
    if(nrow(df) > 1){
      lm(as.formula(paste(input$lm_y, "~", input$lm_x)), data=df)
    } else {
      NULL
    }
  })
  
  output$lm_out <- renderPrint({
    model <- lm_model()
    if(!is.null(model)){
      summary(model)
    } else {
      "Trop peu de données pour ajuster un modèle."
    }
  })
  
  output$lm_plot <- renderPlot({
    model <- lm_model()
    req(model)
    df <- filtered_data() %>% select(all_of(c(input$lm_x, input$lm_y))) %>% na.omit()
    ggplot(df, aes_string(x=input$lm_x, y=input$lm_y)) +
      geom_point(color="#007BFF") +
      geom_smooth(method="lm", se=TRUE, color="red") +
      labs(title=paste("Régression linéaire :", input$lm_y, "vs", input$lm_x)) +
      theme_minimal()
  })
  
  output$export_lm_plot <- downloadHandler(
    filename = function() { "regression.png" },
    content = function(file) {
      model <- lm_model()
      req(model)
      df <- filtered_data() %>% select(all_of(c(input$lm_x, input$lm_y))) %>% na.omit()
      p <- ggplot(df, aes_string(x=input$lm_x, y=input$lm_y)) +
        geom_point(color="#007BFF") +
        geom_smooth(method="lm", se=TRUE, color="red") +
        labs(title=paste("Régression linéaire :", input$lm_y, "vs", input$lm_x)) +
        theme_minimal()
      ggsave(file, p, width=8, height=6)
    }
  )
}

# JavaScript pour changer dynamiquement le skin
js <- "
Shiny.addCustomMessageHandler('changeSkin', function(skin) {
  $('body').removeClass(function(index, className) {
    return (className.match(/\\bskin-\\S+/g) || []).join(' ');
  });
  $('body').addClass('skin-' + skin);
});
"

shinyApp(ui = tagList(tags$head(tags$script(HTML(js))), ui), server = server)