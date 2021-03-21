library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(ggplot2)
library(DT)
library(stringr)
library(tools)
library(shinyWidgets)
library(tidyverse)
library(rsconnect)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(sp)
library(rgeos)
library(htmltools)
library(RColorBrewer)

# data source
# https://data.wprdc.org/dataset/allegheny-county-supermarkets-convenience-stores
# https://data.wprdc.org/dataset/allegheny-county-obesity-rates

# Load and clean data ----------------------------------------------
stores <- read.csv('data-conveniencesupermarkets.csv') %>% 
          filter(Category %in% c("Convenience Store", "Supermarket"))
obesityShape <- readOGR("./obesityrates/obesity_rates.shp", layer = "obesity_rates")
obesityRank <- obesityShape@data %>% group_by(MUNICIPALI) %>% summarise(mean(X2006_2010)) 
obesityRank <- obesityRank[order(-obesityRank[,2]),] %>% rename( avg_rate = `mean(X2006_2010)`)

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Allegheny County Obesity Rate")

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Map", icon = icon("map"), tabName = "map"),
    
    # Select whether to diaplay supermarkets----------------------------------------------
    checkboxInput(inputId = "storeDisplay", 
                  label = "Display supermarkets and convenience stores",
                  value = F
    ),
    
    # Select whether to diaplay supermarkets----------------------------------------------
    checkboxInput(inputId = "obDisplay", 
                  label = "Display Obesity Rate",
                  value = F
    ),
    
    # Menu Items ----------------------------------------------
    menuItem("Visualizations", icon = icon("bar-chart"), tabName = "vis"),
    
    # Select obesity rate range ----------------------------------------------
    sliderInput("obesitySelect",
                "Select Obesity Rate: ",
                min = min(round(obesityShape@data$X2006_2010,2), na.rm = T),
                max = max(round(obesityShape@data$X2006_2010,2), na.rm = T),
                value = c(0.3,0.5),
                step = 0.1),
    
    # Inputs: select municipalis ----------------------------------------------
    pickerInput(inputId = "municipaliSelect",
                label = "Select Municipali:",
                choices = sort(unique(obesityShape$MUNICIPALI)),
                multiple = TRUE,
                selected = unique(obesityShape$MUNICIPALI)),
    
    # Menu Items ----------------------------------------------
    menuItem("Table", icon = icon("table"), tabName = "table")
    
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Map page
  tabItem("map",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            box(title = "Allegheny County Obesity Rate Choropleth Map",
                width = 12, 
                leafletOutput("leaflet"))
            )
  ),
  
  # Plot page ----------------------------------------------
  tabItem("vis",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("num"),
            valueBoxOutput("unemployment"),
            valueBoxOutput("income")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plots",
                   width = 12,
                   tabPanel("Obesity Rate by Municipali", br(),br(),plotlyOutput("barchart")),
                   tabPanel("Store type distribution",br(),br(),plotlyOutput("pie"))
            )
          )
  ),
  
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidRow(
            box(title = "Selected Municipalis", DT::dataTableOutput("tableSelected"), width = 12))
  )
  
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  obesity_subset <- reactive({
    req(input$municipaliSelect)
    filter(obesityShape@data, MUNICIPALI %in% input$municipaliSelect)
  })
  
  obesity_subset2 <- reactive({
    req(c(input$obesitySelect,input$municipaliSelect))
    filter(obesityRank, MUNICIPALI %in% input$municipaliSelect &
                        avg_rate >= input$obesitySelect[1] & avg_rate <= input$obesitySelect[2])
  })
  
  # set color -------------------------------------------
  pal <- colorNumeric(
    palette = "Oranges",
    domain = obesityShape$`X2006_2010`)
  
  # build a leaflet map: choropleth map of obesity rate -------------------------------------------
  output$leaflet <- renderLeaflet({
    #obesity <- obesity_subset()
    
    # Build Map
    map <- leaflet(data = obesityShape) %>%
      setView(-80, 40.5, 10) %>%
      addTiles() #%>%
      #addPolygons(color = ~pal(`X2006_2010`), stroke = F,fillOpacity = 1, popup = ~paste0("<b>", `MUNICIPALI`, ":</b> ", round(X2006_2010*100, 2), " %")) %>%
      #addLegend(position = "bottomright", pal = pal, values = obesityShape$`X2006_2010`, title = "Obesity Rate")
    
    map
  })
  
  
  # update the map when "obDisplay" is selected -------------------------------------------
  
  observe({
    map0 <- leafletProxy("leaflet", data = obesityShape) 
    
    if (input$obDisplay) {
      map0 %>%  addPolygons(color = ~pal(`X2006_2010`), 
                            stroke = F,
                            fillOpacity = 1, 
                            popup = ~paste0("<b>", `MUNICIPALI`, ":</b> ", round(X2006_2010*100, 2), " %")) %>%
                addLegend(position = "bottomright", 
                          pal = pal, 
                          values = obesityShape$`X2006_2010`, 
                          title = "Obesity Rate")
        
    }
    else{
      map0 %>% clearMarkers() %>% clearShapes() 
    }
  })
  
  # update the map when "storeDisplay" is selected -------------------------------------------
  pal_store <- colorFactor(c("#9B8DF2", "#FF5733"), c("Convenience Store", "Supermarket"))
  observe({
    map0 <- leafletProxy("leaflet", data = stores) 
    
    if (input$storeDisplay) {
      map0 %>%  addCircleMarkers(lng = ~Lon, lat = ~Lat, 
                                 radius = 1.5, 
                                 color = ~pal_store(Category), 
                                 popup = ~paste0("<b>", Category, ":</b> ", Name),
                                 clusterOptions = markerClusterOptions()) %>%
                addLegend(position = "topright" , 
                          pal = pal_store, 
                          values = stores$Category, 
                          title = "Category")
    }
    else{
      map0 %>% clearMarkers() %>% clearShapes() 
    }
  })
  
  
  # A plot showing the obesity for selected municiplis -----------------------------
  output$barchart<- renderPlotly({
    fig <- plot_ly(x = obesity_subset2()$avg_rate, 
                   y = obesity_subset2()$MUNICIPALI,
                   type = 'bar') %>% 
           layout(title = 'Obesity Rate by Munucipalis')
    fig
    
  })
  
  
  # A plot showing the distribution of store category -----------------------------------
  output$pie <- renderPlotly({
    store.cat <- stores %>% group_by(Category) %>% summarise(n())
    fig <- plot_ly(
                   labels = ~store.cat$Category, 
                   values = store.cat$`n()`, 
                   type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF')) %>% 
      layout(title = 'Distribution of Store Category',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
  })
  
  
  # Data table ----------------------------------------------
  output$tableSelected <- DT::renderDataTable({
    datatable(
      obesity_subset(), extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  })
  
  # Mass mean info box ----------------------------------------------
  output$num <- renderInfoBox({
    ob <- obesity_subset2()
    num <- nrow(ob)
    avg <- sum(ob$avg_rate)/num
    
    infoBox("Total Municipalis Selected:", 
            value = num, 
            subtitle = paste("Average Obesity Rate:", round(avg*100,2),"%"), 
            icon = icon("balance-scale"), 
            color = "orange")
  })
  

}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)