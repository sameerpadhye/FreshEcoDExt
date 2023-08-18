#### R Shiny app for extracting Freshwater ecoregion information for any location ####

# Libraries used

require(shiny)
require(tidyverse)
require(sp)
require(sf)
require(leaflet)
require(shinythemes)
require(DT)
require(bslib)
require(thematic)
require(RColorBrewer)
require(webshot)
require(mapview)
require(htmltools)
library(randomcoloR)
library(janitor)

# Install this once on the machine to save the leaflet plots

webshot::install_phantomjs(force = TRUE)

# Convert all GIS based data into planar representations

sf_use_s2(FALSE)

# Define UI for data upload app 

ui<-navbarPage(
  
  theme = shinytheme("cerulean"),
  
  collapsible = TRUE,
  
  title = "FreshEcoDExt",
  
  ## Changes the font size of the navbar titles (navbar-brand) and tabpanels (navbar-static-top)
  
  tags$style(type = 'text/css', "
      .navbar.navbar-default.navbar-static-top{font-size: 18px;} 
      .navbar-default .navbar-brand {font-size: 26px;} "
             
  ),
 # Data input
  tabPanel("Data Input", 
           tags$h3(class = 'heading text-center', 
                   style = "color:steelblue", 
                   'What is FreshEcoDExt?'),
           p(h3("FreshEcoDExt (Freshwater Ecoregions Data Extraction and visualization Tool) is an app designed for extracting, transforming, visualizing and downloading information on freshwater ecoregions of the world. It builds upon the FEOW data/maps that are available on their webpage thereby giving more customized visualizations based on the user data.",
                align = 'center'),
             style = "color:steelblue"),
           fluidRow(column(12,
                           align = 'center',
                           fileInput("file1", 
                                     h4("Choose Input File (.csv)"),
                                     accept = c("text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv")
                           ),
                           p(h6("Please check the Read Me for Data input format and other details",
                                align = 'center')
                           
           )
           ),
           ## Map image specs
           HTML('<center><img src="world_fresheco.jpg" width="100%"></center>')
           
  )
  ),
  # Leaflet map
  tabPanel("Map",
                       fluidRow(column(width = 12,
                                       align="center",
                                       downloadButton("download2",
                                                      "Download Map"),
                                       downloadButton("download_shp",
                                                      "Download Shapefile")
                       )
                       ),
                       fluidRow(column(width = 12,
                                       leafletOutput("plot1",
                                                     height = 800))) 
                       
),
 # Tabular data
  tabPanel("Table",
         
         br(),
         
         fluidRow(column(width=12,
                         align="center",
                         DT::dataTableOutput("table1",
                                             width = '100%',
                                             height = '100%'))),
         fluidRow(column(width = 12,
                         align="center",
                         downloadButton("download", "Download Table")
         )
         )
),

# Heatmap
 tabPanel("Heatmap",
         fluidRow(column(width = 12,
                         align= 'center',
         plotOutput("heatmap",
                    width=1250,
                    height=650)
         )
         )
         ),

# Read Me
tabPanel("Read Me", 
         fluidRow(
           column(12,
                  align = 'center',
                  tags$h2(class = 'heading text-left', 
                          style = "color:steelblue", 
                          'What are Ecoregions?'),
                  p(h3("'An ecoregion (ecological region) is an ecologically and geographically defined area that is smaller than a bioregion, which in turn is smaller than a biogeographic realm. Ecoregions cover relatively large areas of land or water, and contain characteristic, geographically distinct assemblages of natural communities and species.' (Wikipedia)",
                       align = 'left'),
                    style = "color:steelblue"),
                  br(),
                  tags$h2(class = 'heading text-left', 
                          style = "color:steelblue", 
                          'What are Freshwater Ecoregions?'),
                  p(h3("'A freshwater ecoregion is a large area encompassing one or more freshwater systems that contains a distinct assemblage of natural freshwater communities and species. The freshwater species, dynamics, and environmental conditions within a given ecoregion are more similar to each other than to those of surrounding ecoregions and together form a conservation unit.'(Wikipedia).",
                       align = 'left'),
                    style = "color:steelblue"),
                  tags$a(href="https://www.feow.org/", 
                         p(h5("Click here to read more on Freshwater Ecoregions"),
                           align='center')
                         )
                           )
         ),
         br(),
         fluidRow(column(12,
                         align = 'center',
                         tags$h4(p("Data Input Format",
                                   align='center')),
                         tableOutput("sample_data")
                         
         )
         ),
         br(),
         fluidRow(column(12,
                         align = 'center',
                         tags$a(href="https://github.com/sameerpadhye/Ecoregions-and-Biomes-of-India--R-Shiny-app-/blob/main/readme.docx", 
                                p("How to use the app?",
                                  align='center')),
                         tags$div(
                           "Created by: ",
                           tags$a(href="https://sameermpadhye.wixsite.com/website", 
                                  "Sameer M. Padhye"),
                           tags$img(src = "Biologia Logo n.jpg",
                                    align = "center")
                         ),
                         p(h6("Made with", a("Shiny",
                                             href = "https://shiny.posit.co/")
                         )
                         )
         )
         )
)


  )

# Server

server <- function(input, output) {
  
  ##### Input (Loading the underlying data (India map and the ecoregions map))
  
  freshwater_eco<-read_sf("data/Freshwater_Ecoregions.shp")
  
  freshwater_eco<-st_transform(freshwater_eco,4326)
  
  # Creating a separate reactive object of the freshwater_eco shapefile (st_intersection doesnt consider reactive objects as inputs)
  
  # Reactive Tabular data output
  
  table_data <- reactive ({
    
    # checking if the necessary files are available and in the required format
    
    validate(need(freshwater_eco, 
                  message = FALSE))
    
    validate(need(input$file1$datapath, 
                  message = FALSE))
    
    # Calling the necessary data for creating the plot
    
    file <- input$file1
    
    extension <- tools::file_ext(file$datapath)
    
    validate(need(extension == "csv", 
                  "Please upload a csv file"))
    
    spatial_data<-read.csv(file$datapath)
    
    tabular_data<-SpatialPointsDataFrame(coords = spatial_data[,c(grep('^[Ll]on.*', colnames(spatial_data)),grep('^[Ll]at.*', colnames(spatial_data)))],
                                         data = spatial_data,
                                         proj4string = CRS("+proj=longlat +datum=WGS84"))%>%
      st_as_sf(.)%>%
      st_intersection(freshwater_eco,.)%>%
      data.frame(.)%>%
      dplyr::select(ECOREGION,
                    MHT_TXT,
                    matches("[lL]oca.*"),
                    matches("[Ll]on.*"),
                    matches("[Ll]at.*"))%>%
      dplyr::rename("Ecoregion"="ECOREGION",
                    "Subregion"="MHT_TXT")
    # Optional: Convert similar worded colnames to exact names
      # dplyr::rename_at(vars(matches("[lL]oca.*")),~"Locality")%>%
      # dplyr::rename_at(vars(matches("[Ll]on.*")),~"Longitude")%>%
      # dplyr::rename_at(vars(matches("[Ll]at.*")),~"Latitude")
    
  })
  
  
  ## Reactive heatmap based on the table_data
  
  heatmap<-reactive({
    
    # Calling (and validating) the necessary data for creating the plot
    
    validate(need(table_data(), 
                  message = FALSE))
    
    table_data = table_data()   
    
    # Generate a crosstab followed by a dataframe
    heatmap_data<-tabyl(table_data, 
                        Subregion,
                        Ecoregion)%>%
      data.frame(.)%>%
      pivot_longer(.,cols=-c(1), 
                   names_to = "Ecoregions", 
                   values_to = "Counts")%>%
      dplyr::arrange(desc(Counts)) 
    
    heatmap_data$Subregion<-str_to_title(heatmap_data$Subregion)
    
    heatmap_data$Ecoregions<-gsub("."," ",heatmap_data$Ecoregions,fixed = TRUE)
    
    heatmap_data$Ecoregions<-gsub("  ","",heatmap_data$Ecoregions,fixed = TRUE)
    
    heatmap_data%>%
      ggplot(aes(x = Subregion,
                 y = Ecoregions,
                 fill = Counts)) +
      geom_tile(color="white") +
      geom_text(aes(label = Counts)) + 
      xlab(label = "Subregions") + 
      ylab(label = "Ecoregions") +
      scale_fill_gradient(low = "orange2", 
                          high = "forestgreen",
                          na.value = "grey80")+
      theme(panel.grid.major.x=element_blank(), 
            panel.grid.minor.x=element_blank(), 
            panel.grid.major.y=element_blank(), 
            panel.grid.minor.y=element_blank(),
            panel.background=element_rect(fill="white"),
            strip.placement = "outside",
            axis.text.x = element_text(angle=25, 
                                       hjust=1,
                                       size = 16),
            plot.title = element_text(size=16,
                                      face="bold"),
            axis.text.y = element_text(size = 16),
            legend.text  = element_text(size=14, 
                                        face="bold"),
            legend.key.height = grid::unit(1,"cm"), # height of legend key
            legend.key.width  = grid::unit(0.6,"cm")) +
      scale_y_discrete(limits = rev(levels(as.factor(heatmap_data$Ecoregions))))+
      scale_x_discrete(limits = rev(levels(as.factor(heatmap_data$Subregion))))+
      ggtitle("Ecoregion-Subregion Frequency Heatmap") + 
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16)) +
      theme(legend.title=element_text(size=16)) 
    
  })    
  
  ## Reactive freshwater ecoregion output  
  
  ecoregion_shp<-reactive({
    
    validate(need(table_data(), 
                  message = FALSE))
    
    validate(need(freshwater_eco, 
                  message = FALSE))
    
    # Calling the necessary data for creating the plot
    
    table_data = table_data()   
    
    # Extract all the relevant ecoregion polygons based on the input data and bind into a single object
    
    shapes_semfinal<-lapply(table_data$Ecoregion, 
                            function (x) {
                              
                              step<-freshwater_eco$ECOREGION==x
                              
                              step2 = freshwater_eco[step,]
                              
                              return(step2)
                              
                            })%>%
      do.call(rbind,.)
    
  })
  
  # Leaflet plot output
  
  leaf_plot<-reactive({
    
    validate(need(table_data(), 
                  message = FALSE))
    
    
    # Calling the necessary data for creating the plot
    
    table_data = table_data()   
    
    shapes_for_map<-ecoregion_shp()
    
    # Color palette for the map using randomColor package
    
    #1. Number of color combinations needed
    
    n <- 600
    
    #2. Creating the pallette
    
    palette_shapes <- distinctColorPalette(n)
    
    # Defining the color palette
    
    pal <- colorFactor(palette = palette_shapes, 
                       levels= unique(freshwater_eco$ECOREGION))  
    # The map
    
    leaflet(shapes_for_map) %>%
      addProviderTiles("Esri.WorldGrayCanvas", 
                       group="WorldGrayCanvas") %>%
      addProviderTiles("Esri.WorldPhysical",
                       options = providerTileOptions(noWrap = TRUE), 
                       group="Physical Map") %>%
      addProviderTiles("Esri.WorldShadedRelief",
                       options = providerTileOptions(noWrap = TRUE), 
                       group="WorldShaded") %>%
      addPolygons(data=shapes_for_map,
                  color = 'black',
                  smoothFactor = 0.2,
                  weight = 1.1,
                  opacity = 0.3, 
                  fillOpacity = 0.6,
                  fillColor = ~pal(shapes_for_map$ECOREGION),
                  label = as.character(shapes_for_map$ECOREGION),
                  labelOptions=labelOptions(noHide = FALSE,
                                            textsize=14)
      )%>%
      addCircleMarkers(
        lng = ~table_data$Longitude, # These are options for the GIS points
        lat = ~table_data$Latitude,
        label = ~table_data$Locality,
        radius = 4, 
        opacity = 1,
        color = ~"gray40",
        stroke = FALSE,
        fillOpacity = 1)%>%
      addLegend("bottomleft", 
                pal = pal, 
                values = ~unique(shapes_for_map$ECOREGION),
                title = "Eco Regions",
                opacity = 0.6)%>%
      setView(lng = mean(table_data$Longitude),
              lat = mean(table_data$Latitude),
              zoom=4)%>%
      addScaleBar(position = 'topright')%>%
      addLayersControl(baseGroups = c("WorldGrayCanvas",
                                      "Physical Map",
                                      "WorldShaded"), 
                       options = layersControlOptions(collapsed = FALSE))
    
  })
  
  
  # Sample data displayed on the app
  
  sample_data<-data.frame(Location=c("Location1","Location2"),
                          Latitude=c(18.5045,28.5834),
                          Longitude=c(76.5034,56.6745))
  
  
  ### Output ###
  
  output$sample_data<-renderTable({sample_data})
  
  output$table1<- DT::renderDataTable({
    
    datatable(table_data(),
              class = 'cell-border stripe',
              caption = htmltools::tags$caption('Ecoregions & Subregions data of selected localities',
                                                style = 'heading; text-align: center; color:black; font-size: 18px'),
              rownames = FALSE)
    
  })
  
  # Heatmap
  
  output$heatmap<-renderPlot({heatmap()})
  
  # The leaflet plot  
  
  output$plot1 <- leaflet::renderLeaflet({leaf_plot()})
  
  # Download buttons
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$file1, 
             ".csv")
    },
    content = function(file) {
      write.csv(table_data(), 
                file)
    }
  )
  
  output$download2 <- downloadHandler(
    filename = paste0(input$file1, 
                      "_plot",
                      ".jpg"
    )
    
    , content = function(file) {
      mapshot(x = leaf_plot()
               , file = file
               , cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
               , selfcontained = FALSE # when this is not specified, the function produces a PDF of two pages: one of the leaflet map, the other a blank page.
      )
    } 
  )

  # This chunk of code has been directly adopted from https://stackoverflow.com/questions/41707760/download-a-shape-file-from-shiny
  
  output$download_shp <- downloadHandler(
    filename <- function() {
      
      "Data_shpExport.zip"
      
    },
    content = function(file) {
      withProgress(message = "Exporting Data", {
        
        incProgress(0.5)
        tmp.path <- dirname(file)
        shapefile_eco<-ecoregion_shp()
        name.base <- file.path(tmp.path,
                               "ecoregionshapefiles")
        name.glob <- paste0(name.base, ".*")
        name.shp  <- paste0(name.base, ".shp")
        name.zip  <- paste0(name.base, ".zip")
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        sf::st_write(shapefile_eco, 
                     dsn = name.shp,
                     driver = "ESRI Shapefile", 
                     quiet = TRUE)
        
        zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
        req(file.copy(name.zip, file))
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        
        incProgress(0.5)
      })
    }
  )
  
}

shinyApp(ui = ui, server = server)