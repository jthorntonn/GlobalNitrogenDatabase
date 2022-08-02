################
library(rgdal)
library(shiny)
library(leaflet)
library(dplyr)
library(plotly)

# r_colors <- rgb(t(col2rgb(colors()) / 255))
# names(r_colors) <- colors()

# ui <- fluidPage(
#   leafletOutput("mymap"),
#   p(),
#   actionButton("recalc", "New points")
# )

getwd()
melt_df <- read.csv('melt_df.csv')
ISO3_lst <- unique(melt_df$ISO3)
CountryNames <- unique(melt_df$Country)
keep <- c("Benchmark_median_Fertilizer", "Benchmark_median_Manure",
          "Benchmark_median_Fixation", "Benchmark_median_Deposition",
          "Benchmark_median_Harvest", "Benchmark_median_Area_km2")

## Load the shape file to a Spatial Polygon Data Frame (SPDF) using the readOGR() function
myspdf = readOGR(dsn=getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")

## R Shiny ui code begins here
ui <- bootstrapPage(
  tags$style(type = "text/css", "html,
             body {width:100%;height:100%}"),
  leafletOutput("mymap", width= "90%", height = "50%"),
  p(),
  actionButton("recalc", "New points"),
  
  plotlyOutput("myPlot", width= "90%", height = "40%"),
  # Absolute panel will house the user input widgets
  # Div tag is used to add styling to absolute panel
  absolutePanel(top = 10, right = 10, fixed = TRUE,
                tags$div(style = "opacity: 0.40; background: #FFFFEE; padding: 8px; ",
                         helpText("Welcome to the World map"),
                         textOutput("text") # to display the lat, long and name in absolute panel when shape is clicked
                )
  )
)

## End of ui code

server <- function(input, output, session) {
  
  # points <- eventReactive(input$recalc, {
  #   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  # }, ignoreNULL = FALSE)
  #
  # worldCountry <-rgdal::readOGR(dsn=getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")
  #
  # #  Read in the Happy Planet data
  # happiness <- read.csv('https://remiller1450.github.io/data/HappyPlanet.csv', stringsAsFactors = FALSE)
  # ## Join the Happy Planet data to the countries in WorldCountry
  # CountryHappy <- left_join(data.frame(Name = worldCountry$NAME), happiness, by = c("Name" ="Country"))
  #
  # ## Create a color palette (using the "magma" color scheme)
  # pal <- colorBin("magma", domain = CountryHappy$LifeExpectancy)
  #
  # myLabels <- paste("<strong>", CountryHappy$Name, "</strong>", "<br/>",
  #                   "Life Expectancy:", CountryHappy$LifeExpectancy)
  #
  # myPopups <- paste("Happy Planet Rank", CountryHappy$HPIRank)
  #
  #
  #
  # output$mymap <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles()  %>%
  #     addMarkers(data = points()) %>%
  #     addPolygons(data=worldCountry,
  #                 fillColor = pal(CountryHappy$LifeExpectancy),
  #                 weight = 2,
  #                 layerId = ~ISO3,
  #                 opacity = 1,
  #                 color = "white",
  #                 fillOpacity = 0.7,
  #                 highlight = highlightOptions(
  #                   weight = 3,
  #                   color = "grey",
  #                   fillOpacity = 0.7,
  #                   bringToFront = TRUE),
  #                 label = lapply(myLabels, HTML),
  #                 popup = myPopups) %>%
  #     addLegend(pal = pal, values = CountryHappy$LifeExpectancy,
  #               title = "Life Expectancy", position = "bottomright")
  # })
  output$mymap <- renderLeaflet({
    # Create the map data and add ploygons to it
    leaflet(data=myspdf) %>%
      addTiles() %>%
      setView(lat=10, lng=0, zoom=2) %>%
      addPolygons(fillColor = "green",
                  highlight = highlightOptions(weight = 5,
                                               color = "red",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = ~ISO3,
                  layerId = ~ISO3) # add a layer ID to each shape. This will be used to identify the shape clicked
  })
  
  output$"myPlot" <-
    renderPlotly({ p <- plot_ly(type="scatter",mode="markers")
    p <- layout(p,title="test")
    p <- add_trace(p, x=0,y=0,name="ABC_test",mode="lines+markers")
    })
  
  
  # Zoom and set the view after click on state shape
  # input$mymap_shape_click will be NULL when not clicked initially to any shape
  # input$mymap_shape_click will have the ID, lat and lng corresponding to the shape clicked
  # Observe to update the view and zoom level when a shape(country in this case) is clicked
  observe(
    {  click = input$mymap_shape_click
    #  subset the spdf object to get the lat, lng and country name of the selected shape (Country in this case)
    print(click$id)
    sub = myspdf[myspdf$ISO3==input$mymap_shape_click$id, c("LAT", "LON", "NAME")]
    lat = sub$LAT
    lng = sub$LON
    nm=sub$ISO3
    if(is.null(click))
      return()
    else
      leafletProxy("mymap") %>%
      setView(lng = lng , lat = lat, zoom = 3) %>%
      clearMarkers() %>%
      addMarkers(lng =lng , lat = lat, popup = nm)
    # using lat long from spdf will not change the view on multiple clicks on the same shape
    
    }
  )
  
  # Observe to display click data when a shape (country in this case) is clicked
  observe(
    {click = input$mymap_shape_click
    sub = myspdf[myspdf$ISO3==input$mymap_shape_click$id, c("LAT", "LON", "NAME")]
    lat = sub$LAT
    lng = sub$LON
    nm = sub$ISO3
    if(is.null(click))
      return()
    else
      output$text <- renderText({paste("Latitude= ", lat,
                                       "Longitude=", lng,
                                       "Country=", nm
      )})}
  )
  
  observe({
    click = input$mymap_shape_click
    output$"myPlot" <-
      renderPlotly({
        # ISO_slct <- ISO3_lst[which(CountryNames==click$id)[1]]
        iso_df <- melt_df[melt_df[,"ISO3"]== click$id[1],]
        df_edt <- iso_df[iso_df$data_type %in% keep, ]
        new_names <- c('4_Fertilizer', '3_Manure', '2_Fixation', '1_Deposition',
                       'Harvest', 'Area')
        for (i in 1:length(keep)){
          df_edt$'data_type'[df_edt$'data_type' == keep[i]] <- new_names[i]}
        df_edt$Year <- as.numeric(as.character(df_edt$Year))
        sapply(df_edt, class)
        keepv2 <- c('4_Fertilizer', '3_Manure', '2_Fixation', '1_Deposition') # rename to control order in stack plot
        StckPlt_df <- df_edt[df_edt$data_type %in% keepv2, ]
        
        
        myPlot <- ggplot(StckPlt_df, aes(x = Year, y = value, group=data_type, color=data_type)) + geom_line()+
          theme(legend.position="none") +
          ggtitle("Nitrigon dataset") +
          xlab("Year (1961-2015)") +
          ylab("Median Benchmark Est. of Ag Inputs/Outputs (k ton of N)") +
          labs(title= click$id[1]) +
          theme_classic() +
          theme(legend.title=element_blank())
      })
  }
  )
}


# Run the application
shinyApp(ui = ui, server = server)