library(shiny)
library(plotly)
library(shiny)
library(reshape2)
library(dplyr) # I think this is for filter()
library(png)
library(pdftools)
library(ggplot2)
library(patchwork)
library(jpeg)
library(googlesheets4)
library(DT)
library(readxl)
library(Rcpp)
library(shinythemes)
library(shinyscreenshot)

df_min <- read.csv('GlobalNStudyFinal_Amin.csv')
df_max <- read.csv('GlobalNStudyFinal_Amax.csv')
df_med <- read.csv('GlobalNStudyFinal_Amed.csv')
melt_df <- read.csv('melt_df.csv')
DataType_lst <- unique(df_min$data_type)
Fertilizer_lst <- c("Benchmark_max_Fertilizer","Benchmark_median_Fertilizer","Benchmark_min_Fertilizer")
Manure_lst <- c("Benchmark_max_Manure","Benchmark_median_Manure","Benchmark_min_Manure")
Fixation_lst <- c("Benchmark_max_Fixation","Benchmark_median_Fixation","Benchmark_min_Fixation")
Deposition_lst <- c("Benchmark_max_Deposition","Benchmark_median_Deposition","Benchmark_min_Deposition")
Harvest_lst <- c("Benchmark_max_Harvest","Benchmark_median_Harvest","Benchmark_min_Harvest")
keep <- c("Benchmark_median_Fertilizer", "Benchmark_median_Manure", 
          "Benchmark_median_Fixation", "Benchmark_median_Deposition", 
          "Benchmark_median_Harvest", "Benchmark_median_Area_km2")
ISO3_lst <- unique(melt_df$ISO3)
CountryNames <- unique(melt_df$Country)

l <- list(color = toRGB("grey"), width = 0.5)
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator'))

###########################################
ui <- fluidPage(theme = shinytheme("superhero"),
  titlePanel("Global Nitrogen Database"),
    tabsetPanel(
      tabPanel("Home page", fluid = TRUE,
          titlePanel("Welcome to the Global Nitrogen Database (Draft Application)!"),
          mainPanel(
                    tags$b("What is the Global Nitrogen Database?"),
                    p('Input–output estimates of nitrogen on cropland are essential for improving nitrogen management and better understanding the global nitrogen cycle. Here, we compare 13 nitrogen budget datasets covering 115 countries and regions over 1961–2015.'),
                    plotOutput("image1"),
                    tags$b("Why use the Global Nitrogen Database?"),
                    p('Although most datasets showed similar spatiotemporal patterns, some annual estimates varied widely among them, resulting in large ranges and uncertainty. In 2010, global medians (in TgN yr−1) and associated minimum–maximum ranges were 73 (64–84) for global harvested crop nitrogen; 161 (139–192) for total nitrogen inputs; 86 (68–97) for nitrogen surplus; and 46% (40–53%) for nitrogen use efficiency. Some of the most uncertain nitrogen budget terms by country showed ranges as large as their medians, revealing areas for improvement. A benchmark nitrogen budget dataset, derived from central tendencies of the original datasets, can be used in model comparisons and inform sustainable nitrogen management in food systems.'),
                    plotOutput("image2"),
                    br(),
                    hr(),
                    tags$b("Reading the graphs:"),
                    plotOutput("map1"),
                    p('When under the |World Map| tab, you are shown 3 drop down tabs in which you can edit to change the information that is displayed. You are able to change the input/output values of either min, median, or max cropland areas. You are also able to change the type of Nitrogen input/output: Fertilizer, manure, etc. After selecting the desired options, when you hover of a certain country a number will be displayed. This number is the estimates of that countries global nitrogen use.'),
                    hr(),
                    plotOutput("thumbnail"),
                    p('When under the |Individual Countries| tab, you are given a drop down menu in which you can select a country. In this graph, instead of selecting a type of Nitrogen input/output, all of them will be displayed within the graph. They will be stacked upon one another with a new line in red that shows the harvest within these types.')
          ),
          sidebarPanel(
            tags$a(href = "https://doi.org/10.1016/j.oneear.2021.08.015", "Source: Zhang et al., 2021", target="_blank")
          )
      ),
      tabPanel("World Map", fluid = TRUE,
  # Copy the line below to make a select box 
  selectInput("select", label = h6("Which Nitrogen Input/Output Would You Like to View?"), 
              choices = c('Fertilizer', 'Manure', 'Fixation', 'Deposition', 'Harvest'), 
              selected = 1),
  selectInput("select2", label = h6("Which Benchmark Would You like to View?"), 
              choices = c('min', 'max', 'median'), 
              selected = 1),
  selectInput("select3", label = h6("Normalize By Area: Min, Max, or Median Area (km^2)?"), 
              choices = c('min', 'max', 'median'), 
              selected = 1),
  fluidRow(plotlyOutput("map")),
  sidebarPanel(
    screenshotButton(label = "Download Image")
   # actionButton("go", "Download Image")
  )
      ),
      tabPanel("NUE", fluid = TRUE),
      tabPanel("Individual Countries", fluid = TRUE,
               selectInput("select", label = h6("Which Country's Nitrogen Input/Output Data Would You Like to View?"), 
                           choices = CountryNames, 
                           selected = 1),
               fluidRow(plotlyOutput("myPlot")),
               screenshotButton(label = "Download Image")),
    ),
  
  
)
###########################################
server <- function(input, output) {
  observeEvent(input$go, {
    screenshot()
  })
  
  output$'image1' <- renderPlot({
    my_image <- readPNG("\\Users\\JakeT\\Documents\\GlobalNitrogen_app_v3\\GlobalNitrogen_app_v3\\n_app\\GND-pics\\image1.png", native = TRUE)
    df_empty <- data.frame()
    ggplot(df_empty) +                  
      inset_element(p = my_image,
                    left = 0,
                    bottom = 0,
                    right = 1,
                    top = 1)
  })
  
  output$'image2' <- renderPlot({
    my_image <- readPNG("\\Users\\JakeT\\Documents\\GlobalNitrogen_app_v3\\GlobalNitrogen_app_v3\\n_app\\GND-pics\\image2.png", native = TRUE)
    df_empty <- data.frame()
    ggplot(df_empty) +                
      inset_element(p = my_image,
                    left = 0,
                    bottom = 0,
                    right = 1,
                    top = 1)
  })
  
  output$'map1' <- renderPlot({
    my_image <- readPNG("\\Users\\JakeT\\Documents\\GlobalNitrogen_app_v3\\GlobalNitrogen_app_v3\\n_app\\GND-pics\\map1.png", native = TRUE)
    df_empty <- data.frame()
    ggplot(df_empty) +                
      inset_element(p = my_image,
                    left = 0,
                    bottom = 0,
                    right = 1,
                    top = 1)
  })
  
  output$'thumbnail' <- renderPlot({
    my_image <- readPNG("\\Users\\JakeT\\Documents\\GlobalNitrogen_app_v3\\GlobalNitrogen_app_v3\\n_app\\GND-pics\\thumbnail_USA.png", native = TRUE)
    df_empty <- data.frame()
    ggplot(df_empty) +                
      inset_element(p = my_image,
                    left = 0,
                    bottom = 0,
                    right = 1,
                    top = 1)
  })
  
  output$"map" <- 
    renderPlotly({
      if (input$'select3'== 'min'){ # choose which normalized df you want to view
        df <- df_min
      }
      else if (input$'select3'== 'max'){
        df <- df_max
      }
      else if (input$'select3'== 'median'){
        df <- df_med
      }
      # create a string of user-chosen input/output (e.g., 'Benchmark_max_Fertilizer') based on user input
      data_selection <- paste0('Benchmark_',input$'select2' ,'_',input$'select')
      df_select <- filter(df, df$data_type == data_selection)
      
      if (data_selection %in% Fertilizer_lst){
        graph_color <- 'Reds'
      }
      else if (data_selection %in% Manure_lst){
        graph_color <- 'Greens'
      }
      else if (data_selection %in% Fixation_lst){
        graph_color <- 'Purples'
      }
      else if (data_selection %in% Deposition_lst){
        graph_color <- 'Oranges'
      }
      else if (data_selection %in% Harvest_lst){
        graph_color <- 'Greys'
      }
      plot_geo(df_select, frame = ~Year) %>% 
        add_trace(z = ~value, color = ~value, colors = graph_color, 
                  text = ~Country, locations = ~ISO3, marker = list(line = l)) %>% 
        colorbar(title = 'k ton N/ km^2') %>% # include limits = c(min, max)  in colorbar() if you want static legend (might have to be input/output specific)
        layout(title = 'Global And National Nitrogen Budgets<br>Source:<a href="https://www.nature.com/articles/s43016-021-00318-5">Zhang et al 2021</a>', 
               geo = g) %>% 
        config(displayModeBar = FALSE)
      })
  output$"myPlot" <- 
    renderPlotly({
      ISO_slct <- ISO3_lst[which(CountryNames==input$'select')[1]]
      iso_df <- melt_df[melt_df[,"ISO3"]== ISO_slct,]
      df_edt <- iso_df[iso_df$data_type %in% keep, ]
      new_names <- c('4_Fertilizer', '3_Manure', '2_Fixation', '1_Deposition', 
                     'Harvest', 'Area') 
      for (i in 1:length(keep)){
        df_edt$'data_type'[df_edt$'data_type' == keep[i]] <- new_names[i]}
      df_edt$Year <- as.numeric(as.character(df_edt$Year))
      sapply(df_edt, class)
      keepv2 <- c('4_Fertilizer', '3_Manure', '2_Fixation', '1_Deposition') # rename to control order in stack plot
      StckPlt_df <- df_edt[df_edt$data_type %in% keepv2, ]
      keepv3 <- c('Harvest')
      LnPlt_df <- df_edt[df_edt$data_type %in% keepv3, ]
      keepv4 <- c('4_Fertilizer', '3_Manure', '2_Fixation', '1_Deposition','Harvest')
      Ylim_df <- df_edt[df_edt$data_type %in% keepv4, ]
      
      StckPlt_df_v2 <- StckPlt_df %>% # create df that sums input values by year. Use max annual value as basis for ylim
        group_by(Year) %>%
        summarize(value = sum(value))
      
      myPlot <- ggplot(StckPlt_df, aes(x = Year, y = value, fill = data_type)) + # use 'value' if you want to exclude area data
        geom_area(position = 'stack') +
        scale_x_continuous(limits = c(1961,2015), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0,1.5*max(StckPlt_df_v2$value)), expand = c(0, 0)) +
        geom_line(data=LnPlt_df, aes(x=Year,y=value, colour = data_type), size = 2) + # use 'value' if you want to exclude area data
        scale_colour_manual(name="Outputs",labels=c("Harvest"), values=c("red")) +
        scale_fill_manual(name="Inputs",labels=c("Deposition", "Fixation", 
                                                 "Manure", "Fertilizer", ""), 
                          values=c("gold", "gray64", "sienna1", "steelblue", "white")) +
        xlab("Year (1961-2015)") +
        ylab("Median Benchmark Est. of Ag Inputs/Outputs (k ton of N)") +
        labs(title= ISO_slct) +
        theme_classic() + 
        theme(legend.title=element_blank())
    })
  }
###########################################
shinyApp(ui = ui, server = server)
