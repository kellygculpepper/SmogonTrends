# load packages
library(tidyverse)
library(shiny)
library(shinythemes)

elo = 0 # see below

# TODO
# test gen 6 data reading
# add links
# update header, fonts, colors, change some formatting on plot

# TODO for elo gap:
# research about elo and about when each gen started
# create function that returns boolean of whether given generation was current
# during given month
# write code to read data using above function as conditional for which ELOs to 
# read
# write function to subtract the numbers for each month?


source("colormatch.R")

# helper function to read data
read_usage = function(file) {
  data = fread(file, skip = 5, header = FALSE, strip.white = TRUE, sep = "|",
               fill = TRUE, select = c(3,8), na.string = "") %>%
    na.omit()
  data$year = str_extract(file, "\\d{4}(?=\\-)") 
  data$month = str_extract(file, "(?<=\\-)\\d{2}")
  return(data)
}

months = ifelse(1:12 < 10, paste0("0", 1:9), 1:12)

# UI
ui = fluidPage(
  theme = shinytheme("yeti"),
  
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Bangers&display=swap", rel = "stylesheet"),
    
    tags$style(
      HTML("
        .title-class {
          font-family: 'Bangers', cursive;
          font-size: 40px;
          color: #000;
        }
      ")
    )
  ),
  
  tags$div(
    class = "title-class",
    "SmogonTrends"
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("generation", "Generation", choices = paste0("Gen ", 1:9)),
      selectInput("tier", "Tier", choices = c("Ubers", "OU", "UU", "RU", "NU"),
                  selected = "OU"),
      fluidRow(
        column(width = 6, 
               selectInput("start_month", "Start Month", choices = months)),
        column(width = 6,
               numericInput("start_year", "Start Year", 2023))
      ),
      
      fluidRow(
        column(width = 6, 
               selectInput("end_month", "End Month", choices = months)),
        column(width = 6,
               numericInput("end_year", "End Year", 2023))
      ),
      uiOutput("pokemon")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Usage", plotOutput("usage_plot")), 
        tabPanel("ELO Gap")  # placeholder
      )
    )
  )
)

# Server
server = function(input, output, session) {
  
  data <- reactiveVal()
  unique_pokemon <- reactiveVal()
  
  # read data when user changes gen/tier/elo/time input (ELO removed for now, need to fix)
  observeEvent(list(input$generation, input$tier, input$start_month, input$start_year, input$end_month, input$end_year), {
    urls = list()
    for (year in seq(as.integer(input$start_year), as.integer(input$end_year))) {
      for (month in seq(as.integer(input$start_month), as.integer(input$end_month))) {
        month_str = ifelse(month < 10, paste0("0", month), toString(month))
        
        # FIX, not working
        # meant to use alternative URL format for when gen 6 was current
        if (substr(input$generation, 5, 5) == "6" & (year < 2017 | (year == 2017 & month <= 06))) {
          urls[[paste0(year, "-", month_str)]] = paste0("https://www.smogon.com/stats/",
                                                        paste0(year, "-", month_str, "/", str_to_lower(input$tier), "-", elo, ".txt")) # fix elo later
        } else {
          urls[[paste0(year, "-", month_str)]] = paste0("https://www.smogon.com/stats/",
                                                        paste0(year, "-", month_str, "/", "gen", substr(input$generation, 5, 5), str_to_lower(input$tier), "-", elo, ".txt")) # fix elo later
        }
      }
      
    }
    
    df = urls %>%
      map_dfr(~ read_usage(.))
    colnames(df) = c("pokemon", "usage", "year", "month")
    df$date = as.Date(paste(df$year, df$month, "01", sep = "-"), format = "%Y-%m-%d") # Create date column
    df = df %>%
      arrange(pokemon, date) # sort so it will graph correctly
    data(df)
    unique_pokemon(unique(df$pokemon))
  })
  
  # mon input
  output$pokemon = renderUI({
    selectizeInput("pokemon", "Select Pokémon", choices = unique_pokemon(), multiple = TRUE, options = list(maxItems = 5))
  })
  
  # update mon input options
  observe({
    updateSelectizeInput(session, "pokemon", choices = unique_pokemon(), selected = NULL, server = TRUE)
  })
  
  # usage over time plot
  output$usage_plot = renderPlot({
    req(data())
    req(input$pokemon)
    selected_data = data() %>%
      filter(pokemon %in% input$pokemon) %>%
      match_colors()
    color_mapping <- setNames(selected_data$color, selected_data$pokemon)
    ggplot(selected_data, aes(x = date, y = usage, color = pokemon, group = pokemon)) +
      geom_line(linewidth = 1.2) +
      labs(x = "Time", y = "Usage", color = "Pokémon") +
      theme_minimal() +
      #ggtitle("Usage of selected Pokemon over time") +
      scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
      scale_color_manual(values = color_mapping)
  })
}

# run app
shinyApp(ui = ui, server = server)
