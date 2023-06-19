# load packages
library(tidyverse)
library(shiny)
library(shinythemes)

elo = 0 # see below

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
  # shinytheme("yeti"),
  sidebarLayout(
    sidebarPanel(
      selectInput("generation", "Generation", choices = paste0("Gen ", 1:9)),
      selectInput("tier", "Tier", choices = c("Ubers", "OU", "UU", "RU", "NU"),
                  selected = "OU"),
      #selectInput("elo", "Minimum ELO", choices = c(0, 1500, 1630, 1760)),
      fluidRow(
        column(width = 6, 
               selectInput("start_month", "Start Month", choices = months)
        ),
        column(width = 6,
               numericInput("start_year", "Start Year", 2023))),
      
      fluidRow(
        column(width = 6, 
               selectInput("end_month", "End Month", choices = months)
        ),
        column(width = 6,
               numericInput("end_year", "End Year", 2023))
      ),
      uiOutput("pokemon")
    ),
    
    mainPanel(
      plotOutput("usage_plot")
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
        urls[[paste0(year, "-", month_str)]] = paste0("https://www.smogon.com/stats/",
                                                      paste0(year, "-", month_str, "/", "gen", substr(input$generation, 5, 5), str_to_lower(input$tier), "-", elo, ".txt")) # change back to input$elo
      }
    }
    
    df = urls %>%
      map_dfr(~ read_usage(.))
    colnames(df) = c("pokemon", "usage", "year", "month")
    df$date = as.Date(paste(df$year, df$month, "01", sep = "-"), format = "%Y-%m-%d") # Create date column
    df = df %>% arrange(pokemon, date) # Ensure data is sorted by pokemon and date
    data(df) # update the reactive value
    unique_pokemon(unique(df$pokemon)) # update the unique Pokemon
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
    selected_data = data() %>% filter(pokemon %in% input$pokemon)
    ggplot(selected_data, aes(x = date, y = usage, color = pokemon, group = pokemon)) +
      geom_line() +
      labs(x = "Time", y = "Usage", color = "Pokémon") +
      theme_minimal() +
      ggtitle("Usage of selected Pokemon over time") +
      scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") 
  })
}

# run app
shinyApp(ui = ui, server = server)
