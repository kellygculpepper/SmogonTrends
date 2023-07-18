# load packages
library(tidyverse)
library(shiny)
library(shinythemes)

# TODO
# test gen 6 data reading
# add links
# update header, fonts, colors, change some formatting on plot
# fix usage plot updating—-when changing dates etc. after already selecting
# pokemon it doesn't update
# consider tweaking the purple in the playstyle graph

source("colormatch.R")

# function to read usage data
read_usage = function(file) {
  data = fread(file, skip = 5, header = FALSE, strip.white = TRUE, sep = "|",
               fill = TRUE, select = c(3, 4), na.string = "") %>%
    na.omit()
  data$year = str_extract(file, "\\d{4}(?=\\-)") 
  data$month = str_extract(file, "(?<=\\-)\\d{2}")
  data$elo = str_extract(file, "(?<=-)\\d+(?=\\.txt)")
  return(data)
}

# function to read teams/metagame data
read_teams = function(file) {
  
  data = readLines(file)
  data = data[data != ""]
  
  names = str_trim(str_extract(data, "[^\\.]*"))
  percents = str_trim(str_extract(data, "\\d+\\.\\d+")) %>% as.numeric()
  
  df = data.frame(names, percents)
  
  selected_vals = c("weatherless", "rain", "sun", "sand", "hail", "multiweather", "offense", "hyperoffense", "semistall", "stall", "balance")
  df = df %>% filter(names %in% selected_vals)
  df$year = str_extract(file, "\\d{4}(?=\\-)")
  df$month = str_extract(file, "(?<=\\-)\\d{2}")
  df$elo = str_trim(str_extract(file, "(?<=-)\\d+(?=\\.txt)"))
  
  return(df)
}

# function to get links for metagame data
get_team_links = function(generation, tier, year, month) {
  url = paste0("https://www.smogon.com/stats/", year, "-", month, "/metagame/")
  
  webpage = read_html(url)
  all_links = webpage %>%
    html_nodes("a") %>%
    html_attr("href")
  
  if(generation == "6" & (year < 2017 | (year == 2017 & as.integer(month) <= 6))){
    pattern = paste0("^", tier, "-\\d+\\.txt$")
  } else {
    pattern = paste0("^gen", generation, tier, "-\\d+\\.txt$")
  }
  
  links = all_links[stringr::str_detect(all_links, pattern)]
  return(paste0(url, links))
}

# function to retrieve URLs for usage
get_data_links = function(generation, tier, year, month) {
  
  url = paste0("https://www.smogon.com/stats/", year, "-", month, "/")
  
  webpage = read_html(url)
  all_links = webpage %>%
    html_nodes("a") %>%
    html_attr("href")
  
  if(generation == "6" & (year < 2017 | (year == 2017 & as.integer(month) <= 6))){
    pattern = paste0("^", tier, "-\\d+\\.txt$")
  } else {
    pattern = paste0("^gen", generation, tier, "-\\d+\\.txt$")
  }
  
  links = all_links[stringr::str_detect(all_links, pattern)]
  
  # get links for 0 elo and highest elo
  link_digits = sapply(strsplit(sub("\\.txt", "", links), "-"), `[`, 2)
  link_digits = as.integer(link_digits)
  
  index_zero = which(link_digits == 0)
  index_max = which.max(link_digits)
  
  ans = c(links[index_zero], links[index_max])
  return(paste0(url, ans))
}

# calculate elo gap
# do we need handling for if a mon is listed in one elo's data but not the other?
calculate_gap = function(df) {
  df %>%
    group_by(pokemon, year, month) %>%
    mutate(elo_gap = usage[elo != 0] - usage[elo == 0]) %>%
    ungroup() %>%
    filter(elo == 0) %>%
    select(pokemon, year, month, usage, elo_gap)
}

months = ifelse(1:12 < 10, paste0("0", 1:9), 1:12)

# UI
ui = navbarPage(
    title = tags$div(
      class = "title-class",
      "SmogonTrends"),
    
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
          color: white;
        }
        
         .navbar .navbar-nav {
          float: right;
        }
      ")
    )
  ),
  
  tabPanel("Mons",
    fluidPage(
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
        tabPanel("ELO Gap", plotOutput("elo_gap_plot")) 
      )
    )
  )
  )
  ),
  
  tabPanel("Teams",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("teams_gen", "Generation", choices = paste0("Gen ", 1:9)),
                 selectInput("teams_tier", "Tier", choices = c("Ubers", "OU", "UU", "RU", "NU"),
                             selected = "OU"),
                 fluidRow(
                   column(width = 6, 
                          selectInput("teams_start_month", "Start Month", choices = months)),
                   column(width = 6,
                          numericInput("teams_start_year", "Start Year", 2023))
                 ),
                 
                 fluidRow(
                   column(width = 6, 
                          selectInput("teams_end_month", "End Month", choices = months)),
                   column(width = 6,
                          numericInput("teams_end_year", "End Year", 2023))
                 ),
                 uiOutput("teams_elo")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Weather", plotOutput("weather_plot")), 
                   tabPanel("Playstyle", plotOutput("style_plot")) 
                 )
               )
             )
           )),
  
  tabPanel("Monthly Summary",
           fluidPage(
             sidebarLayout(
               sidebarPanel(),
               mainPanel()
             )
           )),
  tabPanel("About")
)

# Server
server = function(input, output, session) {
  
  data = reactiveVal()
  unique_pokemon = reactiveVal()
  
  teams_data = reactiveVal()
  team_lvls = reactiveVal()
  
  # read data when user changes gen/tier/elo/time input (ELO removed for now, need to fix)
  observeEvent(list(input$generation, input$tier, input$start_month, input$start_year, input$end_month, input$end_year), {
    urls = list()
    for (year in seq(as.integer(input$start_year), as.integer(input$end_year))) {
      for (month in seq(as.integer(input$start_month), as.integer(input$end_month))) {
        month_str = ifelse(month < 10, paste0("0", month), toString(month))
    
        current_urls = get_data_links(generation = substr(input$generation, 5, 5), tier = tolower(input$tier), year = year, month = month_str)
        
        urls = c(urls, current_urls)
      }
      
    }
    
    df = urls %>%
      map_dfr(~ read_usage(.))
    colnames(df) = c("pokemon", "usage", "year", "month", "elo")
    df$usage = gsub("%", "", df$usage)
    df$usage = as.numeric(df$usage)
    df = df %>%
      calculate_gap()
    df$date = as.Date(paste(df$year, df$month, "01", sep = "-"), format = "%Y-%m-%d") # Create date column
    df = df %>%
      arrange(pokemon, date) %>% # sort so it will graph correctly
      match_colors() 
    data(df)
    unique_pokemon(unique(df$pokemon))
  })
  
  observeEvent(list(input$teams_gen, input$teams_tier, input$teams_start_month, input$teams_start_year, input$teams_end_month, input$teams_end_year), {
    urls = list()
    for (year in seq(as.integer(input$teams_start_year), as.integer(input$teams_end_year))) {
      for (month in seq(as.integer(input$teams_start_month), as.integer(input$teams_end_month))) {
        month_str = ifelse(month < 10, paste0("0", month), toString(month))
        
        current_urls = get_team_links(generation = substr(input$teams_gen, 5, 5), tier = tolower(input$teams_tier), year = year, month = month_str)
        urls = c(urls, current_urls)
      }
    }
  
  df = urls %>%
    map_dfr(~ read_teams(.))
  df$date = as.Date(paste(df$year, df$month, "01", sep = "-"), format = "%Y-%m-%d")
  df = df %>%
    arrange(names, date)
  teams_data(df)
  team_lvls(unique(df$elo))
  print(team_lvls)
  }
  )
  
  # mon input
  output$pokemon = renderUI({
    selectizeInput("pokemon", "Select Pokémon", choices = unique_pokemon(), multiple = TRUE, options = list(maxItems = 5))
  })
  
  # update mon input options
  observe({
    updateSelectizeInput(session, "pokemon", choices = unique_pokemon(), selected = NULL, server = TRUE)
  })
  
  
  selected_data = reactiveVal()
  
  observeEvent(input$pokemon, {
    selected_data(data() %>%
                    filter(pokemon %in% input$pokemon))
  })
  
  # teams elo input
  output$teams_elo = renderUI({
    selectInput("teams_elo", "Minimum ELO", choices = team_lvls())
  })
  
  # update teams elo input options
  observe({
    updateSelectizeInput(session, "teams_elo", choices = team_lvls(), server = TRUE)
  })
  
  teams_filtered = reactiveVal()
  
  observeEvent(input$teams_elo, {
    teams_filtered(teams_data() %>%
                     filter(elo == input$teams_elo))
  })
  
  # usage over time plot
  output$usage_plot = renderPlot({
    req(data())
    req(input$pokemon)
    selected_data = selected_data()
    color_mapping = setNames(selected_data$color, selected_data$pokemon)
    ggplot(selected_data, aes(x = date, y = usage, color = pokemon, group = pokemon)) +
      geom_line(linewidth = 1.2) +
      labs(x = "Time", y = "Usage", color = "Pokémon") +
      theme_minimal() +
      ggtitle("Usage over time") +
      scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
      scale_color_manual(values = color_mapping)
  })
  
  output$elo_gap_plot = renderPlot({
    req(data())
    req(input$pokemon)
    selected_data = selected_data()
    color_mapping = setNames(selected_data$color, selected_data$pokemon)
    ggplot(selected_data, aes(x = date, y = elo_gap, color = pokemon, group = pokemon)) +
      geom_line(linewidth = 1.2) +
      labs(x = "Time", y = "ELO Gap", color = "Pokémon") +
      theme_minimal() +
      ggtitle("Difference in Usage (Highest minus lowest) over time") +
      scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
      scale_color_manual(values = color_mapping)
  })


# excluding weatherless and multiweather for now)
# add handling for all weatherless
output$weather_plot = renderPlot({
  req(teams_data())
  req(input$teams_elo)
  teams_filtered = teams_filtered()
  teams_filtered %>%
    filter(names %in% c("rain", "sun", "sand", "hail")) %>%
    ggplot(aes(x = date, y = percents, color = names, group = names)) +
    geom_line(linewidth = 1.2) +
    labs(x = "Time", y = "Usage", color = "Weather") +
    theme_minimal() + 
    ggtitle("Usage over time") + 
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
    scale_color_manual(values = c("rain" = "#6890F0", "sun" = "#F08030", "sand" = "#B8A038", "hail" = "#98D8D8"),
                       labels = c("Rain", "Sun", "Sand", "Hail"))
})

output$style_plot = renderPlot({
  req(teams_data())
  req(input$teams_elo)
  teams_filtered = teams_filtered()
  teams_filtered %>%
    filter(names %in% c("hyperoffense", "offense", "balance", "semistall", "stall")) %>%
    mutate(names = factor(names, levels = c("hyperoffense", "offense", "balance", "semistall", "stall"))) %>% 
    ggplot(aes(x = date, y = percents, color = names, group = names)) +
    geom_line(linewidth = 1.2) +
    labs(x = "Time", y = "Usage", color = "Playstyle") +
    theme_minimal() + 
    ggtitle("Usage over time") + 
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
    scale_color_manual(values = c("hyperoffense" = "#d7191c", "offense" = "#fdae61", "balance" = "#9370DB", "semistall" = "#abd9e9", "stall" = "#2c7bb6"),
                       labels = c("Hyperoffense", "Offense", "Balance", "Semistall", "Stall"))
})

}

# run app
shinyApp(ui = ui, server = server)
