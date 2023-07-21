# load packages
library(tidyverse)
library(shiny)
library(shinythemes)
library(scales)

# TODO
# test gen 6 data reading
# add links
# update fonts, change some formatting on plot
# fix usage plot updating—-when changing dates etc. after already selecting
# pokemon it doesn't update
# moreover if you change e.g. gen after selecting pokemon, it will just crash
# (trying to run color match on mons that aren't in data)
# consider tweaking the purple in the playstyle graph
# metagame graphs like won't display the first time you do 0 elo, you have to
# click another elo and then back to 0???
# axis scaling?
# message for when there's no weather teams
# elo gap--handling for when a mon is not present in highest elo
# add reactive title for monthly usage summary
# fix size or placement of percents on bar chart

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
  df$elo = str_extract(file, "(?<=-)\\d+(?=\\.txt)")
  
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

# get links for usage data
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

# modified for monthly summary (all elo)
get_sum_usage_links = function(generation, tier, year, month) {
  
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
  
  return(paste0(url, links))
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
               sidebarPanel(
                 selectInput("sum_gen", "Generation", choices = paste0("Gen ", 1:9)),
                 selectInput("sum_tier", "Tier", choices = c("Ubers", "OU", "UU", "RU", "NU"),
                             selected = "OU"),
                 fluidRow(
                   column(width = 6, 
                          selectInput("sum_month", "Month", choices = months)),
                   column(width = 6,
                          numericInput("sum_year", "Year", 2023))
                 ),
                 selectInput("chart_type", "Chart Type", choices = c("Bar", "Pie")),
                 uiOutput("sum_elo")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Usage", plotOutput("sum_usage_plot")),
                   # elo gap? that would be weird to implement with elo selection though
                   tabPanel("Weather", plotOutput("sum_weather_plot")),
                   tabPanel("Playstyle", plotOutput("sum_style_plot"))
                 )
               )
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
  
  sum_usage_data = reactiveVal()
  sum_teams_data = reactiveVal()
  sum_lvls = reactiveVal()
  
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
  
  # teams
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
  
  # teams elo input
  output$teams_elo = renderUI({
    selectInput("teams_elo", "Minimum ELO", choices = team_lvls())
  })
  }
  )
  
  # monthly summary
  observeEvent(list(input$sum_gen, input$sum_tier, input$sum_year, input$sum_month), {
  
    usage_urls = get_sum_usage_links(generation = substr(input$sum_gen, 5, 5), tier = tolower(input$sum_tier), year = input$sum_year, month = input$sum_month)
    teams_urls = get_team_links(generation = substr(input$sum_gen, 5, 5), tier = tolower(input$sum_tier), year = input$sum_year, month = input$sum_month)
    
    usage_df = usage_urls %>%
      map_dfr(~ read_usage(.))
    colnames(usage_df) = c("pokemon", "usage", "year", "month", "elo")
    usage_df$usage = gsub("%", "", usage_df$usage)
    usage_df$usage = as.numeric(usage_df$usage)
    usage_df$date = as.Date(paste(usage_df$year, usage_df$month, "01", sep = "-"), format = "%Y-%m-%d")
    usage_df = usage_df %>%
      arrange(pokemon, date)
    sum_usage_data(usage_df)
    
    teams_df = teams_urls %>%
      map_dfr(~ read_teams(.))
    teams_df$date = as.Date(paste(teams_df$year, teams_df$month, "01", sep = "-"), format = "%Y-%m-%d")
    teams_df = teams_df %>%
      arrange(names, date)
    sum_teams_data(teams_df)
    
    sum_lvls(unique(teams_df$elo))
    
    output$sum_elo = renderUI({
      selectInput("sum_elo", "Minimum ELO", choices = sum_lvls())
    })
  })
  
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
  
  teams_filtered = reactiveVal()
  
  observeEvent(input$teams_elo, {
    tf = teams_data() %>%
      filter(elo == input$teams_elo)
    teams_filtered(tf)
    # print(tf) # looks like actual data is fine so idk
  })
  
  sum_usage_filtered = reactive({
    req(sum_usage_data(), input$sum_elo)
    suf = sum_usage_data() %>%
      filter(elo == input$sum_elo)
    suf
  })
  
  sum_teams_filtered = reactive({
    req(sum_teams_data(), input$sum_elo)
    stf = sum_teams_data() %>%
      filter(elo == input$sum_elo)
    stf
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
      ggtitle("Difference in Usage (Highest ELO minus all) over time") +
      scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
      scale_color_manual(values = color_mapping)
  })


# excluding weatherless and multiweather for now
# add handling for all weatherless
output$weather_plot = renderPlot({
  req(teams_data())
  req(input$teams_elo)
  teams_filtered = teams_filtered()
  teams_filtered %>%
    filter(names %in% c("rain", "sun", "sand", "hail")) %>%
    mutate(names = factor(names, levels = c("rain", "sun", "sand", "hail"))) %>%
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

output$sum_usage_plot = renderPlot({
  req(sum_usage_filtered())
  sum_usage_filtered = sum_usage_filtered()
  sum_usage_filtered = sum_usage_filtered %>%
    filter(usage >= 4.52)
  
  sum_usage_filtered = sum_usage_filtered[order(sum_usage_filtered$usage),]
  sum_usage_filtered$rank = nrow(sum_usage_filtered):1
  sum_usage_filtered$pokemon_ranked = paste(sum_usage_filtered$rank, ". ", sum_usage_filtered$pokemon)
  
  ggplot(sum_usage_filtered, aes(x = reorder(pokemon_ranked, usage), y = usage, fill = usage)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 54.52) +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y = element_text(hjust = 1)) +
    labs(y = "Usage") +
    geom_text(aes(label = paste0(sprintf("%.2f", usage),"%")), position = position_stack(vjust = 0.5), color = 'black') +
    ggtitle("Usage Ranking")
  
})

# figure out colors
output$sum_weather_plot = renderPlot({
  req(sum_teams_filtered())
  sum_teams_filtered = sum_teams_filtered()
  sum_teams_filtered = sum_teams_filtered %>%
    filter(names %in% c("rain", "sun", "sand", "hail", "weatherless", "multiweather")) %>%
    mutate(names = factor(names, levels = c("rain", "sun", "sand", "hail", "weatherless", "multiweather")))
  
  weather_color_mapping = c("rain" = "#6890F0", "sun" = "#F08030", "sand" = "#B8A038", "hail" = "#98D8D8", "weatherless" = "#cccccf", "multiweather" = "#8c8c8f")
  weather_label_mapping = c("rain" = "Rain", "sun" = "Sun", "sand" = "Sand", "hail" = "Hail", "weatherless" = "None", "multiweather" = "Multiple")
  
  if (input$chart_type == "Bar") {
    ggplot(sum_teams_filtered, aes(x = names, y = percents, fill = names)) +
      geom_bar(stat = "identity") + 
      labs(y = "Usage", fill = "Weather") +
      theme_minimal() +
      ggtitle("Weather Team Usage") + 
      scale_fill_manual(values = weather_color_mapping,
                        labels = weather_label_mapping, drop = FALSE) 
  }
  
  else {
    ggplot(sum_teams_filtered, aes(x = "", y = percents, fill = names)) +
      geom_bar(stat = "identity", width = 1) + 
      coord_polar("y", start=0) +
      labs(x = NULL, y = NULL, fill = "Weather") +
      theme_void() +
      theme(legend.position = "right") +
      ggtitle("Weather Team Usage") + 
      scale_fill_manual(values = weather_color_mapping,
                        labels = weather_label_mapping, drop = FALSE) +
      geom_text(aes(label = sprintf("%.2f%%", percents)), position = position_stack(vjust = 0.5))
  }
})

output$sum_style_plot = renderPlot({
  req(sum_teams_filtered())
  sum_teams_filtered = sum_teams_filtered()
  sum_teams_filtered = sum_teams_filtered %>%
    filter(names %in% c("hyperoffense", "offense", "balance", "semistall", "stall")) %>%
    mutate(names = factor(names, levels = c("hyperoffense", "offense", "balance", "semistall", "stall")))
  
  style_color_mapping = c("hyperoffense" = "#d7191c", "offense" = "#fdae61", "balance" = "#9370DB", "semistall" = "#abd9e9", "stall" = "#2c7bb6")
  style_label_mapping = c("hyperoffense" = "Hyperoffense", "offense" = "Offense", "balance" = "Balance", "semistall" = "Semistall", "stall" = "Stall")
  
  if (input$chart_type == "Bar") {
    ggplot(sum_teams_filtered, aes(x = names, y = percents, fill = names)) +
      geom_bar(stat = "identity") +
      labs(y = "Usage", fill = "Playstyle") + 
      theme_minimal() + 
      ggtitle("Team Style Usage") +
      scale_fill_manual(values = style_color_mapping, labels = style_label_mapping,
                        drop = FALSE)
  }
  else {
    ggplot(sum_teams_filtered, aes(x = "", y = percents, fill = names)) +
      geom_bar(stat = "identity", width = 1) + 
      coord_polar("y", start=0) +
      labs(x = NULL, y = NULL, fill = "Playstyle") +
      theme_void() +
      theme(legend.position = "right") +
      ggtitle("Team Style Usage") + 
      scale_fill_manual(values = style_color_mapping,
                        labels = style_label_mapping, drop = FALSE) +
      geom_text(aes(label = sprintf("%.2f%%", percents)), position = position_stack(vjust = 0.5))
  }
})

}

# run app
shinyApp(ui = ui, server = server)
