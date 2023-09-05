# load packages
library(tidyverse)
library(shiny)
library(shinythemes)
library(scales)
library(ggrepel)
library(showtext)
library(rvest)
library(data.table)
library(ggfun)
library(RSQLite)
library(DBI)
library(lubridate)

# TODO
# hyphen wrapping does not work well for ruin legendaries
# names in bottom legend have space to the right even when there's nothing to their right

font_add_google("Lato", "Lato")

source("colormatch.R")
source("read_data.R")

# calculate elo gap
calculate_gap = function(df) {
  df = df %>%
    group_by(date) %>%
    mutate(max_elo_in_group = max(elo)) %>%
    filter(elo == 0 | elo == max_elo_in_group) %>%
    select(-max_elo_in_group)
  
    df %>% 
      group_by(name, date) %>%
    mutate(
      elo_gap = ifelse(any(elo != 0), usage[elo != 0] - usage[elo == 0], -usage[elo == 0])
    ) %>%
    ungroup() %>%
    filter(elo == 0) %>%
    select(name, date, usage, elo_gap)
}

add_missing_combinations = function(df) {
  all_pokemon = unique(df$name)
  all_dates = unique(df$date)
  
  missing_combinations = expand.grid(name = all_pokemon,
                                      date = all_dates) %>%
    anti_join(df, by = c("name", "date"))
  
  if (nrow(missing_combinations) > 0) {
    missing_combinations = missing_combinations %>%
      mutate(usage = 0)
    
    df = bind_rows(df, missing_combinations)
  }
  
  return(df)
}

months = ifelse(1:12 < 10, paste0("0", 1:9), 1:12)

con = dbConnect(RSQLite::SQLite(), dbname = "data/smogon.db")

# UI
ui = navbarPage(
    title = tags$div(
      class = "title-class",
      "SmogonTrends"),
    
    # header = tagList(
    #   useShinydashboard()
    # ),
    
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
        
        .about-class {
          margin-top: -20px;
          padding-top: 0px; 
        }
        
        .about-class hr { margin-top: 0px; }
        
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
      selectInput("tier", "Tier", choices = c("Ubers", "OU", "UU", "RU", "NU", "PU"),
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
      uiOutput("pokemon"),
      uiOutput("usage_elo")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Usage", 
                 tags$div(
                   style = "margin-top: 20px;",
                   textOutput("usage_data_msg"),
                   plotOutput("usage_plot")
                   
                 )), 
        tabPanel("Elo Gap", 
                 tags$div(
                   style = "margin-top: 20px",
                   textOutput("elo_data_msg"),
                   plotOutput("elo_gap_plot"))
        )
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
                 selectInput("teams_tier", "Tier", choices = c("Ubers", "OU", "UU", "RU", "NU", "PU"),
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
                   tabPanel("Weather", 
                            tags$div(
                              style = "margin-top: 20px;",
                              textOutput("teams_msg"),
                              textOutput("weather_data_msg"),
                              plotOutput("weather_plot")
                            )),
                   tabPanel("Playstyle", 
                            tags$div(
                              style = "margin-top: 20px;",
                              textOutput("playstyle_data_msg"),
                              plotOutput("style_plot")
                            ))
                 )
               )
             )
           )),
  
  tabPanel("Monthly Summary",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("sum_gen", "Generation", choices = paste0("Gen ", 1:9)),
                 selectInput("sum_tier", "Tier", choices = c("Ubers", "OU", "UU", "RU", "NU", "PU"),
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
                   tabPanel("Usage", 
                            tags$div(
                              style = "margin-top: 20px; overflow-y: scroll; height:600px",
                              textOutput("sum_usage_data_msg"),
                              plotOutput("sum_usage_plot")
                            )),
                   tabPanel("Elo Gap",
                            tags$div(
                              style = "margin-top: 20px;",
                              textOutput("sum_elo_data_msg"),
                              plotOutput("sum_elo_plot", width = "100%")
                            )),
                   tabPanel("Weather", 
                            tags$div(
                              style = "margin-top: 20px;",
                              textOutput("sum_msg"),
                              textOutput("sum_weather_data_msg"),
                              plotOutput("sum_weather_plot")
                            )),
                   tabPanel("Playstyle", 
                            tags$div(
                              style = "margin-top: 20px;",
                              textOutput("sum_playstyle_data_msg"),
                              plotOutput("sum_style_plot")
                            ))
                 )
               )
             )
           )),
  tabPanel("About",
           fluidRow(class = "about-class",
             column(width = 12,
                    h3("About SmogonTrends"),
                    hr(),
                    p("SmogonTrends allows users to visualize usage and metagame data from Pokémon Showdown, a popular online battle simulator.
                      This app was developed by Kelly Culpepper using the R Shiny framework."),
                    p("The primary data source for this project is ", a("Smogon.", href = "https://www.smogon.com/stats/"), " Additional data is sourced from ", a("pokemonData,", href = "https://github.com/lgreski/pokemonData"), " authored by lgreski, and ", a("Bulbapedia.", href = "https://bulbagarden.net/")),
                    p("The full source code for this app is available on ", a("Github.", href = "https://github.com/kellygculpepper/SmogonTrends"), " Feel free to provide feedback or report any bugs via the Issues section.")
             )
           ))
)

# Server
server = function(input, output, session) {

  data = reactiveVal()
  unique_pokemon = reactiveVal()
  usage_lvls = reactiveVal()
  
  teams_data = reactiveVal()
  team_lvls = reactiveVal()
  
  sum_usage_data = reactiveVal()
  sum_teams_data = reactiveVal()
  sum_lvls = reactiveVal()
  
  usage_has_no_data = reactiveVal(FALSE)
  teams_has_no_data = reactiveVal(FALSE)
  sum_has_no_data = reactiveVal(FALSE)
  
  selected_pokemon = reactiveVal(character(0))
  
  # read data when user changes gen/tier/elo/time input (ELO removed for now, need to fix)
  observeEvent(list(input$generation, input$tier, input$start_month, input$start_year, input$end_month, input$end_year), {
    
    gen = as.integer(substr(input$generation, 5, 5))
    tier = tolower(input$tier)
    
    df = read_data(type = "usage", input_gen = gen, input_tier = tier, 
                   start_month = input$start_month, start_year = input$start_year,
                   end_month = input$end_month, end_year = input$end_year,
                   connection = con)
      
      if(nrow(df) == 0) {
        usage_has_no_data(TRUE)
      } else {
        usage_has_no_data(FALSE)
        
      df = df %>%
        add_missing_combinations()
      
      data(df)
      unique_pokemon(unique(df$name))
      
      choices = sort(unique_pokemon())
      selected = input$pokemon
      if (!is.null(selected)) {
        selected = intersect(selected, choices)
        selected_pokemon(selected)
      }
      
      output$pokemon = renderUI({
        selectizeInput("pokemon", "Select Pokémon", choices = choices, selected = selected, multiple = TRUE, options = list(maxItems = 5))
      })
      
      usage_lvls(unique(df$elo))
      
      output$usage_elo = renderUI({
        selectInput("usage_elo", "Minimum Elo", choices = sort(usage_lvls()))
      })
    }
  })
  
  # teams
  observeEvent(list(input$teams_gen, input$teams_tier, input$teams_start_month, input$teams_start_year, input$teams_end_month, input$teams_end_year), {
    
    gen = as.integer(substr(input$teams_gen, 5, 5))
    tier = tolower(input$teams_tier)
    
    df = read_data(type = "team", input_gen = gen, input_tier = tier, 
                   start_month = input$teams_start_month, start_year = input$teams_start_year,
                   end_month = input$teams_end_month, end_year = input$teams_end_year,
                   connection = con)
    
    if(nrow(df) == 0) {
      teams_has_no_data(TRUE)
    } else {
      teams_has_no_data(FALSE)
      
  teams_data(df)
  team_lvls(unique(df$elo))
  
  # teams elo input
  output$teams_elo = renderUI({
    selectInput("teams_elo", "Minimum Elo", choices = sort(team_lvls()))
  })
    }
  })
  
  # monthly summary
  observeEvent(list(input$sum_gen, input$sum_tier, input$sum_year, input$sum_month), {
  
    gen = as.integer(substr(input$sum_gen, 5, 5))
    tier = tolower(input$sum_tier)
    
    usage_df = read_data(type = "usage", input_gen = gen, input_tier = tier, 
              start_month = input$sum_month, start_year = input$sum_year,
              end_month = input$sum_month, end_year = input$sum_year,
              connection = con)
    
    teams_df = read_data(type = "team", input_gen = gen, input_tier = tier, 
                         start_month = input$sum_month, start_year = input$sum_year,
                         end_month = input$sum_month, end_year = input$sum_year,
                         connection = con)
      
      if(nrow(usage_df) == 0 | nrow(teams_df) == 0) {
        sum_has_no_data(TRUE)
      } else {
        sum_has_no_data(FALSE)

      sum_usage_data(usage_df)
    
      sum_teams_data(teams_df)
      
      sum_lvls(unique(teams_df$elo))
      
      output$sum_elo = renderUI({
        selectInput("sum_elo", "Minimum Elo", choices = sort(sum_lvls()))
      })
    }
  })
  
  selected_data = reactive({
   req(data(), input$pokemon)
    df = data() %>%
      filter(name %in% input$pokemon)
    df
  })
  
  teams_filtered = reactive({
    req(teams_data(), input$teams_elo)
    tf = teams_data() %>%
      filter(elo == input$teams_elo)
    tf
  })
  
  sum_usage_filtered = reactive({
    req(sum_usage_data(), input$sum_elo)
    suf = sum_usage_data() %>%
      filter(elo == input$sum_elo & usage >= 4.52) %>%
      calculate_gap()
    suf
  })
  
  sum_teams_filtered = reactive({
    req(sum_teams_data(), input$sum_elo)
    stf = sum_teams_data() %>%
      filter(elo == input$sum_elo)
    stf
  })
  
  legend_theme = theme(
    legend.position = "bottom",
    legend.title = element_blank(), 
    legend.box.background = element_roundrect(fill = "transparent", colour = "#DDDDDD", size = 0.7, linetype = "solid"),
    legend.text = element_text(margin = margin(r = 25, unit = "pt"))
  )
  
  # usage over time plot
  output$usage_plot = renderPlot({
    req(data())
    req(input$pokemon)
    if(usage_has_no_data()) {
      return(NULL)
    } else{
    selected_data = selected_data()
    selected_data = selected_data %>%
      filter(elo == input$usage_elo) %>%
      match_colors()
    color_mapping = setNames(selected_data$color, selected_data$name)
    
    num_months = length(unique(selected_data$date))
    if (num_months <= 6) {
      breaks = "1 month"
    } else {
      breaks = paste0(toString(num_months %/% 6), " months")
    }
    
    ggplot(selected_data, aes(x = date, y = usage, color = name, group = name)) +
      geom_line(linewidth = 1.2) +
      labs(x = "Month", y = "Usage (%)", color = "Pokémon", title = "Usage over time") +
      theme_minimal(base_size = 16, base_family = "Lato") +
      scale_x_date(date_breaks = breaks, labels = function(z) gsub("^0", "", strftime(z, "%m/%y"))) +
      scale_color_manual(values = color_mapping) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(), 
        legend.box.background = element_roundrect(fill = "transparent", colour = "#DDDDDD", size = 0.7, linetype = "solid"),
        legend.text = element_text(margin = margin(r = 25, unit = "pt"))
  ) 
  }}, height = 500)
  
  output$elo_gap_plot = renderPlot({
    req(data())
    req(input$pokemon)
    if(usage_has_no_data()) {
      return(NULL)
    } else {
    selected_data = selected_data()
    selected_data = selected_data %>%
      calculate_gap() %>%
      match_colors()
    color_mapping = setNames(selected_data$color, selected_data$name)
    
    num_months = length(unique(selected_data$date))
    if (num_months <= 6) {
      breaks = "1 month"
    } else {
      breaks = paste0(toString(num_months %/% 6), " months")
    }
    
    ggplot(selected_data, aes(x = date, y = elo_gap, color = name, group = name)) +
      geom_line(linewidth = 1.2) +
      labs(x = "Month", y = "Elo Gap", color = "Pokémon") +
      theme_minimal(base_size = 16, base_family = "Lato") +
      ggtitle("Difference in Usage (Highest Elo minus all) over time") +
      scale_x_date(date_breaks = breaks, labels = function(z) gsub("^0", "", strftime(z, "%m/%y"))) +
      scale_color_manual(values = color_mapping) +
      legend_theme
  }}, height = 500)

output$weather_plot = renderPlot({
  if(input$teams_gen != "Gen 1") {
  req(teams_data())
  req(input$teams_elo)
  
  if(teams_has_no_data()) {
    return(NULL)
  } else {
  teams_filtered = teams_filtered()
  teams_filtered = teams_filtered %>%
    filter(name %in% c("rain", "sun", "sand", "hail")) %>%
    mutate(name = factor(name, levels = c("rain", "sun", "sand", "hail")))
  
  num_months = length(unique(teams_filtered$date))
  if (num_months <= 6) {
    breaks = "1 month"
  } else {
    breaks = paste0(toString(num_months %/% 6), " months")
  }
  
    ggplot(teams_filtered, aes(x = date, y = usage, color = name, group = name)) +
    geom_line(linewidth = 1.2) +
    labs(x = "Month", y = "Usage (%)", color = "Weather", title = "Usage over time") +
    theme_minimal(base_size = 16, base_family = "Lato") + 
    scale_x_date(date_breaks = breaks, labels = function(z) gsub("^0", "", strftime(z, "%m/%y"))) +
    scale_color_manual(values = c("rain" = "#6890F0", "sun" = "#F08030", "sand" = "#B8A038", "hail" = "#98D8D8"),
                       labels = c("Rain", "Sun", "Sand", "Hail"))
  }
}}, height = 500)

output$style_plot = renderPlot({
  req(teams_data())
  req(input$teams_elo)
  if(teams_has_no_data()) {
    return(NULL)
  } else {
  teams_filtered = teams_filtered()
  teams_filtered = teams_filtered %>%
    filter(name %in% c("hyperoffense", "offense", "balance", "semistall", "stall")) %>%
    mutate(name = factor(name, levels = c("hyperoffense", "offense", "balance", "semistall", "stall")))
  
  num_months = length(unique(teams_filtered$date))
  if (num_months <= 6) {
    breaks = "1 month"
  } else {
    breaks = paste0(toString(num_months %/% 6), " months")
  }
  
    ggplot(teams_filtered, aes(x = date, y = usage, color = name, group = name)) +
    geom_line(linewidth = 1.2) +
    labs(x = "Month", y = "Usage (%)", color = "Playstyle") +
    theme_minimal(base_size = 16, base_family = "Lato") + 
    ggtitle("Usage over time") + 
    scale_x_date(date_breaks = breaks, labels = function(z) gsub("^0", "", strftime(z, "%m/%y"))) +
    scale_color_manual(values = c("hyperoffense" = "#d7191c", "offense" = "#fdae61", "balance" = "#E0B0D5", "semistall" = "#abd9e9", "stall" = "#2c7bb6"),
                       labels = c("Hyperoffense", "Offense", "Balance", "Semistall", "Stall"))
}}, height = 500)

output$sum_usage_plot = renderPlot({
  req(sum_usage_filtered())
  if(sum_has_no_data()) {
    return(NULL)
  } else {
  sum_usage_filtered = sum_usage_filtered()
  sum_usage_filtered = sum_usage_filtered %>%
    filter(usage >= 4.52)
  
  sum_usage_filtered = sum_usage_filtered[order(sum_usage_filtered$usage),]
  sum_usage_filtered$rank = nrow(sum_usage_filtered):1
  sum_usage_filtered$pokemon_ranked = sprintf("%d. %s", sum_usage_filtered$rank, sum_usage_filtered$name)
  
  sum_usage_filtered$name = str_replace(sum_usage_filtered$name, "-", "-\n")
  
  split_names = str_split_fixed(sum_usage_filtered$name, "-", 2)[, 1]
  name_lengths = nchar(split_names)
  
  name_size = (16*9) / max(name_lengths)
  
  # switch to reorder(pokemon_ranked, usage) again if want to put numbers back
  ggplot(sum_usage_filtered, aes(x = reorder(name, usage), y = usage, fill = usage)) +
    geom_bar(stat = "identity", width = 0.9) +
    scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 54.52) +
    coord_flip() +
    theme_void(base_size = 16, base_family = "Lato") +
    theme(axis.text.y = element_text(hjust = 1, size = name_size)) +
    labs(x = NULL, y = "Usage", title = paste0("Usage of ", input$sum_gen, " ", input$sum_tier, " Mons"),
         subtitle = paste0(input$sum_month, "-", input$sum_year)) +
    geom_text(aes(label = paste0(sprintf("%.1f", usage),"%")), position = position_stack(vjust = 0.5), color = 'black', size = 4) +
    guides(fill = FALSE)
  }
}, height = function(){30 * nrow(sum_usage_filtered())})

output$sum_elo_plot = renderPlot({
  req(sum_usage_data())
  if(sum_has_no_data()) {
    return(NULL)
  } else {
    sum_usage_data = sum_usage_data()
    sum_usage_data = sum_usage_data %>%
      filter(usage >= 4.52)
    sum_usage_data$date = as.Date(paste(sum_usage_data$year, sum_usage_data$month, "01", sep = "-"), format = "%Y-%m-%d")
    sum_usage_data = sum_usage_data %>%
      calculate_gap() %>%
      filter(elo_gap > 1) # hypothetically interested in all >0 but doing this for now
    
    sum_usage_data = sum_usage_data[order(sum_usage_data$elo_gap),]
    sum_usage_data$rank = nrow(sum_usage_data):1
    sum_usage_data$pokemon_ranked = sprintf("%d. %s", sum_usage_data$rank, sum_usage_data$name)
  
    sum_usage_data$name = str_replace(sum_usage_data$name, "-", "-\n")
    
    split_names = str_split_fixed(sum_usage_data$name, "-", 2)[, 1]
    name_lengths = nchar(split_names)
    
    name_size = (16*9) / max(name_lengths)
    
    ggplot(sum_usage_data, aes(x = reorder(name, elo_gap), y = elo_gap, fill = elo_gap)) +
      geom_bar(stat = "identity", width = 0.9) +
      scale_fill_gradient2(low = "red", mid = "yellow", high = "green") +
      coord_flip() +
      theme_void(base_size = 16, base_family = "Lato") +
      theme(axis.text.y = element_text(hjust = 1, size = name_size)) +
      labs(x = NULL, y = "Usage", title = paste0("Top Elo Gap, ", input$sum_gen, " ", input$sum_tier),
           subtitle = paste0(input$sum_month, "-", input$sum_year)) +
      geom_text(aes(label = paste0(sprintf("%.1f", elo_gap),"%")), position = position_stack(vjust = 0.5), color = 'black', size = 4) +
      guides(fill = FALSE)
        }
}, height = 600)

# plot_height = 400
output$sum_weather_plot = renderPlot({
  if (input$sum_gen != "Gen 1") {
  req(sum_teams_filtered())
  if(sum_has_no_data()) {
    return(NULL)
  } else {
  sum_teams_filtered = sum_teams_filtered()
  sum_teams_filtered = sum_teams_filtered %>%
    filter(name %in% c("rain", "sun", "sand", "hail", "weatherless")) %>%
    mutate(name = factor(name, levels = c("rain", "sun", "sand", "hail", "weatherless")))
  
  weather_color_mapping = c("rain" = "#6890F0", "sun" = "#F08030", "sand" = "#B8A038", "hail" = "#98D8D8", "weatherless" = "#cccccf")
  weather_label_mapping = c("rain" = "Rain", "sun" = "Sun", "sand" = "Sand", "hail" = "Hail", "weatherless" = "None")
  
  if (input$chart_type == "Bar") {
    ggplot(sum_teams_filtered, aes(x = name, y = usage, fill = name)) +
      geom_bar(stat = "identity") + 
      labs(y = "Usage", x = "Weather", title = "Weather Team Usage",
           subtitle = paste0(input$sum_month, "-", input$sum_year)) +
      theme_minimal(base_size = 16, base_family = "Lato") +
      scale_fill_manual(values = weather_color_mapping, drop = FALSE) +
      scale_x_discrete(labels = weather_label_mapping) +
      guides(fill = FALSE)
  }
  
  else {
    # plot_height = 500
    sum_teams_filtered2 = sum_teams_filtered %>% 
      mutate(csum = rev(cumsum(rev(usage))), 
             pos = usage/2 + lead(csum, 1),
             pos = if_else(is.na(pos), usage/2, pos))
    
    ggplot(sum_teams_filtered2, aes(x = "" , y = usage, fill = fct_inorder(name))) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = weather_color_mapping,
                        labels = weather_label_mapping, drop = FALSE) +
      geom_label_repel(data = sum_teams_filtered2,
                       aes(y = pos, label = paste0(round(usage), "%")),
                       size = 6.5, nudge_x = 0.7, show.legend = FALSE, box.padding = 0.1, label.size = 0,
                       color = "white", segment.color = "black") +
      theme_void(base_size = 16, base_family = "Lato") +
      labs(x = NULL, y = NULL, fill = "Weather", title = "Weather Team Usage", 
           subtitle = paste0(input$sum_month, "-", input$sum_year)) +
      legend_theme
  }
  }
}})

# , height = plot_height

output$sum_msg = renderText({
  if(input$sum_gen == "Gen 1") "Weather is unavailable in Gen 1."
})

output$sum_style_plot = renderPlot({
  req(sum_teams_filtered())
  if(sum_has_no_data()) {
    return(NULL)
  } else{
  sum_teams_filtered = sum_teams_filtered()
  sum_teams_filtered = sum_teams_filtered %>%
    filter(name %in% c("hyperoffense", "offense", "balance", "semistall", "stall")) %>%
    mutate(name = factor(name, levels = c("hyperoffense", "offense", "balance", "semistall", "stall")))
  
  style_color_mapping = c("hyperoffense" = "#d7191c", "offense" = "#fdae61", "balance" = "#E0B0D5", "semistall" = "#abd9e9", "stall" = "#2c7bb6")
  style_label_mapping = c("hyperoffense" = "Hyperoffense", "offense" = "Offense", "balance" = "Balance", "semistall" = "Semistall", "stall" = "Stall")
  
  if (input$chart_type == "Bar") {
    ggplot(sum_teams_filtered, aes(x = name, y = usage, fill = name)) +
      geom_bar(stat = "identity") +
      labs(y = "Usage", x = "Playstyle",  title = "Team Style Usage", subtitle = paste0(input$sum_month, "-", input$sum_year)) + 
      theme_minimal(base_size = 16, base_family = "Lato") + 
      scale_fill_manual(values = style_color_mapping, drop = FALSE) +
      scale_x_discrete(labels = style_label_mapping) +
      guides(fill = FALSE)
  }
  
  else {
    sum_teams_filtered2 = sum_teams_filtered %>% 
      mutate(csum = rev(cumsum(rev(usage))), 
             pos = usage/2 + lead(csum, 1),
             pos = if_else(is.na(pos), usage/2, pos))
    
    ggplot(sum_teams_filtered2, aes(x = "" , y = usage, fill = fct_inorder(name))) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = style_color_mapping,
                        labels = style_label_mapping, drop = FALSE) +
      geom_label_repel(data = sum_teams_filtered2,
                       aes(y = pos, label = paste0(round(usage), "%")), color = "white", segment.color = "black",
                       size = 6.5, nudge_x = 0.7, show.legend = FALSE, box.padding = 0.1, label.size = 0) +
      theme_void(base_size = 16, base_family = "Lato") +
      labs(x = NULL, y = NULL, fill = "Playstyle", title = "Team Style Usage", 
           subtitle = paste0(input$sum_month, "-", input$sum_year))
  }
}})

generate_data_message = function(has_no_data) {
  if (has_no_data) {
    return("No data available for selected parameters.")
  } else {
    return(NULL)
  }
}

output$teams_msg = renderText({
  if(input$teams_gen == "Gen 1") "Weather is unavailable in Gen 1."
})

output$usage_data_msg = renderText({
  generate_data_message(usage_has_no_data())
})

output$elo_data_msg = renderText({
  generate_data_message(usage_has_no_data())
})

output$playstyle_data_msg = renderText({
  generate_data_message(teams_has_no_data())
})

output$weather_data_msg = renderText({
  generate_data_message(teams_has_no_data())
})

output$sum_usage_data_msg = renderText({
  generate_data_message(sum_has_no_data())
})

output$sum_elo_data_msg = renderText({
  generate_data_message(sum_has_no_data())
})

output$sum_weather_data_msg = renderText({
  generate_data_message(sum_has_no_data())
})

output$sum_playstyle_data_msg = renderText({
  generate_data_message(sum_has_no_data())
})
}

# run app
shinyApp(ui = ui, server = server, onStop(function() {
  connection = get(con, envir = .GlobalEnv)
  dbDisconnect(connection)
}))