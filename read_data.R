# values of "name" corresponding to team data
team_vals = c("rain", "sun", "sand", "hail", "weatherless", "offense", "hyperoffense", "semistall", "stall", "balance")

# manually scrape usage links for given metagame and month
get_usage_links = function(generation, tier, year, month) {

    month = ifelse(month < 10, paste0("0", month), toString(month))
    
    url = paste0("https://www.smogon.com/stats/", year, "-", month, "/")
    
    webpage = tryCatch(read_html(url), error = function(e) return(NULL))
    
    if(is.null(webpage)) {
      return(character(0))
    }
    
    all_links = webpage %>%
      html_nodes("a") %>%
      html_attr("href")
    
    if(generation == "6" & (year < 2017 | (year == 2017 & as.integer(month) <= 6))){
      pattern = paste0("^", tier, "-\\d+\\.txt$")
    } else {
      pattern = paste0("^gen", generation, tier, "-\\d+\\.txt$")
    }
    
    links = all_links[stringr::str_detect(all_links, pattern)]
    
    if (length(links) == 0) {
      return(character(0))
    } else {
      return(paste0(url, links))
    }
  }

# manually scrape team links for given metagame and month
get_team_links = function(generation, tier, year, month) {
  
    month = ifelse(month < 10, paste0("0", month), toString(month))
    
    url = paste0("https://www.smogon.com/stats/", year, "-", month, "/metagame/")
    
    webpage = tryCatch(read_html(url), error = function(e) return(NULL))
    
    if(is.null(webpage)) {
      return(character(0))
    }
    
    all_links = webpage %>%
      html_nodes("a") %>%
      html_attr("href")
    
    if(generation == "6" & (year < 2017 | (year == 2017 & as.integer(month) <= 6))){
      pattern = paste0("^", tier, "-\\d+\\.txt$")
    } else {
      pattern = paste0("^gen", generation, tier, "-\\d+\\.txt$")
    }
    
    links = all_links[stringr::str_detect(all_links, pattern)]
    
    if (length(links) == 0) {
      return(character(0))
    } else {
      return(paste0(url, links))
    }
  }

# manually read usage from given link
read_usage_manual = function(file) {
  
  empty_data = data.frame(name = character(0), usage = numeric(0), 
                          date = character(0), elo = integer(0))
  
  tryCatch({
    data = fread(file, skip = 5, header = FALSE, strip.white = TRUE, sep = "|",
                 fill = TRUE, select = c(3, 4), na.string = "") %>%
      na.omit()
    
    colnames(data) = c("name", "usage")
    
    data$year = str_extract(file, "\\d{4}(?=\\-)") 
    data$month = str_extract(file, "(?<=\\-)\\d{2}")
    data$elo = as.integer(str_extract(file, "(?<=-)\\d+(?=\\.txt)"))
    
    data$usage = gsub("%", "", data$usage)
    data$usage = as.numeric(data$usage)
    
    data$date = as.Date(paste(data$year, data$month, "01", sep = "-"), format = "%Y-%m-%d")
    data = data %>%
      select(name, usage, date, elo)
    
    return(data)
  }, error = function(e) {
    return(empty_data) # return empty df if error (e.g. page exists but no data)
  })
}

# function to read teams/metagame data
read_teams_manual = function(file) {
  
  empty_data = data.frame(name = character(0), usage = numeric(0), 
                          date = character(0), elo = integer(0))
  
  tryCatch({
    data = readLines(file)
    data = data[data != ""]
    
    name = str_trim(str_extract(data, "[^\\.]*"))
    usage = str_trim(str_extract(data, "\\d+\\.\\d+")) %>% as.numeric()
    
    df = data.frame(name, usage)
    
    df = df %>% filter(name %in% team_vals)
    df$year = str_extract(file, "\\d{4}(?=\\-)")
    df$month = str_extract(file, "(?<=\\-)\\d{2}")
    df$elo = str_extract(file, "(?<=-)\\d+(?=\\.txt)")
    
    df$date = as.Date(paste(df$year, df$month, "01", sep = "-"), format = "%Y-%m-%d")
    df = df %>%
      select(name, usage, date, elo)
    
    return(df)
  }, error = function(e) {
    return(empty_data) # return empty df if error (e.g. page exists but no data)
  })
}

# read all data in range. type = "usage" or "team"
read_data = function(type, input_gen, input_tier, start_month, start_year, end_month, end_year, connection) {
  
  read_links = function(type, ...) {
    if (type == "usage") {
      return(get_usage_links(...))
    } else if (type == "team") {
      return(get_team_links(...))
    }
  }
  
  read_stats = function(type, ...) {
    if (type == "usage") {
      return(read_usage_manual(...))
    } else if (type == "team") {
      return(read_teams_manual(...))
    }
  }
  
  start_date = paste(start_year, start_month, "01", sep = "-")
  end_date = paste(end_year, end_month, "01", sep = "-")
  
  # load all data available in db
  query = sprintf("SELECT * FROM Data WHERE gen = ? AND tier = ? AND date >= ? AND date <= ?")
  
  df = dbGetQuery(connection, query, params = list(input_gen, input_tier, start_date, end_date))
  
  if (type == "usage") {
    df = df %>%
      filter(!(name %in% team_vals))
  } else if (type == "team") {
    df = df %>%
      filter(name %in% team_vals)
  }
  
  df = df %>%
    select(name, usage, date, elo)
  
  df$date = as.Date(df$date, format = "%Y-%m-%d")
  
  db_cutoff_date = max(df$date)
  
  if(db_cutoff_date == as.Date(end_date)) {
    return(df)
  } else {
    urls = list()
    cutoff_month = month(db_cutoff_date)
    cutoff_year = year(db_cutoff_date)
    
    for (curr_year in seq(cutoff_year, end_year)) {
      loop_start = ifelse(curr_year == cutoff_year, cutoff_month, 1)
      loop_end = ifelse(curr_year == end_year, as.integer(end_month), 12)
      
      for(curr_month in seq(loop_start, loop_end)) {
        if(curr_year == cutoff_year && curr_month == cutoff_month) {
          next
        }
        
        curr_urls = read_links(type, generation = input_gen, tier = input_tier, 
                                    year = curr_year, month = curr_month)
        urls = c(urls, curr_urls)
      }
    }
    
    new_data = urls %>%
      map_dfr(~ read_stats(type, .x))
    
    df = rbind(df, new_data)
    df = df %>%
      arrange(name, date)
    
    return(df)
  }
}