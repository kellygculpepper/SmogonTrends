library(tidyverse)
library(rvest)
library(lubridate)

# return df of all links (either type) given url, year, month
get_links_from_url = function(url, year, month) {
  
  webpage = tryCatch(read_html(url), error = function(e) return(NULL))
  
  if(is.null(webpage)) {
    return(NULL)
  }
  
  all_links = webpage %>%
    html_nodes("a") %>%
    html_attr("href")
  
  pattern = "gen([1-9])(ubers|ou|uu|ru|nu|pu)-([0-9]+)\\.txt$"
  matched_links = str_subset(all_links, pattern)
  link_data = str_match(matched_links, pattern)
  
  df = tibble(
    gen = link_data[, 2],
    tier = link_data[, 3],
    elo = as.integer(link_data[, 4]),
    year = year,
    month = month,
    link = paste0(url, matched_links)
  )
  
  # grab alternate format gen 6 links
  if(year < 2017 | (year == 2017 & as.integer(month) <= 6)) {
    alt_pattern = "(ubers|ou|uu|ru|nu|pu)-([0-9]+)\\.txt$"
    alt_matched_links = str_subset(all_links, alt_pattern)
    alt_link_data = str_match(alt_matched_links, alt_pattern)
    
    alt_df = tibble(
      gen = rep("6", nrow(alt_link_data)),
      tier = alt_link_data[, 2],
      elo = as.integer(alt_link_data[, 3]),
      year = year,
      month = month,
      link = paste0(url, alt_matched_links)
    )
    
    df = bind_rows(df, alt_df)
  }
  
  return(df)
}

# return df of all usage links for a month
get_monthly_usage_links = function(year, month) {
  
  month_str = ifelse(month < 10, paste0("0", month), toString(month))
  url = paste0("https://www.smogon.com/stats/", year, "-", month_str, "/")
  
  ans = get_links_from_url(url = url, year = year, month = month)
  return(ans)
}

# return df of all team links for a month
get_monthly_team_links = function(year, month) {
  
  month_str = ifelse(month < 10, paste0("0", month), toString(month))
  url = paste0("https://www.smogon.com/stats/", year, "-", month_str, "/metagame/")
  
  ans = get_links_from_url(url = url, year = year, month = month)
  return(ans)
}

# writes .csv file of link data up to last_month, last_year
generate_link_data = function(last_year, last_month) {
  
  usage_links_df_list = list()
  team_links_df_list = list()
  
  i = 1
  for (year in seq(2014, last_year)) {
    start_month = ifelse(year == 2014, 11, 1)
    end_month = ifelse(year == last_year, last_month, 12)
    
    for (month in start_month:end_month) {
      curr_usage_df = get_monthly_usage_links(year, month)
      usage_links_df_list[[i]] = curr_usage_df
      
      curr_team_df = get_monthly_team_links(year, month)
      team_links_df_list[[i]] = curr_team_df
      
      i = i + 1
    }
  }
  
  usage_links_df = rbindlist(usage_links_df_list)
  team_links_df = rbindlist(team_links_df_list)
  
  links_df = full_join(usage_links_df, team_links_df, 
                        by = c("elo", "tier", "gen", "year", "month"), 
                        suffix = c("_usage", "_team"))
  
  links_df = rename(links_df, usage_link = link_usage, team_link = link_team)
  
  write.csv(links_df, file = "data/links.csv", row.names = FALSE)
}

# given path to existing link data, update with latest month if available
update_link_data = function(link_data_file) {
  links_df = read.csv(file = link_data_file)
  
  date = Sys.Date()
  
  curr_year = as.integer(format(date, format = "%Y"))
  curr_month = as.integer(format(date, format = "%m"))
  
  if (curr_month == 1) {
    last_month = 12
    last_year = curr_year - 1
  } else {
    last_month = curr_month - 1
    last_year = curr_year
  }
  
  existing_rows = filter(links_df, year == last_year & month == last_month)
  
  if (nrow(existing_rows) == 0) {
    # if links from previous month are not in .csv, try fetching them
    new_usage_links_df = get_monthly_usage_links(year = last_year, month = last_month)
    
    # if new links are found, update .csv
    if (nrow(new_usage_links_df) > 0) {
      new_team_links_df = get_monthly_team_links(year = last_year, month = last_month)
      
      new_links_df = full_join(new_usage_links_df, new_team_links_df, 
                               by = c("elo", "tier", "gen", "year", "month"), 
                               suffix = c("_usage", "_team"))
      
      new_links_df = rename(new_links_df, usage_link = link_usage, team_link = link_team)
      
      updated_df = rbind(links_df, new_links_df)
      
      write.csv(updated_df, file = link_data_file, row.names = FALSE)
    }}
}

# create initial .csv with link data through July 2023
generate_link_data(last_year = 2023, last_month = 7)