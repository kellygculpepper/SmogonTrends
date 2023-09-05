library(RSQLite)
library(DBI)

links_df = read.csv("data/links.csv")

# create db-formatted usage df (includes gen, tier) from row of links_df
read_usage_row = function(row) {

  empty_data = data.frame(gen = character(0), tier = character(0), name = character(0), usage = numeric(0), 
                          date = character(0), elo = integer(0))
  
  tryCatch({
    df = fread(row$usage_link, skip = 5, header = FALSE, strip.white = TRUE, sep = "|",
                 fill = TRUE, select = c(3, 4), na.string = "") %>%
      na.omit()
    
    colnames(df) = c("name", "usage")
    
    year = toString(row$year)
    month = ifelse(row$month < 10, paste0("0", row$month), toString(row$month))
    
    df$elo = row$elo
    df$gen = row$gen
    df$tier = row$tier
    
    df$usage = gsub("%", "", df$usage)
    df$usage = as.numeric(df$usage)
    
    df$date = paste(year, month, "01", sep = "-")
    
    df = df %>%
      arrange(name, date)
    
    return(df)
  }, error = function(e) {
    return(empty_data) # return empty df if error (e.g. page exists but no data)
  })
}

# create db-formatted team df (includes gen, tier) from row of links_df
read_teams_row = function(row) {
  
  empty_data = data.frame(gen = character(0), tier = character(0), name = character(0), usage = numeric(0), 
                          date = character(0), elo = integer(0))
  
  tryCatch({
    data = readLines(row$team_link)
    data = data[data != ""]
    
    name = str_trim(str_extract(data, "[^\\.]*"))
    usage = str_trim(str_extract(data, "\\d+\\.\\d+")) %>% as.numeric()
    
    df = data.frame(name, usage)
    
    selected_vals = c("weatherless", "rain", "sun", "sand", "hail", "offense", "hyperoffense", "semistall", "stall", "balance")
    df = df %>% filter(name %in% selected_vals)
    
    year = toString(row$year)
    month = ifelse(row$month < 10, paste0("0", row$month), toString(row$month))
    
    df$elo = row$elo
    df$gen = row$gen
    df$tier = row$tier
    
    df$date = paste(year, month, "01", sep = "-")
    
    df = df %>%
      arrange(name, date)
      
    return(df)
  }, error = function(e) {
    return(empty_data) # return empty df if error (e.g. page exists but no data)
  })
}

links_small = head(links_df)

con = dbConnect(RSQLite::SQLite(), dbname="data/smogon.db")

dbExecute(con, "CREATE TABLE IF NOT EXISTS Data (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            gen TEXT,
            tier TEXT,
            date DATETIME,
            name TEXT,
            usage REAL,
            elo INTEGER
          )"
)

start_index = 1
end_index = nrow(links_df)
for (i in seq(start_index, end_index)) {
  row = links_df[i, ]
  
  usage_df = read_usage_row(row)
  team_df = read_teams_row(row)
  all_data = rbind(usage_df, team_df)
  
  dbBegin(con)
  dbAppendTable(con, "Data", all_data)
  dbCommit(con)
  
  write(i, "checkpoint.txt")
}

dbExecute(con, "CREATE INDEX idx_gen_tier_date ON Data(gen, tier, date)")

dbDisconnect(con)
