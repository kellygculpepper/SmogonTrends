library(tidyverse)
library(rvest)
library(imgpalr)
library(purrr)

mon_data = read.csv("Pokemon.csv", header = TRUE)

getID <- function(x) {
  # nidoran handling, fix later (also need to fix urshifu single strike, possibly others)
  if (x == "NidoranF") return(29)
  if (x == "NidoranM") return(32)
  
  # check for hyphen
  if (!str_detect(x, "-")) {
    return(mon_data %>% filter(Name == x, str_trim(Form) == "") %>% pull(ID) %>% first())
  }
  
  # handle regionals
  if (str_detect(x, "Galar|Paldea|Alola")) {
    region <- str_extract(x, "Galar|Paldea|Alola")
    name <- str_extract(x, "^[^-]*")
    return(mon_data %>% filter(str_detect(Form, region), str_detect(Form, name)) %>% pull(ID) %>% first())
  }
  
  # split by hyphen
  parts <- str_split(x, "-", simplify = TRUE)
  
  # attempt to match form
  form_match <- mon_data %>% filter(sapply(Form, function(f) all(str_detect(tolower(f), tolower(parts[2:length(parts)]))))) %>% pull(ID) %>% first()
  if (!is.na(form_match)) return(form_match)
  
  # if matching form fails use name
  return(mon_data %>% filter(str_detect(tolower(Name), tolower(parts[1]))) %>% pull(ID) %>% first())
}

add_IDs <- function(df) {
  df %>% mutate(ID = sapply(pokemon, getID))
}

reformat_df <- function(df) {
  # convert ID to 4 digits
  df$ID <- sprintf("%04d", df$ID)
  
  # hyphens to underscores
  df$pokemon <- gsub("(.+)-(.+)-", "\\1-\\2_", df$pokemon)
  
  return(df)
}

# NEEDS ERROR HANDLING
add_color <- function(df) {
  
  get_color <- function(ID, pokemon) {
    url <- paste0("https://bulbapedia.bulbagarden.net/wiki/File:", ID, pokemon, ".png")
    web_content <- read_html(url)
    img_url <- web_content %>% html_nodes(xpath = '//meta[@property="og:image"]') %>% html_attr('content')
    color <- image_pal(file = img_url, n = 1, type = "div", k = 3, bw = c(0.1, 0.8), seq_by = "shv", plot = FALSE)
    return(color)
  }
  
  df$color <- apply(df, 1, function(row) get_color(row['ID'], row['pokemon']))
  
  return(df)
}


# TEST CASE
pokemon_names <- c("Tauros")
df <- data.frame(pokemon = pokemon_names)
final_df = df %>%
  add_IDs() %>%
  reformat_df() %>%
  add_color()

print(final_df)
