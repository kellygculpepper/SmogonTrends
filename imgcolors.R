library(tidyverse)
library(rvest)
library(imgpalr)
library(magrittr)
library(purrr)

mon_data = read.csv("data/Pokemon.csv", header = TRUE)

getID = function(x) {
  # nidoran handling, fix later (also need to fix urshifu single strike, possibly others)
  if (x == "NidoranF") return(29)
  if (x == "NidoranM") return(32)
  
  # check for hyphen
  if (!str_detect(x, "-")) {
    return(mon_data %>% filter(Name == x, str_trim(Form) == "") %>% pull(ID) %>% first())
  }
  
  # handle regionals
  if (str_detect(x, "Galar|Paldea|Alola")) {
    region = str_extract(x, "Galar|Paldea|Alola")
    name = str_extract(x, "^[^-]*")
    return(mon_data %>% filter(str_detect(Form, region), str_detect(Form, name)) %>% pull(ID) %>% first())
  }
  
  # split by hyphen
  parts = str_split(x, "-", simplify = TRUE)
  
  # attempt to match form
  form_match = mon_data %>% filter(sapply(Form, function(f) all(str_detect(tolower(f), tolower(parts[2:length(parts)]))))) %>% pull(ID) %>% first()
  if (!is.na(form_match)) return(form_match)
  
  # if matching form fails use name
  return(mon_data %>% filter(str_detect(tolower(Name), tolower(parts[1]))) %>% pull(ID) %>% first())
}

add_IDs = function(df) {
  df= df %>% mutate(ID = sapply(pokemon, getID))
  df$ID = sprintf("%04d", df$ID)
  df$ID = paste0("#", df$ID)
  return(df)
}

get_URL = function(url, ID) {
  # Parse the HTML from the URL
  parsed_html = read_html(url)
  
  # Extract the table rows
  table_rows = parsed_html %>% html_nodes("tr")
  
  # Loop over the table rows
  for (i in seq_along(table_rows)) {
    row = table_rows[i]
    
    # Extract the ID cell
    id_cells = row %>% html_nodes("td[style*='monospace']")
    
    # Check if the row contains an ID cell
    if (length(id_cells) == 0) {
      next
    }
    
    id_cell = id_cells %>% html_text()
    
    # Check if the ID matches the target ID
    if (id_cell == ID) {
      # Extract the link
      links = row %>% html_nodes("td > a") %>% html_attr("href")
      
      # Check if any link was found
      if (length(links) > 0) {
        # Return the first link only, prepended with base URL
        return(paste0("https://bulbapedia.bulbagarden.net", links[1]))
      }
    }
  }
  
  # Return NULL if no matching ID or URL was found
  return(NULL)
}



get_img_URL = function(url) {
  
  web = read_html(url)
  
  meta_tag = html_nodes(web, xpath = '//meta[@property="og:image"]')
  content_attr = html_attr(meta_tag, "content")
  
  # only use 1st link
  img_url = content_attr[[1]]
  
  return(img_url)
}

get_color = function(img_url) {
  color = image_pal(file = img_url,
                  n = 1, type = "div", k = 3, bw = c(0.1, 0.8), seq_by = "shv")
  return(color)
}

add_colors = function(df) {
  
  page = "https://bulbapedia.bulbagarden.net/wiki/List_of_Pokémon_by_National_Pokédex_number"
  
  # Add IDs
  df = add_IDs(df)
  
  df = df %>%
    mutate(URL = map_chr(ID, function(id) get_URL(page, id)))
  
  df = df %>%
    mutate(img_URL = map_chr(URL, get_img_URL))
  
  df = df %>%
    mutate(color = map_chr(img_URL, function(img_url) {
      image_pal(file = img_url,
                n = 1, type = "div", k = 3, bw = c(0.1, 0.8), seq_by = "shv", plot = FALSE)
    }))
  
  df = df %>%
    select(pokemon, ID, color)
  
  return(df)
}


# TEST CASE
pokemon_names = c("Tauros", "Meowth-Alola")
df = data.frame(pokemon = pokemon_names)
final_df = df %>%
  add_colors()
print(final_df)