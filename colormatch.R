library(tidyverse)
library(magrittr)
library(purrr)

color_data = read.csv("data/colors.csv", header = TRUE)

getID = function(x) {
  # nidoran handling
  if (x == "NidoranF") return("#0029")
  if (x == "NidoranM") return("#0032")
  
  # check for hyphen
  if (!str_detect(x, "-")) {
    return(color_data %>% filter(Name == x, str_trim(Form) == "") %>% pull(ID) %>% first())
  }
  
  # handle regionals
  if (str_detect(x, "Galar|Paldea|Alola")) {
    region = str_extract(x, "Galar|Paldea|Alola")
    name = str_extract(x, "^[^-]*")
    return(color_data %>% filter(str_detect(Form, region), str_detect(Form, name)) %>% pull(ID) %>% first())
  }
  
  # split by hyphen
  parts = str_split(x, "-", simplify = TRUE)
  
  # attempt to match form
  form_match = color_data %>% filter(sapply(Form, function(f) all(str_detect(tolower(f), tolower(parts[2:length(parts)]))))) %>% pull(ID) %>% first()
  if (!is.na(form_match)) return(form_match)
  
  # if matching form fails use name
  return(color_data %>% filter(str_detect(tolower(Name), tolower(parts[1]))) %>% pull(ID) %>% first())
}

add_IDs = function(df) {
  df = df %>% mutate(ID = sapply(pokemon, getID))
  return(df)
}

match_colors <- function(df) {
  df = add_IDs(df)
  df = merge(df, color_data, by = "ID")
  return(df)
}
