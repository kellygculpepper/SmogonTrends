library(tidyverse)
library(rvest)
library(imgpalr)
library(magrittr)
library(purrr)

mon_data = read.csv("data/Pokemon.csv", header = TRUE)

get_URL <- function(url, ID) {
  
  parsed_html <- read_html(url)
  
  table_rows <- parsed_html %>% html_nodes("tr")
  
  # loop over rows
  for (i in seq_along(table_rows)) {
    row <- table_rows[i]
    
    id_cells <- row %>% html_nodes("td[style*='monospace']")
    
    # check if row has ID cell
    if (length(id_cells) == 0) {
      next
    }
    
    id_cell <- id_cells %>% html_text()
    
    if (id_cell == ID) {
      links <- row %>% html_nodes("td > a") %>% html_attr("href")
      
      if (length(links) > 0) {
        # get first link
        return(paste0("https://bulbapedia.bulbagarden.net", links[1]))
      }
    }
  }
  
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

add_colors <- function(data) {
  
  page = "https://bulbapedia.bulbagarden.net/wiki/List_of_Pokémon_by_National_Pokédex_number"
  
  data$ID = sprintf("%04d", data$ID)
  data$ID = paste0("#", data$ID)
  
  data <- data %>%
    mutate(URL = map_chr(ID, function(id) get_URL(page, id)))
  
  data <- data %>%
    mutate(img_URL = map_chr(URL, get_img_URL))
  
  data <- data %>%
    mutate(color = map_chr(img_URL, function(img_url) {
      image_pal(file = img_url,
                n = 1, type = "div", k = 3, bw = c(0.1, 0.8), seq_by = "shv", plot = FALSE,
                seed = 777)
    }))
  
  return(data)
}


mon_data = mon_data %>%
  select(ID, Name, Form) %>%
  add_colors()

write.csv(mon_data, "data/colors.csv", row.names = FALSE)
