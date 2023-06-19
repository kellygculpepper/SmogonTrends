library(tidyverse)
library(rvest)
library(imgpalr)

mon_data = read.csv("Pokemon.csv", header = TRUE)

# remove this later
mon_data %>%
  filter(str_trim(Form) != "") %>%
  nrow()

# add code to standardize names with how Smogon has them and only include
# relevant forms

# EXAMPLE of how to get image URL from normal URL

url <- "https://bulbapedia.bulbagarden.net/wiki/File:0623Golurk.png"
# ^ can get this using ID and name from df
web_content <- read_html(url)
img_url <- web_content %>% html_nodes(xpath = '//meta[@property="og:image"]') %>% html_attr('content')
print(img_url)
image_pal(file = img_url,
          n = 1, type = "div", k = 3, bw = c(0.1, 0.8), seq_by = "shv", plot = TRUE)


