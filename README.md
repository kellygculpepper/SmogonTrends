SmogonTrends
============

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

This project allows users to visualize trends in Pokémon Showdown data over time. Current functionality includes visualizing usage rates, difference between high vs all Elo usage, weather, and team styles, as well as monthly summaries. The app is hosted online [here](https://kculpepper.shinyapps.io/SmogonTrends/).

Data
----
Usage data is sourced from [Smogon](https://www.smogon.com/stats/). Additionally, this project uses [Bulbapedia](https://bulbagarden.net/) and the `Pokemon.csv` dataset from [pokemonData](https://github.com/lgreski/pokemonData), authored by lgreski, to match Pokémon to appropriate colors.

Installation and Usage
----------------------
To run the app locally, clone this repository and run app.R. This app requires the packages `tidyverse`, `shiny`, `shinythemes`, `scales`, `ggrepel`, `magrittr`, `rvest`, `data.table`, and `purrr`. 
