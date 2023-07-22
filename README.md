SmogonTrends
============

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

This project will allow users to visualize trends in Pokémon Showdown data over time. Current functionality includes visualizing usage rates, difference between high vs all ELO usage, weather, and team styles, as well as monthly summaries. The app will ultimately be hosted online.

Data
----
Usage data is sourced from [Smogon](https://www.smogon.com/stats/). Additionally, this project uses the `Pokemon.csv` dataset from [pokemonData](https://github.com/lgreski/pokemonData), authored by lgreski, to match Pokémon with their IDs, enabling scraping of colors.

Installation and Usage
----------------------
To run the app locally, clone this repository, open app.R, and run the code. This app requires the `tidyverse` and `shiny` libraries.
