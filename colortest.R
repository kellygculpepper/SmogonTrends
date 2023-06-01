# RANDOM BLEND

set.seed(420)

rgbBlendRandomWeighted = function(base_hex, w) {
  base_rgb = col2rgb(base_hex)
  
  # Generate components for a random RGB color
  r = runif(min = 0, max = 255, n = 1)
  g = runif(min = 0, max = 255, n = 1)
  b = runif(min = 0, max = 255, n = 1)
  
  rb = base_rgb[1]
  gb = base_rgb[2]
  bb = base_rgb[3]
  
  # Check bounds for weight parameter
  w = ifelse(w > 1, 1, w)
  w = ifelse(w < 0, 0, w)
  
  new_r = sqrt((1 - w) * rb^2 + w * r^2)
  new_g = sqrt((1 - w) * gb^2 + w * g^2)
  new_b = sqrt((1 - w) * bb^2 + w * b^2)
  
  return (rgb(new_r, new_g, new_b, alpha = 1, maxColorValue = 255, names = FALSE))
}


# Define the primary Pokemon types
types = c("Normal", "Fire", "Water", "Electric", "Grass", "Ice", "Fighting", "Poison", "Ground", "Flying", "Psychic", "Bug", "Rock", "Ghost", "Dragon", "Dark", "Steel", "Fairy")

# Define the official colors for each type
type_colors = c("#A8A878", "#F08030", "#6890F0", "#F8D030", "#78C850", "#98D8D8", "#C03028", "#A040A0", "#E0C068", "#A890F0", "#F85888", "#A8B820", "#B8A038", "#705898", "#7038F8", "#705848", "#B8B8D0", "#EE99AC")

# Function to generate a palette of 5 colors for a given starting color
generate_palette = function(type_color) {
  # Create an empty vector to store the palette colors
  palette = rep("", length.out = 5)
  
  # Generate 5 colors for the palette
  for (j in 1:5) {
    palette[j] = rgbBlendRandomWeighted(type_color, 0.2)
  }
  
  return(palette)
}

# Create an empty list to store the color palettes
color_palettes = list()

# Loop through each primary type and generate a color palette
for (i in 1:length(types)) {
  type = types[i]
  color = type_colors[i]
  
  # Generate a color palette for this type
  palette = generate_palette(color)
  
  # Add the palette to the list
  color_palettes[[type]] = palette
}

# Print the resulting color palettes
color_palettes


