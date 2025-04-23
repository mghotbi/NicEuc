# Suppress NOTES from R CMD check about unquoted variables in dplyr code
utils::globalVariables(c(
  "Species",
  "Photosynthetic_Pathway",
  "Ring",
  "Plot",
  "utm_x",
  "utm_y"
))
