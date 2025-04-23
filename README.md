# 📦 Installation

# Install devtools if you haven't already

```{r}
install.packages("devtools")

# Install NicEuc with vignettes
devtools::install_github("mghotbi/NicEuc", build_vignettes = TRUE)

```
To explore the full vignette in your browser:

```{r}

# Open the vignette index in your browser
browseVignettes("NicEuc")       # Opens browser-friendly vignette index
#OR
vignette("NicEuc-intro")        # View in RStudio or Viewer


```



# Key features:

Ring -> Plot -> Cell hierarchy

Stratified sampling by photosynthetic pathway (C3 vs C4)

Spatial separation using a minimum distance
```{r setup}

  library(NicEuc)
  library(sf)
  library(spdep)
  library(ggplot2)
  library(leaflet)
  library(dplyr)
  library(utils)

```


The package includes synthetic_gps, a data frame with realistic spatial and taxonomic structure.

```{r }

data("synthetic_gps", package = "NicEuc")
data("gps_ref", package = "NicEuc")

head(synthetic_gps)

library(dplyr)

NicEuc::gps_ref %>%
  count(Species, Photosynthetic_Pathway, sort = TRUE)


```


Dynamic visualizations and Moran's I for spatial autocorrelation


 

# Example

```{r}

# If you want to filter sampling by specific species:

target_species <- c(
  "centella_asiatica",
  "commelina_cyanea",
  "fimbristylis_dichotoma",
  "axonopus_fissifolius"
)


```

# Run generate_sampling_points()

Sample 6 points per plot, with 0.4 m minimum spacing and stratification by C3/C4.

```{r}

set.seed(123)

result <- suppressWarnings(generate_sampling_points(
  gps_ref = synthetic_gps,
  samples_per_plot = 6,
  min_distance = 0.4,
  target_species = target_species,
  export_csv = FALSE
))


```

# Explore Results

```{r}
head(result$samples)

```

# ggplot2 Layout

```{r}

print(result$plot)

```

# Interactive Leaflet Map

```{r}

result$map

```

# Moran's I Spatial Autocorrelation

```{r}

result$moran_test

```

# Notes
Coordinates are reprojected from WGS84 to UTM Zone 56S (EPSG:32756).

Each plot is sampled independently and visualized in a faceted layout.

Sampling favors spatial separation while preserving C3/C4 balance.

# Citation
If you use NicEuc in publications or reports, please cite the package.
