pacman::p_load(
  "tidyverse", "terra",
  "giscoR", "tidyterra", "geodata", "extrafont", "sf"
)

# 2. LOAD GHSL DATA
#----------------------

file_names <- list.files(
  path = getwd(),
  pattern = "tif$",
  full.names = T
)

pop_rasters <- lapply(
  file_names,
  terra::rast
)

# 3. COUNTRY BORDERS USING geodata PACKAGE
#-------------------
country <- st_read("F:\\POP\\Iran\\gadm41_IRN_1.shp")

# 4. CROP
#--------

country_pop_rasters <- lapply(
  pop_rasters,
  function(x) {
    terra::crop(
      x,
      country,
      snap = "in",
      mask = T
    )
  }
)

# 5. CALCULATE POPULATION DIFFERENCE
#-----------------------------------

crs_lambert <- "EPSG:4326"

pop_change <- (
  country_pop_rasters[[2]] - country_pop_rasters[[1]]
) |>
  terra::project(crs_lambert)

# 6. CATEGORIES
#--------------

get_categories <- function(x){
  terra::ifel(
    pop_change == 0, 0,
    terra::ifel(
      pop_change > 0, 1,
      terra::ifel(
        pop_change < 0, -1, pop_change
      )
    )
  )
}

pop_change_cats <- get_categories(pop_change) |>
  as.factor()

# 7. MAP
#-------

cols <- c(
  "#458B74",
  "#E0EEEE",
  "#CD6600"
)


loadfonts(device = "win", quiet = TRUE)

p <- ggplot() +
  tidyterra::geom_spatraster(
    data = pop_change_cats
  ) +
  geom_sf(
    data = country,
    fill = "transparent",
    color = "black",      
    size = 0.5,             
    linewidth = 0.5,      
    linetype = "solid"
  ) +
  scale_fill_manual(
    name = "Population Change Map (1990-2020)\nGrowth or Decline?",
    values = cols,
    labels = c(
      "Decline",
      "Uninhabited",
      "Growth"
    ),
    na.translate = FALSE
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, "mm"), 
      keywidth = unit(30, "mm"),
      label.position = "bottom",
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      drop = T,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  coord_sf(crs = crs_lambert) +
  theme_void() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = c(.5, 0.95),
    legend.title = element_text(
      size = 25,
      color = "black",
      family = "Times New Roman",
      face = "bold",
      margin = margin(b = 12)
    ),
    legend.text = element_text(
      size = 12,
      color = "black",
      family = "Times New Roman",
      face = "bold"
    ),
    legend.spacing.x = unit(0.5, "cm"),
    legend.key.spacing = unit(0.1, "cm"),
    plot.caption = element_text(
      size = 12,        
      color = "black",
      hjust = 0.5, 
      vjust = 2,           
      family = "Times New Roman",
      face = "bold",
      margin = margin(t = 10)
    ),
    plot.margin = unit(
      c(
        t = 1, b = 0.5,   
        l = 0.5, r = 0.5
      ), "cm"          
    )
  ) +
  labs(
    caption = "Global Human Settlement Layer at 30 arcsecs\nCreated by: Dina Yazdani"
  )

ggsave(
  "IRAN-population-change.png",
  p,
  width = 26,
  height = 24,
  units = "cm",
  bg = "white",
  device = "png"
)
