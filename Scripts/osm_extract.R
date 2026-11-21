#==================
# CHECK AND INSTALL REQUIRED PACKAGES
#==================
packages <- c(
  "osmextract",
  "sf",
  "dplyr",
  "ggplot2",
  "osmdata",
  "svDialogs"
)

missing <- packages[!packages %in% installed.packages()[, "Package"]]

if (length(missing) > 0) {
  install.packages(missing)
}

lapply(packages, library, character.only = TRUE)

#==================
# LOAD PACKAGES
#==================
library("osmextract")
library("sf")
library("dplyr")
library("ggplot2")
library("osmdata")
library("svDialogs")

#==================
# CHOOSE CITY
#==================
repeat {
  city <- dlgInput(
    message = "Enter a city:",
    default = "London"
  )$res

  if (is.null(city) || city == "")
    next

  bb <- tryCatch(
    getbb(city),
    error = function(e) NULL
  )

  if (!is.null(bb)) {
    message("✓ Found: ", city)
    break
  }

  dlgMessage(
    "City not found. Please check the spelling and try again.",
    type = "ok"
  )
}

#==================
# FIND CENTRE
#==================
centre <- st_sfc(
  st_point(
    c(
      mean(bb[1, ]),
      mean(bb[2, ])
    )
  ),
  crs = 4326
) |>
  st_as_sf()

#==================
# CREATE BUFFER
#==================
buffer <- centre |>
  st_transform(3857) |> #web mercator projection
  st_buffer(10000) |> #10000m
  st_transform(4326)

#==================
# DOWNLOAD OSM DATA
#==================
options(timeout = 1800)

osm_network <- oe_get(
  city,
  provider = "geofabrik",
  layer = "lines",
  download_directory = "data", #use tempdir() to store to temp folder
  query = "
    SELECT
      osm_id,
      highway,
      bicycle,
      horse,
      footway,
      geometry
    FROM lines
    WHERE highway IN (
      'cycleway',
      'path',
      'footway',
      'pedestrian',
      'living_street',
      'residential',
      'service',
      'track',
      'unclassified',
      'tertiary'
    )
  ",
  boundary = buffer,
  boundary_type = "clipsrc",
  extra_tags = c("bicycle", "horse", "footway")
)

#==================
# CATEGORISE (as in OSM carto wiki)
#==================
network <- osm_network |>
  mutate(
    network_type = case_when(

      # Cycleway
      highway == "cycleway" |
        (highway == "path" & bicycle == "designated") ~
        "Cycle",

      # Bridleway
      highway == "bridleway" |
        (highway == "path" & horse == "designated") ~
        "Pedestrian",

      # Pedestrian
      highway %in% c("living_street","step","pedestrian","path") ~
        "Pedestrian",

      highway == "footway" &
        (is.na(footway) | !footway %in% c("sidewalk", "crossing")) ~
        "Pedestrian",

      # Sidewalks/ Pavement by road
      highway == "footway" &
        footway %in% c("sidewalk", "crossing") ~
        "Road Network",

      # Roads
      highway %in% c(
        "motorway",
        "trunk",
        "primary",
        "secondary",
        "tertiary",
        "unclassified",
        "residential",
        "service",
        "track"
      ) ~
        "Road Network",

      TRUE ~
        "Other"
    )
  )

#==================
# PLOT
#==================
plot <- ggplot() +
  geom_sf(
    data = filter(network, network_type == "Road Network"),
    aes(colour = "Road Network"),
    linewidth = 0.18
  ) +
  geom_sf(
    data = filter(network, network_type == "Pedestrian"),
    aes(colour = "Pedestrian"),
    linewidth = 0.23
  ) +
  geom_sf(
    data = filter(network, network_type == "Cycle"),
    aes(colour = "Cycle"),
    linewidth = 0.42
  ) +
  scale_colour_manual(
  name = NULL,
  values = c(
    "Road Network" = "#C0C0C0",
    "Pedestrian" = "#00B386",
    "Cycle" = "#6B1FFF"
  ) 
) +
labs(caption = "Data © OpenStreetMap contributors") +
theme_void()+
theme(
  legend.position = "bottom",
  legend.text = element_text(
    colour = "white",
    size = 10
  ),
  plot.caption = element_text(
    colour = "white",
    size = 6,     
    hjust = 0,
    margin = margin(l = 3, b = 3)
  ),
  plot.background = element_rect(fill = "black", colour = NA)
)

#view plot
plot
#==================
# SAVE PLOT
#==================
ggsave(
  paste0("Outputs/", gsub("[^[:alnum:]]+", "_", city),".png"),
  plot,
  width = 10,
  height = 10,
  dpi = 300,
  bg = "black"
)
