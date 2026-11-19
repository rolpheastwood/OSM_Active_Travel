library("osmextract")
library("sf")
library("dplyr")
library("ggplot2")

#==================
# FIND CENTRE
#==================
centre <- st_as_sf(
  data.frame(
    lon = -0.1276,
    lat = 51.5074
  ),
  coords = c("lon", "lat"),
  crs = 4326
)

#==================
# CREATE BUFFER
#==================
buffer <- centre |>
  st_transform(27700) |>
  st_buffer(10000) |> #10000m
  st_transform(4326)

#==================
# DOWNLOAD OSM DATA
#==================
options(timeout = 1800)

osm_network <- oe_get(
  "greater-london",
  provider = "geofabrik",
  download_directory = "data",
  query = "
    SELECT
      osm_id,
      highway,
      bicycle,
      construction,
      horse,
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
   extra_tags = c("bicycle","construction","horse")

)

#==================
# CLIP TO BUFFER
#==================
network_lines <- st_intersection(
  osm_network,
  st_transform(buffer, st_crs(osm_network))
)

#==================
# CATEGORISE (as in OSM carto wiki)
#==================
network <- network_lines |>
  mutate(
    network_type = case_when(

      # Cycleway
      highway == "cycleway" |
        (highway == "path" & bicycle == "designated") ~
        "Cycleway",

      # Bridleway
      highway == "bridleway" |
        (highway == "path" & horse == "designated") ~
        "Footway",

      # Pedestrian
      highway %in% c("living_street","step","pedestrian","footway", "path") ~
        "Footway",

      # Construction
      highway == "construction" &
        construction %in% c(
          "bridleway",
          "cycleway",
          "footway",
          "path",
          "steps",
          "track"
        ) ~
        "Under construction",

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
        "Road network",

      TRUE ~
        "Other"
    )
  )

#==================
# PLOT
#==================
ggplot() +

  geom_sf(
    data = filter(network, network_type == "Road network"),
    colour = "#808080",
    linewidth = 0.18
  ) +

  geom_sf(
    data = filter(network, network_type == "Footway"),
    colour = "#00B386",
    linewidth = 0.23
  ) +

  geom_sf(
    data = filter(network, network_type == "Cycleway"),
    colour = "#6B1FFF",
    linewidth = 0.42
  ) +
  
  geom_sf(
    data = filter(network, network_type == "Under construction"),
    colour = "#A56A43",
    linewidth = 0.18,
    linetype = "22"
  ) +

  theme_void()