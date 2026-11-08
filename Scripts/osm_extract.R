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
# CREATE 10 KM BUFFER
#==================
buffer_10km <- centre |>
  st_transform(27700) |>
  st_buffer(10000) |>
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
  st_transform(buffer_10km, st_crs(osm_network))
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
        "Bridleway",

      # Footway / multi-use path
      highway %in% c("footway", "path") ~
        "Footway / Multi-use path",

      # Steps
      highway == "steps" ~
        "Steps",

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
        "living_street",
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
    colour = "grey60",
    linewidth = 0.18
  ) +

  geom_sf(
    data = filter(network, network_type == "Footway / Multi-use path"),
    colour = "#FA8072",
    linewidth = 0.18
  ) +

  geom_sf(
    data = filter(network, network_type == "Bridleway"),
    colour = "#008000",
    linewidth = 0.18
  ) +

  geom_sf(
    data = filter(network, network_type == "Cycleway"),
    colour = "#0000FF",
    linewidth = 0.22
  ) +

  geom_sf(
    data = filter(network, network_type == "Steps"),
    colour = "#FA8072",
    linewidth = 0.18
  ) +

  geom_sf(
    data = filter(network, network_type == "Under construction"),
    colour = "#AAAAAA",
    linewidth = 0.18,
    linetype = "22"
  ) +

  theme_void()