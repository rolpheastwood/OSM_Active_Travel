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
      foot,
      segregated,
      cycleway,
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
   extra_tags = c("bicycle","foot","segregated","cycleway")

)

#==================
# CLIP TO BUFFER
#==================
network_lines <- st_intersection(
  osm_network,
  st_transform(buffer_10km, st_crs(osm_network))
)

#==================
# CATEGORISE
#==================
network <- network_lines |>
  mutate(
    network_type = case_when(

      highway == "cycleway" &
        segregated == "yes" ~
        "Segregated cycle",

      highway %in% c("footway", "path") &
        bicycle == "yes" &
        segregated == "yes" ~
        "Segregated cycle",

      highway %in% c("footway", "path") &
        bicycle == "yes" &
        segregated != "yes" ~
        "Shared foot/cycle",

      highway %in% c("footway", "pedestrian") &
        (is.na(bicycle) | bicycle != "yes") ~
        "Foot only",

      highway %in% c(
        "living_street",
        "residential",
        "service",
        "track",
        "tertiary",
        "unclassified"
      ) ~
        "Road network",

      TRUE ~
        "Other"
    )
  )

#==================
# PLOT
#==================
ggplot(network) +
  geom_sf(
    aes(colour = network_type),
    linewidth = 0.35
  ) +
  scale_colour_manual(
    values = c(
      "Segregated cycle" = "#009E73",
      "Shared foot/cycle" = "#56B4E9",
      "Foot only" = "#E69F00",
      "Road network" = "grey50",
      "Other" = "grey80"
    )
  ) +
  theme_void() +
  labs(colour = NULL)