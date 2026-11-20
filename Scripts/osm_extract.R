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
  boundary = buffer,
  boundary_type = "clipsrc",
  extra_tags = c("bicycle","construction","horse")

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