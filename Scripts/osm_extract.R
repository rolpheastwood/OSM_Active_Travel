library("osmextract")
library("sf")
library("dplyr")
library("ggplot2")

#==================
#download osm
#==================
options(timeout = 1800)#30 min timeout for extract

osm_network <- oe_get(
  "London",
  download_directory = "data",
  query = "
    SELECT osm_id, highway, other_tags, geometry, bicycle, foot, segregated
    FROM lines
     WHERE highway IS NOT NULL
      AND highway IN (
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
  extra_tags = c("bicycle","foot","segregated")
)