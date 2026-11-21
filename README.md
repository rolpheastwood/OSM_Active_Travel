# OSM Active Travel

Creates an active travel map of cycling and pedestrian infrastructure within 10km circular buffer of a city using OpenStreetMap data.
Developed in R 4.6.2.

## Requirements

- R 4.6.2

## Running the Script

1. Download or clone the repository.
2. Open the project in an R IDE (e.g. RStudio, Positron, VS Code).
3. Run `osm_extract.R`.
4. Enter a city when prompted.

Required packages will be installed automatically if missing.

## Data

OpenStreetMap extracts are stored in the `Data` folder.

## Outputs

PNG maps are exported to the `Outputs` folder using the selected location as the file name.

## Acknowledgement

This project uses OpenStreetMap data.

Data © OpenStreetMap contributors, ODbL 1.0.

Built in R using the following packages:

- osmextract
- sf
- dplyr
- ggplot2
- osmdata
- svDialogs