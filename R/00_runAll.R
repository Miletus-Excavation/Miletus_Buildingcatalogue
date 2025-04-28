
# loads the required data
# make sure field is running and pw is set
# correctly
source("R/load_data.R")

# save the UUIDs as a file
source("R/uuids.R")

# load some helper functions
source("R/functions.R")

# import and reformat data from field to produce the geojson 
# used for the Map Guide
source("R/import_transform.R")

# load and enrich the shp-files, split and duplicate the drawings + data
# along periods and save shp-files for each period that can then be used
# to update the map
source("R/duplicate_drawings.R")