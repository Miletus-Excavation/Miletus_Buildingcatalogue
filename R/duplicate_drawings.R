library(idaifieldR)
library(tidyverse)
library(rgdal)
library(sf)
library(geojsonio)
library(utf8)

## source
source(file = "R/functions.R")
source(file = "R/load_data.R")



##### load paulas polygons 
Miletus_geom <- st_read(dsn = "import/Miletus_geom.geojson")
# add column to mark existing drawings
Miletus_geom$hasGeom <- "Umzeichnung vorhanden"

# change "Name" variable to "identifier" to suit DB export
colnames(Miletus_geom)[which(colnames(Miletus_geom) == "Name")] <- "identifier"

# make a to-do-list with DB-entries and polygons, full join to get both 
# data frame complete and nots (mapWorkflow = "Fertig für Karte?")
buildings_check <- data_mat %>%
  mutate(hasDB = "Datensatz vorhanden") %>%
  select(identifier, hasDB, mapWorkflow, period.start, period.end) %>%
  full_join(Miletus_geom) %>%
  select(identifier, hasDB, hasGeom, mapWorkflow, period.start, period.end)

# save it for later
write.csv(buildings_check, file = "export/currentGeometryDB.csv", 
          fileEncoding = "UTF-8", na = "")
# remove, because we only needed the csv
rm(buildings_check)

# get the identifier and dating / periods of each building (data_mat is made in file "R/load_data.R")
buildings_dating <- data_mat %>% 
  select(identifier, period.start, period.end)


# function to make the loop cleaner
replace_by_period_group <- function(single_building, periods_groups = periods_groups) {
  # note if dating exists
  na_period <- c(is.na(single_building$period.start), is.na(single_building$period.end))
  if (any(na_period)) {
    # if not, set period to NA as well
    single_building$period <- NA
  } else {
    # is there is dating, save start and end in seperate vevtors
    per_start <- single_building$period.start
    per_end <- single_building$period.end
    # slight grouping problem here, because it means the same in terms of 
    # absolute numbers, but we nonetheless need the different names for 
    # pottery... disregard here and make "Byzantinisch" the same as 
    # "Spätantik / Byzantinisch"
    if (per_start == "Byzantinisch") {
      per_start <- "Spätantik / Byzantinisch"
    }
    if (per_end == "Byzantinisch") {
      per_end <- "Spätantik / Byzantinisch"
    }
    # note if the dating recorded in the DB is already a group 
    is_group <- c("start" = per_start %in% periods_groups$group, "end"= per_end %in% periods_groups$group)
    group <- c("start" = NA, "end" = NA)
    if (unname(is_group["start"])) {
      # if it is a group, we need the corresponding first period from the group
      # seperately note group and actual starting period
      group_start <- per_start
      index <- which(periods_groups$group == group_start)
      per_start <- periods_groups$period[min(index)]
    } else {
      # if it is not a group but an actual period, we can store the group here
      group_start <- periods_groups$group[which(periods_groups$period == per_start)]
    }
    if (unname(is_group["end"])) {
      # if it is a group, we need the corresponding last period from the group
      # seperately note group and actual starting period
      group_end <- per_end
      index <- which(periods_groups$group == per_end)
      per_end <- periods_groups$period[max(index)]
    } else {
      # if it is not a group but an actual period, we can store the group here
      group_end <- periods_groups$group[which(periods_groups$period == per_end)]
    }
    # add result to the actual data frame
    single_building$period.start <- per_start
    single_building$period.end <- per_end
    # get the indices of all periods which fall between the start and end period
    new_period_index <- seq(which(periods_groups$period == per_start), 
                            which(periods_groups$period == per_end), 
                            by = 1)
    # duplicate the building data the amount of times that we need to have
    # seperate rows for each period
    single_building <- single_building[rep(1, length(new_period_index)),]
    # fill in the actual period of each row
    single_building$period <- periods_groups$period[new_period_index]
  } 
  # return the row(s)
  return(single_building)
}

# set up a new df in which the duplicated rows will go
buildings_dating_dupl <- as.data.frame(matrix(ncol = ncol(buildings_dating)+1, nrow = 1))
colnames(buildings_dating_dupl) <- c(colnames(buildings_dating), "period")

# replace and suplicate each building, appending the result to the new df
for(i in 1:nrow(buildings_dating)) {
  single_building <- replace_by_period_group(buildings_dating[i, ], periods_groups)
  buildings_dating_dupl <- rbind(buildings_dating_dupl, single_building)
}

# remove the first row (NAs) and set the rownames for all rows
buildings_dating_dupl <- buildings_dating_dupl[-1,]
rownames(buildings_dating_dupl) <- buildings_dating_dupl$identifier


# merge everything with left joins for period (only adding the corresponging values)
# and full join for the geometry (collecting everything, even geometry without a
# db-entry) and remove the obsolete hasGeom-columns 
buildings_complete <- buildings_dating_dupl %>%
  left_join(periods_dat) %>%
  left_join(periods_groups) %>%
  full_join(Miletus_geom) %>%
  select(-hasGeom)


# export and save a geojson again (remember to change date for new version)
st_write(buildings_complete, precision = 10, dsn = "export/20221105_Miletus_geom.geojson")

