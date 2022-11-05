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
#Miletus_geom <- geojson_read("import/Miletus_geom.geojson", )

Miletus_geom <- st_read(dsn = "import/Miletus_geom.geojson")
Miletus_geom$hasGeom <- "Umzeichnung vorhanden"

colnames(Miletus_geom)[which(colnames(Miletus_geom) == "Name")] <- "identifier"

buildings_check <- data_mat %>%
  mutate(hasDB = "Datensatz vorhanden") %>%
  select(identifier, hasDB, mapWorkflow, period.start, period.end) %>%
  full_join(Miletus_geom) %>%
  select(identifier, hasDB, hasGeom, mapWorkflow, period.start, period.end)

write.csv(buildings_check, file = "export/currentGeometryDB.csv", 
          fileEncoding = "UTF-8", na = "")
rm(buildings_check)


buildings_dating <- data_mat %>% 
  select(identifier, period.start, period.end)

single_building <- buildings_dating[grepl("Athenatempel Phase IVb", buildings_dating$identifier),]

replace_by_period_group <- function(single_building, periods_groups = periods_groups) {
  na_period <- c(is.na(single_building$period.start), is.na(single_building$period.end))
  if (any(na_period)) {
    single_building$period <- NA
  } else {
    per_start <- single_building$period.start
    per_end <- single_building$period.end
    if (per_start == "Byzantinisch") {
      per_start <- "Spätantik / Byzantinisch"
    }
    if (per_end == "Byzantinisch") {
      per_end <- "Spätantik / Byzantinisch"
    }
    is_group <- c("start" = per_start %in% periods_groups$group, "end"= per_end %in% periods_groups$group)
    group <- c("start" = NA, "end" = NA)
    if (unname(is_group["start"])) {
      group_start <- per_start
      index <- which(periods_groups$group == group_start)
      per_start <- periods_groups$period[min(index)]
    } else {
      group_start <- periods_groups$group[which(periods_groups$period == per_start)]
    }
    if (unname(is_group["end"])) {
      group_end <- per_end
      index <- which(periods_groups$group == per_end)
      per_end <- periods_groups$period[max(index)]
    } else {
      group_end <- periods_groups$group[which(periods_groups$period == per_end)]
    }
    
    single_building$period.start <- per_start
    single_building$period.end <- per_end
    new_period_index <- seq(which(periods_groups$period == per_start), 
                            which(periods_groups$period == per_end), 
                            by = 1)
    single_building <- single_building[rep(1, length(new_period_index)),]
    single_building$period <- periods_groups$period[new_period_index]
  } 
  return(single_building)
}

buildings_dating_dupl <- as.data.frame(matrix(ncol = ncol(buildings_dating)+1, nrow = 1))
colnames(buildings_dating_dupl) <- c(colnames(buildings_dating), "period")

for(i in 1:nrow(buildings_dating)) {
  single_building <- replace_by_period_group(buildings_dating[i, ], periods_groups)
  buildings_dating_dupl <- rbind(buildings_dating_dupl, single_building)
}

buildings_dating_dupl <- buildings_dating_dupl[-1,]
rownames(buildings_dating_dupl) <- buildings_dating_dupl$identifier

buildings_dating_dupl <- as.data.frame(matrix(ncol = ncol(buildings_dating)+1, nrow = 1))
colnames(buildings_dating_dupl) <- c(colnames(buildings_dating), "period")




buildings_complete <- buildings_dating_dupl %>%
  left_join(periods_dat) %>%
  left_join(periods_groups) %>%
  full_join(Miletus_geom) %>%
  select(-hasGeom)


# export
st_write(buildings_complete, precision = 10, dsn = "export/20221103_Miletus_geom.geojson")
