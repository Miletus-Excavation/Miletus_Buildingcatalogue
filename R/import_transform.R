library(idaifieldR)
library(tidyverse)
library(rgdal)
library(sf)
library(geojsonio)

#browseVignettes("idaifieldR")


options(digits = 20)
connection <- connect_idaifield(serverip = "127.0.0.1",
                                user = "R",
                                pwd = "hallo")

buildings <- get_idaifield_docs(
  connection = connection,
  projectname = "milet",
  simplified = TRUE)  %>% 
  select_by(by = "type", value = "Building")

# Functions

convert_with_crs <- function(coords, from_epsg, to_epsg) {
  epsg <- paste("+init=epsg:", from_epsg, sep = "")
  original_coords <- SpatialPoints(cbind(coords[, 1],
                                         coords[, 2]),
                                   proj4string = CRS(projargs = epsg))
  to_epsg <- paste("+init=epsg:", to_epsg, sep = "")
  new_coords <- spTransform(original_coords, CRS(to_epsg))
  return(new_coords)
}


make_sp_polygon <- function(coords, from_epsg, to_epsg) {
  coords <- convert_with_crs(coords,
                             from_epsg = from_epsg,
                             to_epsg = to_epsg)
  poly <- sp::Polygon(coords = coords, hole = FALSE)
  return(poly)
}


for (i in seq_along(buildings)) {
  type <- buildings[[i]]$geometry$type
  if (is.null(type)) {
    next
  } else if (type == "Polygon" | type == "MultiPolygon") {
    coordslist <- buildings[[i]]$geometry$coordinates
    for (item in seq_along(coordslist)) {
      coordslist[[item]] <- make_sp_polygon(coordslist[[item]],
                                            from_epsg = 32635,
                                            to_epsg = 4326)
      buildings[[i]]$geometry$coordinates[[item]] <- coordslist[[item]]
    }
    polylist <- buildings[[i]]$geometry$coordinates
    polylist <- Polygons(polylist, ID = buildings[[i]]$identifier)
    buildings[[i]]$geometry$coordinates <- polylist
  }
}

sp_geom <- lapply(buildings, function(x) unlist(x$geometry$coordinates))

sp_geom <- SpatialPolygons(sp_geom, pO = 1:length(buildings), proj4string=CRS("+init=epsg:4326"))
plot(sp_geom)

data_mat <- as.data.frame(idaifield_as_matrix(buildings))
rownames(data_mat) <- data_mat[,"identifier"]



keep <- c("identifier", "shortDescription", "description",
          "relation.liesWithin", "period.start", "period.end",
          "buildingCategory", "context", "buildingType",
          "buildingContractor", "associatedDeity",
          "excavatedIn", "excavatedBy",
          "gazId", "literature")

data_mat <- data_mat %>%
  dplyr::select(keep) %>%
  mutate(relation.liesWithin = as.factor(unlist(relation.liesWithin)),
         shortDescription = as.character(unlist(shortDescription)),
         buildingCategory = as.factor(unlist(buildingCategory)),
         period.start = as.factor(unlist(period.start)),
         period.end = as.factor(unlist(period.end)),
         description = as.character(unlist(description)),
         gazId = as.character(unlist(gazId))) %>%
  mutate(URL = ifelse(gazId > 1,
                      paste("https://gazetteer.dainst.org/place/",
                            gazId, sep = ""),
                      NA))




data_mat$lit <- NA
for (i in 1:nrow(data_mat)) {
  list <- data_mat$literature[i]
  list <- unlist(list)
  if (is.null(list)) {
    next
  } else {
    data_mat$lit[i] <- paste(list, collapse = "; ")
  }
}


data_mat$label <- paste('<b><a href="', data_mat$URL, '">', data_mat$identifier,
                   '</a></b><br/>Datierung: ', data_mat$period.start, '--',
                   data_mat$period.end, '<br/><br/>', data_mat$description)#,
#'<br/><br/>Literatur: ', data_mat$lit)
#'

data_mat_clean <- matrix(nrow = nrow(data_mat), ncol = ncol(data_mat), " ")

for (c in 1:ncol(data_mat)) {
  for (r in 1:nrow(data_mat)) {
    cell <-  unlist(data_mat[r,c])
    cell <- paste(cell, collapse = ", ")
    data_mat_clean[r,c] <- cell
  }
}

str(data_mat_clean)

rownames(data_mat_clean) <- data_mat[,"identifier"]
colnames(data_mat_clean) <- colnames(data_mat)

data_df <- as.data.frame(data_mat_clean)

sp_df <- SpatialPolygonsDataFrame(Sr = sp_geom, data = data_df)
#plot(sp_df)


geojson_write(sp_df, precision = 10, file = "export/Miletus_Building_Catalogue.geojson")



