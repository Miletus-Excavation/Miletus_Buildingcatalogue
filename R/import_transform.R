library(idaifieldR)
packageVersion("idaifieldR")
# remotes::install_github("lsteinmann/idaifieldR", build_vignettes = TRUE)
library(dplyr)
library(purrr)
library(sf)
library(utf8)
library(geojsonio)


source(file = "R/load_data.R")

today <- gsub("-", "", Sys.Date())

#browseVignettes("idaifieldR")

Miletus_geom <- st_read(dsn = "SHP/Milet_gesamt_Polygone.shp") %>%
  select(UUID, geometry)

buildings_hull <- st_convex_hull(Miletus_geom)
buildings_hull <- st_transform(buildings_hull, crs = 4326)
# check: 
# plot(buildings_hull)

# in the future, it should be possible to experiment and maybe consider using
# concave hull instead; I don't have geos 3.11. right now, 
# buildings_points <- st_cast(Miletus_geom, "MULTIPOINT")
# buildings_concave <- st_concave_hull(buildings_points, ratio = 1, concavity = 2)
# another package that may or may not work: 
# library(concaveman)
# buildings_concave <- concaveman(buildings_points, concavity = 1)
# plot(buildings_concave)

st_write(
  buildings_hull, 
  paste0("SHP/export/Milet_gesamt_Polygone_convex_hull_", today, ".shp"))


keep <- c("identifier", "shortDescription", "description",
          "period.start", "period.end",
          "buildingCategory", "context", "buildingType",
          "gazId", "arachneId", "id")



attributes <- data_mat %>%
  filter(mapWorkflow == TRUE) %>%
  dplyr::select(all_of(keep)) %>%
  mutate(shortDescription = as.character(unlist(shortDescription)),
         buildingCategory = as.factor(unlist(buildingCategory)),
         period.start = as.factor(unlist(period.start)),
         period.end = as.factor(unlist(period.end)),
         description = as.character(unlist(description)),
         #literature = as.character(paste(unlist(literature), collapse = "; ")),
         gazId = as.character(unlist(gazId))) %>%
  mutate(URL = ifelse(!is.na(arachneId),
                      paste("https://arachne.dainst.org/entity/",
                            arachneId, sep = ""),
                      "https://www.miletgrabung.uni-hamburg.de/milet-tour.html")) %>%
  mutate(LinkTitle = ifelse(!is.na(arachneId),
                     "Link in iDAI.objects",
                      "Webseite der Miletgrabung")) %>%
  rename(UUID = id) %>%
  select(-gazId, -arachneId)




#' in a laughingly complicated manner, find out which 
#' features exist in the db but do not have a drawing
no_geom <- is.na(match(attributes$UUID, Miletus_geom$UUID))
no_geom <- attributes[no_geom, "identifier"]

#' then take all of those and...
dbgeoms <- lapply(buildings_raw, function(x) {
  if (is.null(x[["geometry"]]) || !x["identifier"] %in% no_geom) {
    # do not include if the identifier has not been selected 
    # and if this one doesn't have a geometry either!
    return(NULL)
  } else {
    # take the geometry and fix the polygons to conform to geojson
    # (the last and first coords have to be exactly the same
    # since field does not implement this correctly, we are checking and
    # closing polygons here be adding the first point as last point:
    coords <- x$geometry$coordinates[[1]]
    if (!identical(coords[[1]], coords[[length(coords)]])) {
      coords <- c(coords, coords[1])
    }
    # and we return a new list that only contains the UUID and the st-polygon
    return(
      list(
        UUID = x[["id"]],
        geometry = st_polygon(
          list(matrix(unlist(coords), ncol = 2, byrow = TRUE))
        )
      )
    )
  }
})  %>%
  # and also: let's get rid of all the empty lists
  Filter(Negate(is.null), .)

# make it a feature collection (works only without attributes)
sfc <- st_sfc(lapply(dbgeoms, function(x) x$geometry), crs = 32635)
# and a proper sf collection by re-adding the attributes
dbgeoms <- lapply(dbgeoms, function(x) x$UUID) %>% 
  unlist() %>% as.data.frame() %>% 
  rename(UUID = ".") %>%
  st_sf(geometry = sfc) %>%
  # and switching from UTM to the correct CRS for geojson!
  st_transform(crs = 4326)


map_guide_geom <- rbind(buildings_hull, dbgeoms)

plot(map_guide_geom)

# #TODO:  ---

#'attributes$label <- paste('<b><a href="', attributes$URL, '">', attributes$identifier,
#'                   '</a></b><br/>Datierung: ', attributes$period.start, '--',
#'                   attributes$period.end, '<br/><br/>', attributes$description)#,
#'<br/><br/>Literatur: ', attributes$literature)
#'


attributes$literature <- NA
attributes_clean <- matrix(nrow = nrow(attributes), ncol = ncol(attributes), " ")

for (i in 1:nrow(attributes)) {
  litlist <- buildings_raw[[i]]$literature
  if (!is.null(litlist)) {
    attributes$literature[i] <- list(lapply(litlist, unlist))
  } else {
    attributes$literature[i] <- NA
  }
}



lit_col <- which(colnames(attributes) == "literature")
url_col <- which(colnames(attributes) == "URL")

for (r in 1:nrow(attributes)) {
  for (c in 1:ncol(attributes)) {
    if (c == lit_col) {
      cell <- flatten(attributes[r,c])
      final <- c(rep(NA, length(cell)))
      for (l in seq_along(cell)) {
        list <- unlist(cell[[l]])
        if(is.na(list["page"])) {
          quote <- paste(list["quotation"], sep = ", ")
        } else {
          quote <- paste(list["quotation"], list["page"], sep = ", ")
        }
        quote <- as.character(quote)
        if (!is.na(list['zenonId'])) {
          link <- paste('<a href="https://zenon.dainst.org/Record/', 
                        list["zenonId"],
                        '">', sep = "")
          link <- as.character(link)
          final[l] <- paste('<li>', link, quote, '</a></li>', sep = "")
        } else {
          final[l] <- paste('<li>', quote, '</li>', sep = "")
        }
      }
      cell <- paste("<ul>", paste(final, collapse = " "), "</ul>", sep = " ")
      if (grepl("<li>NA</li>", cell)) {
        cell <- "...folgt"
      }
    } else if (c == url_col){
      cell <- if (is.na(attributes[r,c])) "" else attributes[r,c]
    } else {
      cell <- unlist(attributes[r,c])
      cell <- paste(cell, collapse = ", ")
      cell <- gsub("NA; ", "", cell)
    }
    attributes_clean[r,c] <- as_utf8(cell)
  }
}

rownames(attributes_clean) <- attributes[,"identifier"]
colnames(attributes_clean) <- gsub("\\.", "_", colnames(attributes))

attributes_clean <- as.data.frame(attributes_clean)

# save it as csv in case you want to check for anything: 
write.csv(attributes_clean, 
          "export/attributes_map_guide.csv", fileEncoding = "UTF-8")


filename <- "export/Map_of_Miletus_Map_Guide"

map_guide <- map_guide_geom %>%
  right_join(attributes_clean)

geojson_write(map_guide, precision = 10, file = paste0(filename, "_", today, ".geojson"))
