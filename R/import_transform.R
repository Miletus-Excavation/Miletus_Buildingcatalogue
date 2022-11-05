library(idaifieldR)
library(tidyverse)
library(rgdal)
library(sf)
library(geojsonio)
library(utf8)


source(file = "R/functions.R")

source(file = "R/load_data.R")


#browseVignettes("idaifieldR")



buildings_clean <- buildings


for (i in seq_along(buildings_clean)) {
  type <- buildings_clean[[i]]$geometry$type
  if (is.null(type)) {
    next
  } else if (type == "Polygon" | type == "MultiPolygon") {
    coordslist <- buildings_clean[[i]]$geometry$coordinates
    for (item in seq_along(coordslist)) {
      coordslist[[item]] <- make_sp_polygon(coordslist[[item]],
                                            from_epsg = 32635,
                                            to_epsg = 4326)
      buildings_clean[[i]]$geometry$coordinates[[item]] <- coordslist[[item]]
    }
    polylist <- buildings_clean[[i]]$geometry$coordinates
    polylist <- Polygons(polylist, ID = buildings[[i]]$identifier)
    buildings_clean[[i]]$geometry$coordinates <- polylist
  }
}



sp_geom <- lapply(buildings_clean, function(x) unlist(x$geometry$coordinates))

sp_geom <- SpatialPolygons(sp_geom, pO = 1:length(buildings), proj4string=CRS("+init=epsg:4326"))


plot(sp_geom)



keep <- c("identifier", "shortDescription", "description",
          "period.start", "period.end",
          "buildingCategory", "context", "buildingType",
          "gazId", "arachneId", "literature")

data_mat <- data_mat %>%
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
  select(-gazId)

# #TODO:  ghazetter link bei NA rauswerfen

#'data_mat$label <- paste('<b><a href="', data_mat$URL, '">', data_mat$identifier,
#'                   '</a></b><br/>Datierung: ', data_mat$period.start, '--',
#'                   data_mat$period.end, '<br/><br/>', data_mat$description)#,
#'<br/><br/>Literatur: ', data_mat$literature)
#'

data_mat_clean <- matrix(nrow = nrow(data_mat), ncol = ncol(data_mat), " ")





for (i in 1:length(data_mat)) {
  data_mat$literature[i] <- list(buildings[[i]]$literature)
}


lit_col <- which(colnames(data_mat) == "literature")
url_col <- which(colnames(data_mat) == "URL")

for (c in 1:ncol(data_mat)) {
  for (r in 1:nrow(data_mat)) {
    if(c == lit_col) {
      cell <-  flatten(data_mat[r,c])
      final <- c(rep(NA, length(cell)))
      for (l in seq_along(cell)) {
        list <- unlist(cell[[l]])
        if(is.na(list["page"])) {
          quote <- paste(list["quotation"], sep = ", ")
        } else {
          quote <- paste(list["quotation"], list["page"], sep = ", ")
        }
        quote <- as.character(quote)
        link <- paste('<a href="https://zenon.dainst.org/Record/', 
                      list["zenonId"],
                      '">', sep = "")
        link <- as.character(link)
        final[l] <- paste('<li>', link, quote, '</a></li>', sep = "")
        #print(final)
      }
      cell <- paste("<ul>", paste(final, collapse = " "), "</ul>", sep = " ")
      cell <- gsub('<li><a href="https://zenon.dainst.org/Record/NA">NA, NA</a></li>',
                   "", cell)
    } else if (c == url_col){
      cell <- if (is.na(data_mat[r,c])) "" else data_mat[r,c]
    } else {
      cell <- unlist(data_mat[r,c])
      cell <- paste(cell, collapse = ", ")
      cell <- gsub("NA; ", "", cell)
      #cell <- gsub("NA", "", cell)
    }
    data_mat_clean[r,c] <- as_utf8(cell)
  }
}

rownames(data_mat_clean) <- data_mat[,"identifier"]
colnames(data_mat_clean) <- gsub("\\.", "_", colnames(data_mat))

data_df <- as.data.frame(data_mat_clean)


#buildings[[which(data_df$identifier == "Bouleuterion")]]$literature

#test <- data_df[c("identifier", "literature")]



sp_df <- SpatialPolygonsDataFrame(Sr = sp_geom, data = data_df)
plot(sp_df)




geojson_write(sp_df, precision = 10, file = "export/202211_Miletus_Building_Catalogue_v13.geojson")
