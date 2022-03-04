library(idaifieldR)
library(tidyverse)
library(rgdal)
library(sf)
library(geojsonio)
source(file = "R/functions.R")



#browseVignettes("idaifieldR")


options(digits = 20)
connection <- connect_idaifield(serverip = "127.0.0.1",
                                user = "R",
                                pwd = "hallo")

buildings <- get_idaifield_docs(
  connection = connection,
  projectname = "milet",
  simplified = TRUE)  %>% 
  select_by(by = "liesWithin", value = "Bauwerkskatalog")



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
          "period.start", "period.end",
          "buildingCategory", "context", "buildingType",
          "excavatedIn", "excavatedBy",
          "gazId", "literature")

data_mat <- data_mat %>%
  dplyr::select(keep) %>%
  mutate(shortDescription = as.character(unlist(shortDescription)),
         buildingCategory = as.factor(unlist(buildingCategory)),
         period.start = as.factor(unlist(period.start)),
         period.end = as.factor(unlist(period.end)),
         description = as.character(unlist(description)),
         #literature = as.character(paste(unlist(literature), collapse = "; ")),
         gazId = as.character(unlist(gazId))) %>%
  mutate(URL = ifelse(gazId > 1,
                      paste("https://gazetteer.dainst.org/place/",
                            gazId, sep = ""),
                      NA)) %>%
  select(-gazId)

# #TODO:  ghazetter link bei NA rauswerfen

#'data_mat$label <- paste('<b><a href="', data_mat$URL, '">', data_mat$identifier,
#'                   '</a></b><br/>Datierung: ', data_mat$period.start, '--',
#'                   data_mat$period.end, '<br/><br/>', data_mat$description)#,
#'<br/><br/>Literatur: ', data_mat$literature)
#'

data_mat_clean <- matrix(nrow = nrow(data_mat), ncol = ncol(data_mat), " ")

lit_col <- which(colnames(data_mat) == "literature")

for (c in 1:ncol(data_mat)) {
  for (r in 1:nrow(data_mat)) {
    if(c == lit_col) {
      cell <-  flatten(data_mat[r,c])
      final <- c(rep(NA, length(cell)))
      for (l in seq_along(cell)) {
        list <- unlist(cell[[l]])
        quote <- paste(list["quotation"], list["page"], sep = ", ")
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
    } else {
      cell <- unlist(data_mat[r,c])
      cell <- paste(cell, collapse = ", ")
      cell <- gsub("NA; ", "", cell)
      #cell <- gsub("NA", "", cell)
    }
    data_mat_clean[r,c] <- cell
  }
}

rownames(data_mat_clean) <- data_mat[,"identifier"]
colnames(data_mat_clean) <- gsub("\\.", "_", colnames(data_mat))

data_df <- as.data.frame(data_mat_clean)

sp_df <- SpatialPolygonsDataFrame(Sr = sp_geom, data = data_df)
#plot(sp_df)


geojson_write(sp_df, precision = 10, file = "export/Miletus_Building_Catalogue.geojson")
