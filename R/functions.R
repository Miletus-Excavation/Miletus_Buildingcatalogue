
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
