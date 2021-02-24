#' @title
#' (Multi)polygons' inner rings.
#' 
#' @description
#' The function `st_extract_inner_rings` extracts the inner rings of (multi)polygons, i.e. the holes inside them.
#' It returns an sfc object containing the polygons of the initial holes.
#' @param sf_df sfc object. This object contains the (multi)polygons in the geometry column.


if(!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if(!require(sf)) install.packages("sf"); require(sf)

st_extract_inner_rings <- function(sf_df) {
  # Extract geometry
  geometry <- st_geometry(sf_df)
  # Create empty list to append
  inner_polygons <- list()
  
  for (element in 1:length(geometry)) {
    areas <- geometry[[element]]
    for (pol in 1:length(areas)) {
      # Check if polygon has more than one element, i.e. if it has inner rings
      if (length(areas[[pol]]) > 1) {
        # Create polygons from all the rings
        polygons <- lapply(areas[[pol]], function(x) 
          st_sf(geom = st_sfc(st_polygon(list(x)), 
                              crs = st_crs(sf_df))))
        # Remove first polygon, i.e. outer ring
        polygons <- polygons[-1]
        # Append to list for inner polygons
        inner_polygons <- append(inner_polygons, polygons)
      }
    }
  }
  
  # Transform list to dataframe
  inner_polygons <- do.call(rbind, inner_polygons)
  
  return(inner_polygons)
}