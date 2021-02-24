#' @title
#' Split (multi)polygons across
#' 
#' @description
#' It uses the function `st_split` from the package `lwgeom` to split the polygons with a line passing across.
#' It returns an sfc object containing all the polygons parts.
#' @param sf_df sfc object. This object contains the (multi)polygons in the geometry column.
#' @param id_colname Character. Name of the column with the polygon identifiers.
#' @param geom_colname Character. Name of the geometry column (Default: "geometry").

if(!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if(!require(sf)) install.packages("sf"); require(sf)
if(!require(lwgeom)) install.packages("lwgeom"); require(lwgeom)

st_split_across <- function(sf_df, id_colname, geom_colname = "geometry") {
  # Extract bbox for each geometry
  bbox_matrix <- apply(sf_df, 1, function(x) st_bbox(x[[geom_colname]]))
  # Create lines cutting through each polygon
  lines_list <- apply(t(bbox_matrix), 1, function(x) st_sf(geom = 
                                                             st_sfc(st_linestring(matrix(x, 2, 2, 
                                                                                         byrow = TRUE)), 
                                                                    crs = st_crs(sf_df))))
  lines_df <- do.call(rbind, lines_list) %>%
    mutate(id = pull(st_drop_geometry(sf_df[id_colname])))
  # Split polygons by line
  polygons_split <- st_sf(geometry = st_sfc(lapply(1:nrow(sf_df), function(x) st_polygon())),
                          crs = st_crs(sf_df)) 
  for (i in 1:nrow(sf_df)) {
    tryCatch({
      polygons_split[i, c("geometry", "id")] <- lwgeom::st_split(sf_df[i,],
                                                                 st_geometry(lines_df[i,]))
    },
    error = function(e) {
      print(e)
    })
  }
  polygons_split <- polygons_split %>%
    st_collection_extract("POLYGON") %>%
    mutate(row = rownames(.)) %>%
    rename(!!sym(geom_colname) := geometry,
           !!sym(id_colname) := id)
  
  return(polygons_split)
}