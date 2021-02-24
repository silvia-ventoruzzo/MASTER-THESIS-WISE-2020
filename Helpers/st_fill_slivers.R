#' @title
#' Filling slivers, i.e. empty spaces between (multi)polygons.
#' 
#' @description
#' The function `st_fill_slivers` aims at filling the empty spaces between polygons in an sfc object.
#' It splits the empty spaces, splits them with a line and assigns each new polygon to the initial one with which it has the longest shared border.
#' The output
#' @param sf_df sfc object. This object contains the (multi)polygons in the geometry column.
#' @param id_colname character. Name of identifier variable.
#' @param geom_colname character. Name of variable with geometry (default: geometry).

if(!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if(!require(sf)) install.packages("sf"); require(sf)
if(!require(lwgeom)) install.packages("lwgeom"); require(lwgeom)

source("../Helpers/st_extract_inner_rings.R", chdir = TRUE)
source("../Helpers/st_split_across.R", chdir = TRUE)

st_fill_slivers <- function(sf_df, id_colname, geom_colname = "geometry") {
  cat("1/5 Initialization of data...")
  # Make geometries valid
  sf_df <- st_make_valid(sf_df)
  # Merge all polygons
  sfc_all <- sf_df %>%
    summarize(do_union = TRUE)
  cat(" done!\n")
  cat("2/5 Extraction of slivers...")
  # Extract inner rings, i.e. holes
  empty_polygons <- st_extract_inner_rings(sf_df = sfc_all) %>%
    mutate(empty_id = rownames(.))
  cat(" done!\n")
  cat("3/5 Splitting of sliver areas...")
  # Split polygons
  polygons_split <- st_split_across(sf_df       = empty_polygons,
                                    geom_colname = "geom",
                                    id_colname   = "empty_id") %>%
    # lwgeom::st_make_valid() %>%
    mutate(id = paste(empty_id, row, sep = "_"))
  cat(" done!\n")
  cat("4/5 Assignment of sliver areas to polygons with longest shared border...")
  # Find which polygons each empty split intersects
  polygons_split_intersection <- st_intersection(x = polygons_split,
                                                 y = sf_df) %>%
    mutate(id = paste(empty_id, row, sep = "_"))
  # Keep only station which has the longest border
  polygons_split_intersection <- polygons_split_intersection %>%
    mutate(intersection_length = st_length(.)) %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    top_n(n = 1, wt = intersection_length) %>%
    ungroup() %>%
    dplyr::select(id, !!id_colname, intersection_length)
  # Join with initial object with geometries of the different split polygons
  polygons_split <- polygons_split %>%
    inner_join(polygons_split_intersection, by = "id") %>%
    rename(!!geom_colname := geom) %>%
    dplyr::select(!!id_colname, !!geom_colname)
  cat(" done!\n")
  cat("5/5 Merging of newly formed polygons...")
  # Bind with initial sfc object and merge polygons by `id_colname`
  sf_df_new <- sf_df %>%
    dplyr::select(!!id_colname, !!geom_colname) %>%
    rbind(polygons_split) %>%
    group_by_at(vars(all_of(id_colname))) %>%
    summarize(do_union = TRUE) %>%
    ungroup() %>%
    st_cast("MULTIPOLYGON")
  # Join to have all initial variables
  sf_df_new <- sf_df %>%
    st_drop_geometry() %>%
    full_join(sf_df_new, by = c(id_colname)) %>%
    st_as_sf()
  cat(" done!\n")
  return(sf_df_new)
}