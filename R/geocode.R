#' Geocode a data frame
#'
#' @export

geocode <- function(variable) {
    world <- spData::world %>% dplyr::select(iso_a2, geom)
    return(variable %>% dplyr::inner_join(world, by = c(country_id = 'iso_a2')))
}
