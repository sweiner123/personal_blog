flights_map <- function(api, arr_IATA) {
  
  r <- httr::GET("http://api.aviationstack.com/v1/flights", 
                 query = list(access_key = api, arr_IATA = arr_IATA))
  
  r
  
  httr::status_code(r)
  
  flights <- httr::content(r)
  
  data_flight <- tibble(flights$data)
  
  tb_flight <- data_flight |> 
    tidyr::unnest_wider(col = c("flights$data")) |> 
    tidyr::unnest_wider(col = c(departure), names_sep = "_") |> 
    tidyr::unnest_wider(col = c(arrival), names_sep = "_") |> 
    tidyr::unnest_wider(col = c(airline), names_sep = "_") |> 
    tidyr::unnest_wider(col = c(flight), names_sep = "_") |> 
    dplyr::select(-ends_with(c("codeshared")))
  
  lat_lon_dep <- tb_flight |> 
    dplyr::mutate(departure_country = str_split(departure_timezone, '/')) |> 
    dplyr::pull(departure_iata) |> 
    purrr::map(\(x) airportr::airport_detail(input = x, 
                                             input_type = "IATA")) |> 
    setNames(tb_flight$departure_iata) |> 
    purrr::list_rbind() |> 
    dplyr::bind_cols(tb_flight |> 
                       dplyr::select(ends_with("scheduled"), 
                                     airline_name, 
                                     flight_number)) |>
    dplyr::mutate(departure_scheduled = as.character(ymd_hms(departure_scheduled)),
                  arrival_scheduled = as.character(ymd_hms(arrival_scheduled)))
  
  arr_location <- purrr::map_vec(arr_iata, \(x) airportr::airport_location(input = x, 
                                                                           input_type = "IATA"))
  
  lat_lon_line <- lat_lon_dep |>
    dplyr::group_by(IATA) |> 
    dplyr::group_modify(~ head(.x, 1L)) |> 
    dplyr::group_modify(~ add_row(.x, 
                                  Latitude = arr_location$Latitude, 
                                  Longitude = arr_location$Longitude))
  
  
  dep_sf <- sf::st_as_sf(lat_lon_line, 
                         coords = c("Longitude", "Latitude"), 
                         crs = 4326)
  
  
  dep_line <- dep_sf |> 
    dplyr::group_by(IATA) |> 
    dplyr::summarise() |> 
    sf::st_cast("LINESTRING") |> 
    dplyr::bind_cols(lat_lon_dep |>
                       dplyr::group_by(IATA) |> 
                       dplyr::group_modify(~ head(.x, 1L)) |> 
                       dplyr::ungroup() |> 
                       dplyr::select(-c(IATA))
    )
  
  label <- sprintf(
    "<strong>%s</strong><br/>%s<br/>%s<br/>Flight Number <strong>%s</strong>
  <br/>Departure Time %s<br/> Arrvial Time %s",
  lat_lon_dep$City, 
  lat_lon_dep$Name, 
  lat_lon_dep$airline_name,
  lat_lon_dep$flight_number, 
  lat_lon_dep$departure_scheduled,
  lat_lon_dep$arrival_scheduled
  ) %>% 
    lapply(htmltools::HTML)
  
  m <- 
    leaflet::leaflet() |> 
    leaflet::addTiles() |> 
    leaflet::addPolylines(data = dep_line) |> 
    leaflet::addMarkers(data = lat_lon_dep, 
                        lng = ~Longitude, 
                        lat = ~Latitude, 
                        label = label,
                        clusterOptions = leaflet::markerClusterOptions(freezeAtZoom = 5))
  m
}
}

api <- rstudioapi::askForPassword()

flights_map(api, "SFO")

saveRDS(r, file = "C:/Users/sam/My Drive/sfo_api_call.rds")
