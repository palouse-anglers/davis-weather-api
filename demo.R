library(httr2)
library(jsonlite)
library(tidyverse)
get_current_v2 <- function(station_id) {

  api_key    <- Sys.getenv("DAVIS_API_KEY")
  api_secret <- Sys.getenv("DAVIS_API_SECRET")

  # unix timestamp in seconds
  t <- as.character(as.integer(Sys.time()))

  url <- paste0(
    "https://api.weatherlink.com/v2/current/",
    station_id
  )

  request(url) |>
    req_url_query(
      `api-key` = api_key,
      t = t
    ) |>
    req_headers(
      `X-Api-Secret` = api_secret
    ) |>
    req_perform() |>
    resp_body_json()
}



#  List stations
# Get stations JSON
stations_raw <- request("https://api.weatherlink.com/v2/stations") |>
  req_url_query(
    `api-key` = api_key,
    t = t
  ) |>
  req_headers(`X-Api-Secret` = api_secret) |>
  req_perform() |>
  resp_body_json()


# Convert to data frame
stations_df <- stations_raw$stations %>%
  map_dfr(~tibble(
    station_id        = .x$station_id,
    uuid              = .x$station_id_uuid,
    name              = .x$station_name,
    gateway_id        = .x$gateway_id,
    gateway_id_hex    = .x$gateway_id_hex,
    product_number    = .x$product_number,
    username          = .x$username,
    user_email        = .x$user_email,
    company_name      = .x$company_name,
    active            = .x$active,
    private           = .x$private,
    recording_interval = .x$recording_interval,
    firmware_version  = .x$firmware_version,
    imei              = .x$imei,
    registered_date   = as.POSIXct(.x$registered_date, origin="1970-01-01", tz=.x$time_zone),
    subscription_end  = as.POSIXct(.x$subscription_end_date, origin="1970-01-01", tz=.x$time_zone),
    time_zone         = .x$time_zone,
    city              = .x$city,
    region            = .x$region,
    country           = .x$country,
    latitude          = .x$latitude,
    longitude         = .x$longitude,
    elevation_m       = .x$elevation,
    gateway_type      = .x$gateway_type,
    relationship_type = .x$relationship_type,
    subscription_type = .x$subscription_type
  ))

stations_df


# Get curent conditions as df

library(httr2)
library(dplyr)
library(purrr)
library(tibble)

get_weatherlink_current <- function(uuids) {
  
  api_key    <- Sys.getenv("DAVIS_API_KEY")
  api_secret <- Sys.getenv("DAVIS_API_SECRET")
  
  safe_get <- function(x, field) {
    if (is.list(x) && field %in% names(x)) x[[field]] else NA
  }
  
  # Loop over each UUID and fetch data
  map_dfr(uuids, function(uuid) {
    
    t <- as.character(as.integer(Sys.time()))
    
    resp <- request(paste0("https://api.weatherlink.com/v2/current/", uuid)) |>
      req_url_query(
        `api-key` = api_key,
        t = t
      ) |>
      req_headers(`X-Api-Secret` = api_secret) |>
      req_perform() |>
      resp_body_json()
    
    # Flatten sensors for this station
    map_dfr(resp$sensors %>%
      keep(~.x$sensor_type==31),
    function(sensor) {
      sensor_data <- sensor$data[[1]]
      
      if (is.null(sensor_data) || !is.list(sensor_data)) {
        return(tibble(
          station_uuid = uuid,
          lsid = sensor$lsid,
          sensor_type = sensor$sensor_type,
          ts = as.POSIXct(NA, origin="1970-01-01", tz="UTC"),
          tz_offset = NA_integer_,
          bar = NA_real_,
          bar_absolute = NA_real_,
          dew_point = NA_real_,
          et_day = NA_real_,
          forecast_rule = NA_integer_,
          forecast_desc = NA_character_,
          heat_index = NA_real_,
          hum_out = NA_real_,
          rain_15_min_mm = NA_real_,
          rain_60_min_mm = NA_real_,
          rain_24_hr_mm = NA_real_,
          rain_day_mm = NA_real_,
          rain_rate_mm = NA_real_,
          rain_storm_mm = NA_real_,
          solar_rad = NA_real_,
          temp_out = NA_real_,
          thsw_index = NA_real_,
          uv = NA_real_,
          wind_chill = NA_real_,
          wind_dir = NA_real_,
          wind_speed = NA_real_,
          wet_bulb = NA_real_
        ))
      }
      
      df <- tibble(
        station_uuid  = uuid,
        lsid          = sensor$lsid,
        sensor_type   = sensor$sensor_type,
        ts            = as.POSIXct(safe_get(sensor_data, "ts"), origin="1970-01-01", tz="UTC"),
        tz_offset     = safe_get(sensor_data, "tz_offset"),
        bar           = safe_get(sensor_data, "bar"),
        bar_absolute  = safe_get(sensor_data, "bar_absolute"),
        dew_point     = safe_get(sensor_data, "dew_point"),
        et_day        = safe_get(sensor_data, "et_day"),
        forecast_rule = safe_get(sensor_data, "forecast_rule"),
        forecast_desc = safe_get(sensor_data, "forecast_desc"),
        heat_index    = safe_get(sensor_data, "heat_index"),
        hum_out       = safe_get(sensor_data, "hum_out"),
        rain_15_min_mm = safe_get(sensor_data, "rain_15_min_mm"),
        rain_60_min_mm = safe_get(sensor_data, "rain_60_min_mm"),
        rain_24_hr_mm  = safe_get(sensor_data, "rain_24_hr_mm"),
        rain_day_mm    = safe_get(sensor_data, "rain_day_mm"),
        rain_rate_mm   = safe_get(sensor_data, "rain_rate_mm"),
        rain_storm_mm  = safe_get(sensor_data, "rain_storm_mm"),
        solar_rad      = safe_get(sensor_data, "solar_rad"),
        temp_out       = safe_get(sensor_data, "temp_out"),
        thsw_index     = safe_get(sensor_data, "thsw_index"),
        uv             = safe_get(sensor_data, "uv"),
        wind_chill     = safe_get(sensor_data, "wind_chill"),
        wind_dir       = safe_get(sensor_data, "wind_dir"),
        wind_speed     = safe_get(sensor_data, "wind_speed"),
        wet_bulb       = safe_get(sensor_data, "wet_bulb")
      ) 
      
  df %>%
mutate(
realts = as.POSIXct(ts + tz_offset, origin = "1970-01-01", tz = "UTC")) %>% 
relocate(realts, .before = ts) 
     
    })
  })
}

# Example Get Bluewood Lodge 

bl <- stations_df %>%
  filter(name=="Bluewood Lodge") %>%
  pull(uuid) %>%
  get_current_v2()



get_weatherlink_current(
  stations_df %>%
  filter(name=="Bluewood Lodge") %>%
  pull(uuid) 
 )
