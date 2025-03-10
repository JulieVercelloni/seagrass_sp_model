## -----  functions 
slice_duplicates <- function(df) {
  df |>
    group_by(Longitude, Latitude, image_name) |> 
    nest() |>
    dplyr::select(-data) |>
    count() |>       
    filter(n() > 1) |>
    data.frame()
}

clean_data <- function(df) {
  df |>
    group_by_all() |>
    mutate(is_duplicate = n() > 1) |> # Mark duplicates
    filter(!is_duplicate | row_number() == 1) |> # Keep only one row from duplicates
    select(-is_duplicate) |> # Drop helper column
    ungroup()
}


make_new_geom <- function(df, hex_loc, hexpred_84) {

 df_unique <- df %>% 
  group_by(longitude, latitude) %>%
  summarize() %>%
  arrange(longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude")) 

 nearest_idx <- st_nearest_feature(df_unique, hex_loc)

# Extract the coordinates of the closest points from hex_loc
closest_coordinates <- st_coordinates(hex_loc[nearest_idx, ]) %>%
  data.frame()

df_newgeom <- df_unique %>%
  mutate(longitude = st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  cbind(closest_coordinates) %>%
  left_join(df) %>%
  dplyr::select(! c(longitude, latitude)) %>%
  rename(longitude = X,
         latitude = Y) %>%
 st_as_sf(coords = c("longitude", "latitude")) 

df_newgeom <- df_newgeom %>%
   st_join(hex_loc) %>% 
   st_drop_geometry() %>%
   left_join(hexpred_84)

return(df_newgeom)
}

process_cov <- function(dat_survey, cov, hexpred_84){

  cov <- cov %>%
  mutate(year = year(ymd_hm(Aggregated.Date.Time))) %>%
  mutate(year_month = format(ymd_hm(Aggregated.Date.Time), "%Y-%m"))

# Compute rolling maximum including the current year_month
  cov_max <- cov %>%
  group_by(Variable, tier) %>%  # Group by Variable and tier
  arrange(year_month) %>%
  mutate(rolling_max = slide_dbl(highest, max, .before = 11, .after = 0, .complete = TRUE)) 

# Join with dat_survey to extract only relevant rows
cov_process <- dat_survey %>%
  inner_join(cov_max, by = c("tier", "year", "year_month")) %>%
  select(tier, year, Variable, rolling_max) %>%
  left_join(hexpred_84) %>%
  st_set_geometry("x")

return(cov_process)
}

# Function to create plots
plot_cov <- function(var, data) {
  ggplot() +
    geom_sf(data = hexpred_84, fill = "grey90", color = "white") +  # Background
    geom_sf(data = filter(data, name_plot == var), aes(fill = rolling_max)) +
    facet_wrap(~year) +
    palette_choice +  # Apply nice color palette
    labs(title = var) +
    theme_minimal() +
    theme(legend.position = "right")
}

