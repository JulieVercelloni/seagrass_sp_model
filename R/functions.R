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
