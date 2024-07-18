mtcars2 <- datasets::mtcars |>
  dplyr::mutate(dplyr::across(c(cyl, vs, am, gear, carb), forcats::as_factor)) |>
  dplyr::mutate(vs=forcats::fct_recode(vs, "V-shaped"="0", "straight"="1"),
                am=forcats::fct_recode(am, "automatic"="0", "manual"="1")) |>
  tibble::rownames_to_column("model") |> tibble::as_tibble()

usethis::use_data(mtcars2, overwrite = TRUE)
