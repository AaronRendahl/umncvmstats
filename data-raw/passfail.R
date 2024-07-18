set.seed(15)
passfail <- tidyr::tibble(score1=rnorm(50, mean=80, sd=7),
                          score2=rnorm(50, mean=85, sd=7)) |>
  dplyr::mutate(pass1=score1>80, pass2=score2>80) |>
  dplyr::mutate(across(starts_with("pass"), \(x) factor(x, levels=c(FALSE, TRUE), labels=c("fail", "pass"))))

usethis::use_data(passfail, overwrite = TRUE)
