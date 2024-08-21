combine_tests(
  one_proportion_test(vs~am, mtcars2),
  two_proportion_test(vs~am, mtcars2),
  two_proportion_test(vs~am, mtcars2, method="exact")) |>
  as_gt() |> fmt_numbers(decimals=3)

combine_tests(
  one_proportion_test(vs~am, mtcars2, success="V-shaped"),
  two_proportion_test(vs~am, mtcars2, success="V-shaped"),
  two_proportion_test(vs~am, mtcars2, success="V-shaped", method="exact"))

pp <- xtabs(~am+vs, mtcars2)
prop.test(pp)

m <- lm(log(mpg)~1, data=mtcars)
combine_tests(
  one_t_test(log(mpg)~1, data=mtcars, backtransform = FALSE),
  one_t_test(log(mpg)~1, data=mtcars)
  ) |> as_gt() |> fmt_numbers(decimals=5)

one_t_test(mpg~vs, data=mtcars2, backtransform = FALSE)
one_t_test(log(mpg)~vs, data=mtcars2, backtransform = FALSE)
mtcars2 |> summarize(s=sd(mpg)/sqrt(n()), .by=vs)

exp(-0.39430) * 0.08033

combine_tests(
  two_t_test(log(mpg)~vs, data=mtcars2, backtransform = FALSE, var.equal = TRUE),
  two_t_test(log(mpg)~vs, data=mtcars2, var.equal = TRUE),
  pairwise_t_test(log(mpg)~vs, data=mtcars2, var.equal = TRUE),
  pairwise_t_test(log(mpg)~vs, data=mtcars2, var.equal = TRUE, reverse=TRUE),
  pairwise_model_means(m, ~vs, backtransform = FALSE),
  pairwise_model_means(m, ~vs)
) |> as_gt() |> fmt_numbers(decimals=5)

m <- lm(log(mpg)~vs, data=mtcars)
combine_tests(
  model_means(m, ~vs, backtransform = FALSE),
  model_means(m, ~vs)
  ) |> as_gt() |> fmt_numbers(decimals=5)
combine_tests(
  pairwise_model_means(m, ~vs, backtransform = FALSE),
  pairwise_model_means(m, ~vs)
) |> as_gt() |> fmt_numbers(decimals=5)


model_means(m, ~1, backtransform = FALSE)
