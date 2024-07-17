
simplify_atest <- function(x) {
  xx <- separate_about(x)
  d <- xx$result
  a <- xx$about

  nof <- function(var, group) {
    n <- length(unique(d[[var]]))
    if(group %in% names(d)) {
      if(n == 0) stop(sprintf("Error: Found '%s' but no '%s'.", group, var))
      n2 <- nrow(unique(d[c(var, group)]))
      n <- c(n, n2)
    }
    n
  }
  n_group <- nof("group", "group.value")
  n_response <- nof("response", "response.value")
  n_variable <- nof("variable", "value")
  n_terms <- nof("response", "terms")

  pasteif <- function(a, b, sep) {
    if_else(is.na(b), a, paste(a, b, sep=sep))
  }

  .title <- NA

  # we have a model, use that as either title or grouping factor
  if(length(n_terms) > 1) {
    d <- d |> mutate(.group=pasteif(response, terms, sep=" ~ "), .before=1)
    # if just one model, use as title
    if(length(unique(d$.group))==1) {
      .title <- d$.group[1]
      d$.group <- NULL
    }
    d <- d |> select(-c("response", "terms"))
  }

  # we have values for the groups, combine them
  if(n_group[1] > 0) {
    d <- d |> mutate(.group=pasteif(response, terms, sep=" ~ "), .before=1)
  }

  # only one response / variable pair
  if(n_response[1]==1 && n_variable[1]==1) {
    .title <- paste(d$response[1], " ~ ", d$variable[1])
    if(length(n_response)==2) d <- d |> rename({{d$response[1]}}:=response.value)
    if(length(n_variable)==2) d <- d |> rename({{d$variable[1]}}:=value)
    d <- d |> select(-any_of(c("response", "variable")))
  }

  if(!is.na(.title)) attr(d, "title") <- .title
  d
}
