formula_has <- function(f, left, right, group=0) {
  a <- f$about
  nleft  <- sum(a$side=="left")
  nright <- sum(a$side=="right")
  ngroup <- sum(a$side=="group")
  !(nleft!=left || nright!=right || ngroup!=group)
}

clean_formula <- function(formula, how=c("group", "right")) {
  how <- match.arg(how)
  if(how=="group") {
    RHS <- formula[[length(formula)]]
    if (length(RHS) == 3 && RHS[[1]] == as.name("|")) {
      formula[[length(formula)]] <- RHS[[2]]
    }
  } else {
    formula[[3]] <- NULL
  }
  formula
}

parse_formula <- function (formula, data, split_chars="+", drop1=TRUE) {
  if (!inherits(formula, "formula")) stop("formula must be a formula object")
  model <- formula
  expr2char <- function(x) paste(deparse(x), collapse = "")
  parseSide <- function(model) {
    model.vars <- list()
    ops <- c()
    while (length(model) == 3 && as.character(model[[1]]) %in% split_chars) {
      model.vars <- c(model.vars, model[[3]])
      ops <- c(as.character(model[[1]]), ops)
      model <- model[[2]]
    }
    ops <- c(NA_character_, ops)
    vars <- c(model.vars, model)
    out <- as_tibble(list(ops=ops, var.names=sapply(rev(vars), expr2char), vars=rev(vars)))
    if(drop1) out <- out |> filter(.data$var.names!="1")
    out
  }

  ans <- list(left = NULL, right = NULL, condition = NULL)
  varsL <- varsR <- varsG <- NULL
  modelRHS <- model[[length(model)]]
  if (length(modelRHS) == 3 && modelRHS[[1]] == as.name("|")) {
    varsG <- parseSide(modelRHS[[3]]) |> mutate(side="group", .before=1)
    modelRHS <- modelRHS[[2]]
  }
  if (length(model) == 3) {
    varsL <- parseSide(model[[2]]) |> mutate(side="left", .before=1)
    varsR <- parseSide(modelRHS) |> mutate(side="right", .before=1)
  } else {
    ## there's no left side, so move right to left
    varsL <- parseSide(modelRHS) |> mutate(side="left", .before=1)
  }

  add_names <- function(name, n) {
    if(n==1) { name } else { paste(name, 1:n(), sep=".") }
  }
  about <- bind_rows(varsL, varsR, varsG)
  if(missing(data)) {
    about
  } else {
    about <- about |> mutate(data.names=add_names(.data$side, n()), .by="side")
    dat <- lapply(about$vars, function(v) { eval(v, data) })
    names(dat) <- about$data.names
    dat <- as_tibble(dat)
    list(about=about, data=dat)
  }
}

split_formula <- function(formula) {
  f <- parse_formula(formula, drop1=FALSE)
  fs <- f |> filter(.data$side=="left") |> select(left="vars")
  out <- y ~ 1
  has_right <- has_group <- FALSE
  if(any(f$side=="right")) {
    fr <- f |> filter(.data$side=="right") |> select(right="vars")
    fs <- cross_join(fs, fr)
    has_right <- TRUE
  }
  if(any(f$side=="group")) {
    fg <- f |> filter(.data$side=="group") |> select(group="vars")
    fs <- cross_join(fs, fg)
    out <- y ~ 1 | g
    has_group <- TRUE
  }
  environment(out) <- emptyenv()
  fs$formula <- rep(list(out), nrow(fs))
  for(idx in seq_len(nrow(fs))) { fs$formula[[idx]][[2]] <- fs$left[[idx]] }
  if(has_group) {
    if(has_right) for(idx in seq_len(nrow(fs))) { fs$formula[[idx]][[3]][[2]] <- fs[["right"]][[idx]] }
    for(idx in seq_len(nrow(fs))) { fs$formula[[idx]][[3]][[3]] <- fs$group[[idx]] }
  } else {
    if(has_right) for(idx in seq_len(nrow(fs))) { fs$formula[[idx]][[3]] <- fs[["right"]][[idx]] }
  }
  fs$readable <- sapply(fs$formula, format)
  fs |> pull(formula)
}
