model_one_source <-
    "model{
  # Likelihood
  n0  ~ dbinom(P01, N0)
  n1  ~ dbinom(P11, N1)
  n0s ~ dbinom(P01s, N0s)


  # Priors
  PS10 ~ dbeta(1,1)
  PS01 ~ dbeta(1,1)
  P01  ~ dbeta(1,1)
  P01s ~ dbeta(1,1)


  # Computed quantities
  P11  <- (1-PS10)*P01  + PS01*(1-P01)
  P11s <- (1-PS10)*P01s + PS01*(1-P01s)

  ## bounds
  PS01.l    <- max(0, (P11-P01)/(1-P01))
  PS01.u    <- min(P11/(1-P01), 1)
  P11_1 <- (1-P01s/P01)*PS01.l + (P01s/P01)*P11
  P11_2 <- (1-P01s/P01)*PS01.u + (P01s/P01)*P11
  P11s.l <- min(P11_1, P11_2)
  P11s.u <- max(P11_1, P11_2)
}"


model_one_source_monotonic <-
  "model{

  # Likelihood
  n0  ~ dbinom(P01, N0)
  n1  ~ dbinom(P11, N1)
  n0s ~ dbinom(P01s, N0s)


  # Priors
  PS10 <- 0
  PS01 ~ dbeta(1,1)
  P01  ~ dbeta(1,1)
  P01s ~ dbeta(1,1)


  # Computed quantities
  P11  <- (1-PS10)*P01  + PS01*(1-P01)
  P11s <- (1-PS10)*P01s + PS01*(1-P01s)
}"


model_two_sources <- "model{

  # Likelihood
  n0a ~ dbinom(P01a, N0a)
  n0b ~ dbinom(P01b, N0b)
  n1a ~ dbinom(P11a, N1a)
  n1b ~ dbinom(P11b, N1b)
  n0s ~ dbinom(P01s, N0s)


  # Priors
  P01a ~ dbeta(1, 1)
  P01b ~ dbeta(1, 1)
  P01s ~ dbeta(1, 1)
  PS10 ~ dbeta(1, 1)
  PS01 ~ dbeta(1, 1)

  # Computed quantities
  P11a <- (1-PS10)*P01a + PS01*(1-P01a)
  P11b <- (1-PS10)*P01b + PS01*(1-P01b)
  P11s <- (1-PS10)*P01s + PS01*(1-P01s)
}"
