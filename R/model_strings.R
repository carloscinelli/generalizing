model_one_source <-
    "model{
  # Likelihood
  n0  ~ dbinom(p01, N0)
  n1  ~ dbinom(p11, N1)
  n0s ~ dbinom(p01s, N0s)


  # Priors
  PS10 ~ dbeta(1,1)
  PS01 ~ dbeta(1,1)
  p01  ~ dbeta(1,1)
  p01s ~ dbeta(1,1)


  # Computed quantities
  p11  <- (1-PS10)*p01  + PS01*(1-p01)
  p11s <- (1-PS10)*p01s + PS01*(1-p01s)

  ## bounds
  PS01.l    <- max(0, (p11-p01)/(1-p01))
  PS01.u    <- min(p11/(1-p01), 1)
  p11_1 <- (1-p01s/p01)*PS01.l + (p01s/p01)*p11
  p11_2 <- (1-p01s/p01)*PS01.u + (p01s/p01)*p11
  p11s.l <- min(p11_1, p11_2)
  p11s.u <- max(p11_1, p11_2)
}"


model_one_source_monotonic <-
  "model{

  # Likelihood
  n0  ~ dbinom(p01, N0)
  n1  ~ dbinom(p11, N1)
  n0s ~ dbinom(p01s, N0s)


  # Priors
  PS10 <- 0
  PS01 ~ dbeta(1,1)
  p01  ~ dbeta(1,1)
  p01s ~ dbeta(1,1)


  # Computed quantities
  p11  <- (1-PS10)*p01  + PS01*(1-p01)
  p11s <- (1-PS10)*p01s + PS01*(1-p01s)
}"


model_two_sources <- "model{

  # Likelihood
  n0a ~ dbinom(p01a, N0a)
  n0b ~ dbinom(p01b, N0b)
  n1a ~ dbinom(p11a, N1a)
  n1b ~ dbinom(p11b, N1b)
  n0s ~ dbinom(p01s, N0s)


  # Priors
  p01a ~ dbeta(1, 1)
  p01b ~ dbeta(1, 1)
  p01s ~ dbeta(1, 1)
  PS10 ~ dbeta(1, 1)
  PS01 ~ dbeta(1, 1)

  # Computed quantities
  p11a <- (1-PS10)*p01a + PS01*(1-p01a)
  p11b <- (1-PS10)*p01b + PS01*(1-p01b)
  p11s <- (1-PS10)*p01s + PS01*(1-p01s)
}"
