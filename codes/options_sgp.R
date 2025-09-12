
# Options ----------------------------------------------------------------------

options_sgp <- list()


# 2 regimes, mu change, small difference
options_sgp$r2_ar1_mu1 <- create_regimes(
  create_sgp$ar,
  list(
    list(mu = 0, rho1 = 0.5),
    list(mu = 0.2, rho1 = 0.5)
  )
)

# 2 regimes, mu change, big difference
options_sgp$r2_ar1_mu2 <- create_regimes(
  create_sgp$ar,
  list(
    list(mu = 0, rho1 = 0.5),
    list(mu = 1, rho1 = 0.5)
  )
)


# 2 regimes, persistence change, big difference
options_sgp$r2_ar1_rho1 <- create_regimes(
  create_sgp$ar,
  list(
    list(mu = 0, rho1 = 0.9),
    list(mu = 0, rho1 = 0.1)
  )
)

# 2 regimes, persistence change, small difference
options_sgp$r2_ar1_rho2 <- create_regimes(
  create_sgp$ar,
  list(
    list(mu = 0, rho1 = 0.6),
    list(mu = 0, rho1 = 0.4)
  )
)


# 2 regimes, sign switching, small difference
options_sgp$r2_ar1_sign1 <- create_regimes(
  create_sgp$ar,
  list(
    list(mu = 0, rho1 = 0.3),
    list(mu = 0, rho1 = -0.3)
  )
)

# 2 regimes, sign switching, big difference
options_sgp$r2_ar1_sign2 <- create_regimes(
  create_sgp$ar,
  list(
    list(mu = 0, rho1 = 0.7),
    list(mu = 0, rho1 = -0.7)
  )
)


# 2 regimes, new lag, positive, small
options_sgp$r2_ar2_pos1 <- create_regimes(
  create_sgp$ar,
  list(
    list(mu = 0, rho1 = 0.6),
    list(mu = 0, rho1 = 0.6, rho2 = 0.2)
  )
)

# 2 regimes, new lag, positive, big
options_sgp$r2_ar2_pos2 <- create_regimes(
  create_sgp$ar,
  list(
    list(mu = 0, rho1 = 0.6),
    list(mu = 0, rho1 = 0.6, rho2 = 0.5)
  )
)


# 2 regimes, new lag, negative, small
options_sgp$r2_ar2_neg1 <- create_regimes(
  create_sgp$ar,
  list(
    list(mu = 0, rho1 = 0.6),
    list(mu = 0, rho1 = 0.6, rho2 = -0.2)
  )
)

# 2 regimes, new lag, negative, big
options_sgp$r2_ar2_neg2 <- create_regimes(
  create_sgp$ar,
  list(
    list(mu = 0, rho1 = 0.6),
    list(mu = 0, rho1 = 0.6, rho2 = -0.5)
  )
)


# 2 regimes, vol change, small difference
options_sgp$r2_ar1_vol1 <- create_regimes(
  create_sgp$ar,
  list(
    list(mu = 0, rho1 = 0.6, vol = 1),
    list(mu = 0, rho1 = 0.6, vol = 2)
  )
)

# 2 regimes, vol change, big difference
options_sgp$r2_ar1_vol2 <- create_regimes(
  create_sgp$ar,
  list(
    list(mu = 0, rho1 = 0.6, vol = 1),
    list(mu = 0, rho1 = 0.6, vol = 4)
  )
)
