# --- neoipc_poisson_ci() ---------------------------------------------------

test_that("neoipc_poisson_ci returns correct structure", {
  result <- neoipc_poisson_ci(47, 28904)
  expect_type(result, "list")
  expect_named(result, c("rate", "lower", "upper"))
  expect_true(is.numeric(result$rate))
  expect_true(is.numeric(result$lower))
  expect_true(is.numeric(result$upper))
})

test_that("neoipc_poisson_ci matches known poisson.test result", {
  # Verify against direct poisson.test() call
  pt <- stats::poisson.test(47, T = 28904, conf.level = 0.95)
  result <- neoipc_poisson_ci(47, 28904, multiplier = 1000)
  expect_equal(result$rate, 47 / 28904 * 1000)
  expect_equal(result$lower, pt$conf.int[1] * 1000)
  expect_equal(result$upper, pt$conf.int[2] * 1000)
})

test_that("neoipc_poisson_ci handles zero events", {
  result <- neoipc_poisson_ci(0, 5000, multiplier = 1000)
  expect_equal(result$rate, 0)
  expect_equal(result$lower, 0)
  expect_true(result$upper > 0)
})

test_that("neoipc_poisson_ci handles single event", {
  result <- neoipc_poisson_ci(1, 10000, multiplier = 1000)
  expect_equal(result$rate, 0.1)
  expect_true(result$lower < result$rate)
  expect_true(result$upper > result$rate)
})

test_that("neoipc_poisson_ci respects multiplier", {
  r1000 <- neoipc_poisson_ci(50, 10000, multiplier = 1000)
  r100  <- neoipc_poisson_ci(50, 10000, multiplier = 100)
  r1    <- neoipc_poisson_ci(50, 10000, multiplier = 1)
  expect_equal(r1000$rate, r100$rate * 10)
  expect_equal(r1000$rate, r1$rate * 1000)
  expect_equal(r1000$lower, r100$lower * 10)
  expect_equal(r1000$upper, r100$upper * 10)
})

test_that("neoipc_poisson_ci respects conf.level", {
  ci95 <- neoipc_poisson_ci(47, 28904)
  ci99 <- neoipc_poisson_ci(47, 28904, conf.level = 0.99)
  # 99% CI must be wider than 95% CI
  expect_true(ci99$lower < ci95$lower)
  expect_true(ci99$upper > ci95$upper)
})

test_that("neoipc_poisson_ci CI ordering is consistent", {
  result <- neoipc_poisson_ci(3, 10, multiplier = 1000)
  expect_true(result$lower <= result$rate)
  expect_true(result$upper >= result$rate)
})

test_that("neoipc_poisson_ci validates inputs", {
  expect_error(neoipc_poisson_ci(-1, 100))
  expect_error(neoipc_poisson_ci(2.5, 100))
  expect_error(neoipc_poisson_ci(5, 0))
  expect_error(neoipc_poisson_ci(5, -10))
  expect_error(neoipc_poisson_ci("a", 100))
})

test_that("neoipc_poisson_ci errors on zero exposure", {
  expect_error(neoipc_poisson_ci(0, 0))
})

# --- neoipc_wilson_ci() ----------------------------------------------------

test_that("neoipc_wilson_ci returns correct structure", {
  result <- neoipc_wilson_ci(220, 283)
  expect_type(result, "list")
  expect_named(result, c("proportion", "lower", "upper"))
  expect_true(is.numeric(result$proportion))
  expect_true(is.numeric(result$lower))
  expect_true(is.numeric(result$upper))
})

test_that("neoipc_wilson_ci matches known Wilson formula result", {
  # x = 220, n = 283 — verify against hand-computed Wilson score
  z <- stats::qnorm(0.975)
  p_hat <- 220 / 283
  denom <- 1 + z^2 / 283
  center <- (p_hat + z^2 / (2 * 283)) / denom
  margin <- z * sqrt((p_hat * (1 - p_hat) + z^2 / (4 * 283)) / 283) / denom
  expected_lower <- center - margin
  expected_upper <- center + margin

  result <- neoipc_wilson_ci(220, 283)
  expect_equal(result$proportion, 220 / 283)
  expect_equal(result$lower, expected_lower)
  expect_equal(result$upper, expected_upper)
})

test_that("neoipc_wilson_ci handles zero successes", {
  result <- neoipc_wilson_ci(0, 50)
  expect_equal(result$proportion, 0)
  # Wilson score: when x = 0, margin = center exactly, so lower = 0
  expect_equal(result$lower, 0)
  expect_true(result$upper > 0)
})

test_that("neoipc_wilson_ci handles all successes", {
  result <- neoipc_wilson_ci(50, 50)
  expect_equal(result$proportion, 1)
  # Wilson score: when x = n, center + margin = 1 exactly
  expect_equal(result$upper, 1)
  expect_true(result$lower < 1)
})

test_that("neoipc_wilson_ci handles small sample", {
  result <- neoipc_wilson_ci(1, 3)
  expect_equal(result$proportion, 1 / 3)
  expect_true(result$lower < result$proportion)
  expect_true(result$upper > result$proportion)
  # Wide CI for small sample
  expect_true((result$upper - result$lower) > 0.3)
})

test_that("neoipc_wilson_ci handles n = 1 edge cases", {
  r0 <- neoipc_wilson_ci(0, 1)
  expect_equal(r0$proportion, 0)
  expect_true(r0$upper > 0)

  r1 <- neoipc_wilson_ci(1, 1)
  expect_equal(r1$proportion, 1)
  expect_true(r1$lower < 1)
})

test_that("neoipc_wilson_ci CI ordering is consistent", {
  result <- neoipc_wilson_ci(15, 100)
  expect_true(result$lower <= result$proportion)
  expect_true(result$upper >= result$proportion)
})

test_that("neoipc_wilson_ci respects conf.level", {
  ci95 <- neoipc_wilson_ci(100, 200)
  ci99 <- neoipc_wilson_ci(100, 200, conf.level = 0.99)
  expect_true(ci99$lower < ci95$lower)
  expect_true(ci99$upper > ci95$upper)
})

test_that("neoipc_wilson_ci validates inputs", {
  expect_error(neoipc_wilson_ci(10, 5))   # x > n
  expect_error(neoipc_wilson_ci(-1, 10))  # negative x
  expect_error(neoipc_wilson_ci(0, 0))    # zero n
  expect_error(neoipc_wilson_ci(5, -1))   # negative n
  expect_error(neoipc_wilson_ci("a", 10)) # non-numeric
})
