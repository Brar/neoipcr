#' Exact Poisson confidence interval
#'
#' Computes exact Poisson confidence intervals for count-over-exposure metrics
#' (incidence densities, utilisation densities, procedure rates, antibiotic
#' treatment days, agents per infection). Uses [stats::poisson.test()] which
#' handles zero events gracefully.
#'
#' @param events Integer. Number of observed events (infections, device-days,
#'   procedures, etc.). Must be non-negative.
#' @param exposure Numeric. Total exposure time or denominator (patient-days,
#'   device-days, number of infections, number of patients). Must be positive.
#' @param multiplier Numeric. Scaling factor for the rate. Default 1000
#'   (per 1,000 patient-days). Use 100 for utilisation densities and procedure
#'   rates.
#' @param conf.level Numeric. Confidence level. Default 0.95.
#'
#' @returns A named list with three elements:
#'   \describe{
#'     \item{rate}{The point estimate: `events / exposure * multiplier`}
#'     \item{lower}{Lower bound of the confidence interval, scaled by multiplier}
#'     \item{upper}{Upper bound of the confidence interval, scaled by multiplier}
#'   }
#'
#' @examples
#' # BSI incidence: 47 infections over 28,904 patient-days
#' neoipc_poisson_ci(47, 28904, multiplier = 1000)
#'
#' # Zero events: returns valid CI with lower = 0
#' neoipc_poisson_ci(0, 5000, multiplier = 1000)
#'
#' # Utilisation density (per 100 patient-days)
#' neoipc_poisson_ci(350, 5000, multiplier = 100)
#'
#' @export
neoipc_poisson_ci <- function(events, exposure,
                               multiplier = 1000,
                               conf.level = 0.95) {
  check_number_whole(events, min = 0)
  check_number_decimal(exposure, min = .Machine$double.eps)
  check_number_decimal(multiplier, min = .Machine$double.eps)
  check_number_decimal(conf.level, min = 0, max = 1)

  pt <- stats::poisson.test(events, T = exposure, conf.level = conf.level)
  list(
    rate  = events / exposure * multiplier,
    lower = pt$conf.int[1] * multiplier,
    upper = pt$conf.int[2] * multiplier
  )
}

#' Wilson binomial confidence interval
#'
#' Computes Wilson score confidence intervals for true proportions (detection
#' rates, patient proportions). Implements the Wilson score formula directly,
#' which provides better coverage properties than exact (Clopper-Pearson)
#' intervals, particularly for small samples.
#'
#' Unlike Clopper-Pearson, the Wilson interval does not necessarily include 0
#' when x = 0 or include 1 when x = n. This is mathematically correct
#' behaviour and contributes to the method's superior coverage properties.
#'
#' @param x Integer. Number of successes (infections with pathogen, patients
#'   with antibiotic). Must be non-negative.
#' @param n Integer. Number of trials (total infections, total patients).
#'   Must be positive and >= x.
#' @param conf.level Numeric. Confidence level. Default 0.95.
#'
#' @returns A named list with three elements:
#'   \describe{
#'     \item{proportion}{The point estimate: `x / n`}
#'     \item{lower}{Lower bound of the Wilson confidence interval}
#'     \item{upper}{Upper bound of the Wilson confidence interval}
#'   }
#'
#' @examples
#' # Detection rate: 220 infections with pathogen out of 283 total
#' neoipc_wilson_ci(220, 283)
#'
#' # Zero successes: lower is close to but not exactly 0
#' neoipc_wilson_ci(0, 50)
#'
#' # All successes: upper is close to but not exactly 1
#' neoipc_wilson_ci(50, 50)
#'
#' @export
neoipc_wilson_ci <- function(x, n, conf.level = 0.95) {
  check_number_whole(x, min = 0)
  check_number_whole(n, min = 1)
  if (x > n) rlang::abort("`x` must be <= `n`.")
  check_number_decimal(conf.level, min = 0, max = 1)

  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  p_hat <- x / n
  denom <- 1 + z^2 / n
  center <- (p_hat + z^2 / (2 * n)) / denom
  margin <- z * sqrt((p_hat * (1 - p_hat) + z^2 / (4 * n)) / n) / denom

  list(
    proportion = p_hat,
    lower      = center - margin,
    upper      = center + margin
  )
}
