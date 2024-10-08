% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_initial_pparams_rc.R
\name{sample_initial_pparams_rc}
\alias{sample_initial_pparams_rc}
\title{Use centers and radii to sample initial parameters from box}
\usage{
sample_initial_pparams_rc(
  sh_rc,
  sp_c,
  sp_r,
  n_draws,
  buffer = 5e-08,
  pos_params = c("R0", "mu", "sigmaSE", "iota", "sigma", "psi", "alpha"),
  unit_interval_params = c("cohort", "amplitude", "S_0", "E_0", "I_0", "R_0", "rho")
)
}
\arguments{
\item{sh_rc}{\code{tbl} with \code{param}, \code{center}, and \code{radius} columns.}

\item{sp_c}{\code{data.frame} of specific parameter values in
format of \code{pparams()} output.}

\item{sp_r}{\code{tbl} with \code{param} and \code{radius} columns.}

\item{n_draws}{Number of initial parameter sets to draw.}

\item{buffer}{The minimum space required between the boundaries of the
sampling box and the boundaries of the possible values for the parameters.}

\item{pos_params}{Character vector of parameters which must be positive.}

\item{unit_interval_params}{Character vector of parameters which must be
between 0 and 1.}
}
\value{
A list of parameter sets in the \code{pparams()} format.
}
\description{
This is useful when you want to sample initial parameters
in the vicinity of a large number of specific parameters. Instead of
manually specifying bounds for \code{parameter1[unit1]}, \code{parameter1[unit2]},
etc., which may have quite different values, you can specify vector of
centers (based on a past fit, for example) and then sample in an interval
around each one using radii for \code{parameter1}, \code{parameter2}, etc.
}
\examples{
\dontrun{
AK_mod = AK_model()
sh_rc = tibble::tribble(
  ~param, ~center, ~radius,
 "gamma1",    -0.63695, 0.05,
 "gamma0",    4.61215,    0.5,
 "mu",        0.02,        0
)
specific_radii = tibble::tribble(
  ~param, ~radius,
  "R0",        6,
  "rho",       0.05,
  "sigmaSE",   6,
  "amplitude", 0.08,
  "S_0",       0.01,
  "E_0",       0.0001,
  "I_0",       0.001,
  "R_0",       0.01,
  "sigma",     10,
  "iota",      0.5,
  "psi",       0.4,
  "alpha",     0.02,
  "cohort",    0.1,
  "gamma",     30
)
sample_initial_pparams_rc(
  sh_rc = sh_rc,
  sp_c = panelPomp::coef(AK_mod, format = "list")$specific,
  sp_r = specific_radii,
  n_draws = 3
)
}
}
