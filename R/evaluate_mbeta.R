evaluate_mbeta <- function(data,
                           contrast = define_contrast("raw"),
                           benchmark = 0.5,
                           alpha = 0.05,
                           alternative = "greater",
                           analysis = "co-primary",
                           transformation = "none",
                           regu = c(1, 1 / 2, 0),
                           pars = list(),
                           attrs = list()) {
  ## inference:
  stopifnot(transformation == "none")

  nrep <- get_from_pars(pars, "nrep", 5000)
  lfc_pr <- get_from_pars(pars, "nrep", switch(analysis,
    "co-primary" = 1,
    "full" = 0
  ))

  G <- length(data)
  m <- ncol(data[[1]])
  type <- attr(contrast(data), "type")

  moms <- lapply(data, function(d) {
    add_moments(prior_moments(m, regu), data_moments(d))
  })
  stats <- data2stats(data, contrast, regu, raw = TRUE)

  ## posterior sample:
  pss <- sample_mbeta(moms) %>%
    lapply(function(s) {
      s %*% contrast(data)
    })

  ## credible region:
  qstar <- stats::uniroot(
    f = eval_cr, interval = c(0, 0.5),
    moms = moms, pss = pss, type = type, alpha = alpha,
    alternative = alternative, analysis = analysis,
    lfc_pr = lfc_pr
  )$root
  crs <- get_cr(moms, pss, type = type, q = qstar, alternative = alternative)

  ## output:
  lapply(1:G, function(g) {
    data.frame(
      parameter = stats[[g]]$names,
      hypothesis = hypstr(alternative, benchmark[g]),
      estimate = stats[[g]]$est,
      lower = crs[[g]]$lower,
      upper = crs[[g]]$upper,
      tstat = NA,
      pval = NA
    )
  }) %>%
    complete_results(benchmark) %>%
    set_attrs(
      attrs = attrs
    ) %>%
    return()
}


# Helper functions ----------------------------------------------------------------------------
eval_cr <- function(q, moms, pss, type, alpha, alternative, analysis, lfc_pr) {
  crs <- get_cr(moms, pss, type, q, alternative)
  coverage(crs = crs, pss = pss, analysis = analysis, lfc_pr = lfc_pr) - (1 - alpha)
}


coverage <- function(crs, pss, analysis = "co-primary", lfc_pr = 1) {
  nrep <- nrow(pss[[1]])
  m <- ncol(pss[[1]])
  G <- length(pss)

  H <- switch(analysis,
    "co-primary" = 1,
    "full" = G
  )

  L <- lapply(crs, function(x) matrix(x$lower, nrow = nrep, ncol = m, byrow = TRUE))
  U <- lapply(crs, function(x) matrix(x$upper, nrow = nrep, ncol = m, byrow = TRUE))

  C <- lapply(1:G, function(g) covered(pss[[g]], L[[g]], U[[g]]))

  mean(apply(Reduce("+", postproc(C, nrep, m, G, lfc_pr)), 1, min) >= H)
}

postproc <- function(C, nrep, m, G, lfc_pr = 0) {
  stopifnot(lfc_pr >= 0 & lfc_pr <= 1)
  if (lfc_pr == 0) {
    return(C)
  }

  ## 'split prior' approach (lfc_pr mass on 'LFC', 1-lfc_pr mass on regular distribution):

  ## matrix M determines which values will be projected to LFC (i.e. non-coverage of CR)
  M <-
    ## step 1: binary vector either LFC (TRUE, with prob. lfc_pr) or regular parameter (FALSE):
    base::sample(c(TRUE, FALSE), size = nrep, replace = TRUE, prob = c(lfc_pr, 1 - lfc_pr)) %>%
    ## step 2: generate values:
    lapply(function(x) {
      # case 1: LFC, randomly project all but one component (CR does not cover after proj. by def.):
      x * sample(1:G, m, replace = TRUE) +
        # case 2: original parameter vector, no projections:
        (1 - x) * rep(0, m)
    }) %>%
    ## step 3: bind all vectors together to rows of M:
    do.call(rbind, .)

  ## set values of C to FALSE, depending on values of M (amounts to non-coverage):
  lapply(1:G, function(g) {
    (M %in% c(0, g)) * C[[g]]
  }) %>% return()
}


covered <- function(S, L, U) {
  S >= L & S <= U
}


sample_mbeta <- function(moms, nrep = 5000, proj.pd = FALSE) {
  lapply(moms, function(mom) sample_mbeta1(mom, nrep, proj.pd))
}


#' @importFrom  copula normalCopula
#' @importFrom  copula P2p
#' @importFrom  copula mvdc
#' @importFrom  copula rMvdc
#' @importFrom  Matrix nearPD
sample_mbeta1 <- function(mom, nrep = 5000, proj.pd = FALSE) {
  m <- nrow(mom$moments)
  R <- stats::cov2cor(mom2cov(mom))
  if (proj.pd) {
    R <- as.matrix(Matrix::nearPD(R)$mat)
  }
  cop <- copula::normalCopula(param = copula::P2p(R), dim = m, dispstr = "un")
  mp <- margin_params(mom, c("shape1", "shape2"))

  ## output:
  copula::mvdc(cop, margins = rep("beta", m), paramMargins = mp) %>%
    copula::rMvdc(n = nrep) %>%
    return()
}


margin_params <- function(mom, n = c("alpha", "beta")) {
  lapply(1:nrow(mom$moments), function(j) {
    y <- list(mom$moments[j, j], mom$n - mom$moments[j, j])
    names(y) <- n
    return(y)
  })
}


get_cr1 <- function(mom, ps, type = "raw", q = 0.05, alternative = "two.sided") {
  mp <- margin_params(mom)
  m <- length(mp)
  s <- c(0, 1)
  p <- switch(alternative,
    less = c(min(s), 1 - q),
    greater = c(q, max(s)),
    two.sided = c(q / 2, 1 - q / 2)
  )

  if (type == "raw") {
    cr <- data.frame(t(sapply(1:m, function(j) {
      stats::qbeta(p, mp[[j]]$alpha, mp[[j]]$beta)
    })))
  } else {
    cr <- data.frame(t(apply(ps, 2, function(x) stats::quantile(x, probs = p))))
  }
  colnames(cr) <- c("lower", "upper")

  return(cr)
}


get_cr <- function(moms, pss, type = "raw", q = 0.05, alternative = "two.sided") {
  lapply(1:length(moms), function(j) get_cr1(moms[[j]], pss[[j]], type, q, alternative))
}
