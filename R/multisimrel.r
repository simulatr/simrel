#' Simulation of Multivariate Linear Model Data
#' @param n Number of observations
#' @param p Number of variables
#' @param q Vector containing the number of relevant predictor variables for each relevant response components
#' @param m Number of response variables
#' @param relpos A list of position of relevant component for predictor variables. The list contains vectors of position index, one vector or each relevant response components
#' @param gamma A declining (decaying) factor of eigen value of predictors (X). Higher the value of \code{gamma}, the decrease of eigenvalues will be steeper
#' @param R2 Vector of coefficient of determination (proportion of variation explained by predictor variable) for each relevant response components
#' @param ntest Number of test observation
#' @param eta A declining (decaying) factor of eigenvalues of response (Y). Higher the value of \code{eta}, more will be the declining of eigenvalues of Y. \code{eta = 0} refers that all eigenvalues of responses (Y) are 1.
#' @param muX Vector of average (mean) for each predictor variable
#' @param muY Vector of average (mean) for each response variable
#' @param ypos List of position of relevant response components that are combined to generate response variable during orthogonal rotation
#' @return A simrel object with all the input arguments along with following additional items
#'     \item{X}{Simulated predictors}
#'     \item{Y}{Simulated responses}
#'     \item{W}{Simulated predictor components}
#'     \item{Z}{Simulated response components}
#'     \item{beta}{True regression coefficients}
#'     \item{beta0}{True regression intercept}
#'     \item{relpred}{Position of relevant predictors}
#'     \item{testX}{Test Predictors}
#'     \item{testY}{Test Response}
#'     \item{testW}{Test predictor components}
#'     \item{testZ}{Test response components}
#'     \item{minerror}{Minimum model error}
#'     \item{Xrotation}{Rotation matrix of predictor (R)}
#'     \item{Yrotation}{Rotation matrix of response (Q)}
#'     \item{type}{Type of simrel object \emph{univariate} or \emph{multivariate}}
#'     \item{lambda}{Eigenvalues of predictors}
#'     \item{SigmaWZ}{Variance-Covariance matrix of components of response and predictors}
#'     \item{SigmaWX}{Covariance matrix of response components and predictors}
#'     \item{SigmaYZ}{Covariance matrix of response and predictor components}
#'     \item{Sigma}{Variance-Covariance matrix of response and predictors}
#'     \item{RsqW}{Coefficient of determination corresponding to response components}
#'     \item{RsqY}{Coefficient of determination corresponding to response variables}
#' @keywords simulation, linear model, linear model data
#' @references Sæbø, S., Almøy, T., & Helland, I. S. (2015). simrel—A versatile tool for linear model data simulation based on the concept of a relevant subspace and relevant predictors. Chemometrics and Intelligent Laboratory Systems, 146, 128-135.
#' @references Almøy, T. (1996). A simulation study on comparison of prediction methods when only a few components are relevant. Computational statistics & data analysis, 21(1), 87-107.
#' @export

multisimrel <- function(n = 100, p = 15, q = c(5, 4, 3), m = 5,
                    relpos = list(c(1, 2), c(3, 4, 6), c(5, 7)),
                    gamma = 0.6, R2 = c(0.8, 0.7, 0.8),
                    eta = 0, ntest = NULL, muX = NULL, muY = NULL,
                    ypos = list(c(1), c(3, 4), c(2, 5))) {
  ## Get all input parameter also for output ---
  arg_list <- as.list(environment())
  ## Validate Inputs ----
  if (!all(sapply(list(length(relpos), length(R2)), identical, length(q))))
    stop("Length of relpos, R2 and q must be equal\n")

  if (!all(sapply(seq_along(q), function(i) q[i] >= sapply(relpos, length)[i])))
    stop("Number of relevant predictor is smaller than the number of relevant components\n")

  if (!sum(q) <= p)
    stop("Number of variables can not be smaller than the number of relevant variables\n")

  if (!max(unlist(relpos)) <= p)
    stop("Relevant Position can not exceed the number of variables\n")

  if (!all(R2 < 1 & R2 > 0))
    stop("R2 must be between 0 and 1\n")
  if (!is.null(muX) & length(muX) != p) {
    stop("Mean of X must have same length as the number of X variables\n")
  }
  if (!is.null(muY) & length(muY) != m) {
    stop("Mean of Y must have same length as the number of Y variables\n")
  }
  if (any(duplicated(unlist(ypos)))) {
    stop("Response Space must have unique combination of response variable.")
  }

  ## Warning Conditions
  if (any(unlist(lapply(ypos, identical, 1:2)))) {
    warning("Current setting of ypos will produce uninformative response variable.")
  }

  ## Completing Parameter for required response
  nW0          <- length(relpos)
  nW.extra     <- m - length(relpos)
  nW.extra.idx <- seq.int(nW0 + 1, length.out = nW.extra)

  relpos <- append(relpos, lapply(nW.extra.idx, function(x) integer()))
  R2     <- c(R2, rep(0, nW.extra))
  q      <- c(q, rep(0, nW.extra))

  ## How many components are relevant for each W_i
  n.relpos <- vapply(relpos, length, 0L)

  ## Irrelevant position of predictors
  irrelpos <- setdiff(seq_len(p), Reduce(union, relpos))
  predPos  <- lapply(seq_along(relpos), function(i){
    pos      <- relpos[[i]]
    ret      <- c(pos, sample(irrelpos, q[i] - length(pos)))
    irrelpos <<- setdiff(irrelpos, ret)
    return(ret)
  })
  names(predPos) <- paste0("Relevant for W", seq_along(relpos))

  ## Constructing Sigma
  lambda    <- exp(-gamma * (1:p))/exp(-gamma)
  eta       <- exp(-eta * (1:m))/exp(-eta)
  SigmaZ    <- diag(lambda);
  SigmaZinv <- diag(1 / lambda)
  # SigmaW  <- matrix(rho, nW, nW); diag(SigmaW) <- 1
  # SigmaW  <- as.matrix(Matrix::bdiag(SigmaW, diag(m - nW)))
  SigmaW    <- diag(eta) ## diag(m)
  rhoMat    <- SigmaW

  ### Covariance Construction
  get_cov <- function(pos, Rsq, eta = 1, p = p, lambda = lambda){
    out      <- vector("numeric", p)
    alph     <- runif(length(pos), -1, 1)
    out[pos] <- sign(alph) * sqrt(Rsq * abs(alph) / sum(abs(alph)) * lambda[pos] * eta)
    return(out)
  }
  get_rho <- function(rhoMat, RsqVec) {
    sapply(1:nrow(rhoMat), function(row){
      sapply(1:ncol(rhoMat), function(col){
        if (row == col) return(1)
        rhoMat[row, col] / sqrt((RsqVec[row]) * (RsqVec[col]))
      })
    })
  }

  SigmaZW <- mapply(get_cov, pos = relpos, Rsq = R2, eta = eta, MoreArgs = list(p = p, lambda = lambda))
  Sigma   <- cbind(rbind(SigmaW, SigmaZW), rbind(t(SigmaZW), SigmaZ))
  rho.out <- get_rho(rhoMat, R2)
  rho.out[is.nan(rho.out)] <- 0
  if (any(rho.out < -1 | rho.out > 1))
    stop("Two Responses in orthogonal, but highly relevant spaces must be less correlated. Choose rho closer to zero.")

  ## Rotation Matrix
  RotX <- diag(p)
  RotY <- diag(m)

  getRotate <- function(predPos){
    n    <- length(predPos)
    Qmat <- matrix(rnorm(n ^ 2), n)
    Qmat <- scale(Qmat, scale = FALSE)
    qr.Q(qr(Qmat))
  }

  for (pos in predPos) {
    rotMat         <- getRotate(pos)
    RotX[pos, pos] <- rotMat
  }

  for (pos in ypos) {
    rotMat         <- getRotate(pos)
    RotY[pos, pos] <- rotMat
  }

  ## Fill remaining irrelevant space with random normal variates
  RotX[irrelpos, irrelpos] <- getRotate(irrelpos)


  ## True Regression Coefficient
  betaZ <- SigmaZinv %*% SigmaZW
  betaX <- RotX %*% betaZ %*% t(RotY)

  ## Geting Coef for Intercept
  beta0 <- rep(0, m)
  if (!(is.null(muY))) {
    beta0 <- beta0 + muY
  }
  if (!(is.null(muX))) {
    beta0 <- beta0 - t(betaX) %*% muX
  }

  ## Rotation was not correct, now it is good, i suppose
  SigmaY   <- RotY %*% SigmaW %*% t(RotY)
  SigmaX   <- RotX %*% SigmaZ %*% t(RotX)
  SigmaYX  <- RotY %*% t(SigmaZW) %*% t(RotX)
  SigmaYZ  <- RotY %*% t(SigmaZW)
  SigmaWX  <- t(SigmaZW) %*% RotX
  SigmaOut <- rbind(
    cbind(SigmaY, SigmaYX),
    cbind(t(SigmaYX), SigmaX)
  )

  ## True Coefficient of Determination for W's
  RsqW <- matrix(0, nrow = m, ncol = m)
  for (row in 1:m) {
    for (col in 1:m) {
      RsqW[row, col] <- (SigmaZW[, row] %*% SigmaZinv %*% t(SigmaZW)[col, ])/
        sqrt(SigmaW[row, row] * SigmaW[col, col])
    }
  }
  RsqY <- matrix(0, nrow = m, ncol = m)
  for (row in 1:m) {
    for (col in 1:m) {
      RsqY[row, col] <- (SigmaYX[row, ] %*% RotX %*% SigmaZinv %*% t(RotX) %*% t(SigmaYX)[ , col])/
        sqrt(SigmaY[row, row] * SigmaY[col, col])
    }
  }

  ## Minimum Error
  minerror <- SigmaY - SigmaYX %*% solve(SigmaX) %*% t(SigmaYX)
  ## Check for Positive Definite
  pd <- all(eigen(Sigma)$values > 0)
  if (!pd) stop("No positive definite coveriance matrix found with current parameter settings")

  ## Simulation of Test and Training Data
  SigmaRot  <- chol(Sigma)
  train_cal <- matrix(rnorm(n * (p + m), 0, 1), nrow = n)
  train_cal <- train_cal %*% SigmaRot
  W         <- train_cal[, 1:m, drop = F]
  Z         <- train_cal[, (m + 1):(m + p), drop = F]
  X         <- Z %*% t(RotX)
  Y         <- W %*% t(RotY)
  if (!(is.null(muX))) X <- sweep(X, 2, muX, '+')
  if (!(is.null(muY))) Y <- sweep(Y, 2, muY, '+')
  colnames(X) <- paste0('X', 1:p)
  colnames(Y) <- paste0('Y', 1:m)

  ### Test Data
  if (!is.null(ntest)) {
    test_cal <- matrix(rnorm(ntest * (p + m), 0, 1), nrow = ntest)
    test_cal <- test_cal %*% SigmaRot
    testW    <- test_cal[, 1:m, drop = F]
    testZ    <- test_cal[, (m + 1):(m + p), drop = F]
    testX    <- testZ %*% t(RotX)
    testY    <- testW %*% t(RotY)
    if (!(is.null(muX))) testX <- sweep(testX, 2, muX, '+')
    if (!(is.null(muY))) testY <- sweep(testY, 2, muY, '+')
    colnames(testX) <- paste0('X', 1:p)
    colnames(testY) <- paste0('Y', 1:m)
  } else {
    testX <- NULL; testY <- NULL
    testZ <- NULL; testW <- NULL
  }

  ## Return List
  ret <- list(
    call      = match.call(),
    X         = X,
    Y         = Y,
    W         = W,
    Z         = Z,
    beta      = betaX,
    beta0     = beta0,
    relpred   = predPos,
    testX     = testX,
    testY     = testY,
    testW     = testW,
    testZ     = testZ,
    minerror  = minerror,
    Xrotation = RotX,
    Yrotation = RotY,
    lambda    = lambda,
    SigmaWZ   = Sigma,
    SigmaWX   = SigmaWX,
    SigmaYZ   = SigmaYZ,
    SigmaYX   = SigmaYX,
    Sigma     = SigmaOut,
    rho.out   = rho.out,
    RsqW      = RsqW,
    RsqY      = RsqY,
    type      = "multivariate"
  )
  ret <- `class<-`(append(arg_list, ret), 'simrel')
  return(ret)
}
