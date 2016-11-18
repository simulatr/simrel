
validate_parm <- function(parList){
  list2env(parList, envir = environment())
  stopMsg <- list()
  warnMsg <- list()

  ## Critical Error
  if (!all(sapply(list(length(relpos), length(R2)), identical, length(q))))
    stopMsg$uneql <- "Length of relpos, R2 and q must be equal\n"

  if (!all(sapply(seq_along(q), function(i) q[i] > sapply(relpos, length)[i])))
    stopMsg$bigPred <- "Number of relevant predictor is smaller than the number of relevant components\n"

  if (!sum(q) < p)
    stopMsg$smallnvar <- "Number of variables can not be smaller than the number of relevant variables\n"

  if (!max(unlist(relpos)) < p)
    stopMsg$bigRelpos <- "Relevant Position can not exceed the number of variables\n"

  if (!all(R2 < 1 & R2 > 0))
    stopMsg$invalidR2 <- "R2 must be between 0 and 1\n"
  if (!is.null(muX) & length(muX) != p) {
    stopMsg$muXlength <- "Mean of X must have same length as the number of X variables\n"
  }
  if (!is.null(muY) & length(muY) != m) {
    stopMsg$muYlength <- "Mean of Y must have same length as the number of Y variables\n"
  }
  if (any(duplicated(unlist(ypos)))) {
    stopMsg$duplicateY <- "Response Space must have unique combination of response variable."
  }

  ## Warning Conditions
  if (any(unlist(lapply(ypos, identical, 1:2)))) {
    warnMst$noisY <- "Current setting of ypos will produce uninformative response variable."
  }

  valdMsg <- list(stopMsg = stopMsg, warnMsg = warnMsg)
  return(valdMsg)
}

simrelM <- function(n = 100, p = 15, q = c(5, 4, 3), m = 5,
                    relpos = list(c(1, 2), c(3, 4, 6), c(5, 7)),
                    gamma = 0.6, R2 = c(0.8, 0.7, 0.8),
                    ntest = NULL, muX = NULL, muY = NULL, rho = 0.1,
                    ypos = list(c(1), c(3, 4), c(2, 5))) {

  ## Validate Inputs
  argList <- as.list(environment())
  ### Make Different Function for this and pass argList to the function
  val.out <- validate_parm(argList)
  if (length(val.out$stopMsg) != 0) {
    stop(paste(sapply(val.out$stopMsg, paste, collapse = '\n')), call. = FALSE)
  }

  if (length(val.out$warnMsg) != 0) {
    warning(paste(sapply(val.out$warnMsg, paste, collapse = '\n')), call. = FALSE)
  }

  ## Expected Error Messages
  err.msg <- list(
    lessCor = expression({
      stop("Two Responses in orthogonal, but highly relevant spaces must be less correlated. Choose rho closer to zero.")
    }),
    noPD = expression({
      stop("No positive definite coveriance matrix found with current parameter settings")
    })
  )

  ## Completing Parameter for required response
  nW0 <- length(relpos)
  nW.extra <- m - length(relpos)
  nW.extra.idx <- seq.int(nW0 + 1, length.out = nW.extra)

  relpos <- append(relpos, lapply(nW.extra.idx, function(x) integer()))
  R2 <- c(R2, rep(0, nW.extra))
  q <- c(q, rep(0, nW.extra))

  ## How many components are relevant for each W_i
  n.relpos <- vapply(relpos, length, 0L)

  ## Irrelevant position of predictors
  irrelpos <- setdiff(seq_len(p), Reduce(union, relpos))
  predPos <- lapply(seq_along(relpos), function(i){
    pos <- relpos[[i]]
    ret <- c(pos, sample(irrelpos, q[i] - length(pos)))
    irrelpos <<- setdiff(irrelpos, ret)
    return(ret)
  })
  names(predPos) <- paste0("Relevant for W", seq_along(relpos))

  ## Constructing Sigma
  lambda <- exp(-gamma * (1:p))/exp(-gamma)
  SigmaZ <- diag(lambda); SigmaZinv <- diag(1 / lambda)
  # SigmaW <- matrix(rho, nW, nW); diag(SigmaW) <- 1
  # SigmaW <- as.matrix(Matrix::bdiag(SigmaW, diag(m - nW)))
  SigmaW <- diag(m)
  rhoMat <- SigmaW

  ### Covariance Construction
  get_cov <- function(pos, Rsq, p = p, lambda = lambda){
    out <- vector("numeric", p)
    alph <- runif(length(pos), -1, 1)
    out[pos] <- sign(alph) * sqrt(Rsq * abs(alph) / sum(abs(alph)) * lambda[pos])
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

  SigmaZW <- mapply(get_cov, pos = relpos, Rsq = R2, MoreArgs = list(p = p, lambda = lambda))
  Sigma <- cbind(rbind(SigmaW, SigmaZW), rbind(t(SigmaZW), SigmaZ))
  rho.out <- get_rho(rhoMat, R2)
  rho.out[is.nan(rho.out)] <- 0
  if (any(rho.out < -1 | rho.out > 1)) eval(err.msg$lessCor)

  ## Rotation Matrix
  RotX <- diag(p)
  RotY <- diag(m)

  getRotate <- function(predPos){
    n <- length(predPos)
    Qmat <- matrix(rnorm(n ^ 2), n)
    Qmat <- scale(Qmat, scale = FALSE)
    qr.Q(qr(Qmat))
  }

  for (pos in predPos) {
    rotMat <- getRotate(pos)
    RotX[pos, pos] <- rotMat
  }

  for (pos in ypos) {
    rotMat <- getRotate(pos)
    RotY[pos, pos] <- rotMat
  }


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

  ## True Coefficient of Determination for W's
  RsqW <- t(betaZ) %*% SigmaZW %*% solve(SigmaW)
  RsqY <- t(RotY) %*% RsqW %*% RotY

  ## Var-Covariance for Response and Predictors
  SigmaY <- t(RotY) %*% SigmaW %*% RotY
  SigmaX <- t(RotX) %*% SigmaZ %*% RotX
  SigmaYX <- t(RotY) %*% t(SigmaZW) %*% RotX
  SigmaYZ <- t(RotY) %*% t(SigmaZW)
  SigmaWX <- t(SigmaZW) %*% t(RotX)
  SigmaOut <- rbind(
    cbind(SigmaY, SigmaYX),
    cbind(t(SigmaYX), SigmaX)
  )
  ## Minimum Error
  minerror <- SigmaY - RsqY

  ## Check for Positive Definite
  pd <- all(eigen(Sigma)$values > 0)
  if (!pd) eval(err.msg[['noPD']])

  ## Simulation of Test and Training Data
  SigmaRot <- chol(Sigma)
  train_cal <- matrix(rnorm(n * (p + m), 0, 1), nrow = n)
  train_cal <- train_cal %*% SigmaRot
  W <- train_cal[, 1:m, drop = F]
  Z <- train_cal[, (m + 1):(m + p), drop = F]
  X <- Z %*% t(RotX)
  Y <- W %*% t(RotY)
  if (!(is.null(muX))) X <- sweep(X, 2, '+')
  if (!(is.null(muY))) Y <- sweep(Y, 2, '+')
  colnames(X) <- paste0('X', 1:p)
  colnames(Y) <- paste0('Y', 1:m)

  ### Test Data
  if (!is.null(ntest)) {
    test_cal <- matrix(rnorm(ntest * (p + m), 0, 1), nrow = ntest)
    test_cal <- test_cal %*% SigmaRot
    testW <- test_cal[, 1:m, drop = F]
    testZ <- test_cal[, (m + 1):(m + p), drop = F]
    testX <- testZ %*% t(RotX)
    testY <- testW %*% t(RotY)
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
    call = match.call(),
    X = X,
    Y = Y,
    W = W,
    Z = Z,
    beta = betaX,
    beta0 = beta0,
    relPred = predPos,
    testX = testX,
    testY = testY,
    testW = testW,
    testZ = testZ,
    minerror = minerror,
    Xrotation = RotX,
    Yrotation = RotY,
    type = "multivariate",
    lambda = lambda,
    SigmaWZ = Sigma,
    SigmaWX = SigmaWX,
    SigmaYZ = SigmaYZ,
    SigmaYX = SigmaYX,
    Sigma = SigmaOut,
    rho.out = rho.out,
    RsqW = RsqW,
    RsqY = RsqY
  )
  ret <- `class<-`(append(argList, ret), 'simrel')
  return(ret)
}
