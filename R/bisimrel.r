#' Simulation of Multivariate Linear Model data with response
#' @importFrom stats cov rnorm runif
#' @param n Number of training samples
#' @param p Number of x-variables
#' @param q Vector of number of relevant predictor variables for first, second and common to both responses
#' @param rho A 2-element vector, unconditional and conditional correlation between y_1 and y_2
#' @param relpos A list of position of relevant component for predictor variables. The list contains vectors of position index, one vector or each response
#' @param gamma A declining (decaying) factor of eigen value of predictors (X). Higher the value of \code{gamma}, the decrease of eigenvalues will be steeper
#' @param R2 Vector of coefficient of determination for each response
#' @param ntest Number of test observation
#' @param muX Vector of average (mean) for each predictor variable
#' @param muY Vector of average (mean) for each response variable
#' @param sim A simrel object for reusing parameters setting
#' @return A simrel object with all the input arguments along with following additional items
#'     \item{X}{Simulated predictors}
#'     \item{Y}{Simulated responses}
#'     \item{beta}{True regression coefficients}
#'     \item{beta0}{True regression intercept}
#'     \item{relpred}{Position of relevant predictors}
#'     \item{testX}{Test Predictors}
#'     \item{testY}{Test Response}
#'     \item{minerror}{Minimum model error}
#'     \item{Rotation}{Rotation matrix of predictor (R)}
#'     \item{type}{Type of simrel object, in this case \emph{bivariate}}
#'     \item{lambda}{Eigenvalues of predictors}
#'     \item{Sigma}{Variance-Covariance matrix of response and predictors}
#' @keywords simulation, linear model, linear model data
#' @references Sæbø, S., Almøy, T., & Helland, I. S. (2015). simrel—A versatile tool for linear model data simulation based on the concept of a relevant subspace and relevant predictors. Chemometrics and Intelligent Laboratory Systems, 146, 128-135.
#' @references Almøy, T. (1996). A simulation study on comparison of prediction methods when only a few components are relevant. Computational statistics & data analysis, 21(1), 87-107.
#' @export

bisimrel <- function(n = 50, p = 100, q = c(10, 10, 5),
                    rho = c(0.8, 0.4), relpos = list(c(1, 2), c(2, 3)),
                    gamma = 0.5, R2 = c(0.8, 0.8), ntest = NULL,
                    muY = NULL, muX = NULL, sim = NULL) {
  ## Some internal functions
  .Rfunc <- function(alpha1, alpha2, L){
    t(alpha1) %*% solve(diag(L)) %*% alpha2
  }
  .a21func <- function(a11, a12, a22, R12, l1, l2){
    l1/a11*(R12 - a22*a12/l2)
  }
  .a22func <- function(a11, a12, R2, R12, l1, l2){
    bb    <- R12 * a12/a11^2 * l1/l2
    root  <- sqrt( R12^2 * a12^2/a11^4 * l1^2/l2^2 - (1/l2 + l1/l2 * a12^2/a11^2) * (l1/a11^2 * R12^2 - R2))
    denom <- (1/l2 + l1/l2 * a12^2/a11^2)
    w1    <- (bb - root)/denom
    w2    <- (bb + root)/denom
    return(c(w1, w2))
  }

  ## Function Body
  if (!is.null(sim)) {
    betaX    <- sim$beta
    beta0    <- sim$beta0
    muY      <- sim$muY
    muX      <- sim$muX
    qpos     <- sim$relpred
    p        <- sim$p
    q        <- sim$q
    gamma    <- sim$gamma
    lambdas  <- sim$lambda
    R2       <- sim$R2
    relpos   <- sim$relpos
    minerror <- sim$minerror
    Sigma    <- sim$Sigma
    R        <- sim$Rotation
    warning(paste("All parameters are collected from the supplied 'sim' object.\n"))
  }

  # m : Number of relevant components, a vector of 2 elements --------------------------
  m <- unlist(lapply(relpos, length))

  if (any(m == 0)) {
    stop("You must have minimum one relevant component for each response\n")
  }
  if (q[3] > q[1] | q[3] > q[2]) {
    stop("The number of common relevant predictors cannot exceed the number relevant predictors for either response\n")
  }
  if (q[1] < m[1] | q[2] < m[2]) {
    stop(paste("the number of relevant predictors must at \n
    least be equal to the number of relevant components\n"))
  }
  compos <- sum(relpos[[1]] %in% relpos[[2]])
  if (compos > 0) {
    cpos1 <- which(relpos[[1]] %in% relpos[[2]])
    cpos2 <- which(relpos[[2]] %in% relpos[[1]])
  }
  if (compos == 0 & q[3] > 0) {
    warning("The number of common relevant predictors is set to zero due to orthogonal relevant spaces \n")
    q[3] <- 0
  }
  if (compos == m[1] & compos == m[2] & q[3] != q[1]) {
    q[3] <- min(q[1:2])
    warning(paste("The number of common relevant predictors is set to",
                  q[3], "since the relevant spaces are identical.\n"))
  }
  if (compos > 0 & q[3] == 0) {
    warning(paste("The number of common relevant predictors is set to ",
                  compos, " since the relevant spaces must share common variable(s) \n"))
    q[3] <- compos
  }
  if (compos == 0) {
    rho[2] <- rho[1]/sqrt((1 - R2[1]) * (1 - R2[2]))
    if (rho[2] < -0.99999 | rho[2] > 0.9999)
      stop("Non-PD covariance matrix, choose smaller R2-values\n")
    warning(paste("The conditional correlation between y1 and y2 is fixed equal to",
                  round(rho[2], 2), " due to orthogonal relevant spaces \n"))
    q[3] <- 0
  }

  # Defining variables to be unique or common to the responses. ----------------------------------------------
  if (is.null(sim)) {
    qpos <- vector("list", 3)
    p1extra <- max(0, q[1] - m[1] - (q[3] - compos))
    p2extra <- max(0, q[2] - m[2] - (q[3] - compos))
    irrelpos <- (1:p)[-c(relpos[[1]], relpos[[2]])]
    extra1 <- extra2 <- NULL
    if (p1extra > 0) {
      extra1 <- sample(irrelpos, p1extra, replace = F)
      irrelpos <- (1:p)[-c(relpos[[1]], relpos[[2]], extra1)]
    }
    if (m[1] > compos) {
      if (compos == 0) {
        use <- relpos[[1]]
      } else {
        use <- relpos[[1]][-cpos1]
      }
      qpos[[1]] <- sort(c(extra1, use))
    } else {
      if (!is.null(extra1)) {
        qpos[[1]] <- sort(extra1)
      }
    }

    if (p2extra > 0) {
      extra2 <- sample(irrelpos, p2extra, replace = F)
      irrelpos <- (1:p)[-c(relpos[[1]], relpos[[2]],
                           extra1, extra2)]
    }
    if (m[2] > compos) {
      if (compos == 0) {
        use <- relpos[[2]]
      } else {
        use <- relpos[[2]][-cpos2]
      }
      qpos[[2]] <- sort(c(extra2, use))
    } else {
      if (!is.null(extra1)) {
        qpos[[2]] <- sort(extra2)
      }
    }
    if (compos > 0) {
      if (q[3] > compos) {
        extra3 <- sample(irrelpos, q[3] - compos, replace = F)
        qpos[[3]] <- unique(sort(c(relpos[[1]][cpos1],
                                   extra3)))
      } else {
        qpos[[3]] <- relpos[[1]][cpos1]
      }
    }
    names(qpos) <- c("Unique variables for Y1",
                     "Unique variables for Y2",
                     "Common variables for Y1 and Y2")

    lambdas <- exp(-gamma * (1:p))/exp(-gamma)

    # Construction of Sigma ----------------------------------------------------------------|
    SigmaZ <- diag(lambdas)  #Here all variables are made independent (diagonal matrix)

    SigmaZinv <- diag(1/lambdas)
    SigmaY <- matrix(c(1, rho[1], rho[1], 1), 2, 2)

    Sigmazy <- matrix(0, p, 2)
    if (compos == 0) {
      alpha1 <- runif(m[1], 0, 1) * sample(c(-1, 1), m[1],
                                           replace = TRUE) ## Why not runif(m[1], -1, 1)
      Sigmazy[relpos[[1]], 1] <- alpha1 <- sign(alpha1) *
        sqrt(R2[1] * abs(alpha1)/sum(abs(alpha1)) * lambdas[relpos[[1]]])
      alpha2 <- runif(m[2], 0, 1) * sample(c(-1, 1), m[2],
                                           replace = TRUE)
      Sigmazy[relpos[[2]], 2] <- alpha2 <- sign(alpha2) *
        sqrt(R2[2] * abs(alpha2)/sum(abs(alpha2)) * lambdas[relpos[[2]]])

      R12 <- sum(alpha1 * alpha2/lambdas[relpos[[1]]])
      rho[2] <- (rho[1]) / sqrt((1 - R2[1] ^ 2) * (1 - R2[2] ^ 2))
      if (rho[2] < -1 | rho[2] > 1)
        stop("Two responses in orthogonal, but highly relevant spaces must be less correlated.\n
        Choose a rho[1] closer to zero, or reduce the R2-values\n")
    }
    if (compos == m[1] | compos == m[2]) {
      if (any(m > compos)) {
        cpos <- list(cpos1, cpos2)
        id <- which(m == compos)
        id2 <- c(1:2)[-id]
        warning(paste("The number of relevant predictors for response ",
                      id, "is set to ", q[3], " since its relevant space is a subspace of the other\n"))
        qpos[id] <- vector("list", 1)
        cpos.1 <- cpos[[id]]
        cpos.2 <- cpos[[id2]]
        R12 <- rho[1] - rho[2] * sqrt((1 - R2[1]) * (1 -
                                                       R2[2]))
        alpha <- runif(compos, 0, 1) * sample(c(-1, 1),
                                              compos, replace = TRUE)
        alpha <- sign(alpha) * sqrt(R2[id] * abs(alpha)/sum(abs(alpha)) *
                                      lambdas[relpos[[id]]])

        alpha2sum <- 1
        j <- 1
        while (alpha2sum > R2[id2] & j < 1000) {
          alpha1 <- alpha
          alpha2star <- runif(compos - 1, 0, 1) * sample(c(-1,
                                                           1), compos - 1, replace = TRUE)
          alphasum <- sum(alpha1[-1] * alpha2star/lambdas[relpos[[id]][cpos.1[-1]]])
          a21 <- (R12 - alphasum) * lambdas[relpos[[id]][cpos.1[1]]]/alpha1[1]
          alpha2star <- c(a21, alpha2star)
          alpha2sum <- .Rfunc(alpha2star, alpha2star,
                              lambdas[relpos[[id]]])
          j <- j + 1
        }
        if (j == 1000)
          stop("No PD matrix found in 1000 simulations with current parameter setting")
        if ((m[id2] - compos) > 1) {
          alpha2rest <- runif(m[id2] - compos, 0, 1) *
            sample(c(-1, 1), m[id2] - compos, replace = TRUE)
          alpha2rest <- sign(alpha2rest) * sqrt((R2[id2] -
                                                   alpha2sum) * abs(alpha2rest)/sum(abs(alpha2rest)) *
                                                  lambdas[relpos[[id2]][-cpos.2]])
        } else {
          alpha2rest <- sample(c(-1, 1), 1, replace = FALSE) *
            sqrt((R2[id2] - alpha2sum) * lambdas[relpos[[id2]][-cpos.2]])
        }
        alpha2 <- rep(0, p)
        alpha2[relpos[[id2]][cpos.2]] <- alpha2star
        alpha2[relpos[[id2]][-cpos.2]] <- alpha2rest

        Sigmazy[relpos[[id]], id] <- alpha1
        Sigmazy[, id2] <- alpha2

      } else {
        if (compos == 1) {
          alpha1 <- sqrt(R2[1] * lambdas[relpos[[1]]])
          alpha2 <- sign(rho[1]) * sqrt(R2[2] * lambdas[relpos[[2]]])
          rho[2] <- (rho[1] - alpha1 * alpha2/lambdas[relpos[[1]]])/sqrt((1 -
                                                                            R2[1]) * (1 - R2[2]))
          warning(paste(
            "The conditional correlation between y1 and y2 is fixed equal to",
            round(rho[2], 2),
            "\ndue to overlapping and one-dimensional relevant spaces for y1 and y2\n"
          ))
          Sigmazy[relpos[[1]], 1] <- alpha1
          Sigmazy[relpos[[2]], 2] <- alpha2
        }
        if (compos == 2) {
          R12 <- rho[1] - rho[2] * sqrt((1 - R2[1]) *
                                          (1 - R2[2]))
          R2try <- 2
          tol <- 1
          j <- 1
          while (tol > 0.001 & (j < 5000)) {
            alpha1 <- runif(2, 0, 1) * sample(c(-1, 1),
                                              2, replace = TRUE)
            alpha1 <- sign(alpha1) * sqrt(R2[1] * abs(alpha1)/sum(abs(alpha1)) *
                                            lambdas[relpos[[1]]])
            options(warn = -1)
            a22 <- .a22func(alpha1[1], alpha1[2], R2[2],
                            R12, lambdas[relpos[[2]]][1], lambdas[relpos[[2]]][2])
            a22 <- a22[which(abs(a22) == min(abs(a22)))][1]
            if (is.na(a22))
              next
            options(warn = 0)

            a21 <- .a21func(alpha1[1], alpha1[2], a22,
                            R12, lambdas[relpos[[2]]][1], lambdas[relpos[[2]]][2])
            alpha2 <- c(a21, a22)
            R2try <- .Rfunc(alpha2, alpha2, lambdas[relpos[[2]]])
            tol <- abs(R2try - R2[2])
            j <- j + 1
          }
          if (j == 5000)
            stop("No PD covariance matrix found with current prameter setting\n ")

          Sigmazy[relpos[[1]], 1] <- alpha1
          Sigmazy[relpos[[2]], 2] <- alpha2
        }
        if (compos > 2) {
          R12 <- rho[1] - rho[2] * sqrt((1 - R2[1]) *
                                          (1 - R2[2]))

          R2try <- 2
          tol <- 1
          j <- 1
          while (tol > 0.001 & (j < 5000)) {

            # Sampling all but two positions
            R2rest.1 <- 1
            R2rest.2 <- 1
            R12rest <- 1
            k <- 1
            R1prop <- runif(1, 0.3, 0.8)
            R2prop <- runif(1, 0.3, 0.8)
            while ((R2rest.1 > (m[1] - 2)/m[1] * R2[1] |
                      R2rest.2 > (m[2] - 2)/m[2] * R2[2] |
                        rho[1] - R12rest < -1 |
                        rho[1] - R12rest > 1) &
                     (k < 1000)) {
                       alpha1cp <- runif(m[1] - 2, 0, 1) * sample(c(-1,
                                                                    1), m[1] - 2, replace = TRUE)
                       alpha1cp <- sign(alpha1cp) * sqrt(R1prop *
                                                           R2[1] * abs(alpha1cp)/sum(abs(alpha1cp)) *
                                                           lambdas[relpos[[1]][-c(1:2)]])
                       R2rest.1 <- sum(alpha1cp * alpha1cp/lambdas[relpos[[1]][-c(1:2)]])
                       alpha2cp <- runif(m[2] - 2, 0, 1) * sample(c(-1,
                                                                    1), m[2] - 2, replace = TRUE)
                       alpha2cp <- sign(alpha2cp) * sqrt(R2prop *
                                                           R2[2] * abs(alpha2cp)/sum(abs(alpha2cp)) *
                                                           lambdas[relpos[[2]][-c(1:2)]])
                       R2rest.2 <- sum(alpha2cp * alpha2cp/lambdas[relpos[[2]][-c(1:2)]])
                       R12rest <- sum(alpha1cp * alpha2cp/lambdas[relpos[[2]][-c(1:2)]])
                       k <- k + 1
                     }


            alpha1 <- runif(2, 0, 1) * sample(c(-1, 1),
                                              2, replace = TRUE)
            alpha1 <- sign(alpha1) * sqrt((R2[1] - R2rest.1) *
                                            abs(alpha1)/sum(abs(alpha1)) * lambdas[relpos[[1]]][1:2])
            options(warn = -1)
            a22 <- .a22func(alpha1[1], alpha1[2], (R2[2] -
                                                     R2rest.2), (R12 - R12rest), lambdas[relpos[[2]]][1],
                            lambdas[relpos[[2]]][2])
            a22 <- a22[which(abs(a22) == min(abs(a22)))][1]
            if (is.na(a22))
              next
            options(warn = 0)

            a21 <- .a21func(alpha1[1], alpha1[2], a22,
            (R12 - R12rest), lambdas[relpos[[2]]][1],
            lambdas[relpos[[2]]][2])
            alpha2 <- c(a21, a22, alpha2cp)
            alpha1 <- c(alpha1, alpha1cp)
            R2try <- .Rfunc(alpha2, alpha2, lambdas[relpos[[2]]])
            tol <- abs(R2try - R2[2])
            j <- j + 1
          }
          if (j == 5000)
            stop("No PD covariance matrix found with current prameter setting\n ")

          Sigmazy[relpos[[1]], 1] <- alpha1
          Sigmazy[relpos[[2]], 2] <- alpha2
        }
      }
    }
    if (compos != 0 & all(compos < m)) {
      j <- 1
      cp1sum <- cp2sum <- 1
      while ((cp1sum >= R2[1] | cp2sum >= R2[2]) & (j < 1000)) {
        # cp <- which(relpos[[1]]%in%relpos[[2]])
        cp <- relpos[[1]][cpos1]
        R12 <- rho[1] - rho[2] * sqrt((1 - R2[1]) * (1 -
                                                       R2[2]))
        alpha1cp <- sign(R12) * runif(compos, 0, 1)
        alpha2cp <- runif(compos, 0, 1)
        cp.sum <- sum(alpha1cp * alpha2cp/lambdas[cp])
        k <- R12/cp.sum

        Sigmazy[cp, 1] <- alpha1cp <- sqrt(k) * alpha1cp
        Sigmazy[cp, 2] <- alpha2cp <- sqrt(k) * alpha2cp
        cp1sum <- sum(alpha1cp * alpha1cp/lambdas[cp])
        cp2sum <- sum(alpha2cp * alpha2cp/lambdas[cp])
        j <- j + 1
      }
      if (j == 1000)
        stop("No PD covariance matrix found with current parameter setting\n")

      id <- which(relpos[[1]] %in% cp)
      ncp <- relpos[[1]][-id]
      alpha1 <- runif(length(ncp), 0, 1) * sample(c(-1,
                                                    1), length(ncp), replace = TRUE)
      Sigmazy[ncp, 1] <- alpha1 <- sign(alpha1) * sqrt((R2[1] -
                                                          cp1sum) * abs(alpha1)/sum(abs(alpha1)) * lambdas[ncp])

      id <- which(relpos[[2]] %in% cp)
      ncp <- relpos[[2]][-id]
      alpha2 <- runif(length(ncp), 0, 1) * sample(c(-1,
                                                    1), length(ncp), replace = TRUE)
      Sigmazy[ncp, 2] <- alpha2 <- sign(alpha2) * sqrt((R2[2] -
                                                          cp2sum) * abs(alpha2)/sum(abs(alpha2)) * lambdas[ncp])

    }


    # Putting it all together to a joint covariance matrix
    S1 <- rbind(SigmaY, Sigmazy)
    S2 <- rbind(t(Sigmazy), SigmaZ)
    Sigma <- cbind(S1, S2)

    # Finding a rotation matrix which rotates the latent
    # components to yield relevant X-predictors
    nq <- unlist(lapply(qpos, length))
    if (is.null(sim)) {
      R <- diag(p)
      if (nq[1] > 0) {
        if (nq[1] == 1) {
          Rq1 <- 1
        } else {
          Q <- matrix(rnorm(nq[1]^2), nq[1])
          Q <- scale(Q, scale = F)
          # qrobj <- .QR(Q) Rq1 <- qrobj$Q
          Rq1 <- qr.Q(qr(Q))
        }
        R[qpos[[1]], qpos[[1]]] <- Rq1
      }
      if (nq[2] > 0) {
        if (nq[2] == 1) {
          Rq2 <- 1
        } else {
          Q <- matrix(rnorm(nq[2]^2), nq[2])
          Q <- scale(Q, scale = F)
          # qrobj <- .QR(Q) Rq1 <- qrobj$Q
          Rq2 <- qr.Q(qr(Q))
        }
        R[qpos[[2]], qpos[[2]]] <- Rq2
      }
      if (compos > 0) {
        if (nq[3] == 1) {
          Rq3 <- 1
        } else {
          Q <- matrix(rnorm(nq[3]^2), nq[3])
          Q <- scale(Q, scale = F)
          # qrobj <- .QR(Q) Rq1 <- qrobj$Q
          Rq3 <- qr.Q(qr(Q))
        }
        R[qpos[[3]], qpos[[3]]] <- Rq3
      }
      qsum <- sum(nq)
      allqpos <- unique(unlist(qpos))
      if (qsum < (p - 1)) {
        Q <- matrix(rnorm((p - qsum)^2), (p - qsum))
        Q <- scale(Q, scale = F)
        # qrobj <- .QR(Q) Rq1 <- qrobj$Q
        Rnq <- qr.Q(qr(Q))
        R[(1:p)[-allqpos], (1:p)[-allqpos]] <- Rnq
      }
    } else {
      R <- sim$Rotation
    }

    # The true regression coefficients
    betaZ <- SigmaZinv %*% Sigmazy
    betaX <- R %*% betaZ
    beta0 <- c(0, 0)
    if (!(is.null(muY))) {
      beta0 <- beta0 + muY
    }
    if (!(is.null(muX))) {
      beta0 <- beta0 - t(betaX) %*% muX
    }
    # The (true) coefficient of determination, R ^ 2
    R2 <- t(Sigmazy) %*% betaZ
    # Minimum prediction error
    minerror <- SigmaY - R2
  }

  # Checking that Sigma is PD
  pd <- all(eigen(Sigma)$values > 0)

  # Simulating training and test data
  if (pd) {
    Sigmarot <- chol(Sigma)
    Ucal <- matrix(rnorm(n * (p + 2), 0, 1), nrow = n)
    U1cal <- Ucal %*% Sigmarot
    Y <- U1cal[, 1:2, drop = F]
    if (!(is.null(muY))) {
      Y <- sweep(Y, 2, muY, "+")
    }
    Z <- U1cal[, 3:(p + 2)]
    X <- Z %*% t(R)
    if (!(is.null(muX))) {
      X <- sweep(X, 2, muX, "+")
    }
    colnames(X) <- as.character(1:p)
    # Testdata
    if (!is.null(ntest)) {
      Utest <- matrix(rnorm(ntest * (p + 2), 0, 1), nrow = ntest)
      U1test <- Utest %*% Sigmarot
      testY <- U1test[, 1:2, drop = F]
      if (!(is.null(muY))) {
        testY <- sweep(testY, 2, muY, "+")
      }
      TESTZ <- U1test[, 3:(p + 2)]
      testX <- TESTZ %*% t(R)
      if (!(is.null(muX))) {
        testX <- sweep(testX, 2, muX, "+")
      }
      colnames(testX) <- as.character(1:p)
    } else {
      testX <- NULL
      testY <- NULL
    }

  } else {
    stop("Correlation matrix is not positive definit \n")
  }

  res          <- list()
  res$call     <- match.call()
  res$X        <- X
  res$Y        <- Y
  res$beta     <- betaX
  res$beta0    <- beta0
  res$muY      <- muY
  res$muX      <- muX
  res$relpred  <- qpos
  res$testX    <- testX
  res$testY    <- testY
  res$n        <- n
  res$p        <- p
  res$m        <- m
  res$q        <- q
  res$gamma    <- gamma
  res$lambda   <- lambdas
  res$R2       <- R2
  res$relpos   <- relpos
  res$minerror <- minerror
  res$Sigma    <- Sigma
  res$Rotation <- R
  res$type     <- "bivariate"
  class(res)   <- "simrel"
  return(res)
}
