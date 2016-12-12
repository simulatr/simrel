#' Function for data simulation
#' 
#' Functions for data simulation from a random regression model with one
#' response variable where the data properties can be controlled by a few input
#' parameters.  The data simulation is based on the concept of relevant latent
#' components and relevant predictors, and was developed for the purpose of
#' testing methods for variable selection for prediction.
#' 
#' The data are simulated according to a multivariate normal model for the
#' vector \eqn{(y, z_1, z_2, z_3, ..., z_p)^t} where \eqn{y} is the response
#' variable and \eqn{z = (z_1,..., z_p)^t} is the vector of latent (principal)
#' components. The ordered principal components are uncorrelated variables with
#' declining variances (eigenvalues) defined for component \eqn{j} as
#' \eqn{e^{-\gamma * j}/e^{-\gamma}}. Hence, the variance (eigenvalue) of the
#' first principal component is equal to 1, and a large value of \eqn{\gamma}
#' gives a rapid decline in the variances. The variance of the response
#' variable is by default fixed equal to 1.
#' 
#' Some of the principal components (ordered by their decreasing variances) are
#' assumed to be relevant for the prediction of the response. The indices of
#' the positions of the relevant components are set by the \code{relpos}
#' argument. The joint degree of relevance for the relevant components is
#' determined by the population R-squared defined by \code{R2}.
#' 
#' In order to obtain predictor variables \eqn{x = (x_1, x_2, ..., x_p)^t} for
#' \eqn{y}, a random rotation of the principal components is performed. Hence,
#' \eqn{x = R^t*z} for some random rotation matrix \eqn{R}. For values of
#' \eqn{q} satisfying \eqn{m <= q <p} only a subspace of dimension \eqn{q}
#' containing the \eqn{m} relevant component(s) is rotated. This facilitates
#' the possibility to generate \eqn{q} relevant predictor variables
#' (\eqn{x}'s). The indices of the relevant predictors is randomly selected
#' with the only restriction that the index set contains the indices in
#' \code{relpos}. The final index set of the relevant predictors is saved in
#' the output argument \code{relpred}. If \code{q=p} all \eqn{p} predictor
#' variables are relevant for the prediction of \eqn{y}.
#' 
#' For further details on the simulation approach, please see S<e6>b<f8>,
#' Alm<f8>y and Helland (2015).
#' 
#' @param n The number of (training) samples to generate.
#' @param p The total number of predictor variables to generate.
#' @param q The number of relevant predictor variables (as a subset of \eqn{p}).
#' @param relpos A vector indicating the position (between 1 and \eqn{p}) of the \eqn{m} relevant components, e.g. \eqn{c(1,2)} means that the first two latent components should be relevant. The length of relpos must be equal to \eqn{m}.
#' @param gamma A number defining the speed of decline in eigenvalues (variances) of the latent components. The eigenvalues are assumed to decline according to an exponential model. The first eigenvalue is set equal to 1.
#' @param R2 The theoretical R-squared according to the true linear model. A number between 0 and 1.
#' @param ntest The number of test samples to be generated (optional).
#' @param muY The true mean of the response variable (optional). Default is muY=NULL.
#' @param muX The \code{p}-vector of true means of the predictor variables (optional). Default is muX=NULL.
#' @param lambda.min Lower bound of the eigenvalues. Defaults to .Machine$double.eps.
#' @param sim A fitted simrel object. If this is given, the same regression coefficients will be used to simulate a new data set of requested size. Default is NULL, for which new regression coefficients are sampled.
#' @return A simrel object with list of following items,
#'     \item{call }{The call to simrel.} 
#'     \item{X }{The (n x p) simulated predictor matrix.} 
#'     \item{Y }{The n-vector of simulated response values.}
#'     \item{beta }{The vector of true regression coefficients.} 
#'     \item{beta0 }{The true intercept. This is zero if muY=NULL and muX=NULL} 
#'     \item{muY}{The true mean of the response variable.} 
#'     \item{muX}{The \code{p}-vector of true means of the predictor variables.} 
#'     \item{relpred }{The index of the true relevant predictors, that is the x-variables with non-zero true regression coefficients.} 
#'     \item{TESTX }{The (ntest x p) matrix of optional test samples.} 
#'     \item{TESTY }{The ntest-vector of responses of the optional test samples.} 
#'     \item{n }{The number of simulated samples.} 
#'     \item{p }{The number of predictor variables.} 
#'     \item{m }{The number of relevant components.}
#'     \item{q }{The number of relevant predictors.} 
#'     \item{gamma }{The decline parameter in the exponential model for the true eigenvalues.} 
#'     \item{lambda}{The true eigenvalues of the covariance matrix of the p predictor variables.} 
#'     \item{R2 }{The true R-squared value of the linear model.}
#'     \item{relpos }{The positions of the relevant components.} 
#'     \item{minerror}{The minimum achievable prediction error. Also the variance of the noise term in the linear model.} 
#'     \item{r }{The sampled correlations between the principal components and the response.} 
#'     \item{Sigma }{The true covariance matrix of \eqn{(y,z_1, z_2, ..., z_p)^t}.} 
#'     \item{Rotation }{The random rotation matrix which is used to achieve the predictor variables as rotations of the latent components. Equals the transposed of the eigenvector-matrix of the covariance matrix of \eqn{(x_1,...,x_p)^t}.}
#' 
#'     \item{type}{The type of response generated, either "univariate" as returned from \code{simrel}, or "bivariate" as returned from \code{simrel2}.}
#' @author Solve S<e6>b<f8> and Kristian H. Liland
#' @references Helland, I. S. and Alm<f8>y, T., 1994, Comparison of prediction methods when only a few components are relevant, \emph{J. Amer. Statist. Ass.}, \bold{89}(426), 583 -- 591.
#' @references S<e6>b<f8>, S., Alm<f8>y, T. and Helland, I. S., 2015, simrel - A versatile tool for linear model data simulation based on the concept of a relevant subspace and relevant predictors, \emph{Chemometr. Intell. Lab.}(in press),doi:10.1016/j.chemolab.2015.05.012.
#' @keywords Simulation Model
#' @examples
#' 
#' #Linear model data, large n, small p
#' mydata <- simrel(n=250, p=20, q=5, relpos=c(2,4), gamma=0.25, R2=0.75 )
#' 
#' #Estimating model parameters using ordinary least squares
#' lmfit <- lm(mydata$Y ~ mydata$X)
#' summary(lmfit)
#' 
#' #Comparing true with estimated regression coefficients
#' plot(mydata$beta, lmfit$coef[-1],xlab="True regression coefficients", 
#'   ylab="Estimated regression coefficients")
#' abline(0,1)
#' 
#' #Linear model data, small n, large p
#' mydata <- simrel(n=50, p=200, q=25, relpos=c(2,4), gamma=0.25, R2=0.8 )
#' 
#' #Simulating more samples with identical distribution as previous simulation
#' mydata2 <- simrel(n=2500, sim=mydata)
#'
#' \dontrun{
#' #Estimating model parameters using partial least squares regression with
#' #cross-validation to determine the number of relevant components.
#' require(pls)
#' plsfit <- plsr(mydata$Y ~ mydata$X, 15, validation="CV")
#' 
#' #Validation plot and finding the number of relevant components.
#' plot(0:15, c(plsfit$validation$PRESS0,plsfit$validation$PRESS), 
#'   type="b", xlab="Components", ylab="PRESS")
#' mincomp <- which(plsfit$validation$PRESS==min(plsfit$validation$PRESS))
#' 
#' #Comparing true with estimated regression coefficients
#' plot(mydata$beta, plsfit$coef[,1,mincomp],xlab="True regression coefficients", 
#'   ylab="Estimated regression coefficients")
#' abline(0,1)
#' }
#' @export

simrel <- function (n, p, q, relpos, gamma, R2, ntest = NULL, muY = NULL, 
                    muX = NULL, lambda.min =.Machine$double.eps, sim = NULL){
  if (!is.null(sim)) {
    betaX <- sim$beta
    beta0 <- sim$beta0
    muY <- sim$muY
    muX <- sim$muX
    qpos <- sim$relpred
    p <- sim$p
    q <- sim$q
    gamma <- sim$gamma
    lambdas <- sim$lambda
    R2 <- sim$R2
    relpos <- sim$relpos
    minerror <- sim$minerror
    Sigma <- sim$Sigma
    R <- sim$Rotation
    warning(paste("All parameters are collected from the supplied 'sim' object. \n"))
  }
  m <- length(relpos)
  if (q < m) 
    stop(paste("the number of relevant predictors must at least be equal to", 
               m, "\n"))
  if (is.null(sim)) {
    irrelpos <- (1:p)[-relpos]
    extradim <- q - m
    qpos <- sort(c(relpos, sample(irrelpos, extradim, replace = F)))
    nu <- lambda.min*exp(-gamma)/(1-lambda.min)
    if(lambda.min<0 || lambda.min>=1){stop("Parameter lambda.min must be in the interval [0,1]\n")}
    lambdas <- (exp(-gamma*(1:p))+nu)/(exp(-gamma)+nu)    
    SigmaZ <- diag(lambdas)
    SigmaZinv <- diag(1/lambdas)
    Sigmazy <- matrix(0, p, 1)
    r <- runif(m, 0, 1) * sample(c(-1, 1), m, replace = TRUE)
    Sigmazy[relpos, ] <- sign(r) * sqrt(R2 * abs(r)/sum(abs(r)) * 
                                          lambdas[relpos])
    SigmaY <- 1
    Sigma <- rbind(c(SigmaY, t(Sigmazy)), cbind(Sigmazy, 
                                                SigmaZ))
    Q <- matrix(rnorm(q^2), q)
    Q <- scale(Q, scale = F)
    Rq <- qr.Q(qr(Q))
    R <- diag(p)
    R[qpos, qpos] <- Rq
    if (q < (p - 1)) {
      Q <- matrix(rnorm((p - q)^2), (p - q))
      Q <- scale(Q, scale = F)
      Rnq <- qr.Q(qr(Q))
      R[(1:p)[-qpos], (1:p)[-qpos]] <- Rnq
    }
    betaZ <- SigmaZinv %*% Sigmazy
    betaX <- R %*% betaZ
    beta0 <- 0
    if (!(is.null(muY))) {
      beta0 <- beta0 + muY
    }
    if (!(is.null(muX))) {
      beta0 <- beta0 - t(betaX) %*% muX
    }
    R2 <- t(Sigmazy) %*% betaZ
    minerror <- SigmaY - R2
  }
  pd <- all(eigen(Sigma)$values > 0)
  if (pd) {
    Sigmarot <- chol(Sigma)
    Ucal <- matrix(rnorm(n * (p + 1), 0, 1), nrow = n)
    U1cal <- Ucal %*% Sigmarot
    Y <- U1cal[, 1, drop = F]
    if (!(is.null(muY))) {
      Y <- Y + rep(muY, n)
    }
    Z <- U1cal[, 2:(p + 1)]
    X <- Z %*% t(R)
    if (!(is.null(muX))) {
      X <- sweep(X, 2, muX, "+")
    }
    colnames(X) <- as.character(1:p)
    if (!is.null(ntest)) {
      Utest <- matrix(rnorm(ntest * (p + 1), 0, 1), nrow = ntest)
      U1test <- Utest %*% Sigmarot
      TESTY <- U1test[, 1, drop = F]
      if (!(is.null(muY))) 
        TESTY <- TESTY + rep(muY, ntest)
      TESTZ <- U1test[, 2:(p + 1)]
      TESTX <- TESTZ %*% t(R)
      if (!(is.null(muX))) {
        TESTX <- sweep(TESTX, 2, muX, "+")
      }
      colnames(TESTX) <- as.character(1:p)
    }
    else {
      TESTX <- NULL
      TESTY <- NULL
    }
  }
  else {
    stop("Correlation matrix is not positive definit \n")
  }
  res <- list()
  res$call <- match.call()
  res$X <- X
  res$Y <- Y
  res$beta <- betaX
  res$beta0 <- beta0
  res$muY <- muY
  res$muX <- muX
  res$relpred <- qpos
  res$TESTX <- TESTX
  res$TESTY <- TESTY
  res$n <- n
  res$p <- p
  res$m <- m
  res$q <- q
  res$gamma <- gamma
  res$lambda <- lambdas
  res$R2 <- drop(R2)
  res$relpos <- relpos
  res$minerror <- minerror
  res$Sigma <- Sigma
  res$Rotation <- R
  res$type = "univariate"
  class(res) <- "simrel"
  return(res)
}
