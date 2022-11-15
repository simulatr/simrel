#' Simulation of Multivariate Linear Model Data
#' @param n Number of observations.
#' @param p Number of variables.
#' @param q Number of predictors related to each relevant components
#'    An integer for univariate, a vector of 3 integers for bivariate and 3 or
#'    more for multivariate simulation (for details see Notes).
#' @param relpos A list (vector in case of univariate simulation) of position
#'    of relevant component for predictor variables corresponding
#'    to each response.
#' @param gamma A declining (decaying) factor of eigenvalues of predictors (X).
#'    Higher the value of \code{gamma}, the decrease of eigenvalues
#'    will be steeper.
#' @param R2 Vector of coefficient of determination (proportion of variation
#'    explained by predictor variable) for each relevant response components.
#' @param type Type of simulation - \code{univariate}, \code{bivariate} and
#'    \code{multivariate}
#' @param ... Since this is a wrapper function to simulate univariate,
#'    bivariate or multivariate, it calls their respective function.
#'    This parameter should contain all the necessary arguements for respective
#'    simulations. See \code{\link{unisimrel}}, \code{\link{bisimrel}} and
#'    \code{\link{multisimrel}}
#' @return A simrel object with all the input arguments along with 
#'     following additional items. For more detail on the return values see the
#'     individual simulation functions \code{\link{unisimrel}}, 
#'     \code{\link{bisimrel}} and \code{\link{multisimrel}}.
#' 
#' \strong{Common returns from univariate, bivariate and multivariate simulation:}
#' 
#' \item{call}{the matched call}
#' \item{X}{simulated predictors}
#' \item{Y}{simulated responses}
#' \item{beta}{true regression coefficients}
#' \item{beta0}{true regression intercept}
#' \item{relpred}{position of relevant predictors}
#' \item{n}{number of observations}
#' \item{p}{number of predictors (as supplied in the arguments)}
#' \item{p}{number of responses (as supplied in the arguments)}
#' \item{q}{number of relevant predictors (as supplied in the arguments)}
#' \item{gamma}{declining factor of eigenvalues of predictors 
#'     (as supplied in the arguments)}
#' \item{lambda}{eigenvalues corresponding to the predictors}
#' \item{R2}{theoretical R-squared value (as supplied in the arguments)}
#' \item{relpos}{position of relevant components (as supplied in the arguments)}
#' \item{minerror}{minimum model error}
#' \item{Sigma}{variance-Covariance matrix of response and predictors}
#' \item{testX}{simulated test predictor (in univarite simulation \code{TESTX})}
#' \item{testY}{simulated test response (in univarite simulation \code{TESTY})}
#' \item{Rotation}{Random rotation matrix used to rotate latent components. Is 
#'     equivalent to the transpose of eigenvector-matrix. In multivariate
#'     simulation, \code{Xrotation} (R) and \code{Yrotation} (Q) refers to this matrix
#'     corresponding to the predictor and response.}
#' \item{type}{type of simrel object \code{univariate}, \code{bivariate} or 
#'     \emph{multivariate}}
#' 
#' \strong{Returns from multivariate simulation:}
#' 
#' \item{eta}{a declining factor of eigenvalues of response (Y)
#'     (as supplied in the arguments)}
#' \item{ntest}{number of simulated test observations}
#' \item{W}{simulated response components}
#' \item{Z}{simulated predictor components}
#' \item{testW}{test predictor components}
#' \item{testZ}{test response components}
#' \item{SigmaWZ}{Variance-Covariance matrix of components of response and predictors}
#' \item{SigmaWX}{Covariance matrix of response components and predictors}
#' \item{SigmaYZ}{Covariance matrix of response and predictor components}
#' \item{RsqW}{Coefficient of determination corresponding to response components}
#' \item{RsqY}{Coefficient of determination corresponding to response variables}
#' 
#' @concept simulation 
#' @concept linear model data
#' @note The parameter \code{q} represents the number of predictor variables 
#'    that forms a basis for each of the relevant components. For example,
#'    for \code{q = 8} and relevant components 1, 2, and 3 specified by 
#'    parameter \code{relpos} then the randomly selected 8 predictor variables 
#'    forms basis for these three relevant components and thus in the model 
#'    these 8 predictors will be relevant for the response (outcome).
#' @keywords datagen
#' @references Sæbø, S., Almøy, T., & Helland, I. S. (2015). simrel—A versatile tool for linear model data simulation based on the concept of a relevant subspace and relevant predictors. Chemometrics and Intelligent Laboratory Systems, 146, 128-135.
#' @references Almøy, T. (1996). A simulation study on comparison of prediction methods when only a few components are relevant. Computational statistics & data analysis, 21(1), 87-107.
#' @export

simrel <- function (n, p, q, relpos, gamma, R2, type = "univariate", ...) {
  cl <- match.call(expand.dots = FALSE)
  cl$type <- match.arg(type, c("univariate", "bivariate", "multivariate"))

  sim <- cl$sim
  muX <- cl$muX
  muY <- cl$muY

  ## ---- Validation of parameters --------------------------
  stopMsg <- c()
  switch(type,
         univariate = {
           if (length(q) != 1 & !is.numeric(q))
             stopMsg <-
               append(stopMsg, "Relevant number of variables in univariate simulation should be specified by an integer.")
           if (!is.vector(relpos) & !is.numeric(relpos))
             stopMsg <-
               append(stopMsg, "Relevant position of components in univariate simulation should be specified by a vector of integer.")
           if (length(R2) != 1 & !is.numeric(R2) & R2 < 0 & R2 > 1)
             stopMsg <-
               append(stopMsg, "Coefficient of variation in univariate simulation must be specified by a number between 0 and 1.")
           if (!is.null(sim)) {
             if (sim$type != "univariate") stopMsg <- append(stopMsg, "Not univariate simrel object.")
           }
           if (!is.null(muY))
             if (!is.numeric(muY) & length(muY) != 1) stopMsg <- append(stopMsg, "Univariate simulation can have only one response and a mean for it.")
         },
         bivariate = {
           if (length(q) != 3 & !is.numeric(q))
             stopMsg <-
               append(stopMsg, "Relevant number of variables in bivariate simulation should be specified by a vector of 3 integers.")
           if (!is.list(relpos) & length(relpos) != 2 & !all(sapply(relpos, is.numeric)))
             stopMsg <-
               append(stopMsg, "Relevant position of components in bivariate simulation should be a list with vectors of integer.")
           if (length(R2) != 2 & !is.numeric(R2))
             stopMsg <-
               append(stopMsg, "Coefficient of variation in bivariate simulation must be a vector of two numbers between 0 and 1.")
           if (!is.null(sim)) {
             if (sim$type != "bivariate") stopMsg <- append(stopMsg, "Not bivariate simrel object.")
           }
           if (!is.null(muY))
             if (!is.numeric(muY) & length(muY) != 2)
               stopMsg <- append(stopMsg, "Bivariate simulation can have only two response and a mean for it.")
         },
         multivariate = {
           if (!is.numeric(q))
             stopMsg <-
               append(stopMsg, "Relevant number of variables in multivariate simulation should be a vector of integers.")
           if (!is.list(relpos) & !all(sapply(relpos, is.numeric)))
             stopMsg <-
               append(stopMsg, "Relevant position of components in multivariate simulation should be a list with vectors of integer.")
           if (!is.numeric(R2))
             stopMsg <-
               append(stopMsg, "Coefficient of variation in multivariate simulation must be a vector of numbers between 0 and 1.")
           if (!is.null(sim)) {
             if (sim$type != "multivariate") stopMsg <- append(stopMsg, "Not multivariate simrel object.")
           }
           if (!is.null(muY))
             if (!is.numeric(muY))
               stopMsg <- append(stopMsg, "Mean vector must be numeric")
         })

  if (length(gamma) != 1 & !is.numeric(gamma) & gamma < 0 & gamma > 1)
    stopMsg <-
      append(stopMsg, "Decaying factor of eigenvalues in univariate simulation must be specified by a number between 0 and 1.")
  if (!is.null(sim))
    if (methods::is(sim, "simrel")) stopMsg <- append(stopMsg, "Unknown simulation object.")
  if (!is.null(muX))
    if (length(muX) != p) stopMsg <- append(stopMsg, "Please input mean vector of correct dimension.")

  if (length(stopMsg) >= 1) stop(stopMsg)

  sim_fun <- switch(type, univariate = unisimrel, bivariate = bisimrel, multivariate = multisimrel)
  sobj <- sim_fun(n, p, q, relpos, gamma, R2, ...)
  sobj$call <- match.call()

  return(sobj)
}
