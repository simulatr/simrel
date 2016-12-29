#' Simulation of Multivariate Linear Model Data
#' @param n Number of observations.
#' @param p Number of variables.
#' @param q An integer for univariate, a vector of 3 integers for bivariate and 3 or more for multivariate simulation (for details see Notes).
#' @param relpos A list (vector in case of univariate simulation) of position of relevant component for predictor variables corresponding to each response. 
#' @param gamma A declining (decaying) factor of eigenvalues of predictors (X). Higher the value of \code{gamma}, the decrease of eigenvalues will be steeper.
#' @param R2 Vector of coefficient of determination (proportion of variation explained by predictor variable) for each relevant response components.
#' @param type Type of simulation - \code{univariate}, \code{bivariate} and \code{multivariate}
#' @param ... Since this is a wrapper function to simulate univariate, bivariate or multivariate, it calls their respective function. This parameter should contain all the necessary arguements for respective simulations. See \code{\link{simrel}}, \code{\link{simrel2}} and \code{\link{simrel_m}}
#' @return A simrel object with all the input arguments along with following additional items.
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

simulatr <- function (n, p, q, relpos, gamma, R2, type = "univariate", ...)
{
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
    if (class(sim) == "simrel") stopMsg <- append(stopMsg, "Unknown simulation object.")
  if (!is.null(muX))
    if (length(muX) != p) stopMsg <- append(stopMsg, "Please input mean vector of correct dimension.")

  if (length(stopMsg) >= 1) stop(stopMsg)

  sim_fun <- switch(type, univariate = simrel, bivariate = simrel2, multivariate = simrel_m)

  sobj <- sim_fun(n, p, q, relpos, gamma, R2, ...)
  return(sobj)}
