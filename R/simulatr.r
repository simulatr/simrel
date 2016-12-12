#' Simulation of Multivariate Linear Model Data
#' @param n Number of observations
#' @param p Number of variables
#' @param q Vector containing the number of relevant predictor variables for each relevant response components
#' @param m Number of response variables
#' @param relpos A list of position of relevant component for predictor variables. The list contains vectors of position index, one vector or each relevant response components
#' @param gamma A declining (decaying) factor of eigen value of predictors (X). Higher the value of \code{gamma}, the decrease of eigenvalues will be steeper
#' @param R2 Vector of coefficient of determination (proportion of variation explained by predictor variable) for each relevant response components
#' @param ntest Number of test observation
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

simulatr <- function (n, p, q, relpos, gamma, R2, type = "univariate", ...){
  cl <- match.call(expand.dots = FALSE)
  cl$type <- match.arg(type, c("univariate", "bivariate", "multivariate"))
  sim_fun <- switch(type, univariate = simrel, bivariate = simrel2, multivariate = simrel_m)

  sobj <- sim_fun(n, p, q, relpos, gamma, R2, ...)
  return(sobj)
}
