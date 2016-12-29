#' Simulation Plot: The true beta, relevant component and eigen structure
#' @import ggplot2
#' @import data.table
#' @importFrom gridExtra grid.arrange
#' @keywords simrel-plot, simulation plot
#' @param obj A simrel object
#' @param ncomp Number of components to plot
#' @param ask logical, TRUE: functions ask for comfirmation FALSE: function layout plot on predefined format
#' @param print.cov Output estimated covariance structure
#' @param which A character indicating which plot you want as output, it can take \code{TrueBeta}, \code{RelComp} and \code{EstRelComp}
#' @return A list of plots

plot.simrel <- function(obj, ncomp = min(obj$p, obj$n, 20), ask = TRUE, 
                         print.cov = FALSE, which = c("TrueBeta", "RelComp", "EstRelComp")) {
  plt <- list()
  if (length(which) > 1) which <- NULL
  
  ## Plot 1: True Coefficients Plot
  if (which == "TrueBeta" || is.null(which)) {
    beta <- melt(obj$beta, varnames = c("variable", "response"), value.name = "coefficient")

    ## The Plot
    beta$response <- factor(beta$response)
    plt$TrueBeta <- ggplot(beta, aes_string('variable', 'coefficient',
                                            fill = 'response',
                                            group = 'response')) +
      geom_hline(yintercept = 0, size = 0.25) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "variables", y = expression(paste("Beta(", beta, ") Coefficients"))) +
      ggtitle("True Regression Coefficients") +
      theme_bw() +
      scale_fill_brewer("Response", palette = "Set2")
  }
  
  ## Plot 2: Relevant Comoponents Plot
  if (which == "RelComp" || is.null(which)) {
    idx <- if (obj$type == "univariate") 1 else obj$m
    covs <- obj$Sigma[-c(1:idx), 1:idx]
    covs.sc <- apply(covs, 2, function(x) {
      out <- abs(x)/max(abs(x))
      out[is.nan(out)] <- 0
      return(out)
    })
    covs.dt <- as.data.frame(cbind(1:ncomp, 
                                   obj$lambda[1:ncomp], 
                                   covs.sc[1:ncomp, ]), 1)
    names(covs.dt) <- c("Vars", "lambda", paste0("ResponseY", 1:obj$m))
    covs.dt <- melt(covs.dt, 1:2)
    
    ## The Plot
    plt$RelComp <- ggplot(covs.dt, aes_string('Vars', 'value', group = 'variable')) +
      geom_bar(aes_string(y = 'lambda'), position = "identity", 
               stat = "identity", fill = "lightgray") +
      geom_point(shape = 21, size = 3, alpha = 0.75, aes_string(fill = 'variable'),
                 position = "jitter") +
      labs(x = "Components", y = "Eigenvalues") +
      ggtitle("Relevant Components Plot") +
      theme_bw() +
      theme(legend.title = element_blank(),
            legend.position = c(1, 1),
            legend.justification = c(1, 1))
  }
  
  
  ## Plot 3: Estimated Relevant Component Plot
  if (which == "EstRelComp" || is.null(which)) {
    X <- scale(obj$X, center = TRUE, scale = FALSE)
    Y <- scale(obj$Y, center = TRUE, scale = FALSE)
    
    svdres <- svd(X)
    eigval <- (svdres$d ^ 2)/(obj$n - 1)  #/(svdres$d ^ 2)[1]
    eigval.sc <- eigval/eigval[1]
    
    Z <- X %*% svdres$v
    covs <- t(abs(cov(Y, Z)))
    covs.sc <- apply(covs, 2, function(x) abs(x)/max(abs(x)))
    
    covs.dt <- as.data.frame(cbind(1:ncomp, 
                                   eigval.sc[1:ncomp], 
                                   covs.sc[1:ncomp, ]), 1)
    names(covs.dt) <- c("Vars", "lambda", paste0("ResponseY", 1:obj$m))
    covs.dt <- melt(covs.dt, 1:2)
    
    ## The Plot
    plt$EstRelComp <- ggplot(covs.dt, aes_string('Vars', 'value', group = 'variable')) +
      geom_bar(aes_string(y = 'lambda'), position = "identity", 
               stat = "identity", fill = "lightgray") +
      geom_point(shape = 21, size = 1, alpha = 0.75, aes_string(fill = 'variable')) +
      geom_line(aes_string(color = 'variable')) +
      labs(x = "Components", y = "Eigenvalues") +
      ggtitle("Estimated Relevant Components Plot") +
      theme_bw() +
      theme(legend.title = element_blank(),
            legend.position = c(1, 1),
            legend.justification = c(1, 1))
  }
  
  
  ## Setting-up Layout
  if (!ask & is.null(which)) {
    plt$layout_matrix <- cbind(c(1,2), c(1,3))
    do.call(grid.arrange, plt)
  }
  
  ## Covariance Structure of Y given X
  if (print.cov) {
    covs <- covs[1, 1:min(ncomp, obj$p)]
    cat("Absolute values of estimated covariances\n")
    names(covs) <- paste("Component", 1:min(ncomp, obj$p), 
                         sep = "")
    print(abs(round(covs, 3)))
  }
  
  ## Setting up return
  if (print.cov) {
    return(plt$covariances <- covs)
  } else {
    if (length(plt) == 1) return(plt[[1]])
    return(plt)
  }
}
