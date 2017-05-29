#' Simulation Plot: The true beta, relevant component and eigen structure
#' @keywords simrel-plot, simulation plot
#' @importFrom grDevices dev.flush dev.hold dev.new devAskNewPage palette
#' @importFrom graphics layout legend matplot par title
#' @param obj A simrel object
#' @param ncomp Number of components to plot
#' @param ask logical, TRUE: functions ask for comfirmation FALSE: function layout plot on predefined format
#' @param print.cov Output estimated covariance structure
#' @param which A character indicating which plot you want as output, it can take \code{TrueBeta}, \code{RelComp} and \code{EstRelComp}
#' @return A list of plots
#' @export

plot_simulatr <-
  function(obj, ncomp = min(obj$p, obj$n, 20), ask = TRUE,
           print.cov = FALSE, which = 1L:3L)
{
  show <- rep(FALSE, 3L)
  show[which] <- TRUE
  nx = obj$p
  ny = ncol(obj$Y)
  if (sum(which) == 1) ask <- TRUE
  
  if (ask & !sum(show) == 1) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  } else if (!ask | !sum(show) == 1) {
    dev.new(width = 11, height = 8)
    layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
    op <- par(mar = c(5, 5, 4, 1))
    on.exit({
      par(op)
      dev.flush()
    })
  }

  ## Plot1: True Coefficients Plot
  if (show[1L]) {
    beta <- `dimnames<-`(obj$beta, list(c(paste0("X", 1:obj$p)), c(paste0("Y", 1:ny))))
    dev.hold()
    matplot(beta, type = "b", lty = 1, lwd = 2, pch = 16, col = palette(),
            xlab = "", ylab = "")
    title(main = "True Regression Coefficients",
          xlab = "Variable Number",
          ylab = expression(beta))
    if (ny > 1) {
      legend("topright", legend = paste0("Y", 1:ny), col = palette(),
             lty = 1, lwd = 2, pch = 16, horiz = TRUE)
    }
    dev.flush()
  }

  ## Plot 2: Relevant Comoponents Plot
  if (show[2L]) {
    idx <- if (obj$type == "univariate") 1 else ny
    covs <- obj$Sigma[-c(1:idx), 1:idx, drop = FALSE]
    covs.sc <- apply(covs, 2, function(x) {
      out <- abs(x)/max(abs(x))
      out[is.nan(out)] <- 0
      return(out)
    })
    covs.dt <- as.data.frame(cbind(1:ncomp, 
                                   obj$lambda[1:ncomp], 
                                   covs.sc[1:ncomp, ]), 1)
    names(covs.dt) <- c("Vars", "lambda", paste0("ResponseY", 1:ny))

    dev.hold()
    with(covs.dt, plot(Vars, lambda, type = "h", lwd = 15, col = "lightgrey",
                       xlab = "", ylab = ""))
    matplot(covs.dt[,1], sapply(covs.dt[, -c(1:2)], jitter, 1),
            type = "b", pch = 16, col = palette(), add = TRUE,
            lty = 1, lwd = 2)
    if (ny > 1) {
    legend("topright", lty = 1, lwd = 2, pch = 16, col = palette(),
           legend = paste0("Y", 1:ny), horiz = TRUE)
    }
    title(main = "Relevant Components Plot", 
          xlab = "Components", 
          ylab = "Eigenvalue")
    dev.flush()
  }
  
  ## Plot 3: Estimated Relevant Component Plot
    if (show[3L]) {
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
      names(covs.dt) <- c("Vars", "lambda", paste0("ResponseY", 1:ny))
      
      dev.hold()
      with(covs.dt, plot(Vars, lambda, type = "h", lwd = 15, col = "lightgrey",
                         xlab = "", ylab = ""))
      matplot(covs.dt[,1], sapply(covs.dt[, -c(1:2)], jitter, 1),
              type = "b", pch = 16, col = palette(), add = TRUE,
              lty = 1, lwd = 2)
      if (ny > 1) {
      legend("topright", lty = 1, lwd = 2, pch = 16, col = palette(),
             legend = paste0("Y", 1:ny), horiz = TRUE)
      }
      title(main = "Estimated relevant components plot",
            xlab = "Components", 
            ylab = "Covariance (absolute value)\nEigenvalue")
      dev.flush()
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
  }
}
