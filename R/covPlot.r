#' Plotting Covariance Matrix
#' @param sobj A simrel object
#' @param type Type of covariance matrix - can take two values \code{relpos} for relevant position of principal components  and \code{relpred} for relevant position of predictor variables
#' @param ordering TRUE for ordering the covariance for block diagonal display
#' @param facetting TRUE for facetting the predictor and response space. FALSE will give a single facet plot
#' @return A covariance plot
#' @keywords simulation, linear model, linear model data, covariance plot
#' @references Sæbø, S., Almøy, T., & Helland, I. S. (2015). simrel—A versatile tool for linear model data simulation based on the concept of a relevant subspace and relevant predictors. Chemometrics and Intelligent Laboratory Systems, 146, 128-135.
#' @references Almøy, T. (1996). A simulation study on comparison of prediction methods when only a few components are relevant. Computational statistics & data analysis, 21(1), 87-107.
#' @examples
#' sobj <- simrel(n = 100, p = 10, q = c(4, 5), relpos = list(c(1, 2, 3), c(4, 6, 7)),
#'                R2 = c(0.8, 0.7), ypos = list(c(1, 3), 2), gamma = 0.7, type = "multivariate")
#' cov_plot(sobj, type = "relpred")
#' cov_plot(sobj, type = "relpos", facetting = FALSE)
#' @export

cov_plot <- function(sobj, type = "relpos", ordering = TRUE, facetting = TRUE) {
  m <- switch(sobj$type, multivariate = sobj$m, bivariate = 2, univariate = 1)
  
  p <- sobj$p
  nvar <- p + m
  xvar <- ifelse(type == "relpred", "X", "Z")
  yvar <- ifelse(sobj$type == "multivariate", ifelse(type == "relpred", "Y", "W"), "Y")
  axlbl <- c(paste0(yvar, 1:m), paste0(xvar, 1:p))
  lst <- unname(unlist(switch(type, relpos = sobj$relpos, relpred = sobj$relpred)))
  idx <- unique(c(lst, setdiff(1:p, lst)))
  
  if (sobj$type == "multivariate") {
    mat <- switch(type, relpos = sobj$SigmaWZ, relpred = sobj$Sigma)
    idx <- c(unlist(sobj$ypos), idx + m)
  } else {
    idx <- c(1:m, idx + m)
    if (type != "relpos") {
      rot <- Reduce(rbind, rep(0, m), 
                    Reduce(cbind, rep(0, m), sobj$Rotation, right = TRUE), 
                    right = TRUE)
      diag(rot)[1:m] <- 1
      sgma <- sobj$Sigma
      mat <- rot %*% sgma %*% t(rot)
    } else {
      mat <- sobj$Sigma
    }
  }
  genmat <- (mat[1:m, -c(1:m), drop = FALSE] != 0)
  if (sobj$type == "multivariate") {
    ypos <- vector("character", length = m)
    names(ypos) <- paste0("W", seq.int(m))
    for (x in sobj$ypos) ypos[x] <- paste0("W", x[1])
  }
  for (row in 1:nrow(genmat)) {
    for (col in 1:ncol(genmat)) {
      if (genmat[row, col]) genmat[row, col] <- paste0("W", row)
      else genmat[row, col] <- NA
      if (sobj$type == "multivariate") {
        genmat[row, col] <- ypos[genmat[row, col]]
      }
    }
  }
  sxx <- genmat[apply(genmat, 2, function(col) match(TRUE, !is.na(col))), ]
  syy <- genmat[, apply(genmat, 1, function(row) match(TRUE, !is.na(row)))]
  colmat <- rbind(cbind(syy, genmat), cbind(t(genmat), sxx))
  coldf <- cbind(expand.grid(v1 = axlbl, v2 = axlbl), col = c(colmat))
  covdf <- cbind(expand.grid(v1 = axlbl, v2 = axlbl), cov = c(mat))
  df <- merge(covdf, coldf, by = c("v1", "v2"))
  if (ordering) {
    df$v1 <- factor(as.character(df$v1), axlbl[idx])
    df$v2 <- factor(as.character(df$v2), rev(axlbl[idx]))
  } else {
    df$v1 <- factor(as.character(df$v1), axlbl)
    df$v2 <- factor(as.character(df$v2), rev(axlbl))
  }
  if (facetting) {
    df$facet1 <- factor(gsub("[0-9]+", "", df$v1), c(yvar, xvar))
    df$facet2 <- factor(gsub("[0-9]+", "", df$v2), c(yvar, xvar))
  }
  
  
  plt <- ggplot(df, aes(v1, v2, fill = col)) + 
    geom_tile(aes(alpha = cov), show.legend = c(alpha = FALSE)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_discrete(na.value = '#ffffef') +
    labs(x = NULL, y = NULL, fill = "Relevant for:") +
    theme(legend.position = "top")
  
  if (facetting) {
    plt <- plt  +
      facet_grid(facet2 ~ facet1, scales = 'free', 
                 space = 'free', drop = TRUE)
  }
  return(plt)
}
