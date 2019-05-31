#' Prepare data for Plotting Covariance Matrix
#' @param sobj A simrel object
#' @param type Type of covariance matrix - can take two values \code{relpos} for relevant position
#' of principal components  and \code{relpred} for relevant position of predictor variables
#' @param ordering TRUE for ordering the covariance for block diagonal display
#' @param facetting TRUE for facetting the predictor and response space. FALSE will give a single facet plot
#' @return A data frame with covariances and related values based on \code{type} argument that is  ready to plot
#' @keywords simulation, linear model, linear model data, covariance plot
#' @examples
#' sobj <- simrel(n = 100, p = 10, q = c(4, 5), relpos = list(c(1, 2, 3), c(4, 6, 7)), m = 3,
#'                R2 = c(0.8, 0.7), ypos = list(c(1, 3), 2), gamma = 0.7, type = "multivariate")
#' head(cov_plot_data(sobj))
#' @export

cov_plot_data <- function(sobj, type = "relpos", ordering = TRUE, facetting = TRUE) {
    ## ---- Warm up ----
    simtype <- sobj$type
    m <- switch(simtype, univariate = 1, bivariate = 2, multivariate = sobj$m)
    p <- sobj$p
    if (type == "relpos") {
        lst <- sobj$relpos
        if (simtype == "bivariate") {
            comn <- do.call(intersect, lst)
            lst <- append(lapply(lst, function(x) setdiff(x, comn)), list(comn), after = 1)
        }
    } else {
        lst <- unname(sobj$relpred)
        if (simtype == "bivariate") lst <- append(lst[-3], lst[3], after = 1)
    }
    xvar <- if (type == "relpos") "Z" else "X"
    yvar <- if (type == "relpos" & simtype == "multivariate") "W" else "Y"
    axlbl <- c(paste0(yvar, 1:m), paste0(xvar, 1:p))
    lst <- unlist(lst)
    idx <- unique(c(lst, setdiff(1:p, lst)))

    ## ---- Setting up matrices ----
    if (simtype == "multivariate") {
        ## Rotation Matrix
        rot <- Reduce(rbind, rep(0, m),
                      Reduce(cbind, rep(0, m), sobj$Xrotation, right = TRUE),
                      right = TRUE)
        rot[1:m, 1:m] <- sobj$Yrotation
        ## Covariance Matrix
        mat <- if (type == "relpos") sobj$SigmaWZ else sobj$Sigma
        idx <- c(unlist(sobj$ypos), idx + m)
    } else {
        ## Rotation Matrix
        rotX <- sobj$Rotation
        rot <- Reduce(rbind, rep(0, m),
                      Reduce(cbind, rep(0, m), rotX, right = TRUE),
                      right = TRUE)
        diag(rot)[1:m] <- 1
        ## Covariance Matrix
        sigma <- sobj$Sigma
        sigma <- rot %*% sigma %*% t(rot)
        mat <- if (type == "relpos") sobj$Sigma else sigma
        ## setting up index
        idx <- c(1:m, idx + m)
    }

    ## ---- Color Generator Matrix ----
    genmat <- mat[1:m, -c(1:m), drop = FALSE] != 0
    genmat[!genmat] <- NA

    ## ypos map
    if (sobj$type == "multivariate") {
        ypos <- vector("character", length = m)
        names(ypos) <- paste0(yvar, seq.int(m))
        for (x in sobj$ypos) ypos[x] <- paste0(yvar, x[1])
    }

    for (row in seq_len(NROW(genmat))) {
        yvec <- if (sobj$type == "multivariate") ypos[row] else paste0(yvar, row)
        genmat[row, as.logical(genmat[row, ])] <- yvec
    }
    col_xx <- genmat[apply(genmat, 2, function(col) match(TRUE, !is.na(col))), ]
    col_yy <- genmat[, apply(genmat, 1, function(row) match(TRUE, !is.na(row)))]
    if (simtype == "bivariate") {
        col_xxt <- t(genmat)[, apply(t(genmat), 1, function(col) match(TRUE, !is.na(col)))]
        col_yy[row(col_yy) != col(col_yy)] <- "Both"
        col_xx[!is.na(col_xxt)] <- col_xxt[!is.na(col_xxt)]
        comn_col <- which(apply(is.na(genmat), 2, sum) == 0)
        col_xx[comn_col, comn_col] <- "Both"
    }
    if (type == "relpos") col_xx[col(col_xx) != row(col_xx)] <- NA
    colmat <- rbind(cbind(col_yy, genmat), cbind(t(genmat), col_xx))
    if (type == "rotation") colmat[1:m, -c(1:m)] <- colmat[-c(1:m), 1:m] <- NA

    id_df <- expand.grid(v1 = axlbl, v2 = axlbl)
    coldf <- cbind(id_df, col = c(colmat))
    covdf <- cbind(id_df, cov = if (type == "rotation") c(rot) else c(mat))
    df <- merge(coldf, covdf, by = c("v1", "v2"))
    df$col <- as.character(df$col)
    df[all(df$cov != 0, is.na(df$col)), "col"] <- "None"
    if (simtype == "bivariate") {
        df$col <- factor(df$col, levels = c(sort(unique(df$col)[grepl(yvar, unique(df$col))]), "Both", "None", NA))
    } else {
        col_lvl <- c(sort(unique(df$col)[grepl(yvar, unique(df$col))]), "None", NA)
        df$col <- factor(df$col, levels = col_lvl)
    }
    if (ordering) {
        df$v1 <- factor(as.character(df$v1), axlbl[idx])
        df$v2 <- factor(as.character(df$v2), rev(axlbl[idx]))
    } else {
        df$v1 <- factor(as.character(df$v1), axlbl)
        df$v2 <- factor(as.character(df$v2), rev(axlbl))
    }
    df$facet1 <- factor(gsub("[0-9]+", "", df$v1), c(yvar, xvar))
    df$facet2 <- factor(gsub("[0-9]+", "", df$v2), c(yvar, xvar))

    return(df)
}


#' Plotting Covariance Matrix
#' @param sobj A simrel object
#' @param type Type of covariance matrix - can take two values \code{relpos} for relevant position
#' of principal components  and \code{relpred} for relevant position of predictor variables
#' @param ordering TRUE for ordering the covariance for block diagonal display
#' @param facetting TRUE for facetting the predictor and response space. FALSE will give a single facet plot
#' @return A covariance plot
#' @import ggplot2
#' @examples 
#' sobj <- simrel(n = 100, p = 10, q = c(4, 5), relpos = list(c(1, 2, 3), c(4, 6, 7)), m = 3,
#'                R2 = c(0.8, 0.7), ypos = list(c(1, 3), 2), gamma = 0.7, type = "multivariate")
#' p1 <- plot_cov(sobj, type = "relpos", facetting = FALSE)
#' p2 <- plot_cov(sobj, type = "rotation", facetting = FALSE)
#' p3 <- plot_cov(sobj, type = "relpred", facetting = FALSE)
#' gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
#' @references Sæbø, S., Almøy, T., & Helland, I. S. (2015). simrel—A versatile tool for linear
#' model data simulation based on the concept of a relevant subspace and relevant predictors.
#' Chemometrics and Intelligent Laboratory Systems, 146, 128-135.
#' @references Almøy, T. (1996). A simulation study on comparison of prediction methods when only a
#' few components are relevant. Computational statistics & data analysis, 21(1), 87-107.
#' @references Rimal, R., Almøy, T., & Sæbø, S. (2018). A tool for simulating multi-response linear model data. Chemometrics and Intelligent Laboratory Systems, 176, 1-10.
#' @export
plot_cov <- function(sobj, type = "relpos", ordering = TRUE, facetting = TRUE) {
    dta <- cov_plot_data(sobj, type = type, ordering = ordering, facetting = facetting)
    plt <- ggplot(dta, aes_string("v1", "v2", fill = "col")) +
        geom_tile(aes_string(alpha = "cov"), color = "grey70",
                  show.legend = c(alpha = FALSE)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = NULL, y = NULL, fill = if (type == "rotation") NULL else "Relevant for:") +
        scale_fill_brewer(palette = "Set1", na.value = "#fffffc", breaks = levels(dta$col)) +
        theme(legend.position = "bottom",
              panel.border = element_rect(color = "grey70"),
              panel.grid = element_blank(),
              panel.spacing = unit(1, 'mm'),
              aspect.ratio = 1)
    plt <- plt  +
        facet_grid(facet2 ~ facet1, scales = 'free',
                   space = 'free', drop = TRUE) +
        theme(strip.background = element_rect(color = "grey", fill = "grey90"))
    if (!facetting) {
        plt <- plt +
            theme(strip.background = element_blank(),
                  strip.text = element_blank())
    }
    return(plt)
}
