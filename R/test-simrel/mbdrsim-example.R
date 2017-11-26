## ---- Loading Packages ----
library(simrel)
library(tidyverse)
library(reshape2)

## ---- Without Tidyverse ----
  ## Simrel Parameters ----
  sim_list <- list(
    p = c(20, 150),
    gamma = seq(0.2, 1.1, length.out = 4),
    relpos = list(list(c(1, 2, 3), c(4, 5, 6)), list(c(1, 5, 6), c(2, 3, 4))),
    R2 = list(c(0.4, 0.8), c(0.8, 0.8)),
    ypos = list(list(1, c(2, 3)), list(c(1, 3), 2))
  )
  ## Design ----
  design <- mbrdsim(sim_list, fraction = 3)[["Design"]]
  design <- cbind(
    design,
    q = lapply(design[, "p"], function(x) rep(x/2, 2)),
    type = "multivariate",
    n = 100,
    ntest = 200,
    m = 3,
    eta = 0.6
  )
  ## Simulation ----
  sobj <- apply(design, 1, function(x) do.call(simrel, x))
  names(sobj) <- paste0("Design", seq.int(sobj))
  
  ## Model Fitting ----
  library(pls)
  fit <- list()
  fit$pcr <- lapply(sobj, function(obj) {
    dta <- data.frame(x = I(obj$X), y = I(obj$Y))
    pcr(y ~ x, data = dta, validation = "CV", segments = 10)
  })
  fit$pls <- lapply(sobj, function(obj) {
    dta <- data.frame(x = I(obj$X), y = I(obj$Y))
    plsr(y ~ x, data = dta, validation = "CV", segments = 10)
  })
  
  ## Calculating Error ----
  rmsep <- lapply(fit, function(f){
    sapply(names(f), function(d){
      new_data <- with(sobj[[d]], data.frame(x = I(testX), y = I(testY)))
      out <- RMSEP(f[[d]], newdata = new_data, estimate = "all")
      reshape2::melt(out[["val"]])
    }, simplify = FALSE)
  })
  
  ## Plotting ----
  get_error_plot <- function(dgn){
    df <- do.call(rbind, lapply(rmsep, "[[", dgn))
    df$method <- gsub("\\.[0-9]+", "", rownames(df))
    rownames(df) <- NULL
    df$model <- ifelse(df$model == "(Intercept)", "0", df$model)
    df$model <- as.numeric(gsub("[A-Za-z]+", "", df$model))
    ggplot(df, aes(model, value, color = estimate, group = estimate)) +
      geom_line() + geom_point(size = 0.5) +
      facet_grid(response ~ method, scales = "free_y") +
      theme(legend.position = "top") +
      labs(x = "Predictors", y = "RMSEP", color = "Estimate")
  }
  get_error_plot(2)
  
  
## ---- Tidyverse way ----
  ## Simrel parameter list ----
  sim_list <- list(
    p = c(20, 150),
    gamma = seq(0.2, 1.1, length.out = 4),
    relpos = list(list(c(1, 2, 3), c(4, 5, 6)), list(c(1, 5, 6), c(2, 3, 4))),
    R2 = list(c(0.4, 0.8), c(0.8, 0.8)),
    ypos = list(list(1, c(2, 3)), list(c(1, 3), 2))
  )
  
  ## Create Design ----
  design <- as_tibble(mbrdsim(sim_list, fraction = 3)[["Design"]])
  design <- design %>% 
    mutate_at(vars(p, gamma), unlist) %>% 
    mutate(
      q = map(p, ~rep(.x / 2, 2)),
      type = "multivariate",
      n = 100,
      ntest = 200,
      m = 3,
      eta = 0.6
    )
  
  ## Simulate Data ----
  sim.obj <- design %>% 
    rowwise() %>% 
    do(sobj = do.call(simrel, .)) %>% 
    ungroup() %>% 
    mutate(design = 1:n())
  
  ## Fitting models ----
  sim.obj <- sim.obj %>% 
    group_by(design) %>% 
    mutate(
      train = map(sobj, ~data.frame(x = I(.x$X), y = I(.x$Y))),
      test = map(sobj, ~data.frame(x = I(.x$testX), y = I(.x$testY)))
    )
  
  pcrFit <- sim.obj %>% 
    transmute(
      fit = map(train, ~pcr(y ~ x, data = .x, validation = "CV", segments = 10)),
      rmsep = list(map2_df(fit, test, ~reshape2::melt(RMSEP(.x, newdata = .y, estimate = "all")[["val"]], as.is = TRUE)))
    ) %>% 
    bind_cols(design) %>% 
    unnest(rmsep) %>% 
    rename(PCR = value)
  
  plsFit <- sim.obj %>% 
    transmute(
      fit = map(train, ~plsr(y ~ x, data = .x, validation = "CV", segments = 10)),
      rmsep = list(map2_df(fit, test, ~reshape2::melt(RMSEP(.x, newdata = .y, estimate = "all")[["val"]], as.is = TRUE)))
    ) %>% 
    bind_cols(design) %>% 
    unnest(rmsep) %>% 
    rename(PLS = value)
  
  ## Calculating Error ----
  rmsep <- pcrFit %>% inner_join(plsFit) %>% 
    gather(method, rmsep, PCR:PLS) %>% 
    mutate(
      model = ifelse(model == "(Intercept)", "0", model),
      model = as.numeric(gsub("[A-Za-z]", "", model))
    )
  
  ## Plotting ----
  get_error_plot <- function(dgn) {
    rmsep %>% 
      filter(design == dgn) %>% 
      ggplot(aes(model, rmsep, color = estimate, group = estimate)) +
      geom_line() + geom_point(size = 0.5) +
      facet_grid(response ~ method, scales = "free_y") +
      theme(legend.position = "top") +
      labs(x = "Predictors", y = "RMSEP", color = "Estimate")
  }
  get_error_plot(2)
  