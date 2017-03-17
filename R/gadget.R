#' Simulation of Multivariate Linear Model Data
#' @importFrom grDevices dev.flush dev.hold dev.new devAskNewPage palette
#' @importFrom graphics layout legend matplot par title
#' @importFrom utils write.csv
#' @import shiny
#' @import miniUI
#' @import shinyBS
#' @export

app_simulatr <- function() {
  ui <- miniPage(
    gadgetTitleBar("Simulatr -- Simulation of Multivariate Data"),
    miniTabstripPanel(
      miniTabPanel(
        "Input Parameters", 
        icon = icon("sliders"),
        miniContentPanel(
          fluidRow(
            column(3, 
                   div(h3(actionLink("newSeed", label = "NewSeed", icon = icon("random"))), 
                       class = "container-fluid text-left")),
            column(3,
                   div(h3(actionLink("update", label = "Simulate", icon = icon("refresh"))), 
                       class = "container-fluid text-left")),
            column(2, downloadButton(
              outputId = 'downloadCSV',
              label = "CSV",
              icon = icon("download"), 
              class = "btn btn-primary btn-lg")),
            column(2, downloadButton(
              outputId = 'downloadRDATA',
              label = 'RData', 
              class = "btn btn-primary btn-lg")),
            column(2, downloadButton(
              outputId = 'downloadJSON',
              label = "JSON",
              icon = icon("code"), 
              class = "btn btn-primary btn-lg"))
          ),
          fluidRow(
            column(12, selectInput(
              "type", 
              label = "Type of simulation:", 
              choices = c("Univariate Simulation" = "univariate",
                          "Bivariate Simulation" = "bivariate",
                          "Multivariate Simulation" = "multivariate"), 
              selected = "bivariate",
              width = "100%")),
            bsTooltip("type", 
                      "Type of simulation you want to perform",
                      "top", "hover")
          ),
          fluidRow(
            column(4, numericInput("n", label = "N: Train", value = 200, min = 10, step = 1)),
            column(4, numericInput("n_test", label = "N: Test", value = 50, min = 5, step = 1)),
            column(4, numericInput("p", label = "N: Predictors", value = 15, min = 2, step = 1))
          ),
          fluidRow(
            column(4, textInput("q", label = "Rel.Pred", value = "5, 5, 2")),
            column(4, textInput("R2", label = "Coef. Determination", value = "0.8, 0.7")),
            column(4, textInput("relpos", label = "RelPos.Comp", 
                                value = "1, 2, 3; 3, 4, 6"))
          ),
          conditionalPanel(
            condition = "input.type == 'multivariate'",
            fluidRow(
              column(6, numericInput("m", label = "N: Response", value = 4, min = 2, step = 1, width = '100%')),
              column(6, textInput("ypos", label = "Response Mixup", 
                                  value = "1, 3; 2, 4", width = '100%'))
            )
          ),
          fluidRow(
            conditionalPanel(
              condition = "input.type == 'bivariate'",
              fluidRow(
                column(8, div(em("Correlation between Response")), style = "text-align:center;")
              ),
              column(4, sliderInput("rho1", "Without Given X", 
                                    min = -1, max = 1, value = 0.6, step = 0.1, width = "100%")),
              column(4, sliderInput("rho2", "With Given X", 
                                    min = -1, max = 1, value = 0.7, step = 0.1, width = "100%")),
              column(4, sliderInput("gamma", "Gamma", 
                                    min = 0, max = 1, value = 0.6, step = 0.1, width = "100%"))
            ),
            conditionalPanel(
              condition = "input.type != 'bivariate'",
              column(12, sliderInput("gamma", "Gamma", 
                                     min = 0, max = 1, value = 0.6, step = 0.1, width = "100%"))
            )
          )
        )
      ),
      miniTabPanel(
        "True Coefficients", 
        icon = icon("bar-chart"),
        miniContentPanel(
          fluidRow(
            conditionalPanel(
              condition = "input.update",
              plotOutput("simPlot1", height = '450px')
            )
          )
        )
      ),
      miniTabPanel(
        "Relevant Components", 
        icon = icon("area-chart"),
        miniContentPanel(
          conditionalPanel(
            condition = "input.update",
            plotOutput("simPlot2", height = '450px')
          )
        )
      ),
      miniTabPanel(
        "Est.Rel Components", 
        icon = icon("area-chart"),
        miniContentPanel(
          conditionalPanel(
            condition = "input.update",
            plotOutput("simPlot3", height = '450px')
          )
        )
      )
    ))
  
  server <- function(input, output, session) {
    parseText <- function(x) {
      evl <- function(x) as.numeric(unlist(strsplit(x, split = ",")))
      out <- lapply(strsplit(x, ";")[[1]], evl)
      if (length(out) > 1) return(out)
      return(out[[1]])
    }
    
    observe({
      type <- input$type
      
      if (type == "bivariate") {
        updateTextInput(
          session, "relpos",
          value = "1,2,3;3,4,6"
        )
        updateTextInput(
          session, "q",
          value = "5, 5, 2"
        )
        updateTextInput(
          session, "R2",
          value = "0.8, 0.7"
        )
      }
      if (type == "univariate") {
        updateTextInput(
          session, "relpos",
          value = "2, 3, 4, 6"
        )
        updateTextInput(
          session, "q",
          value = "6"
        )
        updateTextInput(
          session, "R2",
          value = "0.9"
        )
      }
      if (type == "multivariate") {
        updateTextInput(
          session, "q",
          value = "5, 4"
        )
        updateTextInput(
          session, "R2",
          value = "0.8, 0.7"
        )
        updateTextInput(
          session, "relpos",
          value = "1,2; 3,4,6"
        )
      }
    })
    
    simObj <- eventReactive(any(input$update, input$newSeed), {
      set.seed(input$newSeed)
      par <- list(n = input$n,
                  p = input$p,
                  q = parseText(input$q),
                  relpos = parseText(input$relpos),
                  gamma = input$gamma,
                  R2 = parseText(input$R2),
                  ntest = input$n_test,
                  type = input$type)
      if(input$type == "multivariate") {
        par$m <- input$m
        par$ypos <- parseText(input$ypos)
      }
      if(input$type == "bivariate") {
        par$rho = c(input$rho1, input$rho2)
      }
      sim.obj <- do.call(simulatr, par)
      list(sim.obj = sim.obj, par = par)
    })
    # fitObj <- reactive({
    #   sim.obj <- simObj()[["sim.obj"]]
    #   dt.train <- data.frame(y = I(sim.obj$Y), x = I(sim.obj$X))
    #   dt.test <- if (input$type == "multivariate"){
    #     data.frame(y = I(sim.obj$testY), x = I(sim.obj$testX))
    #   } else {
    #     data.frame(y = I(sim.obj$TESTY), x = I(sim.obj$TESTX))
    #   }
    #   if (input$model == "Least Square Estimation") {
    #     mdl <- lm(y ~ x, data = dt.train)
    #   } else if (input$model == "Principal Component Regression") {
    #     mdl <- pcr(y ~ x, data = dt.train, validation = "CV")
    #   } else if (input$model == "Partial Least Square Regression") {
    #     mdl <- plsr(y ~ x, data = dt.train, validation = "CV")
    #   } else if (input$model == "Envelope MLE") {
    #     mdl <- NA
    #   }
    #   list(dt.train = dt.train, dt.test = dt.test, mdl = mdl)
    # })
    
    ## Description of Paramters ----------------------------------
    args <- structure(list(n = "The number of (training) samples to generate.", 
                           p = "The total number of predictor variables to generate.", 
                           m = "The number of relevant latent components to be used.", 
                           q = "The number of relevant predictor variables (as a subset of p).", 
                           relpos = "A vector indicating the position (between 1 and p) of the m relevant components, e.g. c(1,2) means that the first two latent components should be relevant. The length of relpos must be equal to m.", 
                           gamma = "A number defining the speed of decline in eigenvalues (variances) of the latent components. The eigenvalues are assumed to decline according to an exponential model. The first eigenvalue is set equal to 1.", 
                           R2 = "The theoretical R-squared according to the true linear model. A number between 0 and 1.", 
                           n_test = "The number of test samples to be generated (optional)."), 
                      .Names = c("n", "p", "m", "q", "relpos", "gamma", "R2", "n_test"))
    
    ## Creating Tooltips -----------------------------------
    # addTooltip(session, args["n", 'Arguements'], args["n", "Description"], 
    #                     placement = "top", trigger = "hover")
    # addTooltip(session, args["ntest", 'Arguements'], args["n_test", "Description"], 
    #                     placement = "top", trigger = "hover")
    addTooltip(session, "n_test", args[["n_test"]], placement = "left", trigger = "hover")
    addTooltip(session, "n", args[["n"]], placement = "bottom", trigger = "hover")
    addTooltip(session, "gamma", args[["gamma"]], placement = "top", trigger = "hover")
    addTooltip(session, "p", args[["p"]], placement = "top", trigger = "hover")
    addTooltip(session, "m", args[["m"]], placement = "top", trigger = "hover")
    addTooltip(session, "relpos", args[["relpos"]], placement = "top", trigger = "hover")
    
    ## Export Type to update items conditioned upon it
    output$type <- renderText(simObj()[["sim.obj"]][["type"]])
    outputOptions(output, "type", suspendWhenHidden = FALSE)
    
    ## Overview Simulation Plots ------------------------------------
    output$simPlot1 <- renderPlot({
      plot_simulatr(simObj()[["sim.obj"]], which = 1)
    })
    output$simPlot2 <- renderPlot({
      plot_simulatr(simObj()[["sim.obj"]], which = 2)
    })
    output$simPlot3 <- renderPlot({
      plot_simulatr(simObj()[["sim.obj"]], which = 3)
    })
    
    
    # Isolate simObject
    sim.obj <- reactiveValues()
    observe({
      if(!is.null(simObj()))
        isolate({
          sim.obj <- simObj()[["sim.obj"]]
          dt.train <- data.frame(y = I(sim.obj$Y), x = I(sim.obj$X))
          dt.test <- if (input$type == "multivariate"){
            data.frame(y = I(sim.obj$testY), x = I(sim.obj$testX))
          } else {
            data.frame(y = I(sim.obj$TESTY), x = I(sim.obj$TESTX))
          }
          sim.csv <- rbind(train = dt.train, test = dt.test)
          sim.list <- list(train = dt.train, test = dt.test)
          sim.json <- jsonlite::toJSON(sim.list)
        })
    })
    
    ## ---- Download Data ------------------------------
    downloadFn <- function(data, type = "Rdata") {
        downloadHandler(
          filename <- function() paste("sim.obj", type, sep = "."),
          content = function(file) {
            which.type <- tolower(type)
            switch(
              which.type,
              csv = write.csv(data, file = file),
              rdata = save(data, file = file),
              rda = save(data, file = file),
              rds = saveRDS(data, file = file),
              json = jsonlite::write_json(data, path = file)
            )
          }
        )
      }
    output$downloadRDATA <- downloadFn(sim.obj, "RData")
    output$downloadCSV <- downloadFn(sim.csv, "csv")
    output$downloadJSON <- downloadFn(sim.json, "json")
    
    # Handle the Done button being pressed.
    observeEvent(input$done, {
      par <- simObj()[["par"]]
      par$type <- gsub("(.+)", "'\\1'", par$type)
      code <- paste0("sim.obj <- simulatr(", paste(paste(names(par), par, sep = " = "), collapse = ", "), ")")
      rstudioapi::sendToConsole(code, execute = FALSE)
      stopApp(sim.obj)
    })
  }
  
  runGadget(shinyApp(ui, server), viewer = dialogViewer("Simulatr", 800, 650))  
}