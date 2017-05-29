#' Simulation of Multivariate Linear Model Data
#' @importFrom graphics plot text
#' @importFrom utils write.csv
#' @importFrom stats biplot
#' @importFrom utils str
#' @import shiny
#' @import miniUI
#' @import shinyBS
#' @export

app_simulatr <- function() {
  ui <- miniPage(
    tags$head(tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=Source+Code+Pro');
      
      input#seed-id-newSeed {
        height: 100%;
        width: 100%;
        margin-top: 5%;
        font-size: 2.2em;
        border: none;
        box-shadow: none;
        color: #337ab7;
        text-align: left;
        font-family: monospace;
      }
      pre, code {
        font-family: 'Source Code Pro', monospace;
      }
    "))),
    gadgetTitleBar("Simulatr -- Simulation of Multivariate Data",
                   left = miniTitleBarButton("newSeed", "New Seed", primary = TRUE),
                   right = miniTitleBarButton("done", "Accept", primary = TRUE)),
    miniTabstripPanel(
      miniTabPanel(
        "Input Parameters", 
        icon = icon("sliders"),
        miniContentPanel(
          fillPage(
            fillCol(
              flex = c(2, 2, 8),
              fillRow(flex = c(2, 4, 6),
                column(12, simSeedUI('seed-id', input_lbl = NULL)),
                column(12, simUI('sim-id')),
                column(12, conditionalPanel(
                  "input['sim-id-update']",
                  fillRow(
                    downloadUI('simobj', "SimObj"),
                    downloadUI('rdta', "RData"),
                    downloadUI('json', "JSON")
                  )
                ))
              ),
              fillRow(simTypeUI('sim-type')),
              fillRow(
                commonInputUI('common-parm'),
                column(12,
                  conditionalPanel(
                    "input['sim-type-type'] == 'multivariate'", 
                    multivariateInputUI('multi-parm')),
                  conditionalPanel(
                    "input['sim-type-type'] == 'bivariate'", 
                    bivariateInputUI('bi-parm')),
                  conditionalPanel(
                    "!input['sim-id-update']",
                    h3(class = "text-center lead text-primary", 
                       "Press Simulate Now Button for Simulation")
                  ),
                  conditionalPanel(
                    "input['sim-id-update']",
                    uiOutput('msg')
                  )
                )
              )
            )
          )
        )
      ),
      miniTabPanel(
        "True Coefficients", 
        icon = icon("bar-chart"),
        miniContentPanel(
          fillCol(
          conditionalPanel(
            "input['sim-id-update']", 
            simPlotUI('betaPlot')),
          conditionalPanel(
            "!input['sim-id-update']", 
            h3(class = "text-center lead", "Simulate data first!"))
          )
        )
      ),
      miniTabPanel(
        "Relevant Components", 
        icon = icon("area-chart"),
        miniContentPanel(
          fillCol(
            conditionalPanel(
              "input['sim-id-update']", 
              simPlotUI('relComp')),
            conditionalPanel(
              "!input['sim-id-update']", 
              h3(class = "text-center lead", "Simulate data first!"))
          )
        )
      ),
      miniTabPanel(
        "Est.Rel Components", 
        icon = icon("area-chart"),
        miniContentPanel(
          fillCol(
            conditionalPanel(
              "input['sim-id-update']", 
              simPlotUI('estRelComp')),
            conditionalPanel(
              "!input['sim-id-update']", 
              h3(class = "text-center lead", "Simulate data first!"))
          )
        )
      )
    ))
  
  server <- function(input, output, session) {
    ## Some functions ----------------
    parseText <- function(x) {
      evl <- function(x) as.numeric(unlist(strsplit(x, split = ",")))
      out <- lapply(strsplit(x, ";")[[1]], evl)
      if (length(out) > 1) return(out)
      return(out[[1]])
    }
    
    ## Calling Modules ---------------------------
    type <- callModule(simType, 'sim-type')
    callModule(sim, 'sim-id')
    callModule(simSeed, 'seed-id')
    callModule(commonInput, 'common-parm')
    callModule(multivariateInput, 'multi-parm')
    callModule(bivariateInput, 'bi-parm')
    
    ## Make some simulations ----------------------------------------
    state <- reactiveValues(simulated = FALSE)
    currentType <- reactive(NULL)
    sim.obj <- eventReactive(input[["sim-id-update"]], {
      set.seed(input[['seed-id-newSeed']])
      param <- list(
        n = input[['common-parm-n']],
        p = input[['common-parm-p']],
        q = parseText(input[['common-parm-q']]),
        relpos = parseText(input[['common-parm-relpos']]),
        gamma = input[['common-parm-gamma']],
        R2 = parseText(input[['common-parm-R2']]),
        ntest = input[['common-parm-n_test']],
        type = input[['sim-type-type']]
      )
      if (type() == "multivariate") {
        param$m <- input[['multi-parm-m']]
        param$ypos <- parseText(input[['multi-parm-ypos']])
      }
      if (type() == "bivariate") {
        param$rho <- c(input[['bi-parm-rho1']], input[['bi-parm-rho2']])
      }
      obj <- do.call(simulatr, param)
      type <- obj[["type"]]
      state$simulated <- TRUE
      list(param = param, obj = obj, type = type)
    })
    
    simObj <- reactive(sim.obj()[["obj"]])
    currentType <- reactive(if (state$simulated) sim.obj()[["type"]])
    param <- reactive(sim.obj()[["param"]])
    
    ## Creating Output Expression ------------
    output$stdOut <- renderText({
      par <- param()
      par$type <- gsub("(.+)", "'\\1'", par$type)
      seed <- paste0('set.seed(', isolate(input[["seed-id-newSeed"]]), ')')
      paste0(seed, "\n",
             "sim.obj <- simulatr(",
             paste(paste(names(par), par, sep = " = "),
                   collapse = ", "), ")")
    })
    
    
    ## Update Parameter Input -------------------
    observe({
      ## Observe input of Parameters ------------
      if (all(identical(type(), "bivariate"), !identical(type(), currentType()))) {
        updateTextInput(session, "common-parm-relpos", value = "1,2,3;3,4,6")
        updateTextInput(session, "common-parm-q", value = "5, 5, 2")
        updateTextInput(session, "common-parm-R2", value = "0.8, 0.7")
      }
      if (all(identical(type(), "univariate"), !identical(type(), currentType()))) {
        updateTextInput(session, "common-parm-relpos", value = "2, 3, 4, 6")
        updateTextInput(session, "common-parm-q", value = "6")
        updateTextInput(session, "common-parm-R2", value = "0.9")
      }
      if (all(identical(type(), "multivariate"), !identical(type(), currentType()))) {
        updateTextInput(session, "common-parm-q", value = "5, 4")
        updateTextInput(session, "common-parm-R2", value = "0.8, 0.7")
        updateTextInput(session, "common-parm-relpos", value = "1,2; 3,4,6")
      }
    })
    
    ## Observe Some Event ----------------------------------------
    observeEvent(input[['newSeed']], {
      updateNumericInput(session, 'seed-id-newSeed', value = sample(9999, size = 1))
    })
    observeEvent(input[['sim-id-update']], {
      ## Output the simulation type --------
      output$type <- reactive(simObj()[["type"]])
      outputOptions(output, "type", suspendWhenHidden = FALSE)
      
      ## Create Output Message ---------
      output$msg <- renderUI({
        tagList(
          verbatimTextOutput("stdOut"),
          h3(class = "text-center lead text-success",
             paste("Your",
                   isolate(simObj()[["type"]]),
                   "data is ready with seed",
                   isolate(input[["seed-id-newSeed"]]),
                   "You can explore some plots or",
                   "download the data"))
        )
      })
      
      ## Simulation Plot Modules -----------
      callModule(simPlot, 'betaPlot', simObj(), 1)
      callModule(simPlot, 'relComp', simObj(), 2)
      callModule(simPlot, 'estRelComp', simObj(), 3)
      
      ## Download Button Modules ----------
      callModule(download, 'rdta', simObj(), "RData")
      callModule(download, 'csv', simObj(), "CSV")
      callModule(download, 'json', simObj(), "json")
      callModule(download, 'simobj', simObj(), "simobj")
    })
    
    # Handle the Done button being pressed. ---------------
    observeEvent(input$done, {
      par <- param()
      par$type <- gsub("(.+)", "'\\1'", par$type)
      seed <- paste0('set.seed(', isolate(input[["seed-id-newSeed"]]), ')')
      code <- paste0(seed, "\n",
             "sim.obj <- simulatr(",
             paste(paste(names(par), par, sep = " = "),
                   collapse = ", "), ")")
      rstudioapi::sendToConsole('cat("\\014")', execute = TRUE)
      rstudioapi::sendToConsole(code, execute = FALSE)
      stopApp()
    })
  }
  
  runGadget(shinyApp(ui, server), viewer = dialogViewer("Simulatr", 850, 700))
}