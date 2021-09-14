#' Simulation of Multivariate Linear Model Data
#' @importFrom graphics plot text
#' @importFrom utils write.csv
#' @importFrom stats biplot
#' @importFrom utils str
#' @import shiny
#' @return No return value, runs the shiny interface for simulation
#' @export

AppSimrel <- function() {
  ui <- miniUI::miniPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML("
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
    miniUI::gadgetTitleBar("simrel -- Simulation of Multivariate Data",
                   left = miniUI::miniTitleBarButton("newSeed", "New Seed", primary = TRUE),
                   right = miniUI::miniTitleBarButton("done", "Accept", primary = TRUE)),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(
        "Input Parameters",
        icon = icon("sliders"),
        miniUI::miniContentPanel(
          shiny::fillPage(
            shiny::fillCol(
              flex = c(2, 2, 8),
              shiny::fillRow(flex = c(2, 4, 6),
                shiny::column(12, simSeedUI('seed-id', input_lbl = NULL)),
                shiny::column(12, simUI('sim-id')),
                shiny::column(12, conditionalPanel(
                  "input['sim-id-update']",
                  shiny::fillRow(
                    downloadUI('simobj', "SimObj"),
                    downloadUI('rdta', "RData"),
                    downloadUI('json', "JSON")
                  )
                ))
              ),
              shiny::fillRow(simTypeUI('sim-type')),
              shiny::fillRow(
                commonInputUI('common-parm'),
                shiny::column(12,
                  shiny::conditionalPanel(
                    "input['sim-type-type'] == 'multivariate'",
                    multivariateInputUI('multi-parm')),
                  shiny::conditionalPanel(
                    "input['sim-type-type'] == 'bivariate'",
                    bivariateInputUI('bi-parm')),
                  shiny::conditionalPanel(
                    "!input['sim-id-update']",
                    shiny::h3(class = "text-center lead text-primary",
                       "Press 'Simulate Now' to simulate data.")
                  ),
                  shiny::conditionalPanel(
                    "input['sim-id-update']",
                    shiny::uiOutput('msg')
                  )
                )
              )
            )
          )
        )
      ),
      miniUI::miniTabPanel(
        "True Coefficients",
        icon = icon("bar-chart"),
        miniUI::miniContentPanel(
          shiny::fillCol(
            shiny::conditionalPanel(
            "input['sim-id-update']",
            simPlotUI('betaPlot')),
            shiny::conditionalPanel(
            "!input['sim-id-update']",
            shiny::h3(class = "text-center lead", "Simulate data first!"))
          )
        )
      ),
      miniUI::miniTabPanel(
        "Relevant Components",
        icon = icon("area-chart"),
        miniUI::miniContentPanel(
          shiny::fillCol(
            shiny::conditionalPanel(
              "input['sim-id-update']",
              simPlotUI('relComp')),
            shiny::conditionalPanel(
              "!input['sim-id-update']",
              shiny::h3(class = "text-center lead", "Simulate data first!"))
          )
        )
      ),
      miniUI::miniTabPanel(
        "Est.Rel Components",
        icon = icon("area-chart"),
        miniUI::miniContentPanel(
          shiny::fillCol(
            shiny::conditionalPanel(
              "input['sim-id-update']",
              simPlotUI('estRelComp')),
            shiny::conditionalPanel(
              "!input['sim-id-update']",
              shiny::h3(class = "text-center lead", "Simulate data first!"))
          )
        )
      )
    ))

  server <- function(input, output, session) {
    ## Calling Modules ---------------------------
    type <- callModule(simType, 'sim-type')
    callModule(sim, 'sim-id')
    callModule(simSeed, 'seed-id')
    callModule(commonInput, 'common-parm')
    callModule(multivariateInput, 'multi-parm')
    callModule(bivariateInput, 'bi-parm')

    ## Make some simulations ----------------------------------------
    state <- shiny::reactiveValues(simulated = FALSE)
    currentType <- shiny::reactive(NULL)
    sim.obj <- shiny::eventReactive(input[["sim-id-update"]], {
      set.seed(input[['seed-id-newSeed']])
      param <- list(
        n      = input[['common-parm-n']],
        p      = input[['common-parm-p']],
        q      = parse_parm(input[['common-parm-q']]),
        relpos = parse_parm(input[['common-parm-relpos']]),
        gamma  = input[['common-parm-gamma']],
        R2     = parse_parm(input[['common-parm-R2']]),
        ntest  = input[['common-parm-n_test']],
        type   = input[['sim-type-type']]
      )
      if (type() == "multivariate") {
        param$eta <- input[['multi-parm-eta']]
        param$m <- input[['multi-parm-m']]
        param$ypos <- parse_parm(input[['multi-parm-ypos']])
      }
      if (type() == "bivariate") {
        param$rho <- c(input[['bi-parm-rho1']], input[['bi-parm-rho2']])
      }
      obj <- do.call(simrel, param)
      type <- obj[["type"]]
      state$simulated <- TRUE
      list(param = param, obj = obj, type = type)
    })

    simObj <- shiny::reactive(sim.obj()[["obj"]])
    currentType <- shiny::reactive(if (state$simulated) sim.obj()[["type"]])
    param <- shiny::reactive(sim.obj()[["param"]])

    ## Creating Output Expression ------------
    output$stdOut <- shiny::renderText({
      par <- param()
      par$type <- gsub("(.+)", "'\\1'", par$type)
      seed <- paste0('set.seed(', shiny::isolate(input[["seed-id-newSeed"]]), ')')
      paste0(seed, "\n",
             "sim.obj <- simulatr(",
             paste(paste(names(par), par, sep = " = "),
                   collapse = ", "), ")")
    })


    ## Update Parameter Input -------------------
    shiny::observe({
      ## Observe input of Parameters ------------
      if (all(identical(type(), "bivariate"), !identical(type(), currentType()))) {
        shiny::updateTextInput(session, "common-parm-relpos", value = "1,2,3;3,4,6")
        shiny::updateTextInput(session, "common-parm-q", value = "5, 5, 2")
        shiny::updateTextInput(session, "common-parm-R2", value = "0.8, 0.7")
      }
      if (all(identical(type(), "univariate"), !identical(type(), currentType()))) {
        shiny::updateTextInput(session, "common-parm-relpos", value = "2, 3, 4, 6")
        shiny::updateTextInput(session, "common-parm-q", value = "6")
        shiny::updateTextInput(session, "common-parm-R2", value = "0.9")
      }
      if (all(identical(type(), "multivariate"), !identical(type(), currentType()))) {
        shiny::updateTextInput(session, "common-parm-q", value = "5, 4")
        shiny::updateTextInput(session, "common-parm-R2", value = "0.8, 0.7")
        shiny::updateTextInput(session, "common-parm-relpos", value = "1,2; 3,4,6")
      }
    })

    ## Observe Some Event ----------------------------------------
    shiny::observeEvent(input[['newSeed']], {
      shiny::updateNumericInput(session, 'seed-id-newSeed', value = sample(9999, size = 1))
    })
    shiny::observeEvent(input[['sim-id-update']], {
      ## Output the simulation type --------
      output$type <- reactive(simObj()[["type"]])
      shiny::outputOptions(output, "type", suspendWhenHidden = FALSE)

      ## Create Output Message ---------
      output$msg <- shiny::renderUI({
        shiny::tagList(
          shiny::verbatimTextOutput("stdOut"),
          shiny::h3(class = "text-center lead text-success",
             paste("Your",
                   shiny::isolate(simObj()[["type"]]),
                   "data is ready with seed",
                   shiny::isolate(input[["seed-id-newSeed"]])))
        )
      })

      ## Simulation Plot Modules -----------
      shiny::callModule(simPlot, 'betaPlot', simObj(), 1)
      shiny::callModule(simPlot, 'relComp', simObj(), 2)
      shiny::callModule(simPlot, 'estRelComp', simObj(), 3)

      ## Download Button Modules ----------
      shiny::callModule(download, 'rdta', simObj(), "RData")
      shiny::callModule(download, 'csv', simObj(), "CSV")
      shiny::callModule(download, 'json', simObj(), "json")
      shiny::callModule(download, 'simobj', simObj(), "simobj")
    })

    # Handle the Done button being pressed. ---------------
    shiny::observeEvent(input$done, {
      par <- param()
      par$type <- gsub("(.+)", "'\\1'", par$type)
      seed <- paste0('set.seed(', isolate(input[["seed-id-newSeed"]]), ')')
      code <- paste0(seed, "\n",
             "sim.obj <- simrel(",
             paste(paste(names(par), par, sep = " = "),
                   collapse = ", "), ")")
      rstudioapi::sendToConsole('cat("\\014")', execute = TRUE)
      rstudioapi::sendToConsole(code, execute = FALSE)
      shiny::stopApp()
    })
  }

  shiny::runGadget(shiny::shinyApp(ui, server), viewer = shiny::dialogViewer("simrel", 850, 700))
}
