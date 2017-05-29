## Download Buttons
downloadUI <- function(id, label = 'RData', type = "primary") {
  ns <- NS(id)
  h3(downloadLink(
    outputId = ns('downloadFile'),
    label = span(icon("download"), label)),
    class = "text-primary")
}
download <- function(input, output, session, sim.obj, file_type = "RData") {
  dt.train <- data.frame(y = I(sim.obj$Y), x = I(sim.obj$X))
  dt.test <- if (sim.obj$type == "multivariate"){
    data.frame(y = I(sim.obj$testY), x = I(sim.obj$testX))
  } else {
    data.frame(y = I(sim.obj$TESTY), x = I(sim.obj$TESTX))
  }
  out <- switch(
    tolower(file_type),
    csv = rbind(train = dt.train, test = dt.test),
    json = jsonlite::toJSON(list(train = dt.train, test = dt.test)),
    rdata = list(train = dt.train, test = dt.test),
    simobj = sim.obj
  )
  downloadFn <- function(data, type = "Rdata") {
    downloadHandler(
      filename <- function() 
        ifelse(type == "simobj", 
               paste("sim.obj.rdata"),
               paste("sim.obj", type, sep = ".")),
      content = function(file) {
        which.type <- tolower(type)
        switch(
          which.type,
          csv = write.csv(data, file = file),
          rdata = save(data, file = file),
          simobj = save(data, file = file),
          rda = save(data, file = file),
          rds = saveRDS(data, file = file),
          json = jsonlite::write_json(data, path = file)
        )
      }
    )
  }
  output$downloadFile <- downloadFn(data = out, type = file_type)
  return(output)
}

## Simulation Plts :: Plot 1
simPlotUI <- function(id, ht = '100vh', ...) {
  ns <- NS(id)
  tagList(
    plotOutput(ns('plot'), height = ht, ...)
  )
}
simPlot <- function(input, output, session, sim_obj, which) {
  output$plot <- renderPlot({
    ggplot_simulatr(sim_obj, which = which) + 
      theme_grey(base_size = 18) +
      theme(legend.position = "top")
  })
}

## Param Menu Panel
paramMenuUI <- function(id) {
  ns <- NS(id)
  menuItemOutput(ns('paramMenu'))
}
paramMenu <- function(input, output, session) {
  callModule(simType, 'sim-type')
  callModule(commonInput, 'common-parm')
  callModule(multivariateInput, 'multi-parm')
  callModule(bivariateInput, 'bi-parm')
  output$paramMenu <- renderMenu(
    menuItem(
      "Parameter Settings", icon = icon("cogs"), tabName = "settings",
      commonInputUI("common-parm"),
      conditionalPanel("input['sim-type-type'] == 'multivariate'", multivariateInputUI('multi-parm')),
      conditionalPanel("input['sim-type-type'] == 'bivariate'", bivariateInputUI('bi-parm'))
    )
  )
}

## Simulation UI
simUI <- function(id, label = "Simulate Now", ...) {
  ns <- NS(id)
  column(
    12, 
    div(
      h3(actionLink(
        ns("update"), 
        label = label, 
        icon = icon("refresh"), 
        ...
      )), class = "text-center"
    )
  )
}
sim <- function(input, output, session) {
}

## Type UI
simTypeUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12, 
      selectInput(
        inputId = ns("type"), 
        label = "Type of simulation:", 
        choices = c("Univariate Simulation" = "univariate",
                    "Bivariate Simulation" = "bivariate",
                    "Multivariate Simulation" = "multivariate"), 
        selected = "multivariate",
        width = "100%"
      )
    ),
    bsTooltip(ns("type"), "Type of simulation you want to perform", "top", "hover")
  )
}
simType <- function(input, output, session) {
  return(reactive(input$type))
}

## Seed UI
simSeedUI <- function(id, input_lbl = "Seed", btn_lbl = "Seed") {
  ns <- NS(id)
  fluidRow(numericInput(ns('newSeed'), label = input_lbl, value = 123))
}
simSeed <- function(input, output, session) {
  # observe(addClass('newSeed', 'input-lg'))
  return(reactive(input$newSeed))
}

## Common Input Panel
commonInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, numericInput(ns("n"), label = "N: Train", value = 200, min = 10, step = 1)),
      column(6, numericInput(ns("n_test"), label = "N: Test", value = 50, min = 5, step = 1))
    ),
    fluidRow(
      column(6, numericInput(ns("p"), label = "N: Predictors", value = 15, min = 2, step = 1)),
      column(6, textInput(ns("q"), label = "Rel.Pred", value = "5, 4"))
    ),
    fluidRow(
      column(6, textInput(ns("R2"), label = "Coef. Determination", value = "0.8, 0.7")),
      column(6, textInput(ns("relpos"), label = "RelPos.Comp", 
                          value = "1, 2; 3, 4, 6"))
    ),
    fluidRow(
      column(12, sliderInput(ns("gamma"), "Gamma", 
                             min = 0, max = 1, value = 0.6, step = 0.1, width = "100%"))
    )
  )
}
commonInput <- function(input, output, session) {
}

## Multivariate Input Panel
multivariateInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, numericInput(ns("m"), label = "N: Response", value = 4, min = 2, step = 1)),
      column(6, textInput(ns("ypos"), label = "Response Mixup", 
                          value = "1, 3; 2, 4"))
    )
  )
}
multivariateInput <- function(input, output, session) {
}

## Bivariate Input Panel
bivariateInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, div(em("Correlation between Response")), style = "text-align:center;"),
      column(6, sliderInput(ns("rho1"), "Without Given X", 
                            min = -1, max = 1, value = 0.6, step = 0.1, width = "100%")),
      column(6, sliderInput(ns("rho2"), "With Given X", 
                            min = -1, max = 1, value = 0.7, step = 0.1, width = "100%"))
    )
  )
}
bivariateInput <- function(input, output, session) {
}
