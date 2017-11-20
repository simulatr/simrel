## Load Packages ----
library(shiny)
library(simrel)

## Some Functions ----
evl <- function(x) {
  out <- lapply(strsplit(unlist(strsplit(x, ";")), ","), as.numeric)
  if (length(out) == 1) out <- out[[1]]
  return(out)
}

## Sidebar ----
sidebar <- sidebarPanel(
  fluidRow(
    column(12, actionButton('simulate', label = "Simulate Data", class="btn-primary", icon = icon("refresh"), width = '100%')),
    column(12, hr())
  ),
  fluidRow(
    column(6, numericInput("n", "Number of observation", 100, min = 10, width = "100%")),
    column(6, numericInput("p", "Number of predictors", 10, min = 2, width = "100%"))
  ),
  fluidRow(
    column(6, numericInput("m", "Number of responses", 3, min = 2, width = "100%")),
    column(6, numericInput("seed", "seed", 123, min = 2, width = "100%"))
  ),
  fluidRow(
    column(6, textInput("R2", "Coefficient of Determination", value = "0.7, 0.8", width = '100%')),
    column(6, textInput("q", "Number of relevant predictors", value = "5, 4", width = "100%"))
  ),
  fluidRow(
    column(6, textInput("relpos", "Position of Relevant components", value = "1, 2; 3, 4, 6", width = '100%')),
    column(6, textInput("ypos", "Position of response components", value = "1; 2, 3", width = '100%'))
  ),
  fluidRow(
    column(6, sliderInput("gamma", "Decay factor of eigenvector of predictors", 
                          value = 0.5, min = 0, max = 3, width = '100%', step = 0.01)),
    column(6, sliderInput("eta", "Decay factor of eigenvector of response", 
                          value = 0.5, min = 0, max = 3, width = '100%', step = 0.01))
  )
)


## Main panel ----
main <- mainPanel(
  div(
    class = "grid-container",
    id = "parent-container",
    div(
      class = "grid-content",
      id = "child1",
      div(
        h4("Estimated covariance of response"),
        div("Covariance of simulated response"),
        div(code("cov(sim.obj[['Y']]"))
      ),
      verbatimTextOutput("est_cov_resp")
    ),
    div(
      class = "grid-content",
      id = "child2",
      div(
        h4("True covariance of response"),
        div("Covariance of response extracted from true sigma matrix"),
        div(code("cov(sim.obj[['Sigma']][1:m, 1:m])"))
      ),
      verbatimTextOutput("true_cov_resp")
    ),
    div(
      class = "grid-content",
      id = "child3",
      div(
        h4("Estimated coefficient of determination"),
        withMathJax(
          div("\\(R^2\\) obtained from", code("lm"))
        )
      ),
      verbatimTextOutput("est_coef_det_y")
    ),
    div(
      class = "grid-content",
      id = "child4",
      div(
        withMathJax(
          h5("\\[\\rho_y^2 = \\Sigma_{xy}^t\\Sigma_{xx}^{-1}\\Sigma_{xy}\\left[\\text{diag(diag(}\\Sigma_{yy}\\text{))}\\right]^{-1}\\]")
        )
      ),
      verbatimTextOutput("true_coef_det_y")
    )
  )
)

## App UI Start ----
ui <- navbarPage(
  fluid = FALSE,
  title = "Testing Multivariate Simulation",
  theme = shinythemes::shinytheme("yeti"),
  tabPanel("Home", icon = icon("home")),
  tags$head(tags$link(href="style.css", rel="stylesheet")),
  sidebarLayout(
    sidebarPanel = sidebar,
    mainPanel = main
  )
)

## App Server Start ----
server <- function(input, output, session) {
  ## Simulation Parameters ----
  opts <- reactive({
    list(
      n = input$n,
      p = input$p,
      q = evl(input$q),
      relpos = evl(input$relpos),
      gamma = input$gamma,
      R2 = evl(input$R2),
      ypos = evl(input$ypos),
      m = input$m,
      eta = input$eta,
      type = "multivariate"
    )
  })
  
  ## Simulated Object ----
  sobj <- eventReactive(input$simulate, {
    set.seed(input$seed)
    do.call(simrel, opts())
  })
  
  ## Fitted Model
  mdl <- reactive({
    dta <- data.frame(
      y = I(sobj()[["Y"]]),
      x = I(sobj()[["X"]])
    )
    return(lm(y ~ x, data = dta))
  })
  
  ## Covariance of Response ----
  output$est_cov_resp <- renderPrint({
    round(var(sobj()$Y), 4)
  })
  output$true_cov_resp <- renderPrint({
    sigma <- sobj()$Sigma
    m <- sobj()$m
    round(sigma[1:m, 1:m], 4)
  })
  
  ## Coefficient of Determination ----
  output$est_coef_det_y <- renderPrint({
    sapply(summary(mdl()), function(x) {
      round(unname(x[["r.squared"]]), 3)
    })
  })
  
  output$true_coef_det_y <- renderPrint({
    sobj()[["RsqY"]]
  })
}

shinyApp(ui, server)