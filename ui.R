library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Confidence Intervals for Mean of Exponential Distribution"),
  
  helpText("Click Refresh or change trials, samples or mean for a new set of 
           random trials.  Changing coverage rate will recalculate the 
           confidence intervals for the current data."),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("action", label="Refresh"),
      sliderInput("cr",
                  "Coverage rate:",min = 0.01,max = 0.99,value = 0.95),
      sliderInput("trials",
                  "Number of trials:",min = 10,max = 1000,value = 100),
      sliderInput("size",
                  "Samples in each trial:",min = 2,max = 200,value = 20),
      sliderInput("mean",
                  "Mean:",min = 0.1,max = 10.1,value = 2)
    ),    
    mainPanel(
      tabsetPanel(type = "tabs",  
                tabPanel("Plots", 
                         plotOutput("mean.unknown.Plot")), 
                tabPanel("Interval Data", 
                         tableOutput("interval"))
      ),
      br(),
      p("Code ",
        a("here,", 
          href = "https://github.com/lawsonb/CI-exponential", 
          target="_blank"),
        " equations for confidence intervals ",
        a("here.", 
          href = "https://github.com/lawsonb/CI-exponential/blob/master/CI-exponential.pdf", 
          target="_blank")
      )
    )
  )
))
