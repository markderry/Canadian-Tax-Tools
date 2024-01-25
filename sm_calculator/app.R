library(shiny)
library(data.table)
library(plotly)
source("smith_analysis.R")

options(scipen = 999)

employment_income <- c(50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000)
home_value <- c(500000, 1000000, 1500000, 2000000, 2500000, 3000000)
mortgage_balance <- c(100000, 200000, 300000, 400000, 500000, 1000000, 1500000, 2000000, 2500000)
loc_interest_rate <- c(0.03, 0.04, 0.05, 0.06, 0.07, 0.08)
dividend_yeild <- c(0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12)
dividend_type_split <- c(0, 0.25, 0.5, 0.75, 1)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Modified Smith Maneuver"),
  
  tabsetPanel(
    tabPanel(
      "Custom Inputs",
      # Sidebar with a slider input for number of bins 
      # create sidebar
      sidebarLayout(
        sidebarPanel(width = 2,
                     # Input variables
                     numericInput("employment_income", label = "Employment Income:", value = 100000),
                     numericInput("home_value", label = "Home Value:", value = 1000000),
                     numericInput("mortgage_balance", label = "Mortgage Balance:", value = 500000),
                     numericInput("dividend_type_split", label = "Dividend Type Split:", value = 0.5),
                     numericInput("dividend_yeild", label = "Dividend Yeild:", value = 0.09),
                     numericInput("loc_interest_rate", label = "LOC Interest Rate:", value = 0.07)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tableOutput("results_table"),
        )
      )
    ),
    tabPanel(
      "Solution Space",
      # filter panel
      sidebarLayout(
        sidebarPanel(width = 2,
                     selectInput(
                       "employment_income_selection",
                       label = "Employment Income",
                       choices = employment_income
                     ),
                     selectInput(
                       "home_value_selection",
                       label = "Home Value",
                       choices = home_value
                     ),
                     selectInput(
                       "mortgage_balance_selection",
                       label = "Mortgage Balance",
                       choices = mortgage_balance
                     ),
                     selectInput(
                       "loc_interest_rate_selection",
                       label = "LOC Interest Rate",
                       choices = loc_interest_rate
                     ),
                     selectInput(
                       "dividend_yeild_selection",
                       label = "Dividend Yeild",
                       choices = dividend_yeild
                     ),
                     selectInput(
                       "dividend_type_split_selection",
                       label = "Dividend Type Split",
                       choices = dividend_type_split
                     )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          # show plot and fill main panel
          plotlyOutput(outputId = "plot")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe(
    input$run
  )
  
  # create table
  output$results_table <- renderTable({
    employment_income <- input$employment_income
    home_value <- input$home_value
    mortgage_balance <- input$mortgage_balance
    loc_interest_rate <- input$loc_interest_rate
    loc_ballance <- 0.65 * (home_value - mortgage_balance)
    dividend_yeild <- input$dividend_yeild
    dividend_type_split <- input$dividend_type_split
    
    for (i in 1:2) {
      if (i == 1) {
        interest_expense <- loc_interest_rate * loc_ballance
        dividend_income <- loc_ballance * dividend_yeild
        scenarios <- calculate_tax_liability_sm(employment_income = employment_income,
                                                home_value = 0,
                                                mortgage_balance = 0,
                                                dividend_yeild = 0,
                                                dividend_type_split = 0,
                                                loc_interest_rate = 0)
      } else {
        scenarios <- rbind(scenarios, 
                           calculate_tax_liability_sm(employment_income = employment_income,
                                                      home_value = home_value,
                                                      mortgage_balance = mortgage_balance,
                                                      dividend_yeild = dividend_yeild,
                                                      dividend_type_split = dividend_type_split,
                                                      loc_interest_rate = loc_interest_rate)
        )
      }
    }
    # convert all columns to dollar format
    scenarios[, employment_income := dollar(employment_income, big.mark = ",", accuracy = 1)]
    scenarios[, dividend_income := dollar(dividend_income, big.mark = ",", accuracy = 1)]
    scenarios[, interest_expense := dollar(interest_expense, big.mark = ",", accuracy = 1)]
    scenarios[, income_tax := dollar(income_tax, big.mark = ",", accuracy = 1)]
    scenarios[, div_tax := dollar(div_tax, big.mark = ",", accuracy = 1)]
    scenarios[, tax_savings_from_interest := dollar(tax_savings_from_interest, big.mark = ",", accuracy = 1)]
    scenarios[, net_income := dollar(net_income, big.mark = ",", accuracy = 1)]
    scenarios[, sm_effective_value := dollar(sm_effective_value, big.mark = ",", accuracy = 1)]
  },
  striped = TRUE,
  hover = TRUE,
  width = "100%"
  )
  output$plot <- renderPlotly(
    # plot results 
     plot_ly(out[employment_income == input$employment_income_selection & 
                                 home_value == input$home_value_selection &
                                 dividend_yeild == input$dividend_yeild_selection &
                                 dividend_type_split == input$dividend_type_split_selection, ], 
                           x = ~loc_balance, 
                           y = ~sm_effective_value, 
                           color = ~loc_interest_rate, 
                           type = 'scatter', 
                           mode = 'lines+markers', 
                           split = ~loc_interest_rate, 
                           height = 800
             )
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
