## Aileen He
## Spring 2022
## Visually explore the interaction of variables from the 2014 Mental Health in Tech Survey

# This script read the shiny app script. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/library(rsconnect)


library(shiny)
library(tidyverse)
library(plotly)


#Using the cleaned dataset
survey <- read_csv("survey_cleaned.csv")

ui <- fluidPage(
    # Application title
    titlePanel("Mental Health in Tech Survey 2014"),
    
    # Sidebar with different selections
    sidebarLayout(
        sidebarPanel(
            ## Input: Age ----
            sliderInput(
                inputId = "age",
                label = "Age:",
                min = min(survey$Age),
                max = max(survey$Age),
                value = c(20, 40)
            ),
            ## Input: Gender ----
            selectInput(
                inputId = "gender",
                label = "Gender",
                choices = c("All", survey$Gender),
                selected = "All"
            ),
            ## Input: Self_Employed ----
            selectInput(
                inputId = "self_employed",
                label = "Self-employed?",
                choices = c("All", survey$self_employed),
                selected = "All"
            ),
            ## Input: Tech_Company ----
            selectInput(
                inputId = "tech_company",
                label = "Primarily a Tech Company?",
                choices = c("All", survey$tech_company),
                selected = "All"
            ),
            ## Input: Family_History ----
            selectInput(
                inputId = "family_history",
                label = "Family History of Mental Illness?",
                choices = c("All", survey$family_history),
                selected = "All"
            ),
            ## Input: Work_Interfere ----
            selectInput(
                inputId = "work_interfere",
                label = "Mental Health Condition interefece with Work?",
                choices = c("All", survey$work_interfere),
                selected = "All"
            ),
            ## Input: No_Employees ----
            selectInput(
                inputId = "no_employees",
                label = "Number of Employees in the Organization?",
                choices = c("All", survey$no_employees),
                selected = "All"
            )
        ),
        
        # Dashboard
        mainPanel(tabsetPanel(tabPanel(
            "Plot", plotlyOutput(outputId = "outputPlot")
        )))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    filters <- reactive({
        survey_filtered <- survey %>%
            filter(Age >= input$age[1],
                   Age <= input$age[2])
        if (input$gender != "All") {
            survey_filtered <- survey_filtered %>%
                filter(Gender == input$gender)
        }
        if (input$Self_employed != "All") {
            survey_filtered <- survey_filtered %>%
                filter(self_employed == input$Self_employed)
        }
        if (input$tech_company != "All") {
            survey_filtered <- survey_filtered %>%
                filter(tech_company == input$tech_company)
        }
        if (input$family_history != "All") {
            survey_filtered <- survey_filtered %>%
                filter(family_history == input$family_history)
        }
        if (input$work_interfere != "All") {
            survey_filtered <- survey_filtered %>%
                filter(work_interfere == input$work_interfere)
        }
        if (input$no_employees != "All") {
            survey_filtered <- survey_filtered %>%
                filter(no_employees == input$no_employees)
        }
        survey_filtered <- as.data.frame(survey_filtered)
    })
    
    output$outputPlot <- renderPlot({
        survey_final <- as.data.frame(filters())
        cnt <-
            survey_final %>% group_by(treatment) %>% summarise(n = n())
        plot_ly(
            cnt,
            labels = cnt$treatment,
            values = cnt$n,
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent'
        ) %>%
            layout(
                title = 'Percentage of Participants will seek for treatment',
                xaxis = list(
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticklabels = FALSE
                ),
                yaxis = list(
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticklabels = FALSE
                )
            )
        
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
