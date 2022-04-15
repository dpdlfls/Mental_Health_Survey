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
                label = "Age",
                min = min(survey$Age),
                max = max(survey$Age),
                value = c(20, 40)
            ),
            ## Input: Gender ----
            radioButtons(
                inputId = "gender",
                label = "Gender",
                choices = c("All", unique(survey$Gender)),
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
        mainPanel(tabsetPanel(
            tabPanel("Seek for Treatment", 
                     textOutput("treatment_txt"),
                     br(),
                     br(),
                     plotlyOutput(outputId = "treatmentPlot")),
            tabPanel("Take Leave", 
                     textOutput("leave_txt"),
                     br(),
                     br(),
                     plotlyOutput(outputId = "leavePlot")),
            tabPanel("Mental Health Consequences", 
                     textOutput("cons_txt"),
                     br(),
                     br(),
                     plotlyOutput(outputId = "consequencePlot")),
            tabPanel("Mental Health Interview", 
                     textOutput("interview_txt"),
                     br(),
                     br(),
                     plotlyOutput(outputId = "interviewPlot")),
            tabPanel("Mental vs Physical", 
                     textOutput("serious_txt"),
                     br(),
                     br(),
                     plotlyOutput(outputId = "seriousPlot"))
        ))
    )
)

# Define server logic required to draw a graph
server <- function(input, output) {
    

    filters <- reactive({
        
        #define variables
        input_minage <- input$age[1]
        input_maxage <- input$age[2]
        input_gender <- input$gender
        input_se <- input$self_employed
        input_tech <- input$tech_company
        input_hist <- input$family_history
        input_inter <- input$work_interfere
        input_ee <- input$no_employees
        
        data_react <- survey %>%
            filter(Age >= input_minage,
                   Age <= input_maxage)
        if (input_gender != "All") {
            data_react <- data_react %>%
                filter(Gender == input_gender)
        }
        if (input_se != "All") {
            data_react <- data_react %>%
                filter(self_employed == input_se)
        }
        if (input_tech != "All") {
            data_react <- data_react %>%
                filter(tech_company == input_tech)
        }
        if (input_hist != "All") {
            data_react <- data_react %>%
                filter(family_history == input_hist)
        }
        if (input_inter != "All") {
            data_react <- data_react %>%
                filter(work_interfere == input_inter)
        }
        if (input_ee != "All") {
            data_react <- data_react %>%
                filter(no_employees == input_ee)
        }
        data_react <- as.data.frame(data_react)
    })
    
    output$treatmentPlot <- renderPlotly({    
    
        count_treatment <- as.data.frame(filters()) %>% group_by(treatment) %>% summarise(cnt = n())
        
        colors <- c('rgb(200,94,96)', 'rgb(23,139,54)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
        
        fig <- plot_ly(count_treatment, labels = ~treatment, values = ~cnt, type = 'pie', 
                       textposition = 'inside',
                       textinfo = 'label+percent',
                       insidetextfont = list(color = '#FFFFFF'),
                       hoverinfo = 'text',
                       text = ~paste(treatment, cnt, 'counts'),
                       marker = list(colors = colors,
                                     line = list(color = '#FFFFFF', width = 1)),
                       showlegend = FALSE)
        fig <- fig %>% layout(title = 'Percentage of Participants will seek for Treatment',
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        fig

        
    })
    output$consequencePlot <- renderPlotly({    
        
        count_consq <- as.data.frame(filters()) %>% group_by(mental_health_consequence) %>% summarise(cnt = n())
        
        colors <- c('rgb(144,103,67)', 'rgb(171,104,187)','rgb(150,208,203)','rgb(200,94,96)', 'rgb(23,139,54)')
        
        fig <- plot_ly(count_consq, labels = ~mental_health_consequence, values = ~cnt, type = 'pie', 
                       textposition = 'inside',
                       textinfo = 'label+percent',
                       insidetextfont = list(color = '#FFFFFF'),
                       hoverinfo = 'text',
                       text = ~paste(mental_health_consequence, cnt, 'counts'),
                       marker = list(colors = colors,
                                     line = list(color = '#FFFFFF', width = 1)),
                       showlegend = FALSE)
        fig <- fig %>% layout(title = "Percentage of Negative Consequences",
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        fig
        
        
    })
    output$interviewPlot <- renderPlotly({    
        
        count_consq <- as.data.frame(filters()) %>% group_by(mental_health_interview) %>% summarise(cnt = n())
        
        colors <- c('rgb(144,103,67)', 'rgb(223,89,154)','rgb(171,104,187)','rgb(150,208,203)','rgb(200,94,96)')
        
        fig <- plot_ly(count_consq, labels = ~mental_health_interview, values = ~cnt, type = 'pie', 
                       textposition = 'inside',
                       textinfo = 'label+percent',
                       insidetextfont = list(color = '#FFFFFF'),
                       hoverinfo = 'text',
                       text = ~paste(mental_health_interview, cnt, 'counts'),
                       marker = list(colors = colors,
                                     line = list(color = '#FFFFFF', width = 1)),
                       showlegend = FALSE)
        fig <- fig %>% layout(title = "Percentage of Bring up Mental Issue in Interviews",
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        fig
        
        
    })
    output$seriousPlot <- renderPlotly({    
        
        count_consq <- as.data.frame(filters()) %>% group_by(mental_vs_physical) %>% summarise(cnt = n())
        
        colors <- c('rgb(200,94,96)','rgb(144,103,67)', 'rgb(171,104,187)','rgb(223,89,154)','rgb(150,208,203)')
        
        fig <- plot_ly(count_consq, labels = ~mental_vs_physical, values = ~cnt, type = 'pie', 
                       textposition = 'inside',
                       textinfo = 'label+percent',
                       insidetextfont = list(color = '#FFFFFF'),
                       hoverinfo = 'text',
                       text = ~paste(mental_vs_physical, cnt, 'counts'),
                       marker = list(colors = colors,
                                     line = list(color = '#FFFFFF', width = 1)),
                       showlegend = FALSE)
        fig <- fig %>% layout(title = "Percentage of Employer will take Mental Issue Serious",
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        fig
        
        
    })
    output$leavePlot <- renderPlotly({ 
        count_leave <- as.data.frame(filters()) %>% 
            group_by(leave) %>% 
            summarise(cnt = n())
        
        # Bar chart
        ggplot(count_leave, aes(x = leave, y = cnt)) + 
            geom_col(fill = 'pink') +
            geom_text(aes(label = scales::percent(cnt, accuracy = 0.01)), vjust = 0.5, color = "black", size = 2.5)+
            scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
            labs(title = element_text("Level of Difficulty to ask for a Leave")) +
            ylab("Percent of respondents")
    })
        output$treatment_txt <- renderText({
            "Have you sought treatment for a mental health condition?"
        })
        output$leave_txt<- renderText({
            "How easy is it for you to take medical leave for a mental health condition?"
        })
        output$cons_txt <- renderText({
            "Do you think that discussing a mental health issue with your employer would have negative consequences?"
            })
        output$interview_txt <- renderText({
            "Would you bring up a mental health issue with a potential employer in an interview?"
        })
        output$serious_txt <- renderText({
            "Do you feel that your employer takes mental health as seriously as physical health?"
        })
}

# Run the application
shinyApp(ui = ui, server = server)
