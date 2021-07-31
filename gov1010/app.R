#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
library(patchwork)

# Hypothetical grid of Harvard students
students <- expand.grid(1:25, 1:25) %>% 
    rename(person = Var1,
           y = Var2) %>% 
    mutate(id = 1:n(),
           courses = sample(4:42, size = n(), replace = TRUE))

true_avg <- round(mean(students$courses), 2)

# Theme for student grid
student_theme <- theme(plot.title = element_text(hjust = 0.5,
                                                 size = 25),
                       plot.subtitle = element_text(hjust = 0.5,
                                                    size = 20),
                       plot.caption = element_text(hjust = 0.925, 
                                                   vjust = 2,
                                                   size = 20),
                       legend.title = element_text(size = 15),
                       legend.text = element_text(size = 15))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
                
        tags$head(
            tags$link(rel = "stylesheet", 
                      type = "text/css", 
                      href = "app.css")
        ),
                
    # Application title
    titlePanel("Gov 1010: Survey Research Methods"),
    
    p("Hi! This is an instructional application for 
    Gov1010: Survey Research Methods"),
    p("Choose your topic below:"),
                
    #############################################################
    ## Random Sampling
    
    tabsetPanel(
    tabPanel("Random Sampling",

    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
        # sidebarPanel(
        #     p("Input below"),
        #     sliderInput("bins",
        #                 "Number of bins:",
        #                 min = 1,
        #                 max = 50,
        #                 value = 30)
        # ),

        # Show a plot of the generated distribution
        
    fluidRow(column(10, offset = 1,
                    br(),
        wellPanel(
            h2("Our first random sample"),
            p("Big news -- disaster has struck and ",
              em("my.harvard"),
              "is down during course selection for the spring
                semester. The IT Departmment reports that the situation
                looks bleak -- hackers have brought down the system
                and service won't returned for several weeks. ",
              a(href = 'https://www.fas.harvard.edu/pages/dean-claudine-gay', 
                                'Dean Claudine Gay', 
                                .noWS = "outside"), 
              " (Dean of the Faculty of Arts and Sciences at Harvard), 
              has announced a new survey initiative to understand the
              needs and opinions of Harvard undergraduates.", 
              .noWS = c("after-begin", "before-end")),
            
            p("Knowing that you are enrolled in Gov1010 (and doing
              very well by the way), she calls you to ask for
              survey advice. President Bacow, ever the pragmatist,
              has placed the following constraints on what the
              survey is able to do:"),
            tags$ul(
                tags$li("Survey must be", 
                        strong("representative of all 
                        Harvard undergraduates.")), 
                tags$li("We can survey", 
                         code("25%"), 
                        " of the undergraduate population."),
                tags$li(strong("Goal:"),
                        "how many courses has the average Harvard student taken?")
            ),
            
            p("So, what should we do? First, we know that our",
              strong("target population"), "is all Harvard
              undergraduates. We can visualize our 
              target population below:"),
            
            br(),
            
            fluidRow(column(12, align = "center",
                plotOutput("studentPlot", 
                           width = "600px", height = "600px"))),
                br(),
            
            p("We also know that, in reality,
                  every student has some true response -- 
                  the number of courses they have taken. 
                  That's what we want to find out!"),
            
            br(),
            
            fluidRow(column(12, 
                            align = "center",
                plotOutput("truthPlot",
                           width = "600px", height = "600px"))),
            
            br(),
            
            p("You suggest",
              strong("simple random sampling"), 
              " (SRS), which will randomly sample 25% of the students
              such that",
              strong("every student has the same probability of
                     being chosen."),
              "So, with a large enough random sample from the
              target population (how large is
              \"large enough\"? Stay tuned.), we can achieve
,              a valid estimate of the true population statistic.",
              strong("This is the key point -- with SRS, we maintain
              representativeness of our target population.")),
            
            br(),
            
            p("So, let's generate a random sample of 25% of 
              the population. Since our statistic of interest
              is the ", strong("average"), "number of courses taken, 
              we can estimate
              the population mean by simply taking the
              average of the students that we sample."),
            
            fluidRow(column(10, offset = 1, align = "center",
                            actionButton("random_sample", 
                                         "CLICK: Generate a random sample!"))),
            
            br(),
            
            fluidRow(column(12, align = "center",
                            plotOutput("samplePlot",
                                       width = "600px", height = "600px"))),
            br(),
            hr(),
            h2("Stratified Random Sampling"),
        )))
    ), # End Random Sampling
    #############################################################
    tabPanel("Other topics", 
             
             titlePanel("Coming soon!"))
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$studentPlot <- renderPlot({
    students %>% 
            ggplot(aes(x = person, y = y)) + 
            geom_point(size = 2, pch = 18) + 
            theme_void() + 
            student_theme +
            labs(title = "Target Population: All Harvard Students")
    }, height = 600, width = 600)
    
    output$truthPlot <- renderPlot({
        students %>% 
            ggplot(aes(x = person, y = y)) + 
            geom_point(size = 2, pch = 18) + 
            geom_text(aes(y = y + 0.5, 
                          label = courses), size = 4) +
            theme_void() + 
            student_theme +
            labs(title = "Target Population: All Harvard Students",
                 subtitle = "How many Harvard courses \n have you taken?",
                 caption = paste("True Average:", true_avg))
    }, height = 600, width = 600)
    
    take_sample <- eventReactive(input$random_sample, {
        sample(1:nrow(students),
               size = nrow(students) / 4)
    })
    
    output$samplePlot <- renderPlot({
        
        rand_sample <- take_sample()
        est_avg <- round(mean(students[rand_sample,]$courses), 2)
        
        students %>% 
            ggplot(aes(x = person, y = y, 
                       color = id %in% rand_sample)) + 
            geom_point(size = 2, pch = 18) +
            geom_text(aes(y = y + 0.5, 
                          label = courses), 
                      size = 4, show.legend = FALSE) +
            scale_color_manual(name = "Sampled?",
                               values = c("grey", "black")) +
            theme_void() + 
            student_theme +
            theme(legend.position = "top") +
            labs(title = "25% Simple Random Sample",
                 subtitle = "Target Population: All Harvard Students",
                 caption = paste("True Average:", true_avg,
                                 "\n", "Estimate:", est_avg)) + 
            guides(color = guide_legend(override.aes = list(size=5)))
    }, height = 600, width = 600)
}

# Run the application 
shinyApp(ui = ui, server = server)
