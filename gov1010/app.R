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
library(ggtext)
library(shinythemes)
library(patchwork)

# Hypothetical grid of Harvard students
students <- expand.grid(1:15, 1:15) %>% 
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

# argument x doesn't do anything, but required for lapply
one_sample <- function(x) {
    rand_sample <- sample(1:nrow(students),
           size = nrow(students) / 4)
    round(mean(students[rand_sample,]$courses), 2)
}

many_samples <- function(n) {
    res <- lapply(1:n, one_sample)
    unlist(res)
}

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
                
    tags$head(includeCSS("www/app.css")),
    tags$link(
        rel = "stylesheet", 
        href="https://fonts.googleapis.com/css2?family=Alegreya+Sans+SC:wght@500&display=swap"
    ),
                
    # Application title
    titlePanel(tags$div(id='app_title',
                        "Gov 1010: Survey Research Methods")),
    hr(),
    h2("Hi! This is an instructional 
                application for Gov1010: Survey Research Methods.",
                          "Choose your topic below:"),
    br(),
                
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
            h1("Our first random sample", class = "smallcaps"),
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
                        "calculate how many courses the average Harvard student has taken.")
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
              a valid estimate of the true population statistic.",
              strong("This is the key point -- with SRS, we maintain
              representativeness of our target population.")),
            
            br(),
            
            p("So, let's generate a random sample of 25% of 
              the population. Since our statistic of interest
              is the ", strong("average"), "number of courses taken, 
              we can estimate
              the population mean by simply taking the
              average of the students that we sample."),
            
            br(),
            
            fluidRow(column(10, offset = 1, align = "center",
                            actionButton("random_sample", 
                                         "CLICK: Generate a random sample!"))),
            
            br(),
            
            fluidRow(column(12, align = "center",
                            plotOutput("samplePlot",
                                       width = "600px", height = "600px"))),
            br(),
            h1("Unbiasedness - many random samples converge to the truth", 
               class = "smallcaps"),
            p("How do we know that a random sample will give us
              a good estimate of the population mean? 
              If we were to take many random samples and find the
              sample average of each one", 
              strong("then the average of all of the sample
              averages would be, on average, equal to the 
              population mean."),
              "That is a very tricky sentence, I definitely 
              recommend reading it a few times! 
              We call estimators with this property",
              strong("unbiased"), "."),
            hr(),
            fluidRow(column(4, offset = 4, align = "center",
                            numericInput("unbiased_n_samples",
                                         "Enter a number of samples to take",
                                         value = 50, 
                                         min = 1, 
                                         max = 10000,
                                         step = 9))),
            fluidRow(column(12, align = "center",
                            plotOutput("unbiasedPlot",
                                       width = "600px", height = "500px"))),
            br(),
            hr(),
            h1(strong("Stratified Random Sampling"),
               class = "smallcaps"),
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
            geom_point(size = 3, pch = 18) + 
            theme_void() + 
            student_theme +
            labs(title = "Target Population: All Harvard Students")
    }, height = 600, width = 600)
    
    output$truthPlot <- renderPlot({
        students %>% 
            ggplot(aes(x = person, y = y)) + 
            geom_point(size = 3, pch = 18) + 
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
            geom_point(size = 3, pch = 18) +
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
    
    output$unbiasedPlot <- renderPlot({
        
        true_avg <- round(mean(students$courses), 2)
        vals <- many_samples(input$unbiased_n_samples)
        
        vals <- vals %>% 
            as_tibble()
        
        p1 <- vals %>% 
            ggplot(aes(x = value)) + 
            geom_histogram(binwidth = 0.5, fill = "#C6D8D3") + 
            geom_vline(xintercept = mean(students$courses), 
                       col = "#885053", 
                       lty = "dashed", lwd = 1) +
            geom_vline(xintercept = mean(vals$value), 
                       col = "#4D7298", 
                       lty = "dashed", lwd = 1) +
            theme_bw() +
            labs(x = "Sample Average",
                 y = "Count") + 
            student_theme + 
            xlim(c(true_avg - 5, true_avg + 5))
        
        p2 <- vals %>% 
            ggplot(aes(x = value)) + 
            geom_density(fill = "#C6D8D3", color = NA) + 
            geom_vline(xintercept = mean(students$courses), 
                       col = "#885053", 
                       lty = "dashed", lwd = 1) +
            geom_vline(xintercept = mean(vals$value), 
                       col = "#4D7298", 
                       lty = "dashed", lwd = 1) +
            theme_bw() + 
            labs(x = "Sample Average",
                 y = "Density") + 
            student_theme + 
            xlim(c(true_avg - 5, true_avg + 5))
        
        patch <- p1 + p2
        
        # https://stackoverflow.com/questions/61642489/colour-in-title-of-patchwork-of-ggplots-using-ggtext
        patch +
            plot_annotation(
                title = paste("True Average: <span style='color:#885053;'><strong>", true_avg, "</strong></span> <br>",
                        "Avg. of Sample Avgs: <span style='color:#4D7298;'><strong>", round(mean(vals$value), 2)),
                theme = theme(plot.title = element_markdown(lineheight = 1.1, 
                                                            size = 25, hjust = 0.5))) &
                    theme(axis.text = element_text(size = 17),
                              axis.title = element_markdown(size = 20)) 
        
        # p1 + p2 + 
        #     plot_annotation(caption = paste("True Average:", true_avg,
        #                                     "\n", "Avg. of Sample Avgs.:", round(mean(vals$value), 2)),
        #                     title = "Here <span style='color:#953011;'><strong>is a colourful title</strong></span>",
        #                     theme = theme(plot.title = element_markdown(lineheight = 1.1))) &
        #     theme(plot.title = element_text(hjust = 0.5, 
        #                                 vjust = 2,
        #                                 size = 20),
        #           axis.text = element_text(size = 15),
        #           axis.title = element_text(size = 15))
            # geom_text(aes(y = y + 0.5, 
            #               label = courses), 
            #           size = 4, show.legend = FALSE) +
            # scale_color_manual(name = "Sampled?",
            #                    values = c("grey", "black")) +
            # theme_void() + 
            # student_theme +
            # theme(legend.position = "top") +
            # labs(title = "25% Simple Random Sample",
            #      subtitle = "Target Population: All Harvard Students",
            # guides(color = guide_legend(override.aes = list(size=5)))
    }, height = 500, width = 600)
}

# Run the application 
shinyApp(ui = ui, server = server)
