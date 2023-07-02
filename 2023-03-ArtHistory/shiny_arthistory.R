###############################################################################
#Shiny

if(!require(shiny, quietly=T)) install.packages("shiny", repos = "https://cloud.r-project.org/")
library(shiny)
if(!require(shinydashboard, quietly=T)) install.packages("shinydashboard", repos = "https://cloud.r-project.org/")
library(shinydashboard, quietly=T)
if(!require(shinyWidgets, quietly=T)) install.packages("shinyWidgets", repos = "https://cloud.r-project.org/")
library(shinyWidgets, quietly=T)
if(!require(rsconnect, quietly=T)) install.packages("rsconnect", repos = "https://cloud.r-project.org/")
library(rsconnect, quietly=T)

source("arthistory.R")

header <- dashboardHeader(title="Diversity in American art history discourse", titleWidth = 450)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Nationality", tabName = "nationality", icon = icon("globe")),
    menuItem("Gender", tabName = "gender", icon = icon("venus-mars")),
    menuItem("Race", tabName = "race", icon = icon("hand-holding-hand"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "nationality",
            fluidRow(
              tabBox(id="tab_nationality", width=12, height=800,
                     tabPanel("Exhibtions",
                              prettyRadioButtons(
                                inputId = "exbn_selector1a",
                                label = "Choose Exhibition", 
                                choices = c("MoMA"="moma_count_to_year", 
                                            "Whitney"="whitney_count_to_year"),
                                inline = TRUE, 
                                status = "success",
                                fill = TRUE,
                                selected = "moma_count_to_year"
                              ),
                              plotOutput("plot1a")),
                     tabPanel("Page area",
                              dropdownButton(
                                tags$h4("List of Inputs"),
                                br(),
                                selectInput("book_selector1b",
                                            "Select Book",
                                            choices = c("All" = "All",
                                                        "History of Art (H. W. Janson)" = "Janson",
                                                        "Art Through the Ages (Helen Gardner)" = "Gardner"),
                                            selected = "All"),
                                
                                sliderInput(inputId = 'n_nationalities1b',
                                            label = 'Nationalities: Show Top ___',
                                            value = 35,
                                            min = 5,
                                            max = 50),
                                
                                prettySwitch(inputId = "show_artists1b",
                                             label = "Show top 50 artists.",
                                             value = T,
                                             status = "success",
                                             fill = TRUE),
                                
                                circle = TRUE, status = "danger",
                                icon = icon("gear"), width = "300px",
                                
                                tooltip = tooltipOptions(title = "Click to modify graph parameters!")
                              ),
                              plotOutput("plot1b")),
                     tabPanel("Frequency",
                              dropdownButton(
                                sliderInput(inputId = 'n_nationalities1c',
                                            label = 'Nationalities: Show Top ___',
                                            value = 8,
                                            min = 5,
                                            max = 50),
                                
                                circle = TRUE, status = "danger",
                                icon = icon("gear"), width = "300px",
                                
                                tooltip = tooltipOptions(title = "Click to modify graph parameters!")
                              ),
                              br(),
                              selectInput("book_selector1c",
                                          "Select Book",
                                          choices = c("All" = "All",
                                                      "History of Art (H. W. Janson)" = "Janson",
                                                      "Art Through the Ages (Helen Gardner)" = "Gardner"),
                                          selected = "All"),
                              plotOutput("plot1c"))
              )
            )
    ),
    tabItem(tabName = "gender",
            fluidRow(
              tabBox(id="tab_gender", width=12, height=800,
                     tabPanel("Exhibtions",
                              prettyRadioButtons(
                                inputId = "exbn_selector2a",
                                label = "Choose Exhibition", 
                                choices = c("MoMA"="moma_count_to_year", 
                                            "Whitney"="whitney_count_to_year"),
                                inline = TRUE, 
                                status = "success",
                                fill = TRUE,
                                selected = "moma_count_to_year"
                              ),
                              plotOutput("plot2a")),
                     tabPanel("Page area",
                              dropdownButton(
                                tags$h4("List of Inputs"),
                                br(),
                                selectInput("book_selector2b",
                                            "Select Book",
                                            choices = c("All" = "All",
                                                        "History of Art (H. W. Janson)" = "Janson",
                                                        "Art Through the Ages (Helen Gardner)" = "Gardner"),
                                            selected = "All"),
                                
                                prettySwitch(inputId = "show_artists2b",
                                             label = "Show top 50 artists.",
                                             value = T,
                                             status = "success",
                                             fill = TRUE),
                                
                                circle = TRUE, status = "danger",
                                icon = icon("gear"), width = "300px",
                                
                                tooltip = tooltipOptions(title = "Click to modify graph parameters!")
                              ),
                              plotOutput("plot2b")),
                     tabPanel("Frequency",
                              selectInput("book_selector2c",
                                          "Select Book",
                                          choices = c("All" = "All",
                                                      "History of Art (H. W. Janson)" = "Janson",
                                                      "Art Through the Ages (Helen Gardner)" = "Gardner"),
                                          selected = "All"),
                              plotOutput("plot2c"))
              )
            )
    ),
    tabItem(tabName = "race",
            fluidRow(
              tabBox(id="tab_race", width=12, height=800,
                     tabPanel("Exhibtions",
                              prettyRadioButtons(
                                inputId = "exbn_selector3a",
                                label = "Choose Exhibition", 
                                choices = c("MoMA"="moma_count_to_year", 
                                            "Whitney"="whitney_count_to_year"),
                                inline = TRUE, 
                                status = "success",
                                fill = TRUE,
                                selected = "moma_count_to_year"
                              ),
                              plotOutput("plot3a")),
                     tabPanel("Page area",
                              dropdownButton(
                                tags$h4("List of Inputs"),
                                br(),
                                selectInput("book_selector3b",
                                            "Select Book",
                                            choices = c("All" = "All",
                                                        "History of Art (H. W. Janson)" = "Janson",
                                                        "Art Through the Ages (Helen Gardner)" = "Gardner"),
                                            selected = "All"),
                                
                                prettySwitch(inputId = "show_artists3b",
                                             label = "Show top 50 artists.",
                                             value = T,
                                             status = "success",
                                             fill = TRUE),
                                
                                circle = TRUE, status = "danger",
                                icon = icon("gear"), width = "300px",
                                
                                tooltip = tooltipOptions(title = "Click to modify graph parameters!")
                              ),
                              plotOutput("plot3b")),
                     tabPanel("Frequency",
                              selectInput("book_selector3c",
                                          "Select Book",
                                          choices = c("All" = "All",
                                                      "History of Art (H. W. Janson)" = "Janson",
                                                      "Art Through the Ages (Helen Gardner)" = "Gardner"),
                                          selected = "All"),
                              plotOutput("plot3c"))
              )
            )
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin="black")

server <- function(input,output) {
  output$plot1a <- renderPlot({
    plot1a_render(input$exbn_selector1a)
  }, height=650)
  output$plot2a <- renderPlot({
    plot2a_render(input$exbn_selector2a)
  }, height=650)
  output$plot3a <- renderPlot({
    plot3a_render(input$exbn_selector3a)
  }, height=650)
  output$plot1b <- renderPlot({
    plot1b_render(input$book_selector1b,input$n_nationalities1b,input$show_artists1b)
  }, height=650)
  output$plot2b <- renderPlot({
    plot2b_render(input$book_selector2b,input$show_artists2b)
  }, height=650)
  output$plot3b <- renderPlot({
    plot3b_render(input$book_selector3b,input$show_artists3b)
  }, height=650)
  output$plot1c <- renderPlot({
    plot1c_render(input$book_selector1c,input$n_nationalities1c)
  }, height=600)
  output$plot2c <- renderPlot({
    plot2c_render(input$book_selector2c)
  }, height=300)
  output$plot3c <- renderPlot({
    plot3c_render(input$book_selector3c)
  }, height=400)
}

shinyApp(ui, server)
