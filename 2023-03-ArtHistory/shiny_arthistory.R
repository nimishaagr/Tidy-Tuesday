###############################################################################
#Shiny
source("arthistory.R")

if(!require(shiny, quietly=T)) install.packages("shiny", repos = "https://cloud.r-project.org/")
library(shiny)
if(!require(shinydashboard, quietly=T)) install.packages("shinydashboard", repos = "https://cloud.r-project.org/")
library(shinydashboard, quietly=T)
if(!require(shinyWidgets, quietly=T)) install.packages("shinyWidgets", repos = "https://cloud.r-project.org/")
library(shinyWidgets, quietly=T)

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
                              plotOutput("plot1a")),
                     tabPanel("Page area",
                              dropdownButton(
                                tags$h3("List of Inputs"),
                                
                                selectInput("bookSelector",
                                            "Select Book",
                                            choices = c("All" = "All",
                                                        "History of Art (H. W. Janson)" = "Janson",
                                                        "Art Through the Ages (Helen Gardner)" = "Gardner"),
                                            selected = "All"),
                                
                                sliderInput(inputId = 'n_nationalities',
                                            label = 'Nationalities: Show Top ___',
                                            value = 35,
                                            min = 5,
                                            max = 50),
                                
                                prettySwitch(inputId = "show_artists",
                                             label = "Show top 20 artists' names",
                                             value = T,
                                             status = "success",
                                             fill = TRUE),
                                
                                circle = TRUE, status = "danger",
                                icon = icon("gear"), width = "300px",
                                
                                tooltip = tooltipOptions(title = "Click to modify graph parameters!")
                              ),
                              plotOutput("plot1b")),
                     tabPanel("Frequency",
                              plotOutput("plot1c"))
              )
            )
    ),
    tabItem(tabName = "gender",
            fluidRow(
              tabBox(id="tab_gender", width=12, height=800,
                     tabPanel("Exhibtions",
                              plotOutput("plot2a")),
                     tabPanel("Page area",
                              plotOutput("plot2b")),
                     tabPanel("Frequency",
                              plotOutput("plot2c"))
              )
            )
    ),
    tabItem(tabName = "race",
            fluidRow(
              tabBox(id="tab_race", width=12, height=800,
                     tabPanel("Exhibtions",
                              plotOutput("plot3a")),
                     tabPanel("Page area",
                              plotOutput("plot3b")),
                     tabPanel("Frequency",
                              plotOutput("plot3c"))
              )
            )
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin="black")

server <- function(input,output) {
  output$plot1b <- renderPlot({
    
  })
}

shinyApp(ui, server)


#     tabItem(tabName = "nationality",
#       fluidRow(
#         box(
#           width=8,
#           title="Select Book",
#           status = "warning", solidHeader = TRUE,
#           selectInput("bookSelector",
#                       "", 
#                       choices = c(
#                         "All" = "All",
#                         "History of Art (H. W. Janson)" = "Janson",
#                         "Art Through the Ages (Helen Gardner)" = "Gardner"),
#                       selected = "All"),
#           "Horst Woldemar Janson (1913–1982), was a Russian Empire-born German-American professor of art history.", br(),
#           "Helen Gardner (1878–1946) was an American art historian and educator.", br(), 
#           "Their respective books have many editions over the years and have remained a standard text for American art history classes."
#           )
#         ),
#       fluidRow(
#         box(plotOutput("plot1a"), width=5,
#             title="How many times do artists of different nationalities feature in the book(s) across the years?",
#             status = "primary", solidHeader = TRUE
#             ),
#         box(plotOutput("plot1b"), width=7,
#             title="Average space per page given to different artists across editions",
#             status = "primary", solidHeader = TRUE
#             )
#         )
#       )
#     )
#   )
# 
# server <- function(input, output) { 
#    
#   output$plot1a <- renderPlot({
#     art_nation_count <- arthistory |>
#       filter(if (input$bookSelector!="All") {book==input$bookSelector} else {TRUE}, 
#              artist_nationality_other!="Other") |>
#       mutate(year=year(year)) |>
#       group_by(artist_nationality_other, year) |>
#       summarise(feature_count=n()) |>
#       ungroup() 
#     
#     art_nation_count |>
#       ggplot(aes(x=year, y=feature_count, colour=artist_nationality_other)) +
#       geom_line(linewidth=1) +
#       geom_point(size=1) +
#       geom_label_repel(aes(label = artist_nationality_other),
#                        data = art_nation_count |> filter(year == max(year)),
#                        size = 4) +
#       theme_linedraw() +
#       labs(y="Count",
#            x="Textbook Edition Year") +
#       theme(legend.position = "none")
#   })
#   
#   output$plot1b <- renderPlot({
#     art_nation_space <- arthistory |>
#       filter(if (input$bookSelector!="All") {book==input$bookSelector} else {TRUE}, 
#              !str_detect(artist_name,"N/A")) |>
#       group_by(artist_nationality_other, artist_name) |>
#       summarise(space_ratio=mean(space_ratio_per_page_total)) |>
#       ungroup() 
#     
#     top_10_artists <- art_nation_space |>
#       group_by(artist_nationality_other) |>
#       slice_max(space_ratio, n=10)
#     
#     art_nation_space |>
#       ggplot(aes(area=space_ratio, 
#                  subgroup=artist_nationality_other,
#                  fill=artist_nationality_other,
#                  label=ifelse(artist_name %in% (top_10_artists$artist_name),artist_name, ""))
#       ) +
#       geom_treemap() +
#       geom_treemap_subgroup_text(place = "centre", size=12, grow = F, alpha = 0.5, fontface = "bold") +
#       geom_treemap_text(fontface = "italic", place = "centre", size=5, grow=F, reflow=T) +
#       theme(legend.position="none") +
#       scale_fill_brewer(palette = "Pastel1")
#   })
# }

