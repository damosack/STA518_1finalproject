#loading packages
library(shiny)
library(tidyverse)
library(ggExtra)
library(here)

#reading in dataset
MovieData <- read_csv(here::here("MovieData.csv"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bechdel Test Movie Data"),

    # row with first plot and inputs for x-axis
    # y-axis, and color, where color can be any categorical
   fluidRow(
       column(6, plotOutput("Plot")),
       column(6, style='padding-bottom:0px;', selectInput("y",
                        "y-axis",
                        choices = c("budget",
                                    "logbudget",
                                    "revenue",
                                    "logrevenue",
                                    "year",
                                    "runtime",
                                    "votes", 
                                    "TMDB_rating",
                                    "popularity"),
                        selected = "revenue"
                        ),
                   selectInput("x",
                        "x-axis",
                        choices = c("budget",
                                    "logbudget",
                                    "revenue", 
                                    "logrevenue",
                                    "year",
                                    "runtime",
                                    "votes",
                                    "TMDB_rating",
                                    "popularity",
                                    "language",
                                    "bechdel_pass",
                                    "primary_genre"),
                        selected = "primary_genre"
                        ),
              selectInput("z",
                          "color by",
                          choices= c("language",
                                     "bechdel_pass",
                                     "primary_genre"),
                          selected = "bechdel_pass"
              )
        )),

        # bottom row for possible histogram (categorical x)
        # and dataset. style= ' ' here allows me to adjust the plots in the left
        # column to line up vertically as close as possible, and bumps to top of
        # the dataset a bit higher. 
   
        fluidRow( column(6, style='padding-left:28px;',
                         plotOutput("histx", width = "85%")),
           
                  column(6, style='padding-top:0px;',
                         DT::dataTableOutput("moviesTable")
        )
    ))



server <- function(input, output) {

    #deciding what plots to show based on whether x is categorical
    #ggExtra::ggMarginal allows inclusion of marginal distribution plots.

    output$Plot <- renderPlot({
        if(input$x %in% c("bechdel_pass", "primary_genre", "language")) {
            ggMarginal((
                ggplot(data= MovieData, 
                   aes_string(x= input$x, y= input$y, color= input$z)) +
                geom_boxplot() +
                    #adding invisible scatter points allows the marginal y distribution.
                    geom_point(alpha=0) +
                    #theme rotates axis labels and changes their distance from axis
                    #also changed placement and orientation of legend for color
                    theme(axis.text.x = element_text(angle=45, hjust=1), legend.position = "bottom", legend.box = "horizontal")
                ),
                type="histogram",
                margins = "y")
        } else {
            ggMarginal((
                ggplot(data= MovieData,
                   aes_string(x= input$x, y= input$y, color=input$z)) +
                geom_point() +
                    theme(legend.position= "bottom", legend.box= "horizontal")
                ),
                type= "histogram")
        }
    })
    output$histx <- renderPlot({
        if(input$x %in% c("bechdel_pass", "primary_genre", "language")) {
        ggplot(data= MovieData,
               aes_string(x= input$x)) +
               geom_bar() +
                theme(axis.text.x = element_text(angle=45, hjust=1))
        }
    })
    # dataset showing columns for title, ID, & variables selected by user
       output$moviesTable <- DT::renderDataTable({
           DT::datatable(data=select(MovieData, "title", "TMDB_ID", "bechdel_pass", input$x, input$y),
                             options = list(pageLength= 10),
                             rownames= F)
    })

}



# Run the application 
shinyApp(ui = ui, server = server)
