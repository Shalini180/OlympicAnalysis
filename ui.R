library(shiny)
library(plotly)
library(dplyr)
library(shinythemes)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(knitr)
library(bslib)
shinyUI(
  
  dashboardPage(
    dashboardHeader(title = "OLYMPIC GAMES"),
    
    dashboardSidebar(sidebarMenu(
      menuItem("Characteristics_Of_Players",tabName="Characteristics_Of_Players",icon=icon("tree")),
      menuItem("Characteristic_Of_Tournament",tabName="Characteristic_Of_Tournament",icon=icon("car")),
      menuItem("History_of_Medal",tabName="History_of_Medal",icon=icon("medal")),
      menuItem("Influence_of_GDP",tabName="Influence_of_GDP",icon=icon("money")),
      menuItem("Sports_vs_Medal",tabName="Sports_vs_Medal",icon=icon("running")),
      menuItem("Summer_olympics",tabName="Summer_olympics",icon=icon("temperature-high")),
      menuItem("Winter_olympics",tabName="Winter_olympics",icon=icon("snowflake")),
      menuItem("prediction",tabName="prediction",icon=icon("snowflake"))
      
    )),
    dashboardBody(  
      tabItems(
        tabItem("Characteristics_Of_Players",
                fluidPage(
                  fluidRow(
                    column(# width should be between 1 and 12
                      width=6,
                      box(plotlyOutput("plot"), 
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    ),
                    column(# width should be between 1 and 12
                      width=6,
                      box(plotlyOutput("plot1"), 
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    )
                  ),
                  fluidRow(
                    column(# width should be between 1 and 12
                      width=6,
                      box(plotlyOutput("plot2"), 
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    ),
                    column(# width should be between 1 and 12
                      width=6,
                      box(plotlyOutput("plot3"), 
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    )
                  )
                )
        ),
        tabItem("Characteristic_Of_Tournament",
                fluidPage(
                  fluidRow(
                    column(# width should be between 1 and 12
                      width=6,
                      box(plotlyOutput("plot4"), 
                          
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    ),
                    column(# width should be between 1 and 12
                      width=6,
                      box(plotlyOutput("plot5"), 
                          
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    )
                    
                  ),
                  fluidRow(
                    column(# width should be between 1 and 12
                      width=6,
                      box(plotlyOutput("plot6"), 
                          
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    ),
                    column(# width should be between 1 and 12
                      width=6,
                      box(plotlyOutput("plot7"), 
                          
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    )
                    
                  )
                )
        ),
        tabItem("History_of_Medal",
                fluidPage(
                  fluidRow(
                    column(# width should be between 1 and 12
                      width=6,
                      box(plotlyOutput("plot8"), 
                          
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    ),
                    column(# width should be between 1 and 12
                      width=6,
                      box(plotlyOutput("plot9"), 
                          
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    )
                    
                  ),
                  fluidRow(
                    column(# width should be between 1 and 12
                      width=6,
                      box(plotlyOutput("plot10"), 
                          
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    ),
                    column(# width should be between 1 and 12
                      width=6,
                      box(plotlyOutput("plot11"), 
                          
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    )
                    
                  )
                  
                )
        ),
        tabItem("Influence_of_GDP",
                fluidPage(
                  fluidRow(
                    column(# width should be between 1 and 12
                      width=6,
                      box(plotlyOutput("plot12"), 
                          
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    ),
                    column(# width should be between 1 and 12
                      width=6,
                      box(plotlyOutput("plot16"), 
                          
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    )
                    
                    
                  ),
                  fluidRow(
                    column(# width should be between 1 and 12
                      width=12,
                      box(plotlyOutput("plot13"), 
                          
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    ),
                    
                  ),
                  fluidRow(
                    column(# width should be between 1 and 12
                      width=12,
                      box(plotlyOutput("plot17"), 
                          
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    )
                    
                  )
                )
        ),
        tabItem("Sports_vs_Medal",
                fluidPage(
                  fluidRow(
                    column(# width should be between 1 and 12
                      width=12,
                      
                      box(
                        selectInput("Sport","Sport:",c("Swimming","Ice Hockey","Gymnastics","Alpine Skiing","Handball","Hockey","Rowing","Football","Speed Skating","Sailing","Cycling","Fencing","TaekwondoAthletics","Canoeing","Water Polo","Wrestling","Modern Pentathlon","Figure Skating","Golf","Softball","Boxing","Basketball","Nordic Combined","Diving","Baseball","Volleyball","Cross Country Skiing","Bobsleigh","Curling","Shooting","Judo","Equestrianism","Tennis","Rugby Sevens","Rhythmic Gymnastics","Weightlifting","Badminton","Beach Volleyball","Ski Jumping","Rugby","Short Track Speed Skating","Biathlon","Lacrosse","Synchronized Swimming","Archery","Freestyle Skiing","Triathlon","Polo","Luge","Table Tennis","Snowboarding","Cricket","Skeleton","Racquets")),width=4
                      )
                    )
                    
                  ),
                  fluidRow(
                    column(# width should be between 1 and 12
                      width=9,
                      box(plotlyOutput("plot15",height = "80vh"), 
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL)
                      
                    )
                  )
                )
        ),
        tabItem("Summer_olympics",
                fluidPage(
                  fluidRow(
                    column(# width should be between 1 and 12
                      width=6,
                      box(DT::dataTableOutput("mytable"), 
                          title="Which countries have won the most medals?",
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL)
                    ),
                    column(# width should be between 1 and 12
                      width=6,
                      box(DT::dataTableOutput("mytable1"), 
                          title="Frequency of medals won in Summer Olympics",
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL)
                    )
                    
                  ),
                  fluidRow(
                    
                    column(# width should be between 1 and 12
                      width=12,
                      box(plotlyOutput("plot18"), 
                          
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    )
                  )
                )
        ),
        tabItem("Winter_olympics",
                fluidPage(
                  fluidRow(
                    column(# width should be between 1 and 12
                      width=6,
                      box(DT::dataTableOutput("mytable2"), 
                          title="Which countries have won the most medals?",
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL)
                    ),
                    column(# width should be between 1 and 12
                      width=6,
                      box(DT::dataTableOutput("mytable3"), 
                          title="Frequency of medals won in winter Olympics",
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL)
                    )
                    
                  ),
                  fluidRow(
                    
                    column(# width should be between 1 and 12
                      width=12,
                      box(plotlyOutput("plot19"), 
                          
                          # For column based layouts, we can set box width to NULL
                          # This overrides the default value
                          width=NULL) 
                    )
                  )
                )
        ),
        tabItem("prediction",
                fluidPage(
                  
                  theme = shinytheme("cerulean"),
                  title = "Probability of winning the Medal",
                  
                ),
                fluidRow(
                  column(# width should be between 1 and 12
                    width=4,
                    
                  ),
                  column(# width should be between 1 and 12
                    
                    width=8,
                    box(##textOutput("textoutput1"),
                      
                      height = "120vh", title = "Probability of winning the Medal",theme = shinytheme("cerulean"),
                      selectInput("Sex", "Sex", choices = c("M","F")),
                      numericInput("Age", "Age", 10, min = 1, max = 100),
                      numericInput("Height", "Height", 10, min = 1, max = 100),
                      numericInput("Weight", "Weight", 10, min = 1, max = 100),
                      numericInput("Year", "Year", 2001, min = 1990, max =2020 ),
                      textInput("Season", "Season",value = "Winter"),
                      textInput("Sport", "Sport",value = "swimming"),
                      textInput("Team", "Team",value = "India"),
                      br(),
                      submitButton("Submit", icon("refresh")),
                      imageOutput("Alignment", height = "auto")
                    )
                    
                  )
                )
        )
      )
    )
  )
  
)