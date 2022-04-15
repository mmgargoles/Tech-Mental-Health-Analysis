# UI file for shiny

shinyUI(dashboardPage( skin = "blue",
                       
                       #Application title
                       dashboardHeader(title = "Tech Mental Health Analysis",titleWidth = 400),
                       
                       # dashboard sidebar functions will be inserted here
                       dashboardSidebar(
                           
                           sidebarMenu(
                               menuItem("Main",tabName = "main",icon = icon("dashboard")),
                               menuItem("USA Map Comparison",tabName = "map",icon = icon("map"))
                           ),
                           sliderInput("year",
                                       label = "Year Range",
                                       min =  2014,
                                       max = 2016,
                                       step = 1,
                                       value = c(2014,2016),
                                       sep = ""),
                           uiOutput("typeSelectOutput"),
                           selectInput("questions", 
                                       label="Select Survey Question:", 
                                       choices= quesToChoose,
                                       selected= "family_history"),
                           checkboxGroupInput("genders", label="Select Gender",
                                              choices = list("Male" = "Male",
                                                             "Female" = "Female",
                                                             "Others" = "Others"),
                                              selected = c("Male", "Female", "Others")
                           ),
                           sliderInput("ages",
                                       label = "Age Range",
                                       min =  1,
                                       max = 100,
                                       step = 1,
                                       value = c(20,72),
                                       sep = "")
                       ),
                       # functions that must go in the body of the dashboard.
                       dashboardBody(
                           tabItems(
                               tabItem(tabName = "main",
                                       plotOutput("cmpCountry"),
                                       br(),
                                       br(),
                                      fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("cmpSize"), plotOutput("cmpSize2"))),
                                       br(),
                                       br(),
                                       downloadButton("download", "Download results"),
                                       br(),
                                       DT::dataTableOutput("results"),
                                       tags$head(
                                           tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                                       )
                               ),
                               tabItem(tabName = "map",
                                       plotlyOutput("plotusa"),
                                       verbatimTextOutput("click")
                               )
                           )
                       )
)
)
