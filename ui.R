library(shiny)

#####Read the data
#my_data=read.csv("cloud_data.csv")
#choices= as.list(cloud_data$Street)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    # Application title
    titlePanel("          Real-time Prediction and Analysis of Energy Consumption"),
    #Side Panel for user Data
    sidebarLayout(
      sidebarPanel(
      #1 Street
#         selectInput(
#           "street", 
#           label = h5("Select Time Series/Response Variable"),
#           ""
#         ),
#       selectInput('street', 'Street:', 
#                   #choices=list("Ellendale","Orchard", "Menlo", "ThirtyWest" ),
#                   choices=levels(read.csv("cloud_data.csv")$Street)
#                   #selected="Ellendale"
#                   ),
        uiOutput("streetcontrols"),
      helpText("Other street (Not in the list)?"),
      checkboxInput("newuser", "New Street", value = FALSE),
      textInput("newstreet", "Enter the new street", 
                value = "Enter text..."),
      #2 Type
      selectInput('type', 'House Type', 
                  choices=list("Apartment", "Bunglow"),
                  selected="Apartment"
                  ),
      #3 Room
      selectInput("room", "BHK", 
                  choices=list("1", "2", "3"),
                  selected="2"
                  ),
      #4 Occupants
      numericInput("occupants", "Number of Occupants", value = 5),
      #5 Appliance
      numericInput("appliance", "Number of Appliance", value = 3),
      #6 Floor
      checkboxInput("floor", "Top Floor", value = FALSE),
      #7 Occupancy
      numericInput("occupancy", "Occupancy per week (in hours)", value = 90
      ),
      helpText("NEW STREET: Enter your past usage"),
      #1 Nov 2014
      sliderInput("nov", "NOV14", min=0, max=500, value = 100),
      #2 Dec 2014
      sliderInput("dec", "DEC14", min=0, max=500, value = 100),
      #3 jan 2015
      sliderInput("jan", "JAN15", min=0, max=500, value = 100),
      #4 feb 2015
      sliderInput("feb", "FEB15", min=0, max=500, value = 100),
      #5 march 2015
      sliderInput("mar", "MAR15", min=0, max=500, value = 100),
      #6 april 2015
      sliderInput("apr", "APR15", min=0, max=500, value = 100),
      #7 may 2015
      sliderInput("may", "MAY15", min=0, max=500, value = 100),
      #8 june 2015
      sliderInput("jun", "JUN15", min=0, max=500, value = 100),
      #9 july 2015
      sliderInput("jul", "JUL15", min=0, max=500, value = 100),
      #10 august 2015
      sliderInput("aug", "AUG15", min=0, max=500, value = 100),
      #11 sept 2015
      sliderInput("sep", "SEP15", min=0, max=500, value = 100),
      #12 oct 2015
      sliderInput("oct", "OCT15", min=0, max=500, value = 100),
      #ANSWER:
      hr()
      #fluidRow(column(12, verbatimTextOutput("expected_summer"), 
       #               verbatimTextOutput("expected_winter")))
    ), #sidepanel ends
    
      mainPanel(
        tabsetPanel(type = "tabs", 
                    tabPanel("Prediction", 
                             fluidRow(column(12, 
                                             helpText("Expected Average Summer Rent"),
                                             verbatimTextOutput("expected_summer"),
                                             helpText("Expected Average Winter Rent"),
                                             verbatimTextOutput("expected_winter")
                                             )
                                      ) 
                            ),
                    tabPanel("Plot", plotOutput("plot")), 
                    tabPanel("Summary", verbatimTextOutput("summary")), 
                    #tabPanel("Table", tableOutput("table")),
                    tabPanel("Suggestion", verbatimTextOutput("suggestion"))
                  )
              ) #mainpanel ends
      
  
 

   ) #Sidebarlayout ends
) #fluidpage ends
)#shiny function ends