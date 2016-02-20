library(shiny)

#####################################################
# Model Creation:
# Model: Linear Regression
#
#
#
#####################################################
#####Read the data
#cloud_data=read.csv("cloud_data.csv")

#cloud_data$Street = as.factor(cloud_data$Street)
#cloud_data$Type = as.factor(cloud_data$Type)
#Nov'14 to Oct'15
past_temp=c(67.4,60.2,63.5,63.65,67.53,63.9,61.7,67.9,70.8,73.6,66.15,67.56 )
#Nov'15 to Oct'15
future_temp=c(68.2,59.1,63.7,64.02,68.4, 66.4,63.4,69.2,69.3,74.85,65.23,67.98)

#str(cloud_data)

#### Split
set.seed(100)
# train = sample(1:nrow(cloud_data), 10)
# training_data = cloud_data[train,]
# 
# ##### Apply Linear Regression
# model_summer = lm(Summer ~.-Winter , data=training_data)
# summary(model_summer)
# 
# model_winter = lm(Winter ~.-Summer, data=training_data)
# summary(model_winter)

test_street= 'Ellendale'
test_street=as.factor(test_street)
test_type = 'Apartment'
test_type=as.factor(test_type)
test_room = 2
test_occupants=6
test_appliance =3
test_floor=1
test_occupancy =100
test_winter=200
test_summer=200
#test = c(test_street, test_type, test_room, test_occupants, test_appliance, test_floor, test_occupancy)

test = data.frame(Street=test_street, Type=test_type, 
                  Room=test_room, Occupants=test_occupants, 
                  Appliance=test_appliance, Floor=test_floor, 
                  Occupancy=test_occupancy, Winter=test_winter,check.rows=FALSE)
#x = predict(model_summer,test)
x=test$Floor


n <- c("Nov14", "Dec14", "Jan15",
       "Feb15", "Mar15", "Apr15",
       "May15", "Jun15", "Jul15",
       "Aug15", "Sep15", "Oct15")
##################################################
# Define server logic required to predict the
# Energy Consumption
#
#
#
##################################################

shinyServer(function(input, output) {
  
#   observe({
#     updateSelectInput(
#       session,
#       "street",
#       choices=levels(read.csv("cloud_data.csv")$Street)
#     )
#     
#   })
  output$streetcontrols <- renderUI({
    #yy <- read.csv("cloud_data.csv")$Street
    selectInput('street', 'Street:', 
                            #choices=list("Ellendale","Orchard", "Menlo", "ThirtyWest" ),
                                   choices=levels(read.csv("cloud_data.csv")$Street),selectize = FALSE,
                                   selected="Ellendale"
                                   )
    #cities <- getNearestCities(input$lat, input$long)
    #checkboxGroupInput("cities", "Choose Cities", cities)
  })
#   
  ##Prediction
###############################################
#  TAB: 1 Prediction :: expected_summer
#         and expected_winter
###############################################  
  output$expected_summer <- renderPrint({
    
    if(input$newuser==FALSE)
    {
      cloud_data<-read.csv("cloud_data.csv")
      cloud_data$Street <- as.factor(cloud_data$Street)
      cloud_data$Type <- as.factor(cloud_data$Type)
      
      train <- sample(1:nrow(cloud_data), nrow(cloud_data))
      training_data <- cloud_data[train,]
      
      ##### Apply Linear Regression
      model_summer <- lm(Summer ~.-Winter , data=training_data)
      #summary(model_summer)
      
      
      #summary(model_winter)
    paste(predict(model_summer,data.frame(
      Street=as.factor(input$street),
      Type=as.factor(input$type),
      Room=as.integer(input$room),
      Occupants=as.integer(input$occupants),
      Appliance=as.integer(input$appliance),
      Floor=as.integer(input$floor),
      Occupancy=as.integer(input$occupancy),
      Winter=test_winter,check.rows=FALSE
                      )
            ),
         "per month"
              )#paste
    } else {
      0
      
    }#else ends
  }) #output function ends
  
  
  output$expected_winter <- renderPrint({
    if(input$newuser==FALSE)
    {
      cloud_data<-read.csv("cloud_data.csv")
      cloud_data$Street <- as.factor(cloud_data$Street)
      cloud_data$Type <- as.factor(cloud_data$Type)
      
      train <- sample(1:nrow(cloud_data), nrow(cloud_data))
      training_data <- cloud_data[train,]
      
      model_winter <- lm(Winter ~.-Summer, data=training_data)
    paste(predict(model_winter,data.frame(
      Street=as.factor(input$street),
      Type=as.factor(input$type),
      Room=as.integer(input$room),
      Occupants=as.integer(input$occupants),
      Appliance=as.integer(input$appliance),
      Floor=as.integer(input$floor),
      Occupancy=as.integer(input$occupancy),
      Summer=test_summer,check.rows=FALSE
     
              )
        )
      , "per month"
    )#paste
    } else {
      
      0
    }  
  })#output function ends
  

##################################################
# TAB 2: PLOT
#    
##################################################  
  #Plot
    output$plot <- renderPlot({
      
      if(input$newuser==TRUE)
      {
        
        #############################################
        # Writing to csv file
        #
        #############################################
        summer <- as.numeric(input$may)+
                  as.numeric(input$jun)+
                  as.numeric(input$jul)+
                  as.numeric(input$aug)+
                  as.numeric(input$sep)+
                  as.numeric(input$oct)
        winter <- as.numeric(input$nov)+
                  as.numeric(input$dec)+
                  as.numeric(input$jan)+
                  as.numeric(input$feb)+
                  as.numeric(input$mar)+
                  as.numeric(input$apr)      
        features <- c(input$newstreet, 
                      input$type, 
                      as.numeric(input$room), 
                      as.numeric(input$occupants), 
                      as.numeric(input$appliance), 
                      as.numeric(input$floor), 
                      as.numeric(input$occupancy), 
                      as.numeric(summer/6), 
                      as.numeric(winter/6) 
                      )
        FF <- as.matrix(t(features))
        if(input$newstreet != "Enter text...")
        {
        write.table(FF, file = "cloud_data.csv", sep = ",", 
                    col.names = FALSE, append=TRUE, row.names=FALSE)
        }
        #count <-table
#       barplot(c(as.numeric(input$nov), 
#                 as.numeric(input$dec), 
#                 as.numeric(input$jan), 
#                 as.numeric(input$feb), 
#                 as.numeric(input$mar), 
#                 as.numeric(input$apr), 
#                 as.numeric(input$may), 
#                 as.numeric(input$jun), 
#                 as.numeric(input$jul), 
#                 as.numeric(input$aug), 
#                 as.numeric(input$sep), 
#                 as.numeric(input$oct)
#                 ),
#               names.arg=c("Nov14", "Dec14", "Jan15",
#                           "Feb15", "Mar15", "Apr15",
#                           "May15", "Jun15", "Jul15",
#                           "Aug15", "Sep15", "Oct15"),
#               xlab="Month",
#               ylab="Past Bill")
#         
        
         
          #mydatanew <- read.csv(input$file[4])
          #mydatanew$Continent <- factor(mydatanew$Continent)
          #mydatanew$IP <- factor(mydatanew$IP)
          #mydatanew$predicted<-predict(mylogit, newdata=mydatanew, type="response")
          #paste("predictions", mydatanew)
          model_temp<-lm(c(as.numeric(input$nov), 
                          as.numeric(input$dec), 
                          as.numeric(input$jan), 
                          as.numeric(input$feb), 
                          as.numeric(input$mar), 
                          as.numeric(input$apr), 
                          as.numeric(input$may), 
                          as.numeric(input$jun), 
                          as.numeric(input$jul), 
                          as.numeric(input$aug), 
                          as.numeric(input$sep), 
                          as.numeric(input$oct)
                          )~
                         past_temp
                        )
          #future <-predict(model_temp, future_temp$past_temp)
          
          x<-c(as.numeric(input$nov), 
               as.numeric(input$dec), 
               as.numeric(input$jan), 
               as.numeric(input$feb), 
               as.numeric(input$mar), 
               as.numeric(input$apr), 
               as.numeric(input$may), 
               as.numeric(input$jun), 
               as.numeric(input$jul), 
               as.numeric(input$aug), 
               as.numeric(input$sep), 
               as.numeric(input$oct)
          )
          y<-          c(   predict(model_temp,data.frame(past_temp= future_temp[1])),
                            predict(model_temp,data.frame(past_temp= future_temp[2])),
                            predict(model_temp,data.frame(past_temp= future_temp[3])),
                            predict(model_temp,data.frame(past_temp= future_temp[4])),
                            predict(model_temp,data.frame(past_temp= future_temp[5])),
                            predict(model_temp,data.frame(past_temp= future_temp[6])),
                            predict(model_temp,data.frame(past_temp= future_temp[7])),
                            predict(model_temp,data.frame(past_temp= future_temp[8])),
                            predict(model_temp,data.frame(past_temp= future_temp[9])),
                            predict(model_temp,data.frame(past_temp= future_temp[10])),
                            predict(model_temp,data.frame(past_temp= future_temp[11])),
                            predict(model_temp,data.frame(past_temp= future_temp[12]))
                    )
  
          #temp<-data.frame(past_temp=c(67.4,60.2,63.5,63.65,67.53,63.9,61.7,67.9,70.8,73.6,66.15,67.56 ),
           #               future_temp=c(68.2,59.1,63.7,64.02,68.4, 66.4,63.4,69.2,69.3,74.85,65.23,67.98))
          temp<- data.frame(past=x,future=y)
        barplot( t(as.matrix(temp)),
          
          
          

          #c(as.numeric(input$nov), 
#                   as.numeric(input$dec), 
#                   as.numeric(input$jan), 
#                   as.numeric(input$feb), 
#                   as.numeric(input$mar), 
#                   as.numeric(input$apr), 
#                   as.numeric(input$may), 
#                   as.numeric(input$jun), 
#                   as.numeric(input$jul), 
#                   as.numeric(input$aug), 
#                   as.numeric(input$sep), 
#                   as.numeric(input$oct)
#                   ),
        names.arg=c("Nov14-15", "Dec14-15", "Jan15-16",
                    "Feb15-16", "Mar15-16", "Apr15-16",
                    "May15-16", "Jun15-16", "Jul15-16",
                    "Aug15-16", "Sep15-16", "Oct15-16"),
        beside=TRUE,
        xlab="Month",
        ylab="Predicted Bill",
        main = "Past and Predicted Usage")
        legend("right", legend = c("Past Bill", "Predicted Bill"), fill = c("darkblue", "red"))
          #)
        
      } else {
        cloud_data<-read.csv("cloud_data.csv")
        cloud_data$Street <- as.factor(cloud_data$Street)
        cloud_data$Type <- as.factor(cloud_data$Type)
        
        train <- sample(1:nrow(cloud_data), nrow(cloud_data))
        training_data <- cloud_data[train,]
        
        ##### Apply Linear Regression
        model_summer <- lm(Summer ~.-Winter , data=training_data)
        #summary(model_summer)
        model_winter <- lm(Winter ~.-Summer, data=training_data)
        barplot(c(    predict(model_summer,data.frame(
                                              Street=as.factor(input$street),
                                              Type=as.factor(input$type),
                                              Room=as.integer(input$room),
                                              Occupants=as.integer(input$occupants),
                                              Appliance=as.integer(input$appliance),
                                              Floor=as.integer(input$floor),
                                              Occupancy=as.integer(input$occupancy),
                                              Winter=test_winter,check.rows=FALSE
                                                      
                                              )                            
                              ),
                      predict(model_winter,data.frame(
                        Street=as.factor(input$street),
                        Type=as.factor(input$type),
                        Room=as.integer(input$room),
                        Occupants=as.integer(input$occupants),
                        Appliance=as.integer(input$appliance),
                        Floor=as.integer(input$floor),
                        Occupancy=as.integer(input$occupancy),
                        Summer=test_summer,check.rows=FALSE
                      ))
                      ), # c ends

        names.arg=c("Expected Summer", "Expected Winter"),
        xlab="Season",
        ylab="Average Bill")
      }
        })#output function ends
    
    # Generate a summary of the data
    output$summary <- renderPrint({
    if(input$newuser==TRUE)
    {
      model_temp<-lm(c(as.numeric(input$nov), 
                       as.numeric(input$dec), 
                       as.numeric(input$jan), 
                       as.numeric(input$feb), 
                       as.numeric(input$mar), 
                       as.numeric(input$apr), 
                       as.numeric(input$may), 
                       as.numeric(input$jun), 
                       as.numeric(input$jul), 
                       as.numeric(input$aug), 
                       as.numeric(input$sep), 
                       as.numeric(input$oct)
                      )~
                      past_temp
                    )
      c(   predict(model_temp,data.frame(past_temp= future_temp[1])),
           predict(model_temp,data.frame(past_temp= future_temp[2])),
           predict(model_temp,data.frame(past_temp= future_temp[3])),
           predict(model_temp,data.frame(past_temp= future_temp[4])),
           predict(model_temp,data.frame(past_temp= future_temp[5])),
           predict(model_temp,data.frame(past_temp= future_temp[6])),
           predict(model_temp,data.frame(past_temp= future_temp[7])),
           predict(model_temp,data.frame(past_temp= future_temp[8])),
           predict(model_temp,data.frame(past_temp= future_temp[9])),
           predict(model_temp,data.frame(past_temp= future_temp[10])),
           predict(model_temp,data.frame(past_temp= future_temp[11])),
           predict(model_temp,data.frame(past_temp= future_temp[12]))
      )
    } else {
      
      
    }

    })#output summary ends
    
    # Generate an HTML table view of the data
    #output$table <- renderTable({
    output$suggestion <- renderPrint({
    #if(input$newuser==TRUE)
    #{
      paste("Ways to lower the Energy Bill:",
       "1. If you are a student, lower the occupancy hours by studying at library.",
       "2. Disconnect unnecessary appliances while not use.",
       "3. Very trivial but often ignored:: Switch off lights when not in use !")
    #} else {
      
    #}
      
    }) #output table ends
 
    





    
}) #function and shiny ends