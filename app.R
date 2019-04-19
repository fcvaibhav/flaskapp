

#misc code below

library(tidyverse)
library(dplyr)
library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Get Topper score and topic analysis Data"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload attempt summary", 
                multiple = FALSE, 
                accept = c(".zip")),
      numericInput("cut_off", "Enter overall cutoff:",0,1000),
      numericInput("time_limit", "Enter time limit (minutes):",0,1000),
      numericInput("ques_less_10_limit", "questions below 10s limit:", 0,200),
      downloadButton("report", "Download data")
      
    ),
    mainPanel(
      htmlOutput("message"),
      tableOutput("topper_table")    
      )
  )
  
)
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  options(shiny.maxRequestSize=1000*1024^2) 
  vals <- reactiveValues()
    observe({
      vals$cutoff <- input$cut_off
      vals$timelimit <- input$time_limit
      vals$ques_less_10_limit <- input$ques_less_10_limit
    })
    
  dataInput <- reactive ({
    inputfile <- input$datafile
    if(!is.null(inputfile)){
    data <- read_csv(inputfile$datapath, col_types = "cccccdccicdd")
    }
    if(!is.null(data)){
    data <- data %>% distinct(userid, packageId, questionId, .keep_all = TRUE)
    user_score_overall <- data %>% 
      group_by(userid,packageId) %>% 
      summarise(attempts = length(unique(question[evalStatus!="skipped"])), 
                correct = length(unique(question[evalStatus=="correct"])), 
                time = sum(timeTaken)/60, score = sum(positiveMarks[evalStatus=="correct"])-
                  sum(negativeMarks[evalStatus=="incorrect"]), ques_less_10 = length(unique(question[(evalStatus=="correct" & timeTaken<10)])),
                accuracy = round(100*correct/attempts,0))
    
    #user_section_summary <- data %>% group_by(userid, packageId, section) %>% 
    #  summarise(score = sum(positiveMarks[evalStatus=="correct"])-sum(negativeMarks[evalStatus=="incorrect"]))
    #user_section_summary <- user_section_summary %>% spread(section, score)
    #user_section_summary$sections_cleared = 0
    #user_section_summary[which(user_section_summary[,c(3,4,5)]>sections$cutoff),"sections_cleared"] <- 1
    
    #user_score_overall <- user_score_overall %>% left_join(user_section_summary, by = c('userid','packageId'))
    user_score_overall <- user_score_overall %>% arrange(score) %>% distinct(userid, .keep_all = TRUE)
    
    user_data_flags <- user_score_overall %>% 
      mutate(fake_flag = (ques_less_10 > vals$ques_less_10_limit),
             less_time_flag = (time < 0.5*vals$timelimit)) %>% 
      select(userid, fake_flag, less_time_flag, score)
    
    toppers <- user_data_flags %>% 
      filter(fake_flag == FALSE & less_time_flag == FALSE & score >= vals$cutoff) %>% 
      select(userid, score)%>%
      arrange(-score) %>%
      top_n(20)%>%
      mutate(profile_link = paste0("grdp.co/z",userid))
    
    users <- length(unique(data$userid))
    toppers_count <- length(unique(toppers$userid))
    cleared <- round(toppers_count*100/users,1)
    message <- paste0("total ", users, " aspirants have attempted the test so far and only ", toppers_count, " (",cleared, " %) have cleared it.")
    
    
    topicsToppers <- data %>% filter(userid %in% toppers$userid) %>% 
      group_by(section, topicName) %>% 
      summarise(questions = length(unique(question)), avg_attempts = round(length(question[evalStatus!="skipped"])/toppers_count,0), 
                avg_correct = round(length(question[evalStatus=="correct"])/toppers_count,0), 
                avg_time_per_question = round((sum(timeTaken)/(questions*toppers_count)),0),
                avg_score = round((sum(positiveMarks[evalStatus=="correct"])-
                                     sum(negativeMarks[evalStatus=="incorrect"]))/toppers_count,0), 
                attempts_percent = round(100*avg_attempts/questions,0), accuracy = round(100*avg_correct/avg_attempts,0)) %>%
      arrange(-questions)
    

    outlist <- list()
    outlist[[1]] <- toppers
    outlist[[2]] <- topicsToppers
    outlist[[3]] <- message
    
    return(outlist)
    }

  })
    
  
  output$topper_table <- renderTable({
    if(!is.null(dataInput())){
      
    data2 <- dataInput()
    return(data2[[1]] %>% top_n(10,score))
    }
    })
  
  output$message <- renderUI({
    if(!is.null(dataInput())){
      
    data2 <- dataInput()
    return(data2[[3]])
    }
    })
  
  datasetTopics <- reactive ({
    if(!is.null(dataInput())){
    data2 <- dataInput()
    return(data2[[2]])
    }
  })
  
  output$report <- downloadHandler(filename = "report.csv",
                                   content = function(file) {
                                     write.csv(datasetTopics(), file, row.names = FALSE)
                                   })
})

# Run the application 
shinyApp(ui = ui, server = server)

