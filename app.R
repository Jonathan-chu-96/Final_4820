#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggthemes)
library(tidyverse)
library(egg)
library(shinythemes)

mental_health<- read.csv("mental_health.csv")

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  # Application title
  titlePanel("Mental Health in Tech Industries"),
  

  sidebarLayout(
    sidebarPanel(
      
     
      checkboxGroupInput("techcom", "Tech company?(For plot1 and plot2)",
                         choices = unique(mental_health$tech_company),  
                         selected="Yes"),
      
      checkboxGroupInput("Gender", "Gender(For plot1 and plot 4)",
                     choices = unique(mental_health$Gender),  
                     selected="female"),
      
      selectInput("country", "Country (For plot2 and plot3)",
                         choices = unique(mental_health$Country),  
                         selected="United States"),
      
      checkboxGroupInput("remote", "Remote worker(For plot4)",
                         choices = unique(mental_health$remote_work),  
                         selected="Yes"),
      
      checkboxGroupInput("self", "Self-employed worker(For plot5)",
                         choices = unique(mental_health$self_employed),  
                         selected="Yes"),
      
      actionButton("button","close")
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Age distribution with Gender", plotOutput("distPlot1")), 
        tabPanel("Size of the company", plotOutput("distPlot2")), 
        tabPanel("Company mental care", plotOutput("distPlot3")),
        tabPanel("Mental",plotOutput("plot4")),
        tabPanel("Sharing",plotOutput("plot5"))
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot1 <- renderPlot({
    ageplot<-mental_health%>%
      filter(Gender==input$Gender)%>%
      filter(tech_company==input$techcom)%>%
      ggplot(aes(x=Age))+
        geom_density()
    ageplot
    
  })
  
  output$distPlot2 <- renderPlot({
    mental_health%>%
      filter(Country==input$country)%>%
      ggplot(aes(x=no_employees))+
      geom_bar()
  })
  
  output$distPlot3 <- renderPlot({
    mental_health%>%
      filter(tech_company==input$techcom)%>%
      filter(Country==input$country)%>%
      ggplot(aes(x="",y=tech_company,fill=care_options))+
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0)+
      ggtitle("Health Care options in company \n (only use yes or no in tech company option)")+
      xlab("Tech Company")+ylab(input$techcom)
  })
  
  output$plot4 <- renderPlot({
    mental_health%>%
      filter(Gender==input$Gender)%>%
      filter(remote_work==input$remote)%>%
    ggplot(aes(y= treatment,colour=treatment, fill=treatment)) +
      geom_bar()
  })
  
  output$plot5<-renderPlot({
   p1<-mental_health%>%
     filter(self_employed==input$self)%>%
     ggplot(aes(x=seek_help,fill=seek_help))+geom_bar()+ggtitle("Does your employer provide resources to learn more about mental health issues and how to seek help?")
   p2<-mental_health%>%
     filter(self_employed==input$self)%>%
     ggplot(aes(x=coworkers,fill=coworkers))+geom_bar()+ggtitle(" Would you be willing to discuss a mental health issue with your coworkers?")
   p3<-mental_health%>%
     filter(self_employed==input$self)%>%
     ggplot(aes(x=supervisor,fill=supervisor))+geom_bar()+ggtitle("Would you be willing to discuss a mental health issue with your direct supervisor(s)?")
   ggarrange(p1,p2,p3)
    
  })
  
  observe({
    if (input$button>=1){
      stopApp()
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
