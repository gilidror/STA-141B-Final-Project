#Shiny Project

library(shiny)
library(httr)
library(jsonlite)
library(shinyWidgets)


#Define UI for application
ui <- fluidPage(
  
  #give an aplication title
  titlePanel("Diversity Within a University"),
  
  sidebarLayout(
    sidebarPanel(
      #create a drop down menu for year
      selectInput(
        inputId = "year",
        label = "Year",
        choices = c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
      ),
      selectInput(
        inputId = "university", 
        label = "Enter University Name:",
        choices = c("University of California-Davis")
      )
    ),
    mainPanel(
      plotOutput("grad_rate")
    )
  )
)
  

server <- function(input, output) {
  college_score_card <- reactive({
    b = ".student.demographics.race_ethnicity"
    c = input$year
    r <- GET("https://api.data.gov/ed/collegescorecard/v1/schools",
             query = list(
               api_key = "gU41PBUftyfJBVU8g8zTdKlm3IW9Mj4ilfGYEUbP",
               school.name = input$university,
               fields=str_c(c,b)
             ))
    
    json <- content(r, as = "text", encoding = "UTF-8")
    fromJSON(json)$result
  })
  
  output$grad_rate -> renderPlot({
    if(length(input$university) != 0){

    t=c()
    for (i in 1:length(college_score_card)) {
      t[i]=college_score_card[[i]]
    }
    t
    z=data.frame(label=names(college_score_card),value=t)
    z=na.omit(z)
    library(ggplot2)
    ggplot(data=z,aes(x=label,y=value))+geom_bar(stat = "identity")
    }
  })
  
}

#create shiny app
shinyApp(ui = ui, server = server)

