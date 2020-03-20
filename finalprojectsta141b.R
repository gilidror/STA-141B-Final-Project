library(shiny)
library(httr)
library(jsonlite)
library(stringr)
library(ggplot2)
library(plotly)
# Define UI for application 
ui <- fluidPage(
  
  # give the shiny app a title
  headerPanel("Analysis of College Scorecard Data"),
  
  # let user know if there selections have no data
  helpText("If there is no graph outputed, there is no data present for the input selected."),
  
  mainPanel(
    tabsetPanel(
      
      # create a tab for College/University Specific Analysis
      tabPanel("College/University-Specific Analysis",
               
               # drop down menu of Universities/Colleges
               selectInput(inputId = "university", 
                           label = "Choose a California University/College", 
                           choices = c("University of California-Davis",
                                       "California State University-Long Beach",
                                       "Foothill College",
                                       "West Valley College",
                                       "Ohlone College",
                                       "Santa Rosa Junior College",
                                       "San Jose City College",
                                       "Evergreen Valley College",
                                       "Mission College",
                                       "Shasta College")),
               
               # drop down menu of year
               selectInput(inputId = "year", label = "Select year", choices = c(2000:2015)),
               
               # drop down menu of type of our analyses
               selectInput(inputId = "anal", label = "Select Type of University/College Analysis (Note: Some years have no data)",
                           choice=c("none","ethnicity","tuition (no need to select year)","graduation by gender (no need to select year)")),
               plotOutput("plot1")
      ),
      
      # create a tab for City-Specific Analysis
      tabPanel("City-Specific Analysis",
               selectInput(inputId = "city",label = "Select City in California for analysis of its colleges' demographics",
                           choices = c("Fresno","Bakersfield","Sacramento","San Diego","San Jose")),
               
               # drop down menu of year
               selectInput(inputId = "city_year", label = "Select Year", choices = c(2000:2015)),
               
               # drop down menu of our city analyses
               selectInput(inputId = "city_anal", label = "Select Type of City Analysis (Note: Some years have no data)", choices = c("none","gender ratio of college students in a city","number of graduate students (no need to select year)")),
               plotlyOutput("plot2")
      )
    )
  ),
  
  
)


server <- function(input, output) {
  # Define server logic required to draw a histogram for ethnicities in university
  data <- reactive({
    if (input$anal == "ethnicity"){
      b = ".student.demographics.race_ethnicity"
      c = input$year
      r <- GET("https://api.data.gov/ed/collegescorecard/v1/schools",
               query = list(
                 api_key = "gU41PBUftyfJBVU8g8zTdKlm3IW9Mj4ilfGYEUbP",
                 school.name = input$university,
                 fields=str_c(c,b)
               ))
      
      json <- content(r, as = "text", encoding = "UTF-8")
      temp=fromJSON(json)$result
      str_split(c(as.character(names(temp))),"\\.")
      name=c()
      for(i in 1:length(temp)){
        name[i]=str_split(c(as.character(names(temp))),"\\.")[[i]][5]
      }
      x=data.frame(label=name,value=c(unlist(temp[,1:length(temp)])))
      na.omit(x)
    }
  })
  # Define server logic required to draw a line plot of in-state and out-of-state tuition plot for university and year selected
  data2 = reactive({
    if(input$anal == "tuition (no need to select year)"){
      r <- GET("https://api.data.gov/ed/collegescorecard/v1/schools",
               query = list(
                 api_key = "gU41PBUftyfJBVU8g8zTdKlm3IW9Mj4ilfGYEUbP",
                 school.name = input$university
               ))
      
      json <- content(r, as = "text", encoding = "UTF-8")
      result = fromJSON(json)$result
      years=c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
      tuitioni=c()
      tuitiono=c()
      for (i in 1:length(years)){
        tuitiono[i]=result[[years[i]]]$cost$tuition$out_of_state
        tuitioni[i]=result[[years[i]]]$cost$tuition$in_state
      }
      data.frame(year=years,in_state=tuitioni,out_of_state=tuitiono)
    }
  })
  # Define server logic required to obtain a stacked bar chart of graduation rate by gender data for selected year and university
  data3 = reactive({
    if(input$anal == "graduation by gender (no need to select year)"){
      r <- GET("https://api.data.gov/ed/collegescorecard/v1/schools",
               query = list(
                 api_key = "gU41PBUftyfJBVU8g8zTdKlm3IW9Mj4ilfGYEUbP",
                 school.name = input$university
               ))
      
      json <- content(r, as = "text", encoding = "UTF-8")
      result = fromJSON(json)$result
      years=c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
      male=c()
      female=c()
      for (i in 1:length(years)){
        male[i]=result[[years[i]]]$completion$`4_yr_completion`$male_students
        female[i]=result[[years[i]]]$completion$`4_yr_completion`$female_students
      }
      m=data.frame(year=years,gender=rep("male"),num=male)
      f=data.frame(year=years,gender=rep("female"),num=female)
      rbind(m,f)
    }
  })
  # Define server logic required to obtain a pie chart of gender ratio of college students in a city
  data4 = reactive({
    if(input$city_anal == "gender ratio of college students in a city"){
      b = ".student.demographics.men,"
      d = ".student.demographics.women"
      c = input$city_year
      e=".student.demographics.men"
      r <- GET("https://api.data.gov/ed/collegescorecard/v1/schools",
               query = list(
                 api_key = "gU41PBUftyfJBVU8g8zTdKlm3IW9Mj4ilfGYEUbP",
                 school.city = input$city,
                 fields=str_c("school.name,",c,b,c,d),
                 per_page = 100
               ))
      
      json <- content(r, as = "text", encoding = "UTF-8")
      temp=fromJSON(json)$result
      men = temp[[str_c(c,e)]]
      women = temp[[str_c(c,d)]]
      men_value = sum(men,na.rm=TRUE)
      women_value = sum(women,na.rm = TRUE)
      data.frame(gender = c("men","women"), num = c(men_value,women_value))
    }
  })
  # Define server logic required to obtain a line plot of number of graduated students in each university of a city
  data5 = reactive({
    if(input$city_anal == "number of graduate students (no need to select year)"){
      r <- GET("https://api.data.gov/ed/collegescorecard/v1/schools",
               query = list(
                 api_key = "gU41PBUftyfJBVU8g8zTdKlm3IW9Mj4ilfGYEUbP",
                 school.city = input$city,
                 per_page = 100
               ))
      json <- content(r, as = "text", encoding = "UTF-8")
      years=c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
      temp=fromJSON(json)$result
      sname=temp$school$name
      set=data.frame()
      for(i in 1:length(years)){
        num=temp[[years[i]]]$student$grad_students
        tp = data.frame(schoolname = sname,number=num,year = rep(years[i]))
        set=rbind(set,tp)
      }
      set
    }
  })
  
  # when user selects analysis relating to ethnicity
  output$plot1 = renderPlot({
    if (input$anal == "ethnicity"){
      ggplot(data = data(),aes(x=label,y=value*100)) + geom_bar(stat = "identity")+geom_text(stat = "identity",aes(label=value*100),nudge_y = 0.015)+ggtitle(paste("Ethnicities in",input$university,"in the year of", input$year))+xlab("Ethnicities")+ylab("Percentage")
    }
    # when user selects analysis relating to tuition
    # does not relate to the year the user selects
    else if(input$anal == "tuition (no need to select year)" & input$university != "University of California-San Francisco"){
      ggplot(data2(),aes(year))+geom_line(aes(y=in_state,group=1,col= "in-state tuition"))+geom_line(aes(y=out_of_state,group=1,col="out-of-state tuition"))+ggtitle("Tuition vs. Year for", input$university)+xlab("Year") + ylab("Tuition ($)") 
      
    }
    # when user selects analysis relating to graduation by gender
    else if(input$anal == "graduation by gender (no need to select year)"){
      ggplot(data=data3(),aes(fill=gender,y=num,x=year))+geom_bar(position = "stack",stat = "identity") + ggtitle(paste("Number of Graduates vs. Year for",input$university)) + xlab("Year") + ylab("Number of Graduates")
      
    }
  })
  
  output$plot2 = renderPlotly({
    # when user selects analysis relating to gender ratio of college students in a city
    if ((input$city_anal == "gender ratio of college students in a city") & (sum(data4()$num) != 0)){
      plot_ly(data=data4(),type='pie', labels=~gender, values=~num,
              textinfo='label+percent',
              insidetextorientation='radial') %>% layout(title = paste("Percentage of Male and Female Students in", input$city, "Colleges", "for the year", input$city_year))
    }
    # when user select analysis relating to number of graduate students in each university of a city
    # does not relate to the year the user selects
    else if (input$city_anal == "number of graduate students (no need to select year)"){
      data5()%>%
        plot_ly(y=~number,x=~year)%>%
        add_lines(color=~schoolname) %>% layout(title = paste("Number of Graduate Students in",input$city, "vs. Year"),xaxis = list(title = "Year"), yaxis = list(title = "Number of Graduate Students"))
    }
  })
  
  
}
# Run the application
shinyApp(ui = ui, server = server)