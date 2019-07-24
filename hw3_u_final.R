#install.packages('tabulizer')
#install.packages('rJava')
#install.packages('devtools')
#install.packages('pdftools')
#install.packages('data.table')
library(rvest)
library(tidyverse)
library(data.table)
library(tidytext)
library(stringr)
library(ggplot2)
library(shiny)
library(data.table)
library(leaflet)
library(geojsonio)
library(reshape2)

#getwd()
#setwd("C:/Users/veedu/Desktop/Spring 2019/Math 216/HW 3/HW3-2")

education <- read_csv("education.csv") 
median.age <- read_csv("median_age.csv")
net.migration <- read_csv("net_mig.csv")
per.capita.personal.income <- read_csv("per_capita_income.csv")
resident.pop <- read_csv("resident_pop.csv")
ur <- read_csv("ur.csv")

# Rename columns
colnames(education) <- c("DATE", "Education")
colnames(median.age) <- c("DATE", "Median_Age")
colnames(net.migration) <- c("DATE", "Net_Migration")
colnames(per.capita.personal.income) <- c("DATE", "Per_Capital_Personal_Income")
colnames(resident.pop) <- c("DATE", "Population")
colnames(ur) <- c("DATE", "Unemployment_Rate")

# Change to numerical values
education$Education <- as.numeric(education$Education)
median.age$Median_Age <- as.numeric(median.age$Median_Age)
net.migration$Net_Migration <- as.numeric(net.migration$Net_Migration)
per.capita.personal.income$Per_Capital_Personal_Income <- as.numeric(per.capita.personal.income$Per_Capital_Personal_Income)
resident.pop$Population <- as.numeric(resident.pop$Population)
ur$Unemployment_Rate <- as.numeric(ur$Unemployment_Rate)

la.joined.data <- left_join(resident.pop,
                            ur,
                            by = c("DATE")) 
la.joined.data <- left_join(la.joined.data,
                            education,
                            by = c("DATE"))
la.joined.data <- left_join(la.joined.data,
                            median.age,
                            by = c("DATE"))
la.joined.data <- left_join(la.joined.data,
                            net.migration,
                            by = c("DATE"))
la.joined.data <- left_join(la.joined.data,
                            per.capita.personal.income,
                            by = c("DATE"))
la.joined.data <- la.joined.data[-c(9), ] 

#### R Shiny App

la.cities <- read_csv("2010_Census_Populations_by_Zip_Code.csv") #use the census data here
la.cities <- la.cities[-c(1), ]

colnames(la.cities) <- c("zipcode", "city", 
                         "latitude", "longitude", 
                         "population", "age", #age = median age
                         "males", "females", 
                         "total_household", "household_size")

la.cities$zipcode <- as.numeric(la.cities$zipcode)
la.cities$latitude <- as.numeric(la.cities$latitude)
la.cities$longitude <- as.numeric(la.cities$longitude)
la.cities$population <- as.numeric(la.cities$population)
la.cities$age <- as.numeric(la.cities$age)
la.cities$males <- as.numeric(la.cities$males)
la.cities$females <- as.numeric(la.cities$females)
la.cities$total_household <- as.numeric(la.cities$total_household)
la.cities$household_size <- as.numeric(la.cities$household_size)
la.cities$population.2 <- as.character(la.cities$population)
la.cities$age.2 <- as.character(la.cities$age)
la.cities$males.2 <- as.character(la.cities$males)
la.cities$females.2 <- as.character(la.cities$females)
la.cities$total_household.2 <- as.character(la.cities$total_household)

cities.long.lat <- read_csv("uszips.csv") #use the uszips file here

#this data set contains all cities in CA
ca.cities.long.lat <- cities.long.lat %>%
  filter(state_id == "CA") %>%
  select(zip, lat, lng, city, state_id)

colnames(ca.cities.long.lat) <- c("zipcode", "latitude", 
                                  "longitude", "city", "state")
ca.cities.long.lat$zipcode <- as.numeric(ca.cities.long.lat$zipcode)
ca.cities.long.lat$latitude <- as.numeric(ca.cities.long.lat$latitude)
ca.cities.long.lat$longitude <- as.numeric(ca.cities.long.lat$longitude)

la.cities.2 <- la.cities %>%
  select(zipcode, city, population,
         age, males, females, 
         total_household, household_size)


#join data so we can get stats such as med age
la.cities.long.lat.joined <- left_join(ca.cities.long.lat,
                                       la.cities.2,
                                       by = c("zipcode"))
la.cities.long.lat.joined.2 <- la.cities.long.lat.joined %>%
  select(zipcode, latitude, longitude,
         city.x, city.y, population,
         age, males, females,
         total_household, household_size) %>%
  na.omit()

la.cities.long.lat.joined.2$population <- 
  as.character(la.cities.long.lat.joined.2$population)

la.cities.long.lat.joined.3 <- la.cities.long.lat.joined.2
la.cities.long.lat.joined.3$zipcode <- as.character(la.cities.long.lat.joined.3$zipcode)
la.cities.long.lat.joined.3$population <- as.numeric(la.cities.long.lat.joined.3$population)

#make a map of LA with the zipcodes
la.millenial.pop <- la.cities.long.lat.joined.3 %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lat = ~latitude,
                   lng = ~longitude,
                   clusterOptions = markerClusterOptions(),
                   popup = paste(la.cities.long.lat.joined.3$city.x, "<br>",
                                 "population of millennials:", la.cities.long.lat.joined.3$population, "<br>",
                                 "median age:", la.cities.long.lat.joined.3$age, "<br>",
                                 "avg household size:", la.cities.long.lat.joined.3$household_size, "<br>",
                                 "avg education level?", "<br>",
                                 "avg income?", "<br>",
                                 "# of public schools?", "<br>",
                                 "marriage stats?", "<br>",
                                 "avg cost of living"), #pop needs to be a string to show
                   label = ~la.cities.long.lat.joined.3$zipcode) #zipcode also needs to be a string

#Gender dataset and graph
la.cities.pie <- la.cities.long.lat.joined.3
la.cities.pie <- la.cities.pie[-1:-3]
la.cities.pie <- la.cities.pie[-2:-4]
la.cities.pie <- la.cities.pie[-4:-5]
names(la.cities.pie)[1] <-"city"

#Create dataset for barchart
gender.list <- aggregate(la.cities.pie, by=list(la.cities.pie$city), 
                         FUN=mean, na.rm=TRUE)
gender.list <- gender.list[,-2]
names(gender.list)[1] <- "city"

#Reshape
gender.list.t=t(gender.list)
names(gender.list.t)[1] <- "gender"

#Barchart
data.new <- reshape(gender.list, 
                    varying = c("males", "females"), 
                    v.names = "gender",
                    timevar = "gen", 
                    times = c("males", "females"), 
                    direction = "long")

colors <- c("pink", "skyblue")

#education and income dataset
income.race <- read_csv("income_race.csv")
newdata <- income.race[ which(income.race$GEO_NAME=='City of Los Angeles' | income.race$GEO_NAME=='Inglewood' |
                                income.race$GEO_NAME=='Santa Monica' | income.race$GEO_NAME=='Torrance' |
                                income.race$GEO_NAME=='Whittier' | income.race$GEO_NAME=='Long Beach' |
                                income.race$GEO_NAME=='Pasadena' | income.race$GEO_NAME=='Glendale' |
                                income.race$GEO_NAME=='Burbank' | income.race$GEO_NAME=='Alhambra'
), ]
newdata <- newdata[-1:-2]
newdata <- newdata[-16:-17]
newdata <- newdata[-2:-7]
edu <- newdata[-5:-9]
new.edu <- reshape(edu, 
                   varying = c("LESS_THAN_HIGH_SCHOOL", "AT_LEAST_BACHELORS", "GRADUATE_DEGREE"), 
                   v.names = "education",
                   timevar = "edu", 
                   times = c("LESS_THAN_HIGH_SCHOOL", "AT_LEAST_BACHELORS", "GRADUATE_DEGREE"), 
                   direction = "long")

colorsedu <- c("red", "skyblue", "yellow")

city.choices <- sort(unique(la.cities.long.lat.joined.3$city.x))
gender.choices <- c("female-heavy", "male-heavy", "more or less equal (5% error)")
#personal income data

la.cities.long.lat.joined.3 <- la.cities.long.lat.joined.3 %>%
  mutate(gender.equality = females/(males+females)*100)
  
ui <- fluidPage(
  titlePanel("The millennial trend in LA county - by Vee Duong, Veronica Estudillo, and Jennifer Ko"),
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("description {white-space: nowrap;}")),
      fluidRow(offset = 0, (textOutput(outputId = "description")))
    ),
  mainPanel(
    tabsetPanel(
      tabPanel("Summary", 
               verbatimTextOutput(outputId = "la.millenials.graph.description"),
               plotOutput(outputId = "la.millenials.graph")),
      tabPanel("The Annual Trend",
               sliderInput(inputId = "slide",
                           label = "slide to preferred year",
                           min = 2009,
                           max = 2016,
                           value = 2009,
                           animate = TRUE),  
               plotOutput(outputId = "annual.trend"),
               textOutput(outputId = "warning"),
               br()),
      tabPanel("Your unique city", 
               selectInput(inputId = "select",
                           label = "Select city to see its unique demographics!",
                           choices = city.choices,
                           selected = la.cities.long.lat.joined.3$city.x[1]),
               leafletOutput(outputId = "la.millenial.pop"),
               br())
      )
    )
  )
)

server <- function(input, output, session) {
  
  cities.subset <- reactive(la.cities.long.lat.joined.3 %>%
                                sample_n(input$select))
  
  ##
  cities.subset.3 <- reactive(gender.list.t %>% 
                                  sample_n(input$select))
  
  cities.subset.4 <- reactive(la.cities.long.lat.joined.3 %>% 
                              sample_n(input$select))
  
  output$la.millenial.pop <- renderLeaflet({
    
    input.city <- la.cities.long.lat.joined.3 %>%
      mutate(is.selected.city = 
               ifelse(la.cities.long.lat.joined.3$city.x == input$select,
                      "city",
                      "not.city")) %>%
      filter(is.selected.city == "city") 
    
    input.city %>%
      leaflet() %>%
      addTiles() %>%
      addCircleMarkers(lat = ~latitude,
                       lng = ~longitude,
                       clusterOptions = markerClusterOptions(),
                       popup = paste(input.city$city.x, "<br>",
                                     "population of millennials:", input.city$population, "<br>",
                                     "median age:", input.city$age, "<br>",
                                     "avg household size:", input.city$household_size, "<br>",
                                     "number of males", input.city$males, "<br>",
                                     "number of females", input.city$females, "<br>"), #pop needs to be a string to show
                       label = input.city$zipcode)
  })
  
  output$la.millenials.graph <- renderPlot({
    la.millenials <- la.cities %>%
      group_by(city) %>%
      summarize(mean.age = mean(age)) %>%
      filter(mean.age > 23 & mean.age < 38) %>%
      select(city)

    la.cities %>%
      group_by(city) %>%
      summarize(mean.age = mean(age)) %>%
      mutate(Millennials = ifelse(mean.age > 23 & mean.age < 38,
                                 "Millennial city",
                                 "Non-milennial city")) %>%
      ggplot(aes(x = city,
                 y = mean.age)) +
      geom_bar(aes(fill = Millennials),
               stat = "identity") +
      ylab("Mean Age")+
      ggtitle("Mean age in Non-Millennial & Millennial Cities") +
      coord_flip()})
  
  la.cities$city <- factor(la.cities$city,
                           levels = rev(levels(factor(la.cities$city))))
  
  cities.subset.5 <- reactive(la.cities %>%
                                sample_n(input$slide))
  
  la.joined.data$DATE <- as.numeric(str_sub(la.joined.data$DATE,1,4))
  la.joined.data$Population <- as.numeric(la.joined.data$Population)
  
  output$annual.trend <- renderPlot({
    la.joined.data %>%
      filter(between(DATE, 2009, input$slide)) %>%
      ggplot(aes(x = DATE,
                 y = Median_Age)) + 
      scale_x_continuous("Year",
                         limits = c(2009,2016),
                         breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
      scale_y_continuous("Median Age", 
                         limits = c(33, 37)) +
      geom_point(aes(size = Population,
                     color = Education)) +
      ggtitle("Median Age, Population, and Education in LA over time") + 
      xlab("Year") +
      ylab("Median Age") 
  })
  
  output$warning <- renderText(
    paste("** Note: Education = proportion of people aged 25 or older with associate's degree or higher")
  )
  
  output$description <- renderText(
    paste("The county of Los Angeles has become populated due to its rising popularity amongst millennials today. Our app is aimed towards people (specifically, the millennials) who are interested in moving to LA and would like to know more about unique cities in the county. We will examine population across different cities and any potential trend in education and population size across the years. Our sources include various census data.")
  )
  
  output$la.millenials.graph.description <- renderText(
    paste('The "millennials" as described in the graph below are individuals aged between 22 and 37. "Non-millennials" are those whose ages do not lie within the range.')
  )

}

shinyApp(ui = ui, server = server)

