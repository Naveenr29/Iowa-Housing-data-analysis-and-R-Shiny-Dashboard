#################################################
#################################################
######### Week 4 Assignment - R Shiny ###########
#################################################
############# Dien Bao Tran Thai ################
#################################################
######### Naveen Narayana Peddyreddy ############
#################################################
#################################################

# install.packages("shiny")
# install.packages("shinydasboard")
# install.packages("shinydashboardPlus")
# install.packages("dplyr")
# install.packages("DT")
# install.packages("shinyWidgets")

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(dplyr)
#library(dbplyr)
library(DT)
library(shinyWidgets)

#setwd("C:/Users/navee/OneDrive/Desktop/communication visualization/week 4")
data <- read.csv("House_Price_data.csv")

#CLEAN AND SELECT DATASET
a <- select(data, BedroomAbvGr, SalePrice)

#Convert unit from $ to thousand
a$SalePrice<- a$SalePrice/1000

a$Bed0 <- ifelse(a$BedroomAbvGr=="0", a$SalePrice ,NA)
a$Bed1 <- ifelse(a$BedroomAbvGr=="1", a$SalePrice ,NA)
a$Bed2 <- ifelse(a$BedroomAbvGr=="2", a$SalePrice ,NA)
a$Bed3 <- ifelse(a$BedroomAbvGr=="3", a$SalePrice ,NA)
a$Bed4 <- ifelse(a$BedroomAbvGr=="4", a$SalePrice ,NA)
a$Bed5 <- ifelse(a$BedroomAbvGr=="5", a$SalePrice ,NA)
a$Bed6 <- ifelse(a$BedroomAbvGr=="6", a$SalePrice ,NA)
a$Bed7 <- ifelse(a$BedroomAbvGr=="7", a$SalePrice ,NA)
a$Bed8 <- ifelse(a$BedroomAbvGr=="8", a$SalePrice ,NA)

b <- select(data, YrSold, MoSold, SalePrice)
#change col names
colnames(b)[colnames(b)=="YrSold"] <- "Year"
colnames(b)[colnames(b)=="MoSold"] <- "Month"

#count the frequency of each month
bMonth <- b %>% group_by(Month) %>% summarise(n = n(), mean = mean(SalePrice))
#count the frequency of each month
bYear <- b %>% group_by(Year) %>% summarise(n = n(), mean = mean(SalePrice))

c <- select(data, OverallQual, OverallCond,SaleCondition, SalePrice)

#change col names
colnames(c)[colnames(c)=="OverallQual"] <- "Qual"
colnames(c)[colnames(c)=="OverallCond"] <- "Cond"
colnames(c)[colnames(c)=="SaleCondition"] <- "SaleCond"


#calculate the frequency of each class in Quality and Condition columns
cqual <- c %>% group_by(Qual) %>% summarise(n = n()) %>% mutate(RelFre = n/sum(n))
ccond <- c %>% group_by(Cond) %>% summarise(n = n()) %>% mutate(RelFre = n/sum(n))

d <- select(data, Neighborhood,BedroomAbvGr,SalePrice, YrSold, MoSold,X1stFlrSF, X2ndFlrSF)
#Change column names
colnames(d)[colnames(d)=="X1stFlrSF"] <- "FirstFloorSquareFeet"
colnames(d)[colnames(d)=="X2ndFlrSF"] <- "SecondFloorSquareFeet"
colnames(d)[colnames(d)=="BedroomAbvGr"] <- "Bedroom"
colnames(d)[colnames(d)=="YrSold"] <- "Year"
colnames(d)[colnames(d)=="MoSold"] <- "Month"
colnames(d)[colnames(d)=="Neighborhood"] <- "Location"


#Change value names in the column Location
recode(d$Location, "Blmngtn"="Bloomington.Heights", "Blueste"="Bluestem", "BrDale"="Briardale ","BrkSide"="Brookside","ClearCr"="Clear.Creek",
"CollgCr"="College.Creek","Crawfor"="Crawford" ,"IDOTRR"="Iowa.DOT","MeadowV"="Meadow.Village","Mitchel"="Mitchell","NAmes"="North.Ames",
"NoRidge"="Northridge","NPkVill"="North.park.Villa","NridgHt"="Northridge.Heights","NWAmes"="Northwest.Ames","SWISU"="South.and.West.of.Iowa.State.University",
"SawyerW"="Sawyer.West","Somerst"="Somerset","StoneBr"="Stone.Brook", "Timber"="Timberland")

ui<-dashboardPage(skin = "purple",
  dashboardHeader(title = "Tran - Naveen Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Table", tabName = "table", icon = icon("th")))),
 
   dashboardBody(
      tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidPage(
                fluidRow(
                 # titlePanel("Monthly and Yearly Sales and Average Price"),
                  mainPanel(width = 7,
                            tabsetPanel( 
                              tabPanel("Monthly Sales", plotOutput("bMonth")), 
                              tabPanel("Monthly Average Prices", plotOutput("bMonthPrice")),
                              tabPanel("Yearly Sales", plotOutput("bYear")),
                              tabPanel("Yearly Average Prices", plotOutput("bYearPrice")))),
                  column(width = 5,
                         fluidRow(
                           valueBox("400%", subtitle = tags$p("Real estate agents should focus on May-July as the sales increase 400% from Feb to June", style = "font-size: 125%;"), width = 16, color = "blue")),
                         fluidRow(
                           valueBox("April", subtitle = tags$p("Customers can get a house for the cheapest price", style = "font-size: 125%;"), width = 16, color = "green")),
                         fluidRow(
                           valueBox("2009-2010", subtitle = tags$p("Real Estate sales experienced a major turning point", style = "font-size: 125%;"), width = 16, color = "red")),
                         fluidRow(
                           valueBox("2007", subtitle = tags$p("Average Real estate prices had a major downfall after 2007", style = "font-size: 125%;"), width = 16, color = "yellow")))),
                hr(),
                fluidRow(
                  titlePanel("House Price and Number of Bedrooms"),
                  sidebarLayout(
                    sidebarPanel(
                      radioButtons("a", "Number of Bedrooms:",
                                   list("0 Bedroom"='Bed0', "1 Bedroom"='Bed1', "2 Bedrooms"='Bed2', "3 Bedrooms"='Bed3',"4 Bedrooms"='Bed4',"5 Bedrooms"='Bed5',"6 Bedrooms"='Bed6',"7 Bedrooms"='Bed7',"8 Bedrooms"='Bed8'))),
                    mainPanel(
                      plotOutput("distPlot")))),
     hr(),
                fluidRow(
                  titlePanel("Frequency of Houses sold as per the Quality"),
                  mainPanel(width = 8,height= 20,
                            plotOutput("qualPlot")),
                  column(width= 4,
                         fluidRow(
                           infoBox(title = NULL, width = 12, "Customers can select the number of bedrooms and know about the price and frequency of the house sold",color = "maroon", icon = shiny::icon("building"))),
                         hr(),
                         fluidRow(
                           infoBox(title = NULL, width = 12, "55% of the houses in the data has 3 bedrooms", icon = shiny::icon("fort-awesome"))),
                         hr(),
                         fluidRow(
                           infoBox(title = NULL, width = 12, "Over 50% of houses that are sold are of average quality", color = "green", icon = shiny::icon("chart-line"))
                         )
              )))),
      tabItem(tabName = "table",
              fluidPage(
                titlePanel("Data Table of Location and Number of Bedrooms"),
                fluidRow(
                  column(4,
                         selectInput("loc","Location:",c("All",unique(as.character(d$Location))))),
                  column(4,
                         selectInput("bed","Number of Bedrooms:",c("All","0","1","2","3","4","5","6","7","8")))),
                DT::dataTableOutput("table")
              )
      )
    )
  )
)

server <- function(input, output) {
  #Output of the Distribution Plot of House Price per number of Bedrooms
  output$distPlot <- renderPlot({
    if(input$a=='Bed0'){
      i<-1}
    if(input$a=='Bed1'){
      i<-2}
    if(input$a=='Bed2'){
      i<-3}
    if(input$a=='Bed3'){
      i<-4}
    if(input$a=='Bed4'){
      i<-5}
    if(input$a=='Bed5'){
      i<-6}
    if(input$a=='Bed6'){
      i<-7}
    if(input$a=='Bed7'){
      i<-8}
    if(input$a=='Bed8'){
      i<-9}
    
    x <- a[, i]
    
    hist(x, breaks=20,  col = "maroon", border = 'white',main = "Histogram of House Price as per number of Bedrooms", xlab ="House Prices $(in thousands)", ylab="Frequency")
  })
  
  #Output of the Histogram of House Prices and the quality of Houses
  output$qualPlot <- renderPlot({
    hist(c$Qual, breaks=10, col = 'green4', border = 'white', xlab ="Quality", ylab="Frequency", main="The frequency of houses sold as per the quality")
    axis(1, at=seq(0, 10, by=1), labels=c("0","1", "2", "3", 
                                          "4", "5", "6", "7", "8", "9", "10")) 
  })
  
  #Tab Panel Graphs
  
  #Monthly number of Houses
  output$bMonth <- renderPlot({
    
    #plot Monthly number of Houses
    myplot<- ggplot(data=bMonth, aes(x= Month, y= n),xlab = "Month", ylab ="Frequency") + 
      geom_line(col= "blue", size=2)  + 
      scale_x_continuous(breaks=1:12,
                         labels=c("Jan", "Feb", "Mar", "Apr",
                                  "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))  +
      
      #label for some values   
      geom_text(data = bMonth[c(5,6,7), ],aes(label=n), vjust=c(-2.5,-1,-1.5)) 
    #remove the backround 
    myplot +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))  
  })
  
  #Monthly Average House Price
  output$bMonthPrice <- renderPlot({
    bMonth$mean <- ceiling(bMonth$mean)
    #plot Monthly Average House Price
    myplot2<- ggplot(data=bMonth, aes(x= Month, y= mean),xlab = "Month", ylab ="Mean_House_Prices") + 
      geom_line(col= "green4", size=2)  + 
      scale_x_continuous(breaks=1:12,
                         labels=c("Jan", "Feb", "Mar", "Apr",
                                  "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))  +
      #label for some values
      geom_text(data = bMonth[c(4,9), ],aes(label=mean), vjust=c(1.5,-1)) 
    #remove the backround 
    myplot2 +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))  
    
  })
  
  #Yearly number of Houses
  output$bYear <- renderPlot({
    
    #plot Yearly number of Houses
    Yeargraph<- ggplot(data=bYear, aes(x= Year, y= n),xlab = "Year", ylab ="Frequency")  + 
      geom_line(col= "red", size=2)+ scale_y_continuous(limits = c(150, 350))+
      #label for some values    
      geom_text(data = bYear[4, ],aes(label= n), vjust=-1) 
    Yeargraph +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))  
  })
  
  #Yearly Average House Price
  output$bYearPrice <- renderPlot({
    bYear$mean <- ceiling(bYear$mean)
    #plot Yearly Average House Prices
    YearPrice<- ggplot(data=bYear, aes(x= Year, y= mean),xlab = "Year", ylab ="Mean House Prices") + 
      geom_line(col= "orange", size=2) +   
      #label for some values
      geom_text(data = bYear[c(2,3), ],aes(label=mean), vjust=c(-1,1.5)) 
    #remove the backround 
    YearPrice +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))  
  })
  
  
  # Table for looking up the loaction and number of Bedrooms
  output$table <- DT::renderDataTable(DT::datatable({
    dat <- d
    if (input$loc != "All") {
      dat <- dat[dat$Location == input$loc,]}
    if (input$bed != "All") {
      dat <- dat[dat$Bedroom == input$bed,]}
    dat
  }))
}

shinyApp(ui = ui, server = server)
