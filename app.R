library(shiny)
library(maps)

myRtl<-"C:\\aug232017b\\dataset.csv"
ds=read.csv(file=myRtl, header=TRUE, sep=",")
head(ds)

ds$State <- as.character(ds$State)
ds$Abbr <- as.character(ds$Abbr)

Tpbar <- mean(ds[,"Temp"])
Sdbar <- mean(ds[,"SunDur"])
Crbar <- mean(ds[,"CriRate"])
Gcbar <- mean(ds[,"Grocery"])
Tsbar <- mean(ds[,"Transport"])
Hhbar <- mean(ds[,"Health"])
Hsbar <- mean(ds[,"Housing"])
Dybar <- mean(ds[,"Density"])
Arbar <- mean(ds[,"AirQuality"])

Tpsdv <- sd(ds[,"Temp"])
Sdsdv <- sd(ds[,"SunDur"])
Crsdv <- sd(ds[,"CriRate"])
Gcsdv <- sd(ds[,"Grocery"])
Tssdv <- sd(ds[,"Transport"])
Hhsdv <- sd(ds[,"Health"])
Hssdv <- sd(ds[,"Housing"])
Dysdv <- sd(ds[,"Density"])
Arsdv <- sd(ds[,"AirQuality"])

v1 <- 1:50
v2 <- 1:50
v3 <- 1:50
v4 <- 1:50
v5 <- 1:50
v6 <- 1:50
v7 <- 1:50
v8 <- 1:50
v9 <- 1:50

for (i in 1:50)
{
  v1[i] <- (ds$Temp[i]-Tpbar)/Tpsdv
  v2[i] <- (ds$SunDur[i]-Sdbar)/Sdsdv
  v3[i] <- (ds$CriRate[i]-Crbar)/Crsdv
  v4[i] <- (ds$Grocery[i]-Gcbar)/Gcsdv
  v5[i] <- (ds$Transport[i]-Tsbar)/Tssdv
  v6[i] <- (ds$Health[i]-Hhbar)/Hhsdv
  v7[i] <- (ds$Housing[i]-Hsbar)/Hssdv
  v8[i] <- (ds$Density[i]-Dybar)/Dysdv
  v9[i] <- (ds$AirQuality[i]-Arbar)/Arsdv
} 

stdds<-data.frame(v1,v2,v3,v4,v5,v6,v7,v8,v9)

myds<-cbind(ds,stdds)

y <- c(0,0.25,0.5,0.75,1.0) 
x <- 1:5
percent <- cbind(x,y)
rm(x)
rm(y)

parameterFunction <- function(Tp,Sd,Cr,Gc,Ts,Hh,Hs,Dy,Ar){
  
  Tp <- as.numeric(Tp) #Average Temperature. Parameter chosen from 1-5: 1.cold 2.cool 3.moderate 4.warm 5.hot
  Tp <- quantile(myds$v1,percent[Tp,2]) #return quantile value in the dataset from 50 states
  Sd <- as.numeric(Sd) #Days of Sunduration. Parameter chosen from 1-5: 1.very less 2.less 3.moderate 4.great 5.adequate
  Sd <- quantile(myds$v2,percent[Sd,2]) #return quantile value in the dataset from 50 states
  Cr <- as.numeric(Cr) #Crime Rate. Parameter chosen from 1-5: 1.very safe 2.safe 3.normal 4.careful 5.risky
  Cr <- quantile(myds$v3,percent[Cr,2]) #return quantile value in the dataset from 50 states
  Gc <- as.numeric(Gc) #Cost of Grocery. Parameter chosen from 1-5: 1.cheap 2.slightly cheap 3.fair 4.slightly expensive 5.expensive
  Gc <- quantile(myds$v4,percent[Gc,2])#return quantile value in the dataset from 50 states
  Ts <- as.numeric(Ts) #Cost of Transportation. Parameter chosen from 1-5: 1.cheap 2.slightly cheap 3.fair 4.slightly expensive 5.expensive
  Ts <- quantile(myds$v5,percent[Ts,2])#return quantile value in the dataset from 50 states
  Hh <- as.numeric(Hh) #Cost of Health care. Parameter chosen from 1-5: 1.cheap 2.slightly cheap 3.fair 4.slightly expensive 5.expensive
  Hh <- quantile(myds$v6,percent[Hh,2])#return quantile value in the dataset from 50 states
  Hs <- as.numeric(Hs) #House Price. Parameter chosen from 1-5: 1.cheap 2.slightly cheap 3.fair 4.slightly expensive 5.expensive
  Hs <- quantile(myds$v7,percent[Hs,2])#return quantile value in the dataset from 50 states
  Dy <- as.numeric(Dy) #Population Density. Parameter chosen from 1-5: 1.rare 2.rural 3.moderate 4.lively 5.crowded
  Dy <- quantile(myds$v8,percent[Dy,2])#return quantile value in the dataset from 50 states
  Ar <- as.numeric(Ar) # 0 for good air quality(AQI<average 42),1 for moderate air quality(AQI>average 42)
  
  
  for (i in 1:50)
  {
    myds$Index[i]<- abs(myds$v1[i]-Tp)+abs(myds$v2[i]-Sd)+abs(myds$v3[i]-Cr)
    +abs(myds$v4[i]-Gc)+abs(myds$v5[i]-Ts)+abs(myds$v6[i]-Hh)
    +abs(myds$v7[i]-Hs)+abs(myds$v8[i]-Dy)-myds$v9*Ar
  }
  
  
  orderds <- myds[order(myds$Index),]
  result <- as.character(orderds[c(1:5),"State"])
  result #Get top 5 fitted state in 50 states
  
}





# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Relocation for Retirement"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      sliderInput("var1", "Average Temperature: 
                  1.cold  2.cool  3.moderate  4.warm  5.hot",
                  min = 1, max = 5,
                  value = 3, step = 1 ),
      
      
      sliderInput("var2", "Days of Sunduration: 
                  1.very less  2.less  3.moderate  4.great  5.adequate",
                  min = 1, max = 5,
                  value = 3, step = 1),
      
      sliderInput("var3", "Crime Rate:
                  1.very safe  2.safe  3.normal  4.careful  5.risky",
                  min = 1, max = 5,
                  value = 3, step = 1),
      
      sliderInput("var4", "Cost of Grocery:
                  1.cheap   2.slightly cheap   3.fair   4.slightly expensive   5.expensive",
                  min = 1, max = 5,
                  value = 3, step = 1),
      
      sliderInput("var5", "Cost of Transportation:
                  1.cheap   2.slightly cheap   3.fair   4.slightly expensive   5.expensive",
                  min = 1, max = 5,
                  value = 3, step = 1),
      
      sliderInput("var6", "Cost of Health Care:
                  1.cheap   2.slightly cheap   3.fair   4.slightly expensive   5.expensive",
                  min = 1, max = 5,
                  value = 3, step = 1),
      
      sliderInput("var7", "House Price:
                  1.cheap   2.slightly cheap   3.fair   4.slightly expensive   5.expensive",
                  min = 1, max = 5,
                  value = 3, step = 1),
      
      sliderInput("var8", "Population Density:
                  1.rare   2.rural   3.moderate   4.lively   5.crowded",
                  min = 1, max = 5,
                  value = 3, step = 1),
      
      sliderInput("var9", "Air Quality: 1.good     2.moderate",
                  min = 0, max = 1,
                  value = 0, step = 1),
      
      actionButton("update", "Update View")
      
    ),
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map", plotOutput("map")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Dataset", tableOutput("dataset")))
    )
  )
      )

# Define server logic for slider examples ----
server <- function(input, output) {
  
  datainput <- eventReactive(input$update, {
    
  parameterFunction(input$var1,input$var2,input$var3,input$var4,input$var5,input$var6,input$var7,input$var8,input$var9)
  },ignoreNULL = FALSE)
  
  
  output$summary <- renderPrint({
    datainput()
  
    
   
    
  })
  
  output$map <- renderPlot({
    map(database = "state")
    c=c(datainput())
    map(database = "state",regions = c,col = "pink",fill=T,add=TRUE)
    
  })
  
  
  output$dataset <- renderTable({
    ds[,c(2:12)]
  })
}

options(shiny.sanitize.errors = FALSE)
shiny.sanitize.errors = FALSE
# Create Shiny app ----
shinyApp(ui, server)

