library(shiny)
library(ggplot2)
ui <- fluidPage(
  titlePanel("Withdraw"),
  sidebarLayout(
    sidebarPanel(
      
      numericInput("initial_asset",label = h3( "Initial asset:"),value = 5000),
      sliderInput("Time",label = h3("End Time:"),min = 0,max = 100,value = 30),
      sliderInput("Age",label = h3("Start age/end age:"),min = 0,max = 110,value = c(30,60)),
      numericInput("mu",label = h3( "mu:"),value = 0.0671,step = 0.0001),
      numericInput("sig",label = h3( "sig:"),value = 0.1524,step = 0.0001)
      
    ),
    mainPanel(
      h3("Histogram for D0"),
      plotOutput("myhist"),
   
      textOutput("runtime"),
    
      textOutput("Mean"),
      h3("Dataframe of all D0s:"),
      dataTableOutput("d0")
      
    )
    
    
    
  )
  
)



server <- function(input,output){
  Generate_scenario <- reactive({
    start_age <- input$Age[1]
    end_age <- input$Age[2]
    mu <- input$mu
    sig <- input$sig
    n_sim <- 1000
    
    #
    duration <- end_age - start_age
    set.seed(1000)
    Z <- matrix(data = rnorm(n_sim * duration),nrow = duration,ncol = n_sim)
    rset <- exp(mu - 0.5 * sig^2) * exp(sig)^(Z)
    return(rset)
  })
  
  myfun <- function(r_vec,initial_asset,I,EndTime=30){
    I[1] <- 1
    x <- prod(initial_asset,r_vec[1:EndTime])
    a <- rep(0,EndTime)
    for (k in 1:EndTime) {
      a[k] <- prod(r_vec[k:EndTime],I[1:k])
    }
    y <- sum(a)
    d0 <- x/y
    
    return(d0)
  }
    
  
  I1 <- 1+rnorm(1000,0.03,0.001)
  df <- reactive({
    t1 <- Sys.time()
    df <- apply(X=Generate_scenario(),2,FUN = myfun,initial_asset=input$initial_asset,
                I=I1,EndTime=input$Time)
    t2 <- Sys.time()
    runtime <- t2-t1
    meanvalue <- mean(df)
    df <- data.frame(df)
    colnames(df) <- "D0"
    
    return(list(df,runtime,meanvalue))
    })
  

 output$d0 <- renderDataTable({
   as.data.frame(df()[1]) 
   })
  
 output$myhist <- renderPlot({
   ggplot(data = data.frame(df()[1]),aes(x=D0,fill="coral"))+
     geom_histogram()
     
    })
 
 output$runtime <- renderText({paste(
   "Runtime is:",toString(df()[2])
   )
   })

 output$Mean <- renderText({paste(
   "Mean of D0 is:",df()[3]
   )
 })

  
  
}

myapp <- shinyApp(ui,server)
myapp


