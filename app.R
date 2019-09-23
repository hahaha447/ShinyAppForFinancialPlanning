library(shiny)
library(ggplot2)
ui <- fluidPage(
  titlePanel("Withdrawal amount calculation"),
  sidebarLayout(
    sidebarPanel(
      
      numericInput("initial_asset",label = h3( "Initial asset:"),value = 5000),
      sliderInput("Time",label = h3("End Time:"),min = 0,max = 100,value = 30),
      sliderInput("Age",label = h3("Start age/end age:"),min = 0,max = 110,value = c(30,60)),
      numericInput("quantile",label = h3("Quantile"),value = 0.05,step = 0.01),
      numericInput("threshold",label = h3("Threshold"),value = 0,step = 100),
      br(),
      h2("parameters of return matrix"),
      numericInput("mu",label = h3( "mu:"),value = 0.04,step = 0.01),
      numericInput("sig",label = h3( "sig:"),value = 0.1,step = 0.01),
      sliderInput("n_sim",label = h3("number of simulation"),min = 0,max = 2000,step = 100,value = 1000)
      
    ),
    mainPanel(
      h3("Remaining"),
      plotOutput("remaining"),
      h3("Histogram for D0"),
      plotOutput("myhist"),
   
      textOutput("runtime"),
      h3(textOutput("quantile")),
    
      textOutput("Mean"),
      h5("Dataframe of all D0s:"),
      dataTableOutput("d0")
      
    )
    
    
    
  )
  
)



server <- function(input,output){
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  Generate_scenario <- reactive({
    start_age <- input$Age[1]
    end_age <- input$Age[2]
    mu <- input$mu
    sig <- input$sig
    n_sim <- input$n_sim
    
    #
    duration <- end_age - start_age
    set.seed(1000)
    Z <- matrix(data = rnorm(n_sim * duration),nrow = duration,ncol = n_sim)
    rset <- exp(mu - 0.5 * sig^2) * exp(sig)^(Z)
    return(rset)
  })
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  myfun <- function(r_vec,initial_asset,I=rep(1,30),EndTime=30,ending_value=0,cashflow=rep(0,50)){
    I[1] <- 1
    cashflow_endvalue <- cashflow
    x <- prod(initial_asset,r_vec[1:EndTime])
    a <- rep(0,EndTime)
    b <- rep(0,EndTime)
    for (k in 1:EndTime) {
      a[k] <- prod(r_vec[k:EndTime],I[1:k])
      b[k] <- prod(cashflow[k],r_vec[k:EndTime],1/I[1:k])
    }
    y <- sum(a)
    d0 <- (x+sum(b)-ending_value)/y
    return(d0)
  }
  #----------------------------------------------------------------------------------------------------
  calculate_d0_NR <-  function(n_year,quantile=0.05,S0,real_return,d0_vec){
    n <- quantile*length(d0_vec)
    d0_optimal <- sort(d0_vec)[n]
    d0_result <- rep(d0_optimal,n_year)
    remaining_result <- rep(S0,n_year)
    for (i in 1:n_year) {
      S0 <- (S0-d0_optimal)*real_return[i] 
      remaining_result[i] <- S0
    }
    return(list("Withdrwal"=d0_result,"Remaining_value"=remaining_result))
  }
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  I1 <- reactive({I <- 1+rnorm(Generate_scenario(),0.00,0.000)
                 return(I)})
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  df <- reactive({
    I1 <- I1()
    R <- Generate_scenario()
    t1 <- Sys.time()
    df <- apply(X=R,2,FUN = myfun,initial_asset=input$initial_asset,
                I=I1,EndTime=input$Time,ending_value=input$threshold)
    t2 <- Sys.time()
    runtime <- t2-t1
    actural_return <- R[,which(order(colSums(R))==input$n_sim/2)]
    
    quantile <- calculate_d0_NR(n_year = input$Time,quantile=input$quantile,S0 = input$initial_asset,real_return =actural_return,d0_vec = df)
    df <- data.frame(df)
    colnames(df) <- "D0"
    return(list(df,runtime,quantile$Withdrwal[[1]],quantile$Remaining_value))
    })

  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

 output$d0 <- renderDataTable({
   as.data.frame(df()[1]) 
   })
  
 output$myhist <- renderPlot({
   ggplot(data = data.frame(df()[1]),aes(x=D0,fill="coral"))+
     geom_histogram()
    })
 
 output$remaining <- renderPlot({
   df <- data.frame(df()[4])
   colnames(df) <- c("remaining")
   df$time <- seq(1:length(df$remaining))
   ggplot(data = df,aes(y=remaining,x=time))+
     geom_point()+
     geom_line()+
     geom_hline(yintercept = input$threshold,col="red")
 })
 
 output$runtime <- renderText({paste(
   "Runtime is:",toString(df()[2])
   )
   })



 output$quantile <- renderText({paste(
   "Quantile of D0 is:",toString(df()[3])
 )
 })
 
 
 
 
  
}

myapp <- shinyApp(ui,server)
myapp


