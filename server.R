library(UsingR)
library(ggplot2)

shinyServer(
  function(input, output) {
    #create data for sleep-graph
    sleep1 <- c(sample(c(18,17.5,17,16.5,16,15.5,15,14.5,14,13.5,13,12.5,12), size=200, replace = T, prob = NULL),
                sample(c(15,14.5,14), size=200, replace = T, prob = NULL),
                sample(c(12,12.5,13,13.5,14), size=100, replace = T, prob = NULL),
                sample(c(11,11.5,12,12.5,13), size=100, replace = T, prob = NULL),
                sample(c(10,10.5,11), size=100, replace = T, prob = NULL),
                sample(c(8.5,9,9.5,10), size=100, replace = T, prob = NULL),
                sample(c(7.5,8,8.5,9), size=100, replace = T, prob = NULL)
                )
    age1 <- c(rep.int(0,100),rep.int(2/12,100),rep.int(3/12,100),rep.int(1,100),rep.int(3,100),rep.int(5,100),rep.int(12,100),rep.int(18,100),sample(c(19:120), size=100, replace = F, prob = NULL))
    sleepdata <- data.frame(age1,sleep1)
    
    #process the all input into reactive functions
    age <- reactive({
    ifelse(input$age >=0 & input$age<=120,input$age,"Please put in your age in years")
    })
    sleep <- reactive({
      ifelse(input$sleep >0 & input$sleep<24,input$sleep,"Please put in your sleep duration in full hours")
    })
    
    #run a regression to extract the error ranges in accordance with the ggplot's smooth function to use as optimal sleep range
    err.fit <- reactive({
      err <- stats::predict((loess(sleep1~age1,sleepdata)), newdata=age(), se = TRUE)
      err$fit
    })
    err.se.fit <- reactive({
      err <- stats::predict((loess(sleep1~age1,sleepdata)), newdata=age(), se = TRUE)
      err$se.fit
    })
    
    #build the sleep plot
    output$plot2 <- renderPlot({
      age <- c(age(),age())
      sleeprange <- c(err.fit() + 1.96 * err.se.fit(),err.fit() - 1.96 * err.se.fit())
      sleepdata$range <- sleeprange
      data1 <- data.frame(sleeprange,age)
      ggplot(sleepdata, aes(age1, sleep1)) + stat_smooth(se=T,aes(outfit=ymin<<-..ymin..,outfit2=ymax<<-..ymax..,outfit3=y<<-..y..)) + 
        annotate("rect",xmin=-Inf,xmax=Inf,ymin=min(sleepdata$range),ymax=max(sleepdata$range),fill="red",alpha = 0.3) +
        geom_hline(yintercept = (err.fit() + 1.96 * err.se.fit())) +
        geom_hline(yintercept = (err.fit() - 1.96 * err.se.fit())) +
        ylab("Recommended Sleep-Time in Hours") +
        xlab("Age in years")
    })
    output$text <- renderUI({
      str1 <- paste("The red bar indicates the range of your optimal sleep-time, in accordance with your age.")
      HTML(paste(str1))
    })
    
    #process the whole first tab with sleep recommendation calculations
    output$sleepout <- renderUI({
      up.range <- strftime(as.POSIXct('2012-01-23 00:00:00 EST') + round(err.fit() + 1.96 * err.se.fit(),digits=2) * 3600, format="%H:%M")
      low.range <- strftime(as.POSIXct('2012-01-23 00:00:00 EST') + round(err.fit() - 1.96 * err.se.fit(),digits=2) * 3600, format="%H:%M")
      text1 <- paste("At an age of ", input$age, " years,")
      text2 <- paste("your upper recommended sleep limit lies at ", up.range," [hrs:min], and")
      text3 <- paste("your lower recommended sleep limit lies at ", low.range," [hrs:min].")
      
      text4 <- paste("Given your usual sleep time of ",
                     strftime(as.POSIXct('2012-01-23 00:00:00 EST') + round(input$sleep,digits=2) * 3600, format="%H:%M"),',')
      text5 <- paste("Your minimum sleep difference accounts for [hrs:min]:",
                     ifelse((input$sleep <= round(err.fit() + 1.96 * err.se.fit(),digits=2)) && (input$sleep >= round(err.fit() - 1.96 * err.se.fit(),digits=2)), "Nothing, you sleep the recommended amount", 
                            ifelse(
                       input$sleep > round(err.fit() + 1.96 * err.se.fit(),digits=2),
                       strftime(as.POSIXct('2012-01-23 00:00:00 EST') + (input$sleep - (round(err.fit() + 1.96 * err.se.fit(),digits=2))) * 3600, format="%H:%M"), 
                       strftime(as.POSIXct('2012-01-23 00:00:00 EST') + ((round(err.fit() - 1.96 * err.se.fit(),digits=2)) - input$sleep) * 3600, format="%H:%M"))),".")
      
      time1 <- strftime(as.POSIXct('2012-01-23 00:00:00 EST') + input$sleephour * 3600 + input$sleepminute * 60, format="%H:%M")
      time2 <- strftime(as.POSIXct('2012-01-23 00:00:00 EST') + input$wakehour * 3600 + input$wakeminute * 60, format="%H:%M")
      timediff1 <- as.POSIXct('2012-01-23 00:00:00 EST') + input$sleephour * 3600 + input$sleepminute * 60
      timediff2 <- as.POSIXct('2012-01-24 00:00:00 EST') + input$wakehour * 3600 + input$wakeminute * 60
      decimalplaces <- function(x) {
        if ((x %% 1) != 0) {
          nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
        } else {
          return(0)
        }
      }
      text6 <- paste("Given that you sleep from [hrs:min] ", time1,
                     "to ", time2,",")
      text7 <- paste("you should consider to ", '\n', ifelse(is.na(difftime(timediff1,timediff2)), "...awaiting input", ifelse(
        decimalplaces(as.numeric(difftime(timediff1,timediff2) / 1.5)) == 0,
                                                          "do nothing, your sleep pattern is optimizing REM breaks.",
                                                          ifelse(
                                                            sqrt((round((as.numeric(difftime(timediff1,timediff2) / 1.5)),digits=4) %% 1)^2) > 0.5, 
                                                            paste("sleep [hrs:min] ", (strftime(as.POSIXct('2012-01-23 00:00:00 EST') + sqrt((round(((round((as.numeric(difftime(timediff1,timediff2) / 1.5)),digits=4) %% 1 *90) -90),0))^2) * 60, format="%H:%M")), "less to optimize REM breaks."),
                                                            paste("sleep [hrs:min] ", (strftime(as.POSIXct('2012-01-23 00:00:00 EST') + sqrt((round(((round((as.numeric(difftime(timediff1,timediff2) / 1.5)),digits=4) %% 1 *90)),0))^2) * 60, format="%H:%M")), "longer to optimize REM breaks.")
                                                            ))))
      text8 <- paste("Don't forget to check out Tab2 with your optimal sleep-range on the sleep-graph!")
      
      HTML(paste(text1, text2, text3,'\n', text4, text5, '\n', text6, text7, '\n', text8, sep = '<br/>'))
    })
    
  })