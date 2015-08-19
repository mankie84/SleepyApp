shinyUI(pageWithSidebar(
  headerPanel("Analyzing Sleep Patterns"),
  
  sidebarPanel(
    p("Welcome to the sleepy-App for tired Coursera students.",
      br(),
      "This App will tell you about your optimal sleep-times!",
      br(),
      "Please provide your input requested below."
      ),
    
    numericInput('age', strong('What is your age in years?'),value="0",min=1,max=120,step=1),
    sliderInput("sleep", "How many hours did you sleep last night?",value="0",min=0,max=24,step=0.25,tick=F),
    
    numericInput("sleephour", "Hour: At what day-time (24hr) do you usually you fall asleep (please input the hour)?",value="20",min=0,max=24,step=1),
    numericInput("sleepminute", "Minute: At what day-time (24hr) do you usually fall asleep (please input the minute)?",value="0",min=0,max=60,step=1),
    numericInput("wakehour", "Hour: At what day-time (24hr) do you usually wake up the next day (please input the hour)?",value="7",min=0,max=24,step=1),
    numericInput("wakeminute", "Minute: At what day-time (24hr) do you usually wake up the next day (please input the minute)?",value="0",min=0,max=60,step=1)
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Sleep Assessment", htmlOutput("sleepout")),
      tabPanel("Your Position on the Sleep-Graph", h5(plotOutput("plot2"), htmlOutput("text")))
    )
  )
))