library(shiny)
               
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("A Simple Artillery Simulation"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
#    "INPUTS",

    sliderInput("stdev", "Artillery precision (in km/std. dev. in the y direction):", min = 1, max = 5, value = 2, step=0.5),
    br(),    
    selectInput("nr_shots", "Number of shots:", c(10,25,50,100,500,1000), selected =10, multiple = FALSE),
    br(),
    br(),
    "SHOW CONTOURS:",
    checkboxInput("sigma_contour", "1-, 2-, and 3-standard deviations", value = FALSE),      
    checkboxInput("cep_contour", "Circular error probable", value = FALSE),   

    br(),
        "Definition: Circular error probable is 'the radius of a circle, centered about the mean, [within] whose boundary is expected to include the landing points of 50% of the rounds' [Wikipedia]."
      
    ),

  mainPanel(plotOutput("shot_plot",height="800px"))

))

