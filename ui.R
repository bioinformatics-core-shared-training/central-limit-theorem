fluidPage(
   
   # Application title
   titlePanel("Central limit theorem"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(id="sidebar",
        uiOutput("choose_distr"),
        uiOutput("choose_mu"),
        uiOutput("choose_sigma"),
        uiOutput("choose_n"),
        uiOutput("choose_R"),
        actionButton("go", "Simulate"),
        p(""),
        p("App developed by DLC / 20170927",style = "color:#FFFFFF;"),
        img(src="http://www.cruk.cam.ac.uk/sites/all/themes/cambridge_theme/images/interface/main-logo-small.png",
            width = 100)        
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
            tabPanel("Theoretical density", plotOutput("density"), helpText("This shows the theoretical density...")),
            tabPanel("Estimated mean's density", plotOutput("mean")),
            tabPanel("Estimated coverage of Student's CI", plotOutput("coverage"))
            )
        )
   )
)

