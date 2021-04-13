#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for random distribution app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("animalarranger: Arrange Animals Evenly in Treatments by Mass"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            h2("Enter Data, Hit Shuffle"),
            p("Make sure each animal has a mass and ID."),
            p("Headers in .csv file must be 'Mass' and 'Animal ID'"),
            #textInput("mass", label = h4("Mass"), value = "5.25, 5.05, 8.55, 5.65, 7.5, 6.55, 7, 6, 6.25, 5.5, 6.75, 7.9, 7.15, 6.05, 5.55, 5.9, 6.45, 4.9, 5.15,5.35, 5.95, 6.9, 7.25, 8.1, 5.35, 6.45, 6.15, 4.95, 5.6, 5.2,5.55, 6.6, 7, 7.75, 5.8, 6.35, 5.35, 4.8, 6.05, 5.1, 5.65, 8.2,6.5, 5.95, 6.25, 7.3, 5.45, 7, 5.25, 5"),
            #textInput("ID", label=h4("Animal ID"), value="1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50"),
            fileInput("file1", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            textInput("numtreats", label=h4("Number of Treatments"), value="5"),
            actionButton("rearrange",label=h3("Shuffle")),
            br(),
            br(),
            br(),
            p("Written by A. East"),
            p(a("GitHub",href="https://github.com/eastandrew/animalarranger"))
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Table", tableOutput("table")),
                        tabPanel("Plot", plotOutput("plot")),
                        tabPanel("ANOVA", tableOutput("anova"))
            )
            
        )
    )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
    
    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    d <- eventReactive(input$rearrange, {
        #Mass <- as.numeric(unlist(strsplit(input$mass,",")))
        #ID <- unlist(strsplit(input$ID,","))
        Treatment <- as.numeric(unlist(strsplit(input$numtreats,",")))
        
        dataset <- read.csv(input$file1$datapath)
        
        #liz <- data.frame(Mass=Mass, ID=ID)
        #liz2 <- liz[order(liz$Mass),]
        dataset2 <- dataset[order(dataset$Mass),]
        #liz2$Treatment <- sample(c(letters[1:Treatment]),prob=c(rep((1/Treatment),Treatment)))
        dataset2$Treatment <- sample(c(letters[1:Treatment]),prob=c(rep((1/Treatment),Treatment)))
        #liz2
        dataset2
        
    })
    
    library(pwr)
    library(sjstats)
    
    
   
    output$table <- renderTable({
        d()
    })
    
    
    output$plot <- renderPlot({
        with(d(), dotchart(d()[,1], labels=d()[,2], groups=factor(d()[,3]), pch=16, xlab="Mass", ylab="Treatment and ID"))#
    }, height=800, width=800, units="px")
    
    
    output$anova <- renderTable({
        with(d(), anova_stats(aov(d()[,1]~factor(d()[,"Treatment"]))))
    })
    
    
    
}

# Create Shiny app ----
shinyApp(ui, server)