library(shiny)
library(ggplot2)
library(tidyverse)
library(citation)

data <- read.csv("unfood.csv")
df <- as.data.frame(data)


#Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Chemical Usage Value in 3 States"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Chemical",
                        "Choose Chemical:",
                        c(
                            unique(as.character(df$Chemical))))
            
            # Show a plot of the generated distribution
            
        ),
        mainPanel(
            plotOutput("Chem_Plot"),
            DT::dataTableOutput("table2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dat <- reactive({
        Cali <- data%>%filter(State == 'CALIFORNIA')%>%filter(Chemical == input$Chemical)
        cal_sum <- sum(as.numeric(Cali$Value))
        
        Flor <- data%>%filter(State == 'FLORIDA')%>%filter(Chemical == input$Chemical)
        flo_sum <- sum(as.numeric(Flor$Value))
        
        Washt <- data%>%filter(State == 'WASHINGTON')%>%filter(Chemical == input$Chemical)
        wa_sum <- sum(as.numeric(Washt$Value))
        df1 <- data.frame('State' = c('CALIFORNIA','FLORIDA','WASHINGTON'),'Value' = c(cal_sum,flo_sum,wa_sum))
    }
    )
    
    output$Chem_Plot <- renderPlot({
        ggplot(dat(), aes(x=State, y=Value, color=State)) +
            geom_bar(stat="identity", fill="white")
        
    })
    
    output$table2 <- DT::renderDataTable(DT::datatable({
        df
    }))
}

# Run the application 
shinyApp(ui = ui, server = server)

# References
# citation("shiny")
# citation("tidyverse")
# citation("ggplot2")
# citation("citation")