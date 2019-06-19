#
#

library(shiny)
library(plotly)

ui <- fluidPage(
    
    # Application title
    titlePanel("Adverse Events Explorer Application"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("sel_name", "Input Drug Name", c("Tylenol", "Advil", "Motrin", "Aleve", 
                                                         "Ambien", "Flonase", "Pepcid", "Prilosec",
                                                         "Zantac", "Zoloft"))
        ),
        
        mainPanel(
            plotlyOutput("adverse_events")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$adverse_events <- renderPlotly({
        male <- get_adverse("1", input$sel_name, age) %>% 
            mutate(gender = 'male')
        
        female <- get_adverse("2", input$sel_name, age) %>% 
            mutate(gender = 'female')
        
        gender_events <- full_join(male, female, by = 'term')
        
        gender_events %>% 
            plot_ly() %>%
            add_trace(x = ~reorder(term, count.x), y = ~count.x, name = 'male', text = 'male', type = "bar") %>%
            add_trace(x = ~reorder(term, count.y), y = ~count.y, name = 'female', text = 'female', type = "bar")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
