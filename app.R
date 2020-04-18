#
# This is a web application that allows you to get medical translations in another language. 
# You can run the application by clicking the 'Run App' button above.
#
#
#

library(dplyr)
library(DT)
library(shinythemes)
library(shiny)

# Load the File here
translation_table <- readRDS("medi-translate.rds")

# Define UI for the Medi-Translate application
ui <- fluidPage(
    
    # Set Shiny Theme
    theme = shinytheme("united"),
    
    # Application title
    titlePanel("Medi-Translate", windowTitle = "Medi-Translate"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "language", 
                        label = "Language",
                        choices = c("Bengali", "Bulgarian", "Chinese", "French", "German", "Hindi", "Italian", "Korean", "Malay", "Portugese", "Tamil", "Telugu", "Turkish", "Vietnamese"
                        )),
            selectInput(inputId = "category", 
                        label = "Category",
                        choices = c("All",
                                    "Greetings",
                                    "Contact with Others",
                                    "Medical Conditions",
                                    "Medical Instructions",
                                    "Symptoms"
                        )), width = 3
        ),
        
        # Output of Shiny pp
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Translation",
                                 h4("Text and Corresponding Translations"),
                                 br(),
                                 DT::dataTableOutput("translations")),
                        tabPanel("Info",
                                 br(),
                                 p("Thank you for visiting this page! Medi-Translate is a web-app inspired by the incredible work
                                 from Sudesna Chowdhury, which you can find",
                                   tags$a(href="https://sudesnaroychowdhury.wixsite.com/covid", "here!"), "We hope to provide a 
                                 platform that facilitates simple to use language translation of frequently used medical instructions
                                 and questions. Hopefully, this tool can help enhance medical assistance and communication between 
                                 patients and doctors."),
                                 br(),
                                 p("The translation for the different languages was a true team effort and this web-app would not 
                                 be possible without the kind help from so many contributors. We are currently working on adding more languages. 
                                 The Tamil and Bengali were taken from Sudesna's website."), 
                                 br(),
                                 p("If you have any questions / feedback or would like to contribute, please feel free to 
                                 shoot me an email at gl2668@columbia.edu. I welcome and greatly appreciate it!",
                                   tags$a(href="https://github.com/gl2668/Medi-Translate", "Github link here.")))
            ), width = 9
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    # Data Table Output
    output$translations <- renderDataTable({
        if (input$category == "All"){
            tt <- translation_table %>% 
                dplyr::filter(Language == input$language) %>%
                select("Text" = Text,
                       "Translation" = Translation,
                       "Category" = Category)
            DT::datatable(tt,
                          caption = 'Type into the text box if you are looking for a specific word',
                          filter = 'top',
                          rownames = FALSE,
                          options = list(pageLength = 100,
                                         columnDefs = list(list(className = 'dt-center', targets = '_all'))),
                          escape = FALSE) %>%
                formatStyle('Translation', backgroundColor = '#FFE5CC')
        } else {
            tt <- translation_table %>% 
                dplyr::filter(Category == input$category,
                              Language == input$language) %>%
                select("Text" = Text,
                       "Translation" = Translation,
                       "Category" = Category)
            DT::datatable(tt,
                          caption = 'Type into the text box if you are looking for a specific word',
                          filter = 'top',
                          rownames = FALSE,
                          options = list(pageLength = 100,
                                         columnDefs = list(list(className = 'dt-center', targets = '_all'))),
                          escape = FALSE) %>%
                formatStyle('Translation', backgroundColor = '#FFE5CC')
        }
    })
}

# Run the application
# You should not have any R code after this line of code
shinyApp(ui = ui, server = server)
