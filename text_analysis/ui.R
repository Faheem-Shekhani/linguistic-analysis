library(shiny)

fluidPage(
  titlePanel("Linguistic Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("inputText1", "Please enter your text as follows:", "", width = "100%", height = "200px"),
      conditionalPanel(
        condition = "input.tabs === 'similarity'",
        textAreaInput("inputText2", "Please enter your second text:", "", width = "100%", height = "200px")
      ),
      actionButton("submitBtn", "Submit"),
      textOutput("wordCount")
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Readability", value = "readability", verbatimTextOutput("readability_output"),
                           helpText(HTML("The Flesch–Kincaid readability tests are readability tests designed to indicate how difficult a passage in English is to understand.<br>
A high score on the Flesch–Kincaid test means that the text is easier to understand.")),
                           tableOutput("flesch_kincaid_table")),
                  tabPanel("Lexical Diversity", value = "lexical_diversity", verbatimTextOutput("lexical_diversity_output"),
                           helpText(HTML("The Carroll's Corrected Type-Token Ratio (CTTR) is a measure of lexical diversity in a text.<br>
         It is calculated by dividing the number of unique words (types) by the square root of twice the number of total words (tokens) in a text.<br>
         A higher CTTR value indicates greater lexical diversity, which means that the text uses a wider variety of words."))),
                  tabPanel("Lexical Complexity", value = "lexical_complexity", verbatimTextOutput("lexical_complexity_output")),
                  tabPanel("Syntactic Complexity", value = "syntactic_complexity", verbatimTextOutput("syntactic_complexity_output")),
                  tabPanel("Growth", value = "growth", verbatimTextOutput("growth_output")),
                  tabPanel("Text Similarity", value = "similarity", verbatimTextOutput("similarity_output"))
      )
    )
  )
)