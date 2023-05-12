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
                           helpText(HTML("The <strong> Flesch readability tests </strong> are readability tests designed to indicate how difficult a passage in English is to understand.<br>
A high score on the Flesch test means that the text is easier to understand.")),
                           tableOutput("flesch_kincaid_table")),
                  tabPanel("Lexical Diversity", value = "lexical_diversity", verbatimTextOutput("lexical_diversity_output"),
                           helpText(HTML("The <strong> Carroll's Corrected Type-Token Ratio (CTTR) </strong> is a measure of lexical diversity in a text.<br>
         A higher CTTR value indicates greater lexical diversity, which means that the text uses a wider variety of words."))),
                  tabPanel("Lexical Complexity", value = "lexical_complexity", verbatimTextOutput("lexical_complexity_output"),
                           helpText(HTML("These 2 methods are used in this project to measure the lexical complexity. <br><br>
                 <strong>Academic word list (AWL) ratio</strong> <br><br>
                 The AWL ratio reflects the percentage of words in a text included in the academic word list. <br>
                 A higher AWL ratio indicates a more advanced academic writing level in certain texts.<br><br>
                 <strong>Low-frequency words (LFW) ratio</strong> <br><br>
                 A critical barrier to understanding sentences and text is the low-frequency words that make up a sentence. <br>
                 A higher LFW ratio indicates a more complex vocabulary, suggesting that the text contains a higher proportion of relatively uncommon words."))),
                  tabPanel("Syntactic Complexity", value = "syntactic_complexity", verbatimTextOutput("syntactic_complexity_output"),
                           helpText(HTML("<em> The function will take some time to run, so please be patient. </em><br><br>
                                         This function is used to determine what percentage of complex sentences are in the text.<br>
                                         The higher the ratio, the more complex the text."))),
                  # tabPanel("Grammar Check", value = "grammar",
                  #          tableOutput("grammar_check_output"),
                  #          helpText(HTML("<em>The function will take some time to run, so please be patient. </em><br><br>
                  #          Our spell and grammar check will automatically detects and corrects typographical, grammatical, and stylistic errors in text data.<br>
                  #      sentence: in which sentence there is a problem. <br>
                  #      short_message: what potential problems exist that need to be checked."))),
                  tabPanel("Text Similarity", value = "similarity", verbatimTextOutput("similarity_output"),
                           helpText("This function compares the similarity of two functions, please enter the text to be compared."))
      )
    )
  )
)