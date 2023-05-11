pacman::p_load_gh(
  "trinker/qdapDictionaries",
  "trinker/qdapRegex",
  "trinker/qdapTools",
  "trinker/qdap"
)

library(shiny)
library(tidyverse)
library(tidytext)
library(dplyr)
library(tidyr)
library(qdap)
library(sylcount)
library(spacyr)
library(hunspell)
library(textstem)
library(tokenizers)
library(languageR)
library(zipfR)
library(koRpus)
library(quanteda)
library(quanteda.textstats)
library(ggplot2)
library(reshape2)
library(igraph)
library(stringr)
library(word.lists)

flesch_kincaid_data <- data.frame(
  Score = c("100.00–90.00", "90.0–80.0", "80.0–70.0", "70.0–60.0", "60.0–50.0", "50.0–30.0", "30.0–10.0", "10.0–0.0"),
  `School level (US)` = c("5th grade", "6th grade", "7th grade", "8th & 9th grade", "10th to 12th grade", "College", "College graduate", "Professional"),
  Notes = c("Very easy to read. Easily understood by an average 11-year-old student.",
            "Easy to read. Conversational English for consumers.",
            "Fairly easy to read.",
            "Plain English. Easily understood by 13- to 15-year-old students.",
            "Fairly difficult to read.",
            "Difficult to read.",
            "Very difficult to read. Best understood by university graduates.",
            "Extremely difficult to read. Best understood by university graduates.")
)

function(input, output) {
  
  userText1 <- eventReactive(input$submitBtn, {
    input$inputText1
  })
  
  userText2 <- eventReactive(input$submitBtn, {
    input$inputText2
  })
  
  # Display the number of words entered
  output$wordCount <- renderText({
    word_count1 <- if (input$inputText1 != "") str_count(input$inputText1, "\\S+") else 0
    word_count2 <- if (input$inputText2 != "") str_count(input$inputText2, "\\S+") else 0
    total_word_count <- word_count1 + word_count2
    paste("Word count:", total_word_count, sep = " ")
  })
  
  
  output$readability_output <- renderPrint({
    if (userText1() != "") {
      readability_score <- qdap::replace_contraction(userText1()) %>%
        qdap::replace_abbreviation() %>%
        textstat_readability(measure = "Flesch")
      readability_score
    } else {
      "Please enter the text on the left and click the submit button to calculate readability."
    }
  })
  
  output$flesch_kincaid_table <- renderTable({
    flesch_kincaid_data
  }, rownames = TRUE)
  
  output$lexical_complexity_output <- renderPrint({
    if (userText1() != "") {
      text_cleaned <- qdap::replace_contraction(userText1()) %>%
        qdap::replace_abbreviation()
      
      words <- tibble(text = text_cleaned) %>%
        mutate(doc = row_number()) %>%
        unnest_tokens(output = word_token, input = text, token = "words") %>%
        mutate(word_lemma = lemmatize_words(word_token)) %>%
        unnest(word_lemma) %>%
        select(doc, word_token, word_lemma)
      
      data.words.count <- words %>%
        group_by(doc, word_lemma) %>%
        summarize(count = n()) %>%
        ungroup()
      
      data.words.count.per.doc <- words %>%
        group_by(doc) %>%
        summarize(count = n()) %>%
        ungroup()
      
      data.words.count <- left_join(data.words.count, data.words.count.per.doc, by = c("doc" = "doc"))
      data.words.count$word.frequency <- data.words.count$count.x / data.words.count$count.y
      
      lex_com_1 <- data.words.count %>% filter(word.frequency < 0.01) %>%
        group_by(doc) %>%
        summarize(N_low_freq_words = n(), Nw = max(count.y), low_freq_ratio = n() / max(count.y)) %>%
        ungroup()
      
      lex_com_1
    } else {
      "Please enter the text on the left and click the submit button to calculate lexical complexity."
    }
  })
  
  output$lexical_diversity_output <- renderPrint({
    if (userText1() != "") {
      lexdiv_score <- qdap::replace_contraction(userText1()) %>%
        qdap::replace_abbreviation() %>%
        quanteda::tokens() %>%
        quanteda.textstats::textstat_lexdiv(measure ="CTTR")
      lexdiv_score
    } else {
      "Please enter the text on the left and click the submit button to calculate lexical diversity."
    }
  })
  
  output$growth_output <- renderPrint({
    if (userText1() != "") {
      # Remove all special characters using regex
      cleaned_text <- userText1() %>%
        qdap::bracketX() %>%
        qdap::replace_contraction() %>%
        gsub("[^[:alpha:][:space:]]", " ", .)
      
      # Tokenize
      tokenized_text <- tibble(text = cleaned_text) %>%
        unnest_tokens(text, output = word_token, token = "words", strip_punct = F) %>%
        anti_join(stop_words, by = c("word_token" = "word")) %>%
        pull(word_token)
      
      # Calculate growth
      growth_result <- growth.fnc(text = tokenized_text, size = 100, nchunks = 1)
      growth_result
    } else {
      "Please enter the text on the left and click the submit button to calculate growth."
    }
  })
  
  output$syntactic_complexity_output <- renderPrint({
    if (userText1() != "") {
      text_cleaned <- qdap::replace_contraction(userText1()) %>%
        qdap::replace_abbreviation()
      
      spacy_initialize()
      doc1 <- spacy_parse(text_cleaned)
      spacy_finalize()
      
      doc1$syn_complexity_check <- (doc1$pos == "SCONJ") |
        (doc1$pos %in% c("DET", "PRON", "ADV") & (str_detect(doc1$lemma, "^wh") | str_detect(doc1$lemma, "that"))) |
        (doc1$pos %in% c("ADV") & (doc1$lemma %in% c("accordingly", "also", "anyway", "besides", "certainly", "consequently", "finally", "further", "furthermore", "hence", "however", "incindentally", "indeed", "instead", "likewise", "meanwhile", "moreover", "namely", "nevertheless", "next", "nonetheless", "now", "otherwise", "similarly", "still", "then", "thereafter", "therefore", "thus", "undoubtedly", "simultaneously", "firstly", "secondly", "thirdly", "lately", "yet", "again", "regardless", "rather", "comparatively")))
      
      syn_com <- doc1 %>%
        group_by(doc_id, sentence_id) %>%
        summarize(Nvb = sum(pos == "VERB"),
                  Nstcplx = sum(syn_complexity_check == TRUE),
                  Nst = n_distinct(sentence_id),
                  Nw = n()) %>%
        group_by(doc_id) %>%
        summarize(Nvb = sum(Nvb),
                  Nstcplx = sum(Nstcplx > 0),
                  Nst = sum(Nst),
                  Nw = sum(Nw)) %>%
        mutate(F_C = (Nvb / Nst) * (Nw / Nst),
               Complex_sntc = Nstcplx / Nst) %>%
        ungroup
      
      syn_com
    } else {
      "Please enter the text on the left and click the submit button to calculate syntactic complexity."
    }
  })
  
  # Use the text similarity of two inputs and show the result
  output$similarity_output <- renderPrint({
    if (userText1() != "" && userText2() != "") {
      
      corpus <- corpus(c(userText1(),userText2()))
      dfm <- dfm(corpus)
      
      # ejaccard similarity
      ejaccard_sim <- textstat_simil(dfm, method = "ejaccard")
      ejaccard_sim
    } else {
      "Please enter texts in both input fields and click the submit button to calculate the similarity comparison."
    }
  })
}