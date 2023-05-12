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
library(hunspell)
library(textstem)
library(tokenizers)
library(zipfR)
library(koRpus)
library(quanteda)
library(quanteda.textstats)
library(ggplot2)
library(reshape2)
library(igraph)
library(stringr)
library(word.lists)
library(udpipe)
library(flextable)
if (!requireNamespace("LanguageToolR", quietly = TRUE)) {
  install.packages("LanguageToolR")
}
library(LanguageToolR)
if (!requireNamespace("DT", quietly = TRUE)) {
  install.packages("DT")
}
library(DT)


klippy::klippy()


flesch_kincaid_data <- data.frame(
  Score = c("100.00–90.00", "90.0–80.0", "80.0–70.0", "70.0–60.0", "60.0–50.0", "50.0–30.0", "30.0–10.0", "Less than 10.0"),
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
        summarize(N_low_freq_words = n(), Nw = max(count.y), LFW_ratio = n() / max(count.y)) %>%
        ungroup()
      
      familiar.words <- as.data.frame(data_char_wordlists$dalechall)
      familiar.words$type <- "familiar"
      colnames(familiar.words) <- c("word", "type")
      
      data.words.2 <- left_join(words, list_academic, by = c("word_lemma" = "lemma"))
      
      data.words.2 <- left_join(data.words.2, familiar.words, by = c("word_lemma" = "word"))
      
      lex_com_2 <- data.words.2 %>%
        group_by(doc) %>%
        summarize(Nw_cplx = sum(on_list == "academic" & is.na(type), na.rm = TRUE),
                  Nw = n()) %>%
        mutate(AWL_ratio = Nw_cplx / Nw) %>%
        ungroup()
      
      final_result <- merge(lex_com_1 %>% select(doc, LFW_ratio), lex_com_2 %>% select(doc, AWL_ratio), by = "doc")
      final_result
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
  
  # output$grammar_check_output <- renderTable({
  #   if (userText1() != "") {
  #     text_cleaned <- qdap::replace_contraction(userText1()) %>%
  #       qdap::replace_abbreviation()
  #     
  #     spell_grammar_check_secondary <- LanguageToolR::languagetool(text_cleaned)
  #     
  #     if (nrow(spell_grammar_check_secondary) == 0) {
  #       empty_df <- data.frame(sentence = "Congratulations, no grammatical errors have been detected!",
  #                              short_message = "")
  #       datatable(empty_df, options = list(pageLength = 5), rownames = FALSE)
  #     } else {
  #       datatable(select(spell_grammar_check_secondary, sentence, short_message),
  #                 options = list(pageLength = 5), rownames = FALSE)
  #     }
  #   } else {
  #     "Please enter the text on the left and click the submit button to get result."
  #   }
  # })
  
  output$syntactic_complexity_output <- renderPrint({
    if (userText1() != "") {
      text_cleaned <- qdap::replace_contraction(userText1()) %>%
        qdap::replace_abbreviation()
      
      # Download the language model
      m_eng <- udpipe::udpipe_download_model(language = "english-ewt")
      # Load the language model using the downloaded model file path
      m_eng <- udpipe_load_model(file = m_eng$file_model)
      
      # tokenize, tag, dependency parsing
      text_anndf <- udpipe::udpipe_annotate(m_eng, x = as.character(text_cleaned)) %>%
        as.data.frame() %>%
        dplyr::select(-sentence)
      # view tagged_text
      # head(text_anndf, 50)
      tagged_text <- paste(text_anndf$token, "/", text_anndf$xpos, collapse = " ", sep = "")
      
      text_anndf$syn_complexity_check <- text_anndf$upos =="SCONJ"| text_anndf$xpos %in% c("WDT", "WP", "WP$", "WRB")
      # SCONJ: subordinating conjunction
      # "WDT": Wh-determiner
      # "WP": Wh-pronoun
      # "WP$": Possessive wh-pronoun
      # "WRB": Wh-adverb
      check <- text_anndf %>% select(doc_id,sentence_id,token,lemma,upos,xpos,syn_complexity_check)
      
      syn_com <- text_anndf %>%
        group_by(doc_id, sentence_id) %>%
        summarize(Nvb = sum(xpos=="VB"),
                  Nstcplx = sum(syn_complexity_check==TRUE),
                  Nst = n_distinct(sentence_id),
                  Nw = n()) %>%
        group_by(doc_id) %>%
        summarize(Nstcplx = sum(Nstcplx>0),
                  Nst = sum(Nst)) %>%
        mutate(Complex_sntc_ratio = Nstcplx / Nst) %>%
        ungroup
      
      syn_com %>%
        select(Complex_sntc_ratio)
      
    } else {
      "Please enter the text on the left and click the submit button to calculate syntactic complexity."
    }
  })
  
  # Use the text similarity of two inputs and show the result
  output$similarity_output <- renderPrint({
    if (userText1() != "" && userText2() != "") {
      
      dfm <- quanteda::tokens(c(userText1(),userText2())) %>%
        dfm()
      
      # ejaccard similarity
      ejaccard_sim <- textstat_simil(dfm, method = "edice")
      ejaccard_sim
    } else {
      "Please enter texts in both input fields and click the submit button to calculate the similarity comparison."
    }
  })
}