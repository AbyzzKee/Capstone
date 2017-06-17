#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#rm(list = ls())

# ------------------------------------------------------------
# BackOff
# ------------------------------------------------------------

suggestion_1gram <- readRDS("suggestion_1gram.RDS")
suggestion_2gram <- readRDS("suggestion_2gram.RDS")
suggestion_3gram <- readRDS("suggestion_3gram.RDS")
suggestion_4gram <- readRDS("suggestion_4gram.RDS")
suggestion_5gram <- readRDS("suggestion_5gram.RDS")

suggestion_ngram <- list(suggestion_1gram, suggestion_2gram, suggestion_3gram, suggestion_4gram, suggestion_5gram)

getPredictResult <- function(all_result) {
  if(length(all_result) > 4) {
    result_list <- strsplit(all_result, " ")
    result_words <- unlist(lapply(result_list, tail, n = 1))
    result_words <- unique(result_words[!is.na(result_words)])
    
    if(length(result_words) > 4) {
      return(result_words[1:5])
    }
  }
  return(NULL)
}
backoffPredict <- function(input) {
  # Cleaning
  input <- as.character(input)
  input <- tm::removePunctuation(input)
  input <- tm::removeNumbers(input)
  input <- tolower(input)
  input <- tm::stripWhitespace(input)
  #input <- tm::removeWords(input, profanity)
  
  # To make sure that it is cleaned
  split_input <- strsplit(input, " ")
  
  if(length(split_input) > 1)
    return("ERROR")
  if(length(split_input[[1]]) < 1)
    return(suggestion_ngram[[1]])
  
  all_result <- character()
  result_words <- character()
  
  input <- tail(split_input[[1]], length(suggestion_ngram)-1)
  # Ngram search
  for( i in seq(length(input), 1, -1) ) {
    cat_input <- paste(tail(input, i), collapse = " ")
    result_index <- grep(paste0('^', cat_input, " "), suggestion_ngram[[i+1]])[1:5]
    result_index <- result_index[!is.na(result_index)]
    
    result <- as.character(suggestion_ngram[[i+1]][result_index])
    all_result <- c(all_result, result)
    result_words <- getPredictResult(all_result)
    if(!is.null(result_words)) return(result_words);
  } # END for( i in seq(length(input), 1, -1) )
  
  all_result <- c(all_result, suggestion_ngram[[1]])
  result_words <- getPredictResult(all_result)
  return(result_words);
  #return(result_words)
} # END function backoffPredict


#input = "i ha"
#backoffPredict(input)
