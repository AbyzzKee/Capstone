setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

cluster <- parallel::makeCluster(parallel::detectCores()) 
doParallel::registerDoParallel(cluster)

# Check for Required Packages
require_packages <- c("readr", "tm")
missing_packages <- setdiff(require_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)  
}

# ------------------------------------------------------------
# Segment Dataset & Make Corpus
# ------------------------------------------------------------

for(segmentPart in 43:100) {
# Decide which part of the dataset 
#segmentPart   <- 1
segmentNumber <- 100
if ( file.exists(paste0("corpus_part", segmentPart, "_", segmentNumber, ".RDS")) ) {
  corpus <- readRDS(paste0("corpus_part", segmentPart, "_", segmentNumber, ".RDS"))
} else {
  start.time <- proc.time() #Sys.time()
  
  # Download Dataset
  if ( !file.exists("en_US.twitter.txt") | !file.exists("en_US.news.txt") | !file.exists("en_US.blogs.txt") ) {
    if ( !file.exists("Coursera-SwiftKey.zip") ) {
      #download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Coursera-SwiftKey.zip");
    }
    unzip("Coursera-SwiftKey.zip", c("final/en_US/en_US.twitter.txt",
                                     "final/en_US/en_US.news.txt","final/en_US/en_US.blogs.txt"))
    file.rename("final/en_US/en_US.twitter.txt", "en_US.twitter.txt")
    file.rename("final/en_US/en_US.news.txt",    "en_US.news.txt")
    file.rename("final/en_US/en_US.blogs.txt",   "en_US.blogs.txt")
    unlink("final", TRUE)
    
    end.time <- proc.time()
    print("Download")
    print(end.time - start.time)
  }
  
  # Load Dataset
  if(file.exists("all_dataset.RDS")) {
    all_dataset <- readRDS("all_dataset.RDS")
  } else {
    # Read File
    en_Twit <- readr::read_lines("en_US.twitter.txt", progress = FALSE)
    en_News <- readr::read_lines("en_US.news.txt", progress = FALSE)
    en_Blog <- readr::read_lines("en_US.blogs.txt", progress = FALSE)
    all_dataset <- c(en_Twit, en_News, en_Blog)
    
    # iconv to prevent tolower error
    all_dataset <- iconv(all_dataset, 'UTF-8', 'ASCII', "byte")
    all_dataset <- all_dataset[!is.na(all_dataset)]
    
    saveRDS(all_dataset, "all_dataset.RDS")
  }
  end.time <- proc.time()
  print("Load")
  print(end.time - start.time)
  
  # Segment Dataset into parts
  dataset_part  <- all_dataset[seq(segmentPart, length(all_dataset), segmentNumber)]
  rm( list = setdiff(ls(), c("dataset_part", "segmentPart", "segmentNumber", "start.time")) ) # clear memory
  
  # Text Mining Cleaning
  corpus <- tm::VCorpus(tm::VectorSource(dataset_part))
  end.time <- proc.time()
  print("VCorpus")
  print(end.time - start.time)
  
  rm( list = setdiff(ls(), c("corpus", "segmentPart", "segmentNumber", "start.time")) ) # clear memory
  gc() # garbage collection
  end.time <- proc.time()
  print("gc")
  print(end.time - start.time)
  
  corpus <- tm::tm_map(corpus, tm::removePunctuation)
  corpus <- tm::tm_map(corpus, tm::removeNumbers)
  corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
  corpus <- tm::tm_map(corpus, tm::stripWhitespace)
  corpus <- tm::tm_map(corpus, tm::PlainTextDocument)
  end.time <- proc.time()
  print("PlainTextDocument")
  print(end.time - start.time)
  
  # Profanity Filtering
  if ( !file.exists("bad-words.txt") ) {
    download.file("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt", "bad-words.txt"); }
  profanity <- readLines("bad-words.txt")
  corpus <- tm::tm_map(corpus, tm::removeWords, profanity)
  end.time <- proc.time()
  print("profanity")
  print(end.time - start.time)
  
  saveRDS(corpus, paste0("corpus_part", segmentPart, "_", segmentNumber, ".RDS"))
}

# ------------------------------------------------------------
# Tokenization Function
# ------------------------------------------------------------

# Tokenization Function
biGramTokenizer <- function(x) {
  unlist(lapply(NLP::ngrams(NLP::words(x), 2), paste, collapse = " "), use.names = FALSE) }
triGramTokenizer <- function(x) {
  unlist(lapply(NLP::ngrams(NLP::words(x), 3), paste, collapse = " "), use.names = FALSE) }
fourGramTokenizer <- function(x) {
  unlist(lapply(NLP::ngrams(NLP::words(x), 4), paste, collapse = " "), use.names = FALSE) }
fiveGramTokenizer <- function(x) {
  unlist(lapply(NLP::ngrams(NLP::words(x), 5), paste, collapse = " "), use.names = FALSE) }

# Get Sorted Term Frequency Function
getFreq <- function(tdm, lowfreq = 3) {
  tdm_freqTerm <- tm::findFreqTerms(tdm, lowfreq = lowfreq)
  tdm <- tdm[tdm_freqTerm,]
  freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  return ( data.frame(WordsCombination = names(freq), 
                      Frequency = freq) )
}

# segmentNumber 20
# lowfreqPercentage1 <- 1.5
# lowfreqPercentage2 <- 0.32
# lowfreqPercentage3 <- 0.07
# lowfreqPercentage4 <- 0.018
# lowfreqPercentage5 <- 0.0056

# segmentNumber 100
lowfreqPercentage2 <- 0.055
lowfreqPercentage3 <- 0.03
lowfreqPercentage4 <- 0.02
lowfreqPercentage5 <- 0.01

# ------------------------------------------------------------
# Ngram
# ------------------------------------------------------------

# 1-gram
# lowfreq <- length(corpus)*lowfreqPercentage1/100
# #lowfreq <- 3000
# if ( file.exists(paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_1gram.RDS")) ) {
#   freq_1gram <- readRDS(paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_1gram.RDS"))
# } else {
#   if ( file.exists(paste0("corpus_part", segmentPart, "_", segmentNumber, "tdm_1gram.RDS")) ) {
#     tdm_1gram <- readRDS(paste0("corpus_part", segmentPart, "_", segmentNumber, "tdm_1gram.RDS"))
#   } else {
#     start.time <- proc.time() #Sys.time()
#     tdm_1gram <- tm::TermDocumentMatrix(corpus)
#     end.time <- proc.time()
#     print("1-gram TermDocumentMatrix")
#     print(end.time - start.time)
#     saveRDS(tdm_1gram, file=paste0("corpus_part", segmentPart, "_", segmentNumber, "tdm_1gram.RDS"))
#   }
#   freq_1gram <- getFreq(tdm_1gram, lowfreq)
#   print(nrow(freq_1gram))
#   saveRDS(freq_1gram, file=paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_1gram.RDS"))
# }

# 2-gram
lowfreq <- length(corpus)*lowfreqPercentage2/100
#lowfreq <- 700
if ( file.exists(paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_2gram.RDS")) ) {
  freq_2gram <- readRDS(paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_2gram.RDS"))
} else {
  if ( file.exists(paste0("corpus_part", segmentPart, "_", segmentNumber, "tdm_2gram.RDS")) ) {
    tdm_2gram <- readRDS(paste0("corpus_part", segmentPart, "_", segmentNumber, "tdm_2gram.RDS"))
  } else {
    start.time <- proc.time() #Sys.time()
    tdm_2gram <- tm::TermDocumentMatrix(corpus, control=list(tokenize=biGramTokenizer))
    end.time <- proc.time()
    print("2-gram TermDocumentMatrix")
    print(end.time - start.time)
    saveRDS(tdm_2gram, file=paste0("corpus_part", segmentPart, "_", segmentNumber, "tdm_2gram.RDS"))
  }
  freq_2gram <- getFreq(tdm_2gram, lowfreq)
  print(nrow(freq_2gram))
  saveRDS(freq_2gram, file=paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_2gram.RDS"))
}

# 3-gram
lowfreq <- length(corpus)*lowfreqPercentage3/100
#lowfreq <- 150
if ( file.exists(paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_3gram.RDS")) ) {
  freq_3gram <- readRDS(paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_3gram.RDS"))
} else {
  if ( file.exists(paste0("corpus_part", segmentPart, "_", segmentNumber, "tdm_3gram.RDS")) ) {
    tdm_3gram <- readRDS(paste0("corpus_part", segmentPart, "_", segmentNumber, "tdm_3gram.RDS"))
  } else {
    start.time <- proc.time() #Sys.time()
    tdm_3gram <- tm::TermDocumentMatrix(corpus, control=list(tokenize=triGramTokenizer))
    end.time <- proc.time()
    print("3-gram TermDocumentMatrix")
    print(end.time - start.time)
    saveRDS(tdm_3gram, file=paste0("corpus_part", segmentPart, "_", segmentNumber, "tdm_3gram.RDS"))
  }
  freq_3gram <- getFreq(tdm_3gram, lowfreq)
  print(nrow(freq_3gram))
  saveRDS(freq_3gram, file=paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_3gram.RDS"))
}

# 4-gram
lowfreq <- length(corpus)*lowfreqPercentage4/100
#lowfreq <- 40
if ( file.exists(paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_4gram.RDS")) ) {
  freq_4gram <- readRDS(paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_4gram.RDS"))
} else {
  if ( file.exists(paste0("corpus_part", segmentPart, "_", segmentNumber, "tdm_4gram.RDS")) ) {
    tdm_4gram <- readRDS(paste0("corpus_part", segmentPart, "_", segmentNumber, "tdm_4gram.RDS"))
  } else {
    start.time <- proc.time() #Sys.time()
    tdm_4gram <- tm::TermDocumentMatrix(corpus, control=list(tokenize=fourGramTokenizer))
    end.time <- proc.time()
    print("4-gram TermDocumentMatrix")
    print(end.time - start.time)
    saveRDS(tdm_4gram, file=paste0("corpus_part", segmentPart, "_", segmentNumber, "tdm_4gram.RDS"))
  }
  freq_4gram <- getFreq(tdm_4gram, lowfreq)
  print(nrow(freq_4gram))
  saveRDS(freq_4gram, file=paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_4gram.RDS"))
}

# 5-gram
lowfreq <- length(corpus)*lowfreqPercentage5/100
#lowfreq <- 12
if ( file.exists(paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_5gram.RDS")) ) {
  freq_5gram <- readRDS(paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_5gram.RDS"))
} else {
  if ( file.exists(paste0("corpus_part", segmentPart, "_", segmentNumber, "tdm_5gram.RDS")) ) {
    tdm_5gram <- readRDS(paste0("corpus_part", segmentPart, "_", segmentNumber, "tdm_5gram.RDS"))
  } else {
    start.time <- proc.time() #Sys.time()
    tdm_5gram <- tm::TermDocumentMatrix(corpus, control=list(tokenize=fiveGramTokenizer))
    end.time <- proc.time()
    print("5-gram TermDocumentMatrix")
    print(end.time - start.time)
    saveRDS(tdm_5gram, file=paste0("corpus_part", segmentPart, "_", segmentNumber, "tdm_5gram.RDS"))
  }
  freq_5gram <- getFreq(tdm_5gram, lowfreq)
  print(nrow(freq_5gram))
  saveRDS(freq_5gram, file=paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_5gram.RDS"))
}
}

combineFreqNgram <- function(freq_ngram, ngram, segmentPart, segmentNumber, lowfreqPercentage) {
  lowfreq <- length(readRDS(paste0("corpus_part", segmentPart, "_", segmentNumber, ".RDS")))*lowfreqPercentage/100
  freq_ngram <- merge(freq_ngram, readRDS(paste0("corpus_part", segmentPart, "_", segmentNumber, "freq", lowfreq, "_", ngram, "gram.RDS")), "WordsCombination", all = T)
  freq_ngram[is.na(freq_ngram)] <- 0
  freq_ngram$Frequency <- freq_ngram$Frequency.x + freq_ngram$Frequency.y
  freq_ngram <- freq_ngram[,c("WordsCombination", "Frequency")]
  return(freq_ngram)
}

# segmentNumber 100
lowfreqPercentage2 <- 0.055
lowfreqPercentage3 <- 0.03
lowfreqPercentage4 <- 0.02
lowfreqPercentage5 <- 0.01

freq_2gram <- data.frame(WordsCombination = character(), Frequency = numeric())
freq_3gram <- data.frame(WordsCombination = character(), Frequency = numeric())
freq_4gram <- data.frame(WordsCombination = character(), Frequency = numeric())
freq_5gram <- data.frame(WordsCombination = character(), Frequency = numeric())

# Decide which part of the dataset 
segmentNumber <- 100
for(segmentPart in 1:42) {
  freq_2gram <- combineFreqNgram(freq_2gram, 2, segmentPart, segmentNumber, lowfreqPercentage2) }
freq_2gram <- freq_2gram[order(freq_2gram$Frequency, decreasing = TRUE),]
saveRDS(freq_2gram, "all_freq_2gram.RDS")

for(segmentPart in 1:42) {
  freq_3gram <- combineFreqNgram(freq_3gram, 3, segmentPart, segmentNumber, lowfreqPercentage3) }
freq_3gram <- freq_3gram[order(freq_3gram$Frequency, decreasing = TRUE),]
saveRDS(freq_3gram, "all_freq_3gram.RDS")

for(segmentPart in 1:42) {
  freq_4gram <- combineFreqNgram(freq_4gram, 4, segmentPart, segmentNumber, lowfreqPercentage4) }
freq_4gram <- freq_4gram[order(freq_4gram$Frequency, decreasing = TRUE),]
saveRDS(freq_4gram, "all_freq_4gram.RDS")

for(segmentPart in 1:42) {
  freq_5gram <- combineFreqNgram(freq_5gram, 5, segmentPart, segmentNumber, lowfreqPercentage5) }
freq_5gram <- freq_5gram[order(freq_5gram$Frequency, decreasing = TRUE),]
saveRDS(freq_5gram, "all_freq_5gram.RDS")


#saveRDS(as.character(freq_1gram[1:5,"WordsCombination"]), "suggestion_1gram.RDS")
saveRDS(as.character(freq_2gram[   ,"WordsCombination"]), "suggestion_2gram.RDS")
saveRDS(as.character(freq_3gram[   ,"WordsCombination"]), "suggestion_3gram.RDS")
saveRDS(as.character(freq_4gram[   ,"WordsCombination"]), "suggestion_4gram.RDS")
saveRDS(as.character(freq_5gram[   ,"WordsCombination"]), "suggestion_5gram.RDS")

parallel::stopCluster(cluster) 
