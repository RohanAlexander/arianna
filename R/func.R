install.packages(c("quanteda", "readtext", "data.table"))
library(quanteda)
library(readtext)
library(data.table)

# Load text samples and turn it into a corpus
training_text <- readtext("samples/*.txt")
corp<- corpus(training_text)

# Tokenize the corpus
toks <-tokens(corp, remove_punct = TRUE)

# Generate unigrams
unigrams <- tokens_ngrams(toks, n = 1)

# Generate bigrams
bigrams <- tokens_ngrams(toks, n = 2)

# Turn n-grams to dfm
unigrams_dfm <- dfm(unigrams)
unigrams_dfm <- dfm_trim(unigrams_dfm)
bigrams_dfm <- dfm(bigrams)
bigrams_dfm <- dfm_trim(bigrams_dfm)

# Create named vectors with counts of words
sums_unigrams <- colSums(unigrams_dfm)
sums_bigrams <- colSums(bigrams_dfm)

# Create data tables with individual words as columns
uni_words <- data.table(
  word_1 = sapply(strsplit(names(sums_unigrams), "_", fixed = TRUE), '[[', 1),
  count = sums_unigrams)

bi_words <- data.table(
  word_1 = sapply(strsplit(names(sums_bigrams), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_bigrams), "_", fixed = TRUE), '[[', 2),
  count = sums_bigrams)

# Index the N-Grams to speed up calculation
setkey(uni_words, word_1)
setkey(bi_words, word_1, word_2)
