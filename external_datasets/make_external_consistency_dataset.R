#' @title Make external consistency dataset
#'
#' @description This script takes an external dataset and feed it to the
#' function make_internal_concistency_dataset() to create an external dataset.
#' This is for internal use only and will not be available on the client-side.

path <- getwd()
require(readtext)
raw_data <- readtext(paste0(path, "/external_datasets/raw_data*"))
raw_data_corpus <- quanteda::corpus(raw_data)

make_external_consistency_dataset <- function(body_of_text) {
  # Create tokens
  tokens_from_example <- quanteda::tokens(raw_data_corpus, remove_punct = TRUE, )
  tokens_from_example <- quanteda::tokens_tolower(tokens_from_example)

  # Create ngrams from the tokens
  toks_ngram <- quanteda::tokens_ngrams(tokens_from_example, n = 5:3)

  all_tokens <- tibble(tokens = "how_are_you")
  # Convert to tibble so we can use our familiar verbs
  for(i in 1:length(toks_ngram)){
    all_tokens <- add_row(all_tokens, tokens = toks_ngram[[i]])
  }

  # We only want the common ones, not every one.
  all_tokens <-
    all_tokens %>%
    dplyr::group_by(tokens) %>%
    dplyr::count() %>%
    dplyr::filter(n > 1) %>%
    dplyr::ungroup()

  # Create a tibble that has the first two words in one column then the third
  all_tokens <-
    all_tokens %>%
    dplyr::mutate(tokens = stringr::str_replace_all(tokens, "_", " "),
                  first_words = stringr::word(tokens, start = 1, end = -2),
                  last_word = stringr::word(tokens, -1),
                  tokens = stringr::str_replace_all(tokens, " ", "_"),
                  first_words = stringr::str_replace_all(first_words, " ", "_")
    ) %>%
    dplyr::rename(last_word_expected = last_word) %>%
    dplyr::select(-n)

  return(all_tokens)
}

external_training_data <- make_external_consistency_dataset(raw_data_corpus)
write.csv(all_tokens, paste0(path,"/external_datasets/external_training_dataset_5-3-grams.csv"), row.names=FALSE)
