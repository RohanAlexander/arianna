path <- getwd()
require(readtext)
raw_data <- readtext(paste0(path, "/external_datasets*"))
raw_data_corpus <- quanteda::corpus(raw_data)

make_internal_consistency_dataset <- function(body_of_text) {
  # Create tokens
  tokens_from_example <- quanteda::tokens(body_of_text, remove_punct = TRUE)

  # Create ngrams from the tokens
  toks_ngram <- quanteda::tokens_ngrams(tokens_from_example, n = 3)

  # Convert to tibble so we can use our familiar verbs
  all_tokens <- tibble::tibble(tokens = toks_ngram[[1]])

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
                  first_words = stringr::word(tokens, start = 1, end = 2),
                  last_word = stringr::word(tokens, -1),
                  tokens = stringr::str_replace_all(tokens, " ", "_"),
                  first_words = stringr::str_replace_all(first_words, " ", "_")
    ) %>%
    dplyr::rename(last_word_expected = last_word) %>%
    dplyr::select(-n, -tokens)

  return(all_tokens)
}

external_training_data <- make_internal_consistency_dataset(raw_data_corpus)

