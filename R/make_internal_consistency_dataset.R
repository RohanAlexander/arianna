#' @title Create an internal consistency dataset
#'
#' @description This function takes a large text input creates the dataset that
#' is needed.
#'
#' @param body_of_text larger amount of text
#'
#' @return A collection of n-gram tokens in a tibble
#'
#' @examples
#' some_text <- c("we had no idea that you had no idea that they had no idea")
#' aRianna::make_internal_consistency_dataset(some_text)
#'
#' @export
make_internal_consistency_dataset <- function(body_of_text) {
  # Create tokens
  tokens_from_example <- quanteda::tokens(body_of_text, remove_punct = TRUE)
  tokens_from_example <- quanteda::tokens_tolower(tokens_from_example)

  # Create ngrams from the tokens
  toks_ngram <- quanteda::tokens_ngrams(tokens_from_example, n = 5:3)

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
                  first_words = stringr::word(tokens, start = 1, end = -2),
                  last_word = stringr::word(tokens, -1),
                  tokens = stringr::str_replace_all(tokens, " ", "_"),
                  first_words = stringr::str_replace_all(first_words, " ", "_")
    ) %>%
    dplyr::rename(last_word_expected = last_word) %>%
    dplyr::select(-n)

  return(all_tokens)
}

