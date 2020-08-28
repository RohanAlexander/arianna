#' @title Generate internal consistency score
#'
#' @description This function takes a small amount of text and generates an
#' internal consistency score
#'
#' @param text_to_check smaller amount of text
#'
#' @param consistency_dataset dataset created from make_internal_consistency_dataset
#'
#' @return A list of two tibbles that contain internal consistency and unexpected words
#'
#' @examples
#' aRianna::generate_internal_consistency_score("we had no idea that you had no idae", consistency_dataset)
#'
#' @seealso `make_internal_consistency_dataset` for consistency_dataset generation
#'
#' @export
generate_internal_consistency_score <- function(text_to_check, consistency_dataset) {
  #### Apply to the dataset to create a consistency score ####
  # Now that we have our collection of n-grams (this will be internal consistency
  # because that collection was based on the data itself) we want to work out a measure
  # of consistency.

  # Create tokens with errors
  tokens_from_example_with_errors <- quanteda::tokens(text_to_check, remove_punct = TRUE)
  tokens_from_example_with_errors <- quanteda::tokens_tolower(tokens_from_example_with_errors)

  # Create ngrams from the tokens with errors
  toks_ngram_with_errors <- quanteda::tokens_ngrams(tokens_from_example_with_errors, n = 3)

  all_tokens_with_errors <- tibble::tibble(tokens = toks_ngram_with_errors[[1]])

  all_tokens_with_errors <-
    all_tokens_with_errors %>%
    dplyr::mutate(tokens = stringr::str_replace_all(tokens, "_", " "),
                  first_words = stringr::word(tokens, start = 1, end = 2),
                  last_word = stringr::word(tokens, -1),
                  tokens = stringr::str_replace_all(tokens, " ", "_"),
                  first_words = stringr::str_replace_all(first_words, " ", "_"),
                  is_in_dataset = tokens %in% external_consistency_dataset$tokens
    )

  # Now we combine them so last_word will be what we have and last_word_expected will
  # be what we expect.
  all_tokens_with_errors <-
    all_tokens_with_errors %>%
    dplyr::left_join(dplyr::select(consistency_dataset, -tokens), by = c("first_words"))

  all_tokens_with_errors_only <- all_tokens_with_errors

  for(token in unique(all_tokens_with_errors_only$tokens)) {
    if(token %in% consistency_dataset$tokens){
      all_tokens_with_errors_only <- all_tokens_with_errors_only[all_tokens_with_errors_only$tokens != token, ]
    }
  }

  # Calculate the internal consistency score:
  internal_consistency <-
    all_tokens_with_errors%>%
    dplyr::select(c(tokens, is_in_dataset)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(as_expected = length(which(is_in_dataset == TRUE)))%>%
    dplyr::mutate(not_as_expected = length(which(is_in_dataset == FALSE)))%>%
    dplyr::mutate(consistency = length(which(is_in_dataset == TRUE)) / length(is_in_dataset))%>%
    dplyr::select(-c(tokens, is_in_dataset)) %>%
    dplyr::distinct()

  # Identify which words were unexpected
  unexpected <-
    all_tokens_with_errors_only %>%
    dplyr::mutate(as_expected = last_word == last_word_expected) %>%
    dplyr::filter(as_expected == FALSE) %>%
    dplyr::select(-c(tokens, is_in_dataset))

  newList <- list("internal consistency" = internal_consistency,
                  "unexpected words" = unexpected)

  return(newList)
}
