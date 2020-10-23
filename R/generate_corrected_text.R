#' @title Generate corrected text
#'
#' @description This function takes in a text input, detects errors and corrects them.
#'
#' @param text_original smaller amount of text
#'
#' @return Corrected text data
#'
#' @export
##### Prediction function
generate_corrected_text <- function(text_original) {
  BertForMaskedLM <- transformers$BertForMaskedLM
  BertTokenizer <- transformers$BertTokenizer$from_pretrained('bert-base-uncased')
  BertForMaskedLMModel <- BertForMaskedLM$from_pretrained('bert-base-uncased')

  ### Clean text
  text_stripped <- textclean::strip(text_original, char.keep = c("?", ".", "'", "~~"), digit.remove = TRUE, apostrophe.remove = FALSE,
                         lower.case = FALSE)
  ### Collect text errors
  errors <- hunspell::hunspell(text_stripped)

  ### Replace errors to [MASK]
  for(e in errors){
    pats <- paste(e, collapse = '|')
  }
  text <- stringr::str_replace_all(text_stripped, pats, '[MASK]')

  ### Tokenize the text
  tokenized_text <- BertTokenizer$tokenize(text)
  indexed_tokens <- BertTokenizer$convert_tokens_to_ids(tokenized_text)

  ### Encode the text and create segments tensors
  MASKIDS <- which(tokenized_text == "[MASK]")
  SEGS <- which(tokenized_text == ".")

  segments_ids <- rep(as.integer(0), times = SEGS)

  segments_tensors <- rTorch::torch$tensor(list(segments_ids))
  tokens_tensor <- rTorch::torch$tensor(list(indexed_tokens))

  ### Generate prediction
  py <- rTorch::torch$no_grad()
  with(py, {
    predictions <- BertForMaskedLMModel(tokens_tensor, segments_tensors)
  })

  predicted_token <- list()
  for(i in 1:length(MASKIDS)){
    preds <- rTorch::torch$topk(predictions[[1]][1][MASKIDS[i]], k=as.integer(50))
    indices <- preds$indices$tolist()
    list <- BertTokenizer$convert_ids_to_tokens(indices)
    predicted_token <- append(predicted_token, list(list[1]))
  }
  predicted_token.i <- 1

  for (i in 1:length(text)) {
    while (grepl(pattern = "\\Q[MASK]\\E", text[i])) {
      text[i] <- sub(pattern = "\\Q[MASK]\\E", replacement = predicted_token[predicted_token.i], x = text[i])
      predicted_token.i <- predicted_token.i + 1
    }
  }

  return(text)
}

# segment sentences based on ! ? .
# more than two sentences
# if no errors, pass it
