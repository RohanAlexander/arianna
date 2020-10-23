# Global reference to transformers, BertTokenizer and BertForMaskedLMModel (will be initialized in .onLoad)
transformers <- NULL
BertForMaskedLMModel <- NULL
BertTokenizer <- NULL
.onLoad <- function(libname, pkgname) {
  # Use super assignment to update global reference to transformers, BertTokenizer and BertForMaskedLMModel
  transformers <<- reticulate::import("transformers", delay_load = TRUE)
  BertForMaskedLM <<- transformers$BertForMaskedLM
  BertTokenizer <<- transformers$BertTokenizer$from_pretrained('bert-base-uncased')
  BertForMaskedLMModel <<- BertForMaskedLM$from_pretrained('bert-base-uncased')
}
