# Global reference to transformers, BertTokenizer and BertForMaskedLMModel (will be initialized in .onLoad)
transformers <- NULL

.onLoad <- function(libname, pkgname) {
  # Use super assignment to update global reference to transformers, BertTokenizer and BertForMaskedLMModel
  transformers <<- reticulate::import("transformers", delay_load = TRUE)
}
