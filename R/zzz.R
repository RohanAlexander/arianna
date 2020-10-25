local <- new.env()

.onLoad <- function(libname, pkgname) {
  #if (!reticulate::configure_environment(pkgname)) {
    #reticulate::conda_create(pkgname)
    #use_condaenv(pkgname, required = TRUE) # set Python env to conda
  #}
  # use_condaenv(pkgname, required = TRUE) # set Python env to conda
  transformers <- reticulate::import("transformers", delay_load = TRUE)
  BertForMaskedLM <- transformers$BertForMaskedLM
  BertTokenizer <- transformers$BertTokenizer$from_pretrained('bert-base-uncased')
  BertForMaskedLMModel <- BertForMaskedLM$from_pretrained('bert-base-uncased')
  assign("BertTokenizer", value = BertTokenizer, envir = parent.env(local))
  assign("BertForMaskedLMModel", value = BertForMaskedLMModel, envir = parent.env(local))
}

