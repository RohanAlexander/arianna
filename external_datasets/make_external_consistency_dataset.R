#' @title Make external consistency dataset
#'
#' @description This script takes an external dataset and feed it to the
#' function make_internal_concistency_dataset() to create an external dataset.
#' This is for internal use only and will not be available on the client-side.

path <- getwd()
require(readtext)
raw_data <- readtext(paste0(path, "/external_datasets/raw_data*"))
raw_data_corpus <- quanteda::corpus(raw_data)
external_training_data <- aRianna::make_internal_consistency_dataset(raw_data_corpus)

external_training_data <- write.csv("external_datasets/external_training_data.csv")
