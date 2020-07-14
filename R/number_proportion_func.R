#' @title kctest
#'
#' @description This number_proportion() function takes a group of numbers and caculates the proportion of each number to the sum of all the numbers
#'
#' @param group of number in vector
#'
#' @return a matrix of the numbers and their proportion values
#'
#' @examples
#' numbers <- c(2, 5, 3, 9, 8, 11, 6)
#' number_proportion(numbers)
#'
#' @export
number_proportion <- function(numbers) {
  proportions <- vector()
  for (i in 1:length(numbers)) {
    proportion <- numbers[i]/sum(numbers)
    proportions[i] <- proportion
  }
  output_matrix <- rbind(numbers, proportions)
  return(output_matrix)
}

