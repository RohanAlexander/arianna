# aRianna

#### Makes text cleaning a little easier

# About

Alphabets Really In Absolute Need of New Alphabet-ing or `aRianna` is a package that helps identify potential issues in text datasets by comparing the actual text with that expected by various natural language models. The package also provides a 'consistency score' that can be used to monitor how the text dataset changes during the cleaning and preparation process. 

More information is available at: https://github.com/RohanAlexander/consistency_scores_in_text_datasets 


# Installation

You can install the current version of this package using:

``` r
devtools::install_github("RohanAlexander/arianna")
```

To load the package:

``` r
library(arianna)
```

---


# Using the package

Raw text data are often messy and unready for further analysis. The `aRianna` package provides an easy way to clean your text data by picking up text errors in the dataset and replace them with `aRianna`'s predictions. Additionally, `aRianna` introduces the notion of a consistency score, which refers to the proportion of text that is unchanged by the model. This is used to monitor changes during the cleaning process, and to compare the messiness of different texts. There are two types of consistency scores: the internal consistency score and the external consistency score. We define internal consistency as when the model is trained on the corpus itself, and external consistency as when the model is trained on a more general corpus. In `aRianna`, the general corpus trained for external consistency score generation is [insert data set once we decide on one]. Together, these concepts provide a guide to the cleanliness and consistency of a text dataset. This can be important when deciding whether a dataset is fit for purpose; when carrying out data cleaning and preparation tasks; and as a comparison between datasets. 


# Examples

## Internal consistency generation

``` r
some_text <- c("we had no idea that you had no idea that they had no idea")
consistency_dataset <- aRianna::make_internal_consistency_dataset(some_text)
aRianna::generate_internal_consistency_score("we had no idea that you had no idae", consistency_dataset)

## $`internal consistency`
## # A tibble: 1 x 3
##   as_expected     n consistency
##   <lgl>       <int>       <dbl>
## 1 TRUE            2       0.667

## $`unexpected words`
## # A tibble: 1 x 5
##   tokens      first_words last_word last_word_expected as_expected
##   <chr>       <chr>       <chr>     <chr>              <lgl>      
## 1 had_no_idae had_no      idae      idea               FALSE  
```

## External consistency generation

``` r
aRianna::generate_external_consistency_score("we had no idea that you had no idae")

## $`external consistency`
## # A tibble: 1 x 3
##   as_expected     n consistency
##   <lgl>       <int>       <dbl>
## 1 TRUE            6       0.107

## $`unexpected words`
## # A tibble: 7 x 4
##   first_words last_word last_word_expected as_expected
##   <chr>       <chr>     <chr>              <lgl>      
## 1 had_no      idae      choice             FALSE      
## 2 had_no      idae      clue               FALSE      
## 3 had_no      idae      doubt              FALSE      
## 4 had_no      idae      idea               FALSE      
## 5 had_no      idae      other              FALSE      
## 6 had_no      idae      problem            FALSE      
## 7 had_no      idae      right              FALSE      

```









