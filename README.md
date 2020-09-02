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

### Getting started

To install the package and load the library:
  
``` r
devtools::install_github("RohanAlexander/arianna")
library(aRianna)
```

For demonstration, we use the first paragraph of Jane Eyre as the internal text data. A sentence within the paragraph is modified with the intention to contain an error and serves as the text to be evaluated:
  
``` r
body_of_text <- "There was no possibility of taking a walk that day. 
We had been wandering, indeed, in the leafless shrubbery an hour in 
the morning; but since dinner (Mrs. Reed, when there was no company, 
dined early) the cold winter wind had brought with it clouds so sombre, 
and a rain so penetrating, that further out-door exercise was now out 
of the question."

text_to_evaluate <- "when there was na company"
```

### Create an internal consistency dataset

The first step is to turn the body of text into the internal consistency dataset. The generated internal consistency dataset is a tibble that contains the tri-grams that appear in the internal text data more than once. Since only `there_was_no` has more than one occurrence, it is the only reference tri-gram in the internal consistency dataset:
  
``` r
internal_consistency_dataset <- 
  aRianna::make_internal_consistency_dataset(body_of_text)
internal_consistency_dataset
## # A tibble: 1 x 3
##   tokens       first_words last_word_expected
##   <chr>        <chr>       <chr>             
## 1 there_was_no there_was   no                
```

### Generate internal consistency score

The next step is to compare the text to be evaluated with the internal consistency dataset that we created in the previous step. The function `generate_internal_consistency_score` takes in two arguments — the text to evaluate and the internal consistency dataset. The function identifies the word "na" as an unexpected word, and generates the internal consistency score as zero. The function also lists "no" as the replacement of "na":
  
``` r
aRianna::generate_internal_consistency_score(
  text_to_evaluate, internal_consistency_dataset)

## $`internal consistency`
## # A tibble: 1 x 3
##   as_expected     n consistency
##   <lgl>       <int>       <dbl>
## 1 FALSE           1           0

## $`unexpected words`
## # A tibble: 1 x 4
##   first_words last_word last_word_expected as_expected
##   <chr>       <chr>     <chr>              <lgl>      
## 1 there_was   na        no                 FALSE  

```

### Generate external consistency score

To get the external consistency score, we will employ the `generate_external_consistency_score` function. The function `generate_internal_consistency_score` takes in only one argument — the text to evaluate, and compares it with the external consistency dataset. The function identifies the word "na" as an unexpected word, and generates the external consistency score as zero. Because the external dataset is larger, there are several words generated as the replacement candidates of "na":
  
``` r
aRianna::generate_external_consistency_score(text_to_evaluate)

## aRianna::generate_external_consistency_score(text_to_evaluate)
## $`external consistency`
## # A tibble: 1 x 3
##   as_expected     n consistency
##   <lgl>       <int>       <dbl>
## 1 FALSE           2           0

## $`unexpected words`
## # A tibble: 11 x 4
##    first_words last_word last_word_expected as_expected
##    <chr>       <chr>     <chr>              <lgl>      
##  1 there_was   na        a                  FALSE      
##  2 there_was   na        an                 FALSE      
##  3 there_was   na        no                 FALSE      
##  4 there_was   na        not                FALSE      
##  5 there_was   na        nothing            FALSE      
##  6 there_was   na        of                 FALSE      
##  7 there_was   na        one                FALSE      
##  8 there_was   na        only               FALSE      
##  9 there_was   na        plenty             FALSE      
## 10 there_was   na        someone            FALSE      
## 11 there_was   na        something          FALSE      

```









