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
---


# Using the package

The `aRianna` package....




# Examples


some_text <- c("rohan is the dad and edward is the baby and rohan is the dad")

examples <- aRianna::make_internal_consistency_dataset(some_text)

aRianna::generate_internal_consistency_score("rohan is the mum and rohan is the dad", examples)





