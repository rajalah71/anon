# anon

A package containing tools for data anonymization for RStudio.

## Installation

1. Install "devtools" package on R
    - install.packages("devtools")
2. Install the "anon" package on R
    - devtools::install_github("rajalah71/anon") (or /anon@paketti for developenet version, may be unstable / unfinished)
3. Load the "anon" library
    - library(anon)

## Usage

The package contains (4 at the moment) functions to anonymize your data, as well as helper functions to modify different columns in different ways. 

## Methods

### _k_-anonymity

data = data("iris")

kAnon(data, k=5) 

- This uses all columns as quasi identifiers and operates on the with mean on numeric columns and with combining on categorical ones.

- You can define your own functions if you want, and on what columns the function will operate on. Functions must be on a named list which names match the names of the quasi identifier columns. The columns will be worked on the order you name them, or by cardinality if left unnamed. 

### _l_-diversity

data = data("iris")

lDiversity(data, sensitiveAtrributes = "Species", l=2) 

- Works similiarly to kAnon, just need to specify the sensitive attributes. Won't probably work with more than one of them.

### _t_-closeness

TODO

### RSA

data = data("iris")

encrypt(data)

- Encrypts the data using a new RSA key = 2048 bits by default. 

### Spectral anonymization

data = data("iris")

spectral(data, cell_swap)

- Anonymizes data on the spectral basis provided by SVD. Supports any anoymization function $f: R^{a \times b} \to R^{a \times b}$.