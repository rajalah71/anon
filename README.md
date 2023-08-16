# anon

A package containing data anonymization tools for R.

## 1. Installation

1. Install "devtools" package on R
    - install.packages("devtools")
2. Install the "anon" package on R
    - devtools::install_github("rajalah71/anon") (or /anon@paketti for developenet version, may be unstable / unfinished)
3. Load the "anon" library
    - library(anon)

## 2. Usage

The package contains 4 functions to anonymize your data, as well as helper functions to modify different columns in different ways. 

### 2.1 _k_-anonymity

data = data("iris")

kAnon(data, k=5) 

- This uses all columns as quasi identifiers and operates on the with mean on numeric columns and with combining on categorical ones.

- You can define your own functions if you want, and on what columns the function will operate on. Functions must be on a named list which names match the names of the quasi identifier columns. The columns will be worked on the order you name them, or by cardinality if left unnamed. 

### 2.2 _l_-diversity

data = data("iris")

lDiversity(data, sensitiveAtrributes = "Species", l=2) 

- Works similiarly to kAnon, just need to specify the sensitive attributes. Won't probably work with more than one of them.

### 2.3 RSA

data = data("iris")

encrypt(data)

- Encrypts the data using a new RSA key = 2048 bits by default. Do not distribute the public key.

### 2.4 Spectral anonymization

data = data("iris")

spectral(data, cell_swap)

- Anonymizes data on the spectral basis provided by SVD. Supports any anoymization function $f: R^{a \times b} \to R^{a \times b}$.

spectral(data, sensitive_noise)

- Adds noise to the columns of $U$ from Laplace distribution. 

## 3. Plots

You can plot the predictive threat the anonymized data poses on the general populace against the threat of a non-overlapping sample. 

prediction_plot(data, k=5, data_anon)

