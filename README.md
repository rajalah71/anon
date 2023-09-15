# anon

A package containing data anonymization tools for R.

## 1. Installation

1. Install "devtools" package on R
    - install.packages("devtools")
2. Install the "anon" package on R
    - devtools::install_github("rajalah71/anon") (or /anon@paketti for development version, may be unstable / unfinished)
3. Load the "anon" library
    - library(anon)

## 2. Usage

The package contains 4 functions to anonymize your data, as well as helper functions to modify different columns in different ways. 

### 2.1 _k_-anonymity

data("iris")

kAnon(iris, k=5) 

- This uses all columns as quasi identifiers and operates on the with mean on numeric columns and with combining on categorical ones.

- You can define your own functions if you want, and on what columns the function will operate on. Functions must be on a named list which names match the names of the quasi identifier columns. The columns will be worked on the order you name them, or by cardinality if left unnamed. 

### 2.2 _l_-diversity

data("iris")

lDiversity(iris, sensitiveAtrributes = "Species", l=2) 

- Works similiarly to kAnon, just need to specify the sensitive attributes. Won't probably work with more than one of them.

### 2.3 RSA

data("iris")

encrypt(iris)

- Encrypts the data using a new RSA key = 2048 bits by default. Do not distribute the public key.

### 2.4 Spectral anonymization

data("iris")

spectral(iris, your_function)

- Anonymizes data on the spectral basis provided by SVD, $M = UDV'$. Supports any anoymization function $f: R^{a \times b} \to R^{a \times b}$.

spectral(iris, cell_swap, on_matrices = "U")

- Performs column permutations on each column the matrix $U$.

spectral(iris, sensitive_noise, on_matrices = "UD")

- Adds noise to the columns of $UD$ from Laplace distribution according to the range of values (max - min) on the column. 

## 3. Plots

You can plot the predictive threat the anonymized data poses on the general populace against the threat of a non-overlapping sample. 

prediction_plot(data, k=5, data_anon)

