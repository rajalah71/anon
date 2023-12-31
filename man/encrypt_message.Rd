% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crypto.r
\name{encrypt_message}
\alias{encrypt_message}
\title{Encrypts a message using RSA encryption.}
\usage{
encrypt_message(m, e, n, bits)
}
\arguments{
\item{m}{The message to be encrypted.}

\item{e}{The public exponent used for encryption.}

\item{n}{The modulus used for encryption.}

\item{bits}{The number of bits used for precision in the encrypted message.}
}
\value{
A mpfr list representing the encrypted message.
}
\description{
This function takes a message as input and encrypts it using the RSA encryption
algorithm with the provided parameters.
}
