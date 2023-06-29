# WARNING! These funtions are probably useless; use quantile bands instead.

# This is a helper function that one may apply in the use of making a dataset satisfy some privacy requirement by making the dataset k-bits coarser

# Takes a vector of decimal numbers as input as well as how many bits to suppress

k_bits_uncertainty = function(decimal, suppressed_bits) {
  # Check if suppressed_bits is 0
  if (suppressed_bits == 0) {
    # Return the input decimal numbers as a string
    return(as.character(decimal))
  }

  # Convert the decimal rounded to  numbers to binary
  as_binary = sapply(round(decimal), R.utils::intToBin)

  # Extract the public part of the binary numbers
  public_part = substr(as_binary, 1, nchar(as_binary)-suppressed_bits)

  # Create the lower and upper bounds for the range
  lower_bound = strrep("0", suppressed_bits)
  upper_bound = strrep("1", suppressed_bits)

  # Combine the public part with the lower and upper bounds
  range_in_bits = rbind(paste0(public_part, lower_bound), paste0(public_part, upper_bound))

  # Convert the binary range to decimal and return it as a string
  result = apply(range_in_bits, 2, function(x) paste0(strtoi(x[1], base = 2), "-", strtoi(x[2], base = 2)))

  return(result)
}


# Modified version with support to fractional numbers, not very usefull due to the volatileness of the amount of bits in binary representation of fractional decimal numbers

k_bits_uncertainty_frac = function(decimal, suppressed_bits) {
  # Check if suppressed_bits is 0
  if (suppressed_bits == 0) {
    # Return the input decimal numbers as a string
    return(as.character(decimal))
  }

  # Convert the decimal numbers to binary
  as_binary = sapply(decimal, function(x) {
    # Split the number into its integer and fractional parts
    int_part = floor(x)
    frac_part = x - int_part

    # Convert the integer part to binary
    int_binary = R.utils::intToBin(int_part)

    # Convert the fractional part to binary
    frac_binary = ""
    while (frac_part > 0) {
      frac_part = frac_part * 2
      if (frac_part >= 1) {
        frac_binary = paste0(frac_binary, "1")
        frac_part = frac_part - 1
      } else {
        frac_binary = paste0(frac_binary, "0")
      }
    }

    # Combine the integer and fractional parts
    paste0(int_binary, ".", frac_binary)
  })

  print(as_binary)

  # Extract the public part of the binary numbers
  public_part = substr(as_binary, 1, nchar(as_binary)-suppressed_bits)

  # Create the lower and upper bounds for the range
  lower_bound = strrep("0", suppressed_bits)
  upper_bound = strrep("1", suppressed_bits)

  # Combine the public part with the lower and upper bounds
  range_in_bits = rbind(paste0(public_part, lower_bound), paste0(public_part, upper_bound))

  # Convert the binary range to decimal and return it as a string
  result = apply(range_in_bits, 2, function(x) {
    lb = x[1]
    ub = x[2]

    lb_int = strtoi(substr(lb, 1, regexpr("\\.", lb)-1), base=2)
    ub_int = strtoi(substr(ub, 1, regexpr("\\.", ub)-1), base=2)

    lb_frac = sum(as.integer(strsplit(substr(lb, regexpr("\\.", lb)+1, nchar(lb)), "")[[1]]) / 2^(1:length(strsplit(substr(lb, regexpr("\\.", lb)+1, nchar(lb)), "")[[1]])))
    ub_frac = sum(as.integer(strsplit(substr(ub, regexpr("\\.", ub)+1, nchar(ub)), "")[[1]]) / 2^(1:length(strsplit(substr(ub, regexpr("\\.", ub)+1, nchar(ub)), "")[[1]])))

    paste0(lb_int + lb_frac, "-", ub_int + ub_frac)
  })

  return(result)
}

# A function that can be used as a part of the anonymization process of a dataset,
# using the previous function (k_bits_uncertainty) in the apply part of make_(anonymous dataset) functinos
# his uses the non fractional k_bits_uncertainty

suppress_k_bits_by_quantile = function(column, n_bands, k) {
  # Divide the column support to n_bands such that each band holds equal amount of probability mass, or samples
  quantiles = quantile(column, probs = seq(0, 1, length.out = n_bands + 1))
  bands = cut(column, breaks = quantiles, include.lowest = TRUE)

  # Order the bands in ascending order of wideness
  band_widths = sapply(levels(bands), function(x) diff(range(column[bands == x])))
  ordered_bands = levels(bands)[order(band_widths)]

  # Apply the k_bits_uncertainty function to each sample in a given n_band
  result = sapply(1:length(column), function(i) {
    band_index = which(ordered_bands == bands[i])

    # A given band gets baseline k-bits censored and an additional amount to the floor log of the band index
    suppressed_bits = k + floor(log2(band_index))
    k_bits_uncertainty(column[i], suppressed_bits)
  })

  return(result)
}

# A function that takes a budget of k to distribute to each band according to their width. Then according to each bands budget, k-bits of uncertainty is added.

suppress_k_bits_by_quantile_budget = function(column, n_bands, k) {
  # Divide the column support to n_bands such that each band holds equal amount of probability mass, or samples
  quantiles = quantile(column, probs = seq(0, 1, length.out = n_bands + 1))
  bands = cut(column, breaks = quantiles, include.lowest = TRUE)

  # Calculate the width of each band
  band_widths = sapply(levels(bands), function(x) diff(range(column[bands == x])))

  # Calculate the total width of all bands
  total_width = sum(band_widths)

  # Calculate the proportion of the budget for each band based on its width
  budget_proportions = band_widths / total_width

  # Calculate the suppressed bits for each band based on its proportion of the budget
  suppressed_bits_per_band = ceiling(budget_proportions * k)

  # Apply the k_bits_uncertainty function to each sample in a given n_band
  result = sapply(1:length(column), function(i) {
    band_index = which(levels(bands) == bands[i])
    suppressed_bits = suppressed_bits_per_band[band_index]
    k_bits_uncertainty(column[i], suppressed_bits)
  })

  return(result)
}

