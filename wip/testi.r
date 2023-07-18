# function to combine values in ordered categorical variable column

combine_ordered = function(column){

  # get unique values in the column and sort them
  unique_values = unique(column)
  sorted_uniques = sort(unique_values)
  #count the number of unique values
  n = length(unique_values)

  # check whether the the amount of unique values is even or odd
  if(n %% 2 == 0){

    # make bands of sorted unique values as [sorted_uniques[i], sorted_uniques[i+1]] for all i in [1,3,5,...,n-1]
    for(i in seq(1, n-1, 2)){
      column[column == sorted_uniques[i]] = paste0("[", sorted_uniques[i], ",", sorted_uniques[i+1], "]")
      column[column == sorted_uniques[i+1]] = paste0("[", sorted_uniques[i], ",", sorted_uniques[i+1], "]")
    }
  } else {
    # Take the middle value of the sorted unique values and make it a bands of its ows while the rest of the values are banded as in the even case
    middle = sorted_uniques[(n+1)/2]
    #column[column == middle] = paste0("[", middle, ",", middle, "]")

    for(i in seq(1, middle-2, 2)){
      column[column == sorted_uniques[i]] = paste0("[", sorted_uniques[i], ",", sorted_uniques[i+1], "]")
      column[column == sorted_uniques[i+1]] = paste0("[", sorted_uniques[i], ",", sorted_uniques[i+1], "]")
    }

    for(i in seq(middle+1, n-1 , 2)){
      column[column == sorted_uniques[i]] = paste0("[", sorted_uniques[i], ",", sorted_uniques[i+1], "]")
      column[column == sorted_uniques[i+1]] = paste0("[", sorted_uniques[i], ",", sorted_uniques[i+1], "]")
    }
  }

  return(column)


  }
