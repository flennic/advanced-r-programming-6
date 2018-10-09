# library(binaryLogic)

#' combination_object_output
#'
#' @param obj A list having numerical entries for v (value), w (weight) and s (selector), a numerical vector with TRUE/FALSE entries
#'
#' @return Returns a list with the desired formatting for displaying the object.
combination_object_output = function(obj) {
  value = as.numeric(obj$v)
  elements = which(unlist(obj$s) == TRUE)
  return(list(value = value, elements = elements))
}

#' brute_force_knapsack
#'
#' @param x A data.frame with the colums v (value) and w (weight) as numerical values.
#' @param W Maximum weight.
#'
#' @return Returns the best combination of objects to pick, so that v is max.
#' @export
brute_force_knapsack = function(x, W) {
  knapsack_input_validation(x, W)
  limit = nrow(x)

  #' get_combination_object
  #'
  #' @param selector A combination of TRUE/FALSE to specify which items to pick.
  #'
  #' @return Returns the v (value), w (weight) and the s (selector) depending on the selector s.
  get_combination_object = function(selector) {
    selectedItems = x[selector == TRUE,]
    if (is.na(selectedItems[1,1])) {
      return(list(v = 0, w = 0, s = selector))
    }
    return(list(v = sum(selectedItems$v), w = sum(selectedItems$w), s = selector))
  }

  # All combinations of items as binary. If more than 32 objects, it breaks
  bin_rep = sapply(c(1:2^limit-1), function(x) { as.binary(x, n = limit) })

  # Get a list ob combination objects containign v, w and s. v and w will be 0 if w > W
  combination_objects = apply(bin_rep, 2, get_combination_object)

  # Convert list of lists to a better usable dat.frame
  combination_objects.as.df = as.data.frame(t(sapply(combination_objects, rbind)))
  colnames(combination_objects.as.df) = c('v', 'w', 's')

  # Filter all entries that exceed the weight limit
  combination_objects.as.df = combination_objects.as.df[which(combination_objects.as.df$w < W),]

  # Get the entry with the maximum value
  combination_objects.as.df = combination_objects.as.df[which.max(combination_objects.as.df$v),]

  # Return the desired element as a list representation
  return(combination_object_output(combination_objects.as.df[1,]))
}


#' knapsack_input_validation
#'
#' @param df The dataframe input which is to be validated.
#' @param W The weight which is to be validated.
#'
#' @return Nothing. Stops if validation detected wrong user input.
knapsack_input_validation = function(df, W) {
  if (!is.data.frame(df)) stop("data does not have data.frame format")
  if (!"w" %in% colnames(df)) stop("no column named w.")
  if (!"v" %in% colnames(df)) stop("no column named v.")
  if (!is.numeric(W)) stop("W must be numeric.")
}

# set.seed(42)
# n <- 2000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
#   )
#
# print(brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500))
# print(brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500))
# print(brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000))
# print(brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000))
