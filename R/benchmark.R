#' Knapsack Bruteforce Benchmark
#'
#' A \code{data.frame} which holds data for 2..18 items using the knapsack bruteforce method with single and multi core.
#'
#' @format A \code{data.frame} consisting of three vectors.
#' \describe{
#'   \item{noItems}{Number of items.}
#'   \item{single}{Computational time for single core in milliseconds.}
#'   \item{parallel}{Computational time for multi core in milliseconds.}
#' }
"benchmark"
