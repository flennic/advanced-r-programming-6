context("dynamic_knapsack")

set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

# test 1
test_that("Correct object is returned", {
  expect_silent(gk <- dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500))
  expect_named(gk, c("value", "elements"))
})

# test 2
test_that("functions rejects errounous input.", {
  expect_error(dynamic_knapsack("hej", 3500))
  expect_error(dynamic_knapsack(x = knapsack_objects[1:8,], W = -3500))
})

# test 3
test_that("Function return correct results.", {
  gk <- dynamic_knapsack(x = knapsack_objects[1:400,], W = 3500)
  expect_equal(round(gk$value), 151505)
  expect_true(all(gk$elements %in% c(25, 35, 37, 43, 55, 80, 92, 110, 117, 229, 234, 243, 255, 282, 288, 290, 300, 303, 314, 332, 342, 345, 346, 400)))

  gk <- dynamic_knapsack(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(gk$value), 16770)
  expect_true(all(gk$elements %in% c(5, 8)))
})
