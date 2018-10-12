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
  gk <- dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- dynamic_knapsack(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- dynamic_knapsack(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- dynamic_knapsack(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  st <- system.time(gk <- dynamic_knapsack(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)
  
  gk <- dynamic_knapsack(x = knapsack_objects[1:800,], W = 3500)
  expect_equal(round(gk$value), 192647)
  
  gk <- dynamic_knapsack(x = knapsack_objects[1:1200,], W = 3500)
  expect_equal(round(gk$value), 270290)
})







