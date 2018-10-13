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
  gk <- dynamic_knapsack(x = knapsack_objects[1:800,], W = 3500)
  expect_equal(gk$value, 192647)
  expect_true(all(gk$elements %in% c(92, 574, 472, 80, 110, 537, 332, 117, 37, 776, 577, 288, 234, 255, 500, 794, 55, 290, 436, 346, 282, 764, 599, 303, 345, 300, 243, 43, 747, 35, 77, 229, 719, 564)))

  gk <- dynamic_knapsack(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(gk$value, 212337)
  expect_true(all(gk$elements %in% c(92, 574, 472, 80, 110, 840, 537, 1000, 332, 117, 37, 1197, 1152, 947, 904, 776, 577, 288, 1147, 1131, 234, 255, 1006, 833, 1176, 1092, 873, 828, 1059, 500, 1090, 794, 1033)))

})
