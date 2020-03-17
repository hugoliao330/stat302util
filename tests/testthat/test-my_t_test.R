test_that("my_t.test gives correct result", {
  expected <- t.test(1:10, alternative = "two.sided", mu = 5)
  result <- my_t.test(1:10, "two.sided", 5)
  expect_equal(result$test_stat, expected$statistic[["t"]])
  expect_equal(result$df, 9)
  expect_match(result$alternative, "two.sided")
  expect_equal(result$p_val, expected$p.value)

  expected2 <- t.test(30:50, alternative = "less", mu = 30)
  result2 <- my_t.test(30:50, "less", 30)
  expect_equal(result2$test_stat, expected2$statistic[["t"]])
  expect_equal(result2$df, 20)
  expect_match(result2$alternative, "less")
  expect_equal(result2$p_val, expected2$p.value)

  expected3 <- t.test(1:100, alternative = "greater", mu = 78.9)
  result3 <- my_t.test(1:100, "greater", 78.9)
  expect_equal(result3$test_stat, expected3$statistic[["t"]])
  expect_equal(result3$df, 99)
  expect_match(result3$alternative, "greater")
  expect_equal(result3$p_val, expected3$p.value)
})

test_that("non-valid alternative input throws error", {
  expect_error(my_t.test(1:10, "sadlfkj", 3))
})
