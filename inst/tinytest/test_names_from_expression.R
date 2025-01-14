library(tinytest)

expect_equal(
  csmdeveloper:::names_from_expression(~a+b*c),
  c("a", "b", "c"))
