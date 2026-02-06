library(tinytest)

expect_equal(
  csmbuilder:::names_from_expression(~a+b*c),
  c("a", "b", "c"))
