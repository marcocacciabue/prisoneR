
params <- prepare_parameters()
simulationLV_1 <- game(
  type = "Lotka",
  play1 = "Count_defective", # strategy of player 1#
  play2 = "Count_defective", # strategy of player 2,#
  parameters = params # list of parameters to pass to ode solver
)
set.seed(1)

simulationLV_2 <- game(
  type = "Lotka",
  play1 = "Count_defective_invert", # strategy of player 1#
  play2 = "Count_defective_invert", # strategy of player 2,#
  parameters = params # list of parameters to pass to ode solver
)
test_that("results change when the strategy is count defective vs count defective inverted", {
  expect_true(simulationLV_1[10, 1] != simulationLV_2[10, 1])
})


params <- prepare_parameters_may()

set.seed(1)
simulationMay_1 <- game(
  type = "May",
  play1 = "Count_defective", # strategy of player 1#
  play2 = "Count_defective", # strategy of player 2,#
  parameters = params # list of parameters to pass to ode solver
)

simulationMay_2 <- game(
  type = "May",
  play1 = "Count_defective_invert", # strategy of player 1#
  play2 = "Count_defective_invert", # strategy of player 2,#
  parameters = params # list of parameters to pass to ode solver
)

test_that("results change when the strategy is count defective vs count defective inverted", {
  expect_true(simulationMay_1[20, 1] != simulationMay_2[20, 1])
})
