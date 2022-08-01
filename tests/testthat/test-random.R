
 params <- list(mutation1 = 10**-6,
                mutation2 = 10**-6,
                genome1 = 5000,
                genome2 = 8500,
                #(1:1) cooperate/cooperate
                r1_1_1=0.9,
                r2_1_1=0.19,
                a12_1_1=0.012,
                a21_1_1=-0.2,
                #(1:2) cooperate/defective
                r1_1_2=0.1,
                r2_1_2=0.0018,
                a12_1_2=-1,
                a21_1_2=0.1,
                #(2:1) defective/cooperate
                r1_2_1=0.15,
                r2_2_1=0.18,
                a12_2_1=-1,
                a21_2_1=-0.001,
                #(2:2) defective/defective
                r1_2_2=0.8,
                r2_2_2=-0.8,
                a12_2_2=-0.1,
                a21_2_2=-0.1)
 set.seed(1)
 simulationLV_1 <- game(type="Lotka",
                      play1=randdeff, #strategy of player 1#
                      play2=randdeff, #strategy of player 2,#
                      parameters=params #list of parameters to pass to ode solver
 )
 set.seed(1)

 simulationLV_2 <- game(type="Lotka",
                        play1=randdeff, #strategy of player 1#
                        play2=randdeff, #strategy of player 2,#
                        parameters=params #list of parameters to pass to ode solver
 )
test_that("results are reproducible by using the seed", {
  expect_true(simulationLV_1[10,1] == simulationLV_2[10,1])
})

set.seed(1)
simulationMay_1 <- game(type="May",
                       play1=randdeff, #strategy of player 1#
                       play2=randdeff, #strategy of player 2,#
                       parameters=params #list of parameters to pass to ode solver
)
set.seed(1)

simulationMay_2 <- game(type="May",
                       play1=randdeff, #strategy of player 1#
                       play2=randdeff, #strategy of player 2,#
                       parameters=params #list of parameters to pass to ode solver
)

test_that("results are reproducible by using the seed", {
  expect_true(simulationMay_1[20,1] == simulationMay_2[20,1])
})