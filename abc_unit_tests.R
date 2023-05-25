library(testthat)
library(Rcpp)

sourceCpp("abc.cpp")

test_that("generateParameterSamples returns correct output", {
    # Positive test case
    ps <- generateParameterSamples(3, 100, -1.0, 1.0)
    expect_equal(dim(ps), c(100, 3))
    expect_true(all(ps > -1.0))
    expect_true(all(ps < 1.0))
    # Negative test case
    expect_error(generateParameterSamples(-1, 100, -1.0, 1.0))
    expect_error(generateParameterSamples(3, -100, -1.0, 1.0))
    expect_error(generateParameterSamples(3, 100, 1.0, -1.0))
})

test_that("generateSimulatedData returns correct output", {
    # Unit tests for "generateSimulatedData" function
    # Positive test case
    expectedOutputDimensions <- c(10, 5, 3)
    testInputParameters <- matrix(c(rep(0.1, 10), rep(0.2, 10)), ncol = 2)
    result <- generateSimulatedData(testInputParameters, 10, 5)
    expect_equal(dim(result), expectedOutputDimensions)
    expect_true(all(result >= 0))
    # Negative test case
    testInputParameters <- matrix(c(rep(0.1, 10), -0.2, rep(0.1, 9)), ncol = 2)
    expect_error(generateSimulatedData(testInputParameters, 10, 5))
})

test_that("computeSummaryStatistics returns correct output", {
    # Create test data
    testData <- array(c(500, 1, 0, 484.2, 15.8, 0,
                        300, 200, 0, 280, 60, 160,
                        100, 50, 350, 50, 30, 320,
                        700, 0, 0, 564.8, 135.2, 0), dim = c(4, 3, 3))

    # Positive test case
    expectedOutput <- matrix(c(0), nrow = 4, ncol = 9)
    colnames(expectedOutput) <- c("finalSusceptible", "finalInfected", "finalZombies",
                                "epidemicPeakTime", "duration", "maxInfectedRate",
                                "maxSusceptibleRate", "proportionInfectedPeak", "timeToExtinction")
    rownames(expectedOutput) <- NULL
    expectedOutput[1, ] <- c(0, 0, 0, 1, 2, 0, 16, 0, 2)
    expectedOutput[2, ] <- c(0, 200, 0, 1, 2, 198, 200, 1, 2)
    expectedOutput[3, ] <- c(0, 50, 350, 1, 2, 45, 100, 0.1, 2)
    expectedOutput[4, ] <- c(0, 0, 0, 1, 2, 0, 136, 0, 2)
    result <- computeSummaryStatistics(testData)
    expect_equal(round(result, 6), expectedOutput)

    # Negative test case
    expect_error(computeSummaryStatistics(matrix(c(1:12), nrow = 4, ncol = 3)))
})

# END OF FILE
# -----------------------------
# -----------------------------
# -----------------------------
# Run the above tests using "source("abc_unit_tests.R")" 