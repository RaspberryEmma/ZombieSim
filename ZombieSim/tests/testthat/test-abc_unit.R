test_that("generateParameterSamples returns correct output", {
    # Positive test case
    ps <- generateParameterSamples(100, 3, c(-1.0, -1.0, -1.0), c(1.0, 1.0, 1.0))
    expect_equal(dim(ps), c(100, 3))
    expect_true(all(ps > -1.0))
    expect_true(all(ps < 1.0))
    # Negative test case
    expect_error(generateParameterSamples(100, -1, -1.0, 1.0))
    expect_error(generateParameterSamples(-100, 3, -1.0, 1.0))
    expect_error(generateParameterSamples(100, 3, 1.0, -1.0))
})

# Unit tests for the generateSimulatedData function
test_that("generateSimulatedData returns the correct dimensions", {
    numParticles <- 3
    parameters <- matrix(runif(5*3), nrow = numParticles, ncol = 5)
    numTimePoints <- 10
    
    simulatedData <- generateSimulatedData(parameters, numTimePoints)
    expect_equal(dim(simulatedData), c(numTimePoints, 3, numParticles))
})

test_that("generateSimulatedData throws an error for invalid birth rate", {
    numParticles <- 3
    parameters <- matrix(runif(5*3), nrow = numParticles, ncol = 5)
    parameters[1,] <- c(-0.1, 0.2, 0.3, 0.4, 1.1)
    numTimePoints <- 10

    expect_error(generateSimulatedData(parameters, numTimePoints),
               "The birth rate must be between 0 and 1.")
})

test_that("generateSimulatedData throws an error for invalid encounter rate", {
    numParticles <- 3
    parameters <- matrix(runif(5*3), nrow = numParticles, ncol = 5)
    parameters[2,] <- c(0.1, 1.2, -0.3, 0.4, 0.5)
    numTimePoints <- 10
    
    expect_error(generateSimulatedData(parameters, numTimePoints),
                "The encounter rate must be between 0 and 1.")
})

test_that("computeSummaryStatistics returns correct output", {
    # Generate example simulated data
    numParticles <- 100
    numTimePoints <- 10
    testInputParameters <- generateParameterSamples(numParticles, 5, c(0.0, 0.00949, 0.0, 0.00009, 0.0049), c(0.000000001, 0.0095, 0.000000001, 0.0001, 0.005))
    simulatedData <- generateSimulatedData(testInputParameters, numTimePoints)
    # Compute summary statistics
    summaryStats <- computeSummaryStatistics(simulatedData)

    # Check the dimensions of the output
    expect_equal(dim(summaryStats), c(numParticles, 9), info = "Output matrix has correct dimensions")
    # Check that all summary statistics are greater than or equal to zero
    expect_true(all(summaryStats >= 0), info = "Summary statistics are non-negative")
})

test_that("calculateDistance returns correct Euclidean distance", {
    # Define observed data
    observedData <- matrix(c(1, 3, 2, 4), ncol = 2)

    # Define simulated data
    simulatedData <- array(data = c(1, 1, 3, 3, 2, 2, 4, 4), dim = c(2, 2, 2))

    # Define expected Euclidean distances
    expectedDistances <- matrix(c(sqrt(1), sqrt(5), sqrt(5), sqrt(1)), nrow = 2)

    # Calculate Euclidean distances
    calculatedDistances <- calculateDistance(observedData, simulatedData)

    # Check the dimensions of the output
    expect_equal(dim(calculatedDistances), dim(expectedDistances), info = "Output matrix has correct dimensions")

    # Check that the calculated distances match the expected values
    expect_equal(calculatedDistances, expectedDistances, info = "Calculated distances match the expected values")
})

# Unit tests for the acceptRejectAndUpdate function
test_that("acceptRejectAndUpdate returns the expected output dimensions", {
    parameterSamples <- matrix(runif(100), nrow = 20, ncol = 5)

    numRows <- 5
    numCols <- 20
    distances <- matrix(runif(numRows * numCols), nrow = numRows)
    distances[, 1:10] <- distances[, 1:10] * 0.01
    distances[, 11:20] <- matrix(runif(50, 0.1, 1), nrow = numRows)
    tolerance <- 0.1

    result <- acceptRejectAndUpdate(parameterSamples, distances, tolerance)
    acceptedParamSamples <- result$acceptedParamSamples
    weights <- result$weights

    expect_equal(dim(acceptedParamSamples), c(10, 5))
    expect_equal(length(weights), nrow(parameterSamples))
})

test_that("acceptRejectAndUpdate updates the parameter samples correctly", {
  parameterSamples <- matrix(runif(100), nrow = 20, ncol = 5)
  distances <- matrix(runif(20), nrow = 5, ncol = 20)
  tolerance <- 0.1

  result <- acceptRejectAndUpdate(parameterSamples, distances, tolerance)
  acceptedSamples <- result$acceptedSamples

  expect_true(all(acceptedSamples >= 0 & acceptedSamples <= 1))
})

# Unit tests for the estimatePosterior function
test_that("estimatePosterior returns the expected output dimensions", {
  acceptedSamples <- matrix(runif(100), nrow = 20, ncol = 5)
  weights <- runif(20)

  posterior <- estimatePosterior(acceptedSamples, weights)

  expect_equal(dim(posterior), c(nrow(acceptedSamples), ncol(acceptedSamples)))
})

test_that("estimatePosterior returns valid posterior probabilities", {
  acceptedSamples <- matrix(runif(100), nrow = 20, ncol = 5)
  weights <- runif(20)

  posterior <- estimatePosterior(acceptedSamples, weights)

  expect_true(all(posterior >= 0 & posterior <= 1))
  # expect_equal(sum(posterior), 1) TODO: uncomment
  observedData <- matrix(c(1, 3, 2, 4, 5, 6), ncol = 3)
})

# END OF FILE
# -----------------------------
# -----------------------------
# -----------------------------
# Run the above tests using "source("abc_unit_tests.R")" 