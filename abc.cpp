// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;


// Generate parameter samples from the prior
// Parameters:
// - num_Params: Number of parameters
// - numParticles: Number of parameter samples to generate
// - priorMin: Lower bound of the prior distribution
// - priorMax: Upper bound of the prior distribution
// Returns:
// - parameterSamples: Generated parameter samples as an Armadillo matrix
// [[Rcpp::export]]
arma::mat generateParameterSamples(int numParams, int numParticles, double priorMin, double priorMax) {
  // Check if the number of parameters is greater than zero
  if (numParams <= 0) {
    Rcpp::stop("The number of parameters must be greater than zero.");
  }
  // Check if the number of particles is greater than zero
  if (numParticles <= 0) {
    Rcpp::stop("The number of samples must be greater than zero.");
  }
  // Check if the lower bound of the prior distribution is less than the upper bound
  if (priorMin >= priorMax) {
    Rcpp::stop("The lower bound of the prior distribution must be less than the upper bound.");
  }
  // Create a matrix to store the parameter samples
  arma::mat parameterSamples(numParticles, numParams);

  // Generate parameter samples from the prior distribution
  for (int i = 0; i < numParticles; ++i) {
    for (int j = 0; j < numParams; ++j) {
      // Generate a random value for the j-th parameter from the uniform prior distribution
      parameterSamples(i, j) = arma::randu() * (priorMax - priorMin) + priorMin;
    }
  }

  // Return the parameter samples
  return parameterSamples;
}

// Function to generate simulated data from the zombie outbreak model
// Parameters:
//   parameters: a matrix of parameter values (each row represents a set of parameter values)
//   numParticles: number of data samples to generate
//   numTimePoints: number of time points to simulate
// Returns:
//   A 3D array of simulated data with dimensions (numParticles, numTimePoints, 3)
//   The third dimension represents the populations: 1 - Susceptible, 2 - Infected, 3 - Zombies
// [[Rcpp::export]]
arma::cube generateSimulatedData(const arma::mat& parameters, int numParticles, int numTimePoints) {
  int numCols = parameters.n_cols;
  arma::cube simulatedData(numParticles, numTimePoints, 3, arma::fill::zeros);

  for (int i = 0; i < numParticles; ++i) {
    // Initialize populations with initial values
    double susceptible = 500; // Initial susceptible population size
    double infected = 1; // Initial infected population size
    double zombies = 0; // Initial zombie population size

    for (int t = 0; t < numTimePoints; ++t) {
      // Retrieve parameter values for the current sample
      double infectionRate = parameters(i, 0);
      double killRate = parameters(i, 1);
      // Check if the parameter values are within the allowed range
      if (infectionRate < 0 || infectionRate > 1) {
        Rcpp::stop("The infection rate must be between 0 and 1.");
      }
      if (killRate < 0 || killRate > 1) {
        Rcpp::stop("The kill rate must be between 0 and 1.");
      }

      // Update population sizes based on the model equations
      double newSusceptible = susceptible - (infectionRate * susceptible * zombies);
      double newInfected = infected + (infectionRate * susceptible * zombies) - (killRate * infected);
      double newZombies = zombies + (killRate * infected);

      // Store the population sizes in the simulated data cube
      simulatedData(i, t, 0) = newSusceptible;
      simulatedData(i, t, 1) = newInfected;
      simulatedData(i, t, 2) = newZombies;

      // Update population sizes for the next time step
      susceptible = newSusceptible;
      infected = newInfected;
      zombies = newZombies;
    }
  }

  return simulatedData;
}

// Function to compute summary statistics for simulated data from the zombie outbreak model
// Parameters:
//   simulatedData: a 3D array of simulated data with dimensions (numParticles, numTimePoints, numPopStats)
// Returns:
//   A matrix of summary statistics with dimensions (numParticles, numStats)
//   Each row represents the summary statistics for a particular particle
//   The number of columns (numStats) depends on the selected summary statistics
// [[Rcpp::export]]
arma::mat computeSummaryStatistics(const arma::cube& simulatedData) {
  int numStats = 9; // Number of summary statistics
  int numParticles = simulatedData.n_rows;
  int numTimePoints = simulatedData.n_cols;
  arma::mat summaryStats(numParticles, numStats, arma::fill::zeros);

  for (int i = 0; i < numParticles; ++i) {
    const arma::mat& sampleData = simulatedData.row(i);

    // Compute summary statistics
    double finalSusceptible = sampleData(numTimePoints - 1, 0);
    double finalInfected = sampleData(numTimePoints - 1, 1);
    double finalZombies = sampleData(numTimePoints - 1, 2);
    double epidemicPeakTime = sampleData.col(1).index_max();
    double duration = numTimePoints - 1;
    double maxInfectedRate = sampleData.col(1).max() - sampleData.col(1)(0);
    double maxSusceptibleRate = sampleData.col(0)(0) - sampleData.col(0).max();
    double proportionInfectedPeak = sampleData.col(1).max() / sampleData(0, 0);
    double timeToExtinction = sampleData.col(2).index_min();

    // Store summary statistics
    summaryStats(i, 0) = finalSusceptible;
    summaryStats(i, 1) = finalInfected;
    summaryStats(i, 2) = finalZombies;
    summaryStats(i, 3) = epidemicPeakTime;
    summaryStats(i, 4) = duration;
    summaryStats(i, 5) = maxInfectedRate;
    summaryStats(i, 6) = maxSusceptibleRate;
    summaryStats(i, 7) = proportionInfectedPeak;
    summaryStats(i, 8) = timeToExtinction;
  }

  return summaryStats;
}

// ABC function
// Parameters:
// - model: Model function to generate simulated data based on input params (e.g., as an Rcpp::Function)
// - params: Parameter values (e.g., as an Armadillo vector)
// - observedData: Observed data (e.g., as an Armadillo matrix or vector)
// - numParticles: Number of particles/samples to generate
// - epsilon: Tolerance threshold
// Returns:
// - parameterSamples: Generated parameter samples (e.g., as an Armadillo matrix or vector)
// - posteriorSummaries: Posterior summaries (e.g., as an Armadillo matrix or vector)
// [[Rcpp::export]]
Rcpp::List abc(const Rcpp::Function model, const arma::vec& params, const arma::mat& observedData,
                       int numParticles, double epsilon, double priorMin = 0.0, double priorMax=1.0) {
  // Initialize containers for parameter samples and posterior summaries
  arma::mat parameterSamples(numParticles, params.n_cols);
  arma::mat posteriorSummaries(numParticles, 5); //TODO: Change 5 to number of posterior summaries

  // Generate parameter samples from prior 
  parameterSamples = generateParameterSamples(params.size(), numParticles, priorMin, priorMax);

  // Generate simulated data based on parameter samples
  int numTimePoints = observedData.n_rows;
  arma::cube simulatedData = generateSimulatedData(params, numParticles, numTimePoints);

  // Compute summary statistics for simulated data

  // Compute distance between observed and simulated data

  // Compare distance to tolerance threshold and accept/reject parameter samples

  // Update parameter samples based on acceptance/rejection step, assign higher weights to accepted samples

  // Estimate posterior distribution based on accepted parameter samples and weights

  // Perform posterior inference (e.g., compute posterior summaries)

  // Return parameter samples and posterior summaries as an Rcpp List
  return Rcpp::List::create(
    Rcpp::Named("parameterSamples") = parameterSamples,
    Rcpp::Named("posteriorSummaries") = posteriorSummaries
  );
}

