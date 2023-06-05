// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace arma;
using namespace Rcpp;


//' Generate parameter samples from the prior
//'
//' @name generateParameterSamples
//' @param num_Params Number of parameters
//' @param numParticles Number of parameter samples to generate
//' @param priorMin Lower bound of the prior distribution
//' @param priorMax Upper bound of the prior distribution
//'
//' @return  parameterSamples Generated parameter samples as an Armadillo matrix
//' @export
// [[Rcpp::export]]
arma::mat generateParameterSamples(int numParticles, int numParams, arma::vec priorMin, arma::vec priorMax) {
  // Check if the number of parameters is greater than zero
  if (numParams <= 0) {
    Rcpp::stop("The number of parameters must be greater than zero.");
  }
  // Check if the number of particles is greater than zero
  if (numParticles <= 0) {
    Rcpp::stop("The number of samples must be greater than zero.");
  }
  // Check if the lower bound of the prior distribution is less than the upper bound
  for(int i = 0 ; i < numParams ; ++i){
    if (priorMin(i) >= priorMax(i)) {
      Rcpp::stop("The lower bound of the prior distribution must be less than the upper bound.");
    }
  }
  // Create a matrix to store the parameter samples
  arma::mat parameterSamples(numParticles, numParams);

  // Generate parameter samples from the prior distribution
  for (int i = 0; i < numParticles; ++i) {
    for (int j = 0; j < numParams; ++j) {
      // Generate a random value for the j-th parameter from the uniform prior distribution
      parameterSamples(i, j) = arma::randu() * (priorMax(j) - priorMin(j)) + priorMin(j);
    }
  }
  // Return the parameter samples
  return parameterSamples;
}

//' Function to generate simulated data from the zombie outbreak model
//'
//' @name generateSimulatedData
//' @param parameters A matrix of parameter values (each row represents a set of parameter values)
//' @param numParticles Number of data samples to generate
//' @param numTimePoints Number of time points to simulate
//'
//' @return  A 3D array of simulated data with dimensions (numParticles, numTimePoints, 3). The third dimension represents the populations: 1 - Susceptible, 2 - Zombie, 3 - Removed
//' @export
// [[Rcpp::export]]
arma::cube generateSimulatedData(const arma::mat& parameters, int numTimePoints) {
  int numParticles = parameters.n_rows; // Number of parameter samples
  arma::cube simulatedData(numTimePoints, 3, numParticles, arma::fill::zeros);
  for (int i = 0; i < numParticles; ++i) {
    // Initialize populations with initial values
    double S = 500; // Initial susceptible population size
    double Z = 1; // Initial infected (zombie) population size
    double R = 0; // Initial number removed from the population
    
    simulatedData(0, 0, i) = S;
    simulatedData(0, 1, i) = Z;
    simulatedData(0, 2, i) = R;

    for (int t = 1; t < numTimePoints; ++t) {
      // Retrieve parameter values for the current sample
      double birthRate = parameters(i, 0);
      double encounterRate = parameters(i, 1);
      double deathRate = parameters(i, 2);
      double resurrectionRate = parameters(i, 3);
      double defeatRate = parameters(i, 4);
      // Check if the parameter values are within the allowed range
      if (birthRate < 0 || birthRate > 1) {
        Rcpp::stop("The birth rate must be between 0 and 1.");
      }
      if (encounterRate < 0 || encounterRate > 1) {
        Rcpp::stop("The encounter rate must be between 0 and 1.");
      }
      if (deathRate < 0 || deathRate > 1) {
        Rcpp::stop("The death rate must be between 0 and 1.");
      }
      if (resurrectionRate < 0 || resurrectionRate > 1) {
        Rcpp::stop("The resurrection rate must be between 0 and 1.");
      }
      if (defeatRate < 0 || defeatRate > 1) {
        Rcpp::stop("The defeat rate must be between 0 and 1.");
      }

      // Update population sizes based on the model equations
      int newS = S + birthRate * S - encounterRate * S * Z - deathRate * S;   // Update susceptible individuals
      int newZ = Z + encounterRate * S * Z + R * resurrectionRate - defeatRate * S * Z;   // Update zombies
      int newR = R + deathRate * S + defeatRate * S * Z - resurrectionRate * R;   // Update removed individuals

      // Store the population sizes in the simulated data cube
      simulatedData(t, 0, i) = newS;
      simulatedData(t, 1, i) = newZ;
      simulatedData(t, 2, i) = newR;

      // Update population sizes for the next time step
      S = newS;
      Z = newZ;
      R = newR;
    }
  }
  return simulatedData;
}

//' Function to compute summary statistics for simulated data from the zombie outbreak model
//'
//' @name computeSummaryStatistics
//' @param simulatedData A 3D array of simulated data with dimensions (numParticles, numTimePoints, numPopStats)
//'
//' @return A matrix of summary statistics with dimensions (numParticles, numStats). Each row represents the summary statistics for a particular particle. The number of columns (numStats) depends on the selected summary statistics
//' @export
// [[Rcpp::export]]
arma::mat computeSummaryStatistics(const arma::cube& simulatedData) {
  int numStats = 9; // Number of summary statistics
  int numParticles = simulatedData.n_slices;
  int numTimePoints = simulatedData.n_rows;
  arma::mat summaryStats(numParticles, numStats, arma::fill::zeros);

  for (int i = 0; i < numParticles; ++i) {
    const arma::mat& sampleData = simulatedData.slice(i);

    // Compute summary statistics
    double finalSusceptible = sampleData(numTimePoints - 1, 0);
    double finalZombies= sampleData(numTimePoints - 1, 1);
    double finalRemoved = sampleData(numTimePoints - 1, 2);
    double epidemicPeakTime = sampleData.col(1).index_max() +1; // we add one so will match with R's non-zero indexing convention
    double duration = numTimePoints;
    double maxZombieRate = sampleData.col(1).max() - sampleData.col(1)(0);
    double maxSusceptibleRate = sampleData.col(0)(0) - sampleData.col(0).max();
    double proportionInfectedPeak = sampleData.col(1).max() / sampleData(0, 0);
    double timeToExtinction = sampleData.col(0).index_min()+ 1; // we add one so will match with R's non-zero indexing convention

    // Store summary statistics
    summaryStats(i, 0) = finalSusceptible;
    summaryStats(i, 1) = finalZombies;
    summaryStats(i, 2) = finalRemoved;
    summaryStats(i, 3) = epidemicPeakTime;
    summaryStats(i, 4) = duration;
    summaryStats(i, 5) = maxZombieRate;
    summaryStats(i, 6) = maxSusceptibleRate;
    summaryStats(i, 7) = proportionInfectedPeak;
    summaryStats(i, 8) = timeToExtinction;
  }

  return summaryStats;
}

//' Function to calculate the euclidean distance between observed and simulated data
//'
//' @name calculateDistance
//' @param observedData Matrix of observed data (each row represents a time point)
//' @param simulatedData Cube of simulated data (each second dimension represents a simulated data point)
//'
//' @return distances Vector of distances between observed and simulated data points
//' @export
// [[Rcpp::export]]
arma::mat calculateDistance(const arma::mat& observedData, const arma::cube& simulatedData) {
  // Get the number of time points
  int numTimePoints = observedData.n_rows;
  // Get the number of particles
  int numParticles = simulatedData.n_slices;

  // Create a vector to store the distances
  arma::mat distances(numTimePoints, numParticles);

  // Calculate the distance between observed and simulated data points
  for (int i = 0; i < numTimePoints; ++i) {
    // Get the observed data for current time point
    const arma::rowvec& observedPoint = observedData.row(i);
    for(int j = 0 ; j < numParticles; ++j){
      // Get the simulated data for current time point for current particle
      const arma::rowvec& simulatedPoint = simulatedData.slice(j).row(i);

      // Calculate the distance between the observed and simulated data points
      distances(i,j) = arma::norm(observedPoint - simulatedPoint, 2);  // Euclidean distance
    }
  }
  // Return the distances
  return distances;
}


//' Perform acceptance/rejection of parameter samples and update with weights
//'
//' @name acceptRejectAndUpdate
//' @param parameterSamples Matrix of parameter samples (each row represents a set of parameter values)
//' @param observedData Matrix of observed data (each row represents an observed data point)
//' @param tolerance Tolerance threshold for accepting parameter samples
//'
//' @return  acceptedSamples Matrix of accepted parameter samples
//' @return weights Vector of weights assigned to accepted parameter samples
//' @export
// [[Rcpp::export]]
Rcpp::List acceptRejectAndUpdate(const arma::mat& parameterSamples, const arma::mat& distances, double tolerance) {
  // Get the number of parameter samples and data points
  int numParticles = parameterSamples.n_rows;
  int numTimePoints = distances.n_rows;

  // Perform acceptance/rejection and assign weights for each particle based on average of distances for all time points
  double sumWeights = 0.0;
  arma::uvec acceptedIndices(numParticles, arma::fill::zeros); 
  for (int i = 0; i < numParticles; ++i) {
    double particle_distance = arma::sum(distances.col(i)) / numTimePoints;
    if (particle_distance <= tolerance) {
      acceptedIndices(i) = 1;
      sumWeights += 1.0;
    }
  }
  arma::uvec nonzeroIndices = arma::find(acceptedIndices!= 0);

  // Calculate weights
  arma::vec weights(numParticles, arma::fill::zeros);
  int numAccepted = nonzeroIndices.n_elem;
  if (numAccepted > 0) {
    double weightValue = sumWeights / numAccepted;
    weights.elem(nonzeroIndices).fill(weightValue);
  }

  // Update parameter samples with weights
  arma::mat acceptedParamSamples = parameterSamples.rows(nonzeroIndices);

  // Return accepted parameter samples and weights as a list
  return Rcpp::List::create(Rcpp::Named("acceptedParamSamples") = acceptedParamSamples,
                            Rcpp::Named("weights") = weights);
}

//' Perform simple acceptance/rejection of parameter samples
//'
//' @name acceptReject
//' @param parameterSamples Matrix of parameter samples (each row represents a set of parameter values)
//' @param observedData Matrix of observed data (each row represents an observed data point)
//' @param tolerance Tolerance threshold for accepting parameter samples
//'
//' @return  acceptedSamples Matrix of accepted parameter samples
//' @return weights Vector of weights assigned to accepted parameter samples
//' @export
// [[Rcpp::export]]
arma::mat acceptReject(const arma::mat& parameterSamples, const arma::mat& distances, double tolerance) {
  // Get the number of parameter samples and data points
  int numParticles = parameterSamples.n_rows;
  int numTimePoints = distances.n_rows;

  // Perform acceptance/rejection for each particle based on average of distances for all time points
  arma::uvec acceptedIndices(numParticles, arma::fill::zeros); 
  for (int i = 0; i < numParticles; ++i) {
    double particle_distance = arma::sum(distances.col(i)) / numTimePoints;
    if (particle_distance <= tolerance) {
      acceptedIndices(i) = 1;
    }
  }
  arma::uvec nonzeroIndices = arma::find(acceptedIndices!= 0);

  // Return accepted parameter samples and weights as a list
  return parameterSamples.rows(nonzeroIndices);
}

//' Estimate the posterior distribution based on accepted parameter samples and weights
//'
//' @name estimatePosterior
//' @param acceptedSamples Matrix of accepted parameter samples (each row represents a set of accepted parameter values)
//' @param weights Vector of weights assigned to accepted parameter samples
//'
//' @return posteriorSamples Matrix of posterior parameter samples
//' @export
// [[Rcpp::export]]
arma::mat estimatePosterior(const arma::mat& acceptedSamples, const arma::vec& weights) {
  // Get the number of accepted samples and parameters
  int numAccepted = acceptedSamples.n_rows;
  int numParams = acceptedSamples.n_cols;

  // Calculate the posterior samples based on weights
  arma::mat posteriorSamples(numAccepted, numParams);
  for (int i = 0; i < numAccepted; ++i) {
    double weight = weights(i);

    if (weight > 0.0) {
      posteriorSamples.row(i) = acceptedSamples.row(i) * weight;
    }
  }

  // Normalize the posterior samples
  double sumWeights = arma::sum(weights);
  if (sumWeights > 0.0) {
    posteriorSamples /= sumWeights;
  }

  // Return the posterior samples
  return posteriorSamples;
}

//' ABC function
//'
//' @name abcRej
//' @param observedData Observed data (e.g., as an Armadillo matrix or vector)
//' @param numParticles Number of particles/samples to generate
//' @param epsilon Tolerance threshold
//'
//' @return posteriorSamples Generated posterior parameter samples (e.g., as an Armadillo matrix)
//' @export
// [[Rcpp::export]]
arma::mat abcRej(const arma::mat& observedData, const int numParticles, const double epsilon, const arma::vec& priorMin, const arma::vec& priorMax){
  // Check the dimensions of the observed data
  if (observedData.n_cols != 3) {
    Rcpp::stop("The observed data must have three columns.");
  }
  int numParams = 5;

  // Initialize containers for parameter samples and posterior summaries
  arma::mat parameterSamples(numParticles, numParams);
  arma::mat posteriorSamples;

  // Generate parameter samples from prior 
  parameterSamples = generateParameterSamples(numParticles, numParams, priorMin, priorMax);

  // Generate simulated data based on parameter samples
  int numTimePoints = observedData.n_rows;
  arma::cube simulatedData = generateSimulatedData(parameterSamples, numTimePoints);

  // Compute distance between observed and simulated data
  arma::mat distances = calculateDistance(observedData, simulatedData);

  // Compare distance to tolerance threshold and accept/reject parameter samples and update parameter samples based on acceptance/rejection step, assign higher weights to accepted samples
  arma::mat accepted = acceptReject(parameterSamples, distances, epsilon);

  return accepted;
}

