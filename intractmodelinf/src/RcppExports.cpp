// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// generateParameterSamples
arma::mat generateParameterSamples(int numParams, int numParticles, arma::vec priorMin, arma::vec priorMax);
RcppExport SEXP _intractmodelinf_generateParameterSamples(SEXP numParamsSEXP, SEXP numParticlesSEXP, SEXP priorMinSEXP, SEXP priorMaxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type numParams(numParamsSEXP);
    Rcpp::traits::input_parameter< int >::type numParticles(numParticlesSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type priorMin(priorMinSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type priorMax(priorMaxSEXP);
    rcpp_result_gen = Rcpp::wrap(generateParameterSamples(numParams, numParticles, priorMin, priorMax));
    return rcpp_result_gen;
END_RCPP
}
// generateSimulatedData
arma::cube generateSimulatedData(const arma::mat& parameters, int numTimePoints);
RcppExport SEXP _intractmodelinf_generateSimulatedData(SEXP parametersSEXP, SEXP numTimePointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type parameters(parametersSEXP);
    Rcpp::traits::input_parameter< int >::type numTimePoints(numTimePointsSEXP);
    rcpp_result_gen = Rcpp::wrap(generateSimulatedData(parameters, numTimePoints));
    return rcpp_result_gen;
END_RCPP
}
// computeSummaryStatistics
arma::mat computeSummaryStatistics(const arma::cube& simulatedData);
RcppExport SEXP _intractmodelinf_computeSummaryStatistics(SEXP simulatedDataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::cube& >::type simulatedData(simulatedDataSEXP);
    rcpp_result_gen = Rcpp::wrap(computeSummaryStatistics(simulatedData));
    return rcpp_result_gen;
END_RCPP
}
// calculateDistance
arma::mat calculateDistance(const arma::mat& observedData, const arma::cube& simulatedData);
RcppExport SEXP _intractmodelinf_calculateDistance(SEXP observedDataSEXP, SEXP simulatedDataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type observedData(observedDataSEXP);
    Rcpp::traits::input_parameter< const arma::cube& >::type simulatedData(simulatedDataSEXP);
    rcpp_result_gen = Rcpp::wrap(calculateDistance(observedData, simulatedData));
    return rcpp_result_gen;
END_RCPP
}
// acceptRejectAndUpdate
Rcpp::List acceptRejectAndUpdate(const arma::mat& parameterSamples, const arma::mat& distances, double tolerance);
RcppExport SEXP _intractmodelinf_acceptRejectAndUpdate(SEXP parameterSamplesSEXP, SEXP distancesSEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type parameterSamples(parameterSamplesSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type distances(distancesSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(acceptRejectAndUpdate(parameterSamples, distances, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// estimatePosterior
arma::mat estimatePosterior(const arma::mat& acceptedSamples, const arma::vec& weights);
RcppExport SEXP _intractmodelinf_estimatePosterior(SEXP acceptedSamplesSEXP, SEXP weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type acceptedSamples(acceptedSamplesSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type weights(weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(estimatePosterior(acceptedSamples, weights));
    return rcpp_result_gen;
END_RCPP
}
// performInference
Rcpp::List performInference(const arma::mat& posteriorSamples);
RcppExport SEXP _intractmodelinf_performInference(SEXP posteriorSamplesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type posteriorSamples(posteriorSamplesSEXP);
    rcpp_result_gen = Rcpp::wrap(performInference(posteriorSamples));
    return rcpp_result_gen;
END_RCPP
}
// sl
arma::vec sl(vec& y, mat& X);
RcppExport SEXP _intractmodelinf_sl(SEXP ySEXP, SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(sl(y, X));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_intractmodelinf_generateParameterSamples", (DL_FUNC) &_intractmodelinf_generateParameterSamples, 4},
    {"_intractmodelinf_generateSimulatedData", (DL_FUNC) &_intractmodelinf_generateSimulatedData, 2},
    {"_intractmodelinf_computeSummaryStatistics", (DL_FUNC) &_intractmodelinf_computeSummaryStatistics, 1},
    {"_intractmodelinf_calculateDistance", (DL_FUNC) &_intractmodelinf_calculateDistance, 2},
    {"_intractmodelinf_acceptRejectAndUpdate", (DL_FUNC) &_intractmodelinf_acceptRejectAndUpdate, 3},
    {"_intractmodelinf_estimatePosterior", (DL_FUNC) &_intractmodelinf_estimatePosterior, 2},
    {"_intractmodelinf_performInference", (DL_FUNC) &_intractmodelinf_performInference, 1},
    {"_intractmodelinf_sl", (DL_FUNC) &_intractmodelinf_sl, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_intractmodelinf(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
