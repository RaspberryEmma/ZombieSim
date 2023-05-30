# ****************************************
# Intractable Models Simulation Study
# 
# Run Entire Simulation from Command Line
#   1. Generate SIR model data with "true" parameters
# 	2. ABC to estimate patameters
#   3. Interpret parameter estimation results results
# 
# Henry Bourne, Rachel Wood, Emma Tarmey
#
# Started:          23/05/2023
# Most Recent Edit: 31/05/2023
# ****************************************


clear
echo " *** ✨ Starting ✨ ***"


# Generate SIR model data
echo ""
echo ""
echo " *** Generating Data ***"
Rscript generate_data.R


# Generate SIR model data
#echo ""
#echo ""
#echo " *** Parameter Estimation Simulation ***"
#Rscript simulation.R


# Interpret results
echo ""
echo ""
echo " *** Interpreting Results ***"
Rscript interpret_results.R


echo ""
echo " *** ✨ Complete ✨ ***"




