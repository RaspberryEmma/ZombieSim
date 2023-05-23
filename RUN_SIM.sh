# ****************************************
# Intractable Models Simulation Study
# 
# Run Entire Simulation from Command Line
#   1. Generate all relevant plots
# 	2. Generate data, run SIR models, obtain outcome and record
#   3. Interpret outcome results
# 
# Henry Bourne, Rachel Wood, Emma Tarmey
#
# Started:          23/05/2023
# Most Recent Edit: 23/05/2023
# ****************************************


clear
echo " *** ✨ Starting ✨ ***"


# Generate all relevant plots
echo ""
echo ""
echo " *** Generating Plots ***"
Rscript generate_data.R


# Run SIR model
echo ""
echo ""
echo " *** Performing Simulation ***"
Rscript simulation.R


# Interpret results
echo ""
echo ""
echo " *** Interpreting Results ***"
Rscript interpret_results.R


echo ""
echo " *** ✨ Complete ✨ ***"




