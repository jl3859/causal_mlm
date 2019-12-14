# Causal Final: Simulation study of linear multilevel models

Scripts:
1. dgp.script.R
2. simulation_base.R
3. distibutions_base.R
4. simulation_vre.R
5. distibutions_vre.R
6. simulation_ig.R
7. distibutions_ig.R
8. simulation_gl.R
9. distibutions_igl.R
10. simulation_script.R
11. figures_base.R
12. tables.R
13. simulation_ig_rm_pretest.R


Scripts Description:
1. dgp_script.R: creates functions for the data generating process
2. simulation_base.R: runs dgp_script (classroom_dat_function) from base scenario (just multilevels given classrooms and students) with treatment at the student-level and runs simulations to estimate SATE 
3. distibutions_base.R: creates histograms from base scenario simulation
4. simulation_vre.R: runs dgp_script for violating random effects (vre) assumption (re_dat_function) to simulate data and runs simulations to estimate SATE  
5. distibutions_vre.R: creates histograms from violating random effects simulation
6. simulation_ig.R: runs dgp_script for violating ignorability assumption (ig_dat_function) to simulate data and runs simulations to estimate SATE 
7. distibutions_ig.R: creates histograms from violating ignorability simulation
8. simulation_gl.R: runs dgp_script for randomization at the group-level from the base scenario and runs simulatinos to estimate SATE
9. distibutions_gl.R: creates histograms from group-level treatment simulation
10. simulation_script.R: runs dgp_script and all simulations at once with histograms 
11. figures_base.R: creates figures from original classroom_dat from first run ofr dgp_script
12. tables.R: runs all simulations from separate simulation scripts (this takes ~ 1hr for 1000 iterations) and combines results from said simulations into three different tables: one run, randomization distributions, sampling distributions 
13. simulation_ig_rm_pretest.R runs dgp_script for violating ignorability assumption (ig_dat_function)  by removing pretest (a confounder) to simulate data and runs simulations to estimate SATE

Notes:
1. There is an output folder within the parent directory that contains data from separate simulations scripts that is not included in this repository. Please make output folder or run `dir.create("output")` before running separate simulations scripts


