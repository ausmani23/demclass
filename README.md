# Democracy and the Class Struggle

## Repo Description

This repository contains replication materials for the paper 'Democracy and the Class Struggle', forthcoming at the American Journal of Sociology

To run this code successfully, please follow the instructions below:

1. You will have to run the code sequentially (in order of filename). If you run a later file without having previously run an earlier file, the script will probably throw an error.  
2. Wherever you see the script change the directory to 'codedir', you will need to change this to the working directory in which you have downloaded all the code. I apologize for this. I am now aware of ways to use .RProj files to avoid this, but I was not when I wrote this code. 
3. In addition to needing all packages that the scripts load, to output the same figures that I output you'll need to make sure Ghostscript is installed on your computer, and that 'gsdir' in 'dirs.R' points to the directory containing the Ghostscript executable. For detailed instructions on how to install Ghostscript, see [this blogpost](http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html)
4. To run these scripts sucessfully, you'll need to replicate the same folder structure as you see in this repo: a 'code' directory contains the scripts, a 'files' directory contains the dataset and some other files that they need (and stores output for use in further scripts), a 'meta' directory contains .csv's that give metadata or other info to the scripts, and an 'output' folder stores the Figures and Tables that the scripts produce. 
4.  Figures are output as .pdf's and tables as .csv's, but if you'd like a standalone .tex files (or .tex) fragments with the explanatory foototes, run 99_figures.R and 99_tables.R. 99_figures.R requires that there be a folder called 'upload' in the 'output' folder, to which it will copy all figures that I use in the paper and supplementary appendix

Below I describe briefly describe what each file does, and which figures and tables it generates. Note that I don't explain which files must be run to generate the material in the supplementary appendix. This can be done by running all scripts sequentially (or by running '00_runall.R'), which will run everything from start to finish. 

## Files Description

+ 10_mainmods.R: Runs the democratization models reported in the main paper. 
+ 11_mainmods_graphs.R: Generates Figure 2 
+ 12_mainmods_tables.R: Generates Tables 3 and 4
+ 14_robmods_main.R: Runs some robustness checks
+ 15EX_robmods_lagdv.R: Runs simulation showing importance of including a lagged dependent variable
+ 16_robmods_bin.R: Runs further robustness checks (modeling transitions/consolidation rather than democratization)
+ 18_robmods_output.R: Outputs results from robustness checks
+ 20_dcapmods.R: Runs the unionization and strike models
+ 21_dcapmods_output.R: Generates Figure 1
+ 30_predictions.R: Runs a variety of counterfactuals 
+ 31_predictions_stats.R: Computes summary statistics across counterfatuals
+ 32_preditions_output.R: Generates Figures 3 and 4
+ 40_modelfit.R: Re-runs models on a consistent sample and generates sequential predictions
+ 41_modelfit_stats.R: Computes fit statistics 
+ 42_modelfit_output.R: Outputs results from model fit exercise
+ 50_descriptive_output.R: Generates Tables 1 and 2
+ 51_descriptive_graphs.R: Generates descriptive figures
+ 60_unitroots.R: Examines key series for unit root
+ 61_unitroots_output.R: Outputs results from unit root tests
+ 99_figures.R: Creates .tex files and fragments for figures, with footnotes 
+ 99_tables.R: Creates .tex files and fragments for tables, with footnotes

NB: Scripts that are not numbered contain functions or other code that is called by one or more of these scripts.

## Questions and Comments? 

If you encounter any issues with this code or with reproducing my results, please let me know at adaner.usmani[at]gmail.com. 

