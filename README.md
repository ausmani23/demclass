# Democracy and the Class Struggle

This repository contains replication materials for the paper "Democracy and the Class Struggle", forthcoming at the *American Journal of Sociology*.

## What do you need? 

To replicate my work, you will need: 

1. R, a popular and free statistical software. I wrote these files in R 3.4.1, but it should be forwards-compatible (i.e., you should be able to run these if you download the current version). John Fox offers a [useful intro](https://socialsciences.mcmaster.ca/jfox/Courses/soc740/R-install-instructions.html), if you don't have prior experience. 
2. Several additional packages, which don't come installed with R. If you run my scripts without installing these, R will throw an error noting that you haven't yet installed a required package. To make life easier, I have included a script 'installpacks.R'. This generates a list of all the packages I run and, if you un-comment out the last line, installs them. 
3. Ghostscript, which I use for fancy fonts. For detailed instructions on how to install Ghostscript, see [this blogpost](http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html). Once you do this, open the file 'dirs.R' and make sure that 'gsdir_full' points to the directory that contains the Ghostscript executable. If you install GS in your 'Program Files' directory, you shouldn't need to change anything. (This applies to Windows computers.)

## What is in here?

+ code: Contains all the R scripts that read data, run the models and generate output
+ data: Contains the starting dataset and country code information
+ meta: Contains .csv's that some of the scripts consult, when running
+ files: Stores files created by the scripts, as they run
+ output: Stores output created by the scripts

## How to use it? 

To run this code successfully, please follow the instructions below:

1. Before you run the scripts, load the demclass.Rproj file. This ensures that the default working directory is the "code" subfolder. Once that is set, everything should run smoothly. (Alternatively, you can set the working directory to the "code" subfolder yourself). 
2. Run the code sequentially (in order of filename). If you run a later file without having previously run an earlier file, the script will probably throw an error.  
3.  Figures are output as .pdf's and tables as .csv's, but if you'd like a standalone .tex files (or .tex) fragments with the explanatory foototes, run 99_figures.R and 99_tables.R. 99_figures.R will also create a folder called 'upload' in the 'output' folder, to which it will copy all figures that I use in the paper and supplementary appendix

Below I describe briefly describe what each file does, and which of the main figures and tables it generates. Note that I don't explain which files must be run to generate the material in the supplementary appendix. This can be done by running all scripts sequentially (or by running '00_runall.R', which will run everything from start to finish).

+ !!\_quickreplicate.R: This runs the democratization models I focus on in the paper in much simpler code. If you don't want to reproduce all my output but just want to reproduce the main regression results and/or play around with the dataset, focus on this file. 
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
+ 99_figures.R: Creates .tex files and fragments for figures, with footnotes. 
+ 99_tables.R: Creates .tex files and fragments for tables, with footnotes

NB: Scripts that are not numbered contain functions or other code that is called by one or more of these scripts.

## Questions and Comments? 

If you encounter any issues with this code or with reproducing my results, please let me know at adaner.usmani[at]gmail.com. 

