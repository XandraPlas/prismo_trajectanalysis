# Readme <a href='https://osf.io/zcvbs/'><img src='worcs_icon.png' align="right" height="139" /></a>

<!-- Please add a brief introduction to explain what the project is about    -->

## Where do I start?

You can load this project in RStudio by opening the file called 'prismo_trajectanalysis.Rproj'.

## Project structure

<!--  You can add rows to this table, using "|" to separate columns.         -->
File                         | Description                                      | Usage         
---------------------------- | ------------------------------------------------ | --------------
README.md                    | Description of project                           | Human editable
prismo_trajectanalysis.Rproj | Project file                                     | Loads project 
LICENSE                      | User permissions                                 | Read only     
.worcs                       | WORCS metadata YAML                              | Read only     
data_preparation.R           | Script to process raw data                       | Human editable
descriptives_demographics.R  | Script to assess demographics                    | Human editable
fig_trajectories.R           | Script to customize figure                       | Human editable
group_comparison.R           | Script to compare trajectories                   | Human editable
measurement_invariance.R     | Script to analyze measurement invariance         | Human editable
TrajAna_dataPrep_funcs.R     | Script with functions used for data preparation  | Human editable
manuscript.rmd               | Source code for paper                            | Human editable
references.bib               | BibTex references for manuscript                 | Human editable
renv.lock                    | Reproducible R environment                       | Read only 



<!--  You can consider adding the following to this file:                    -->
<!--  * A citation reference for your project                                -->
<!--  * Contact information for questions/comments                           -->
<!--  * How people can offer to contribute to the project                    -->
<!--  * A contributor code of conduct, https://www.contributor-covenant.org/ -->

## Access to data

Some of the data used in this project are not publically available.
Synthetic data with similar characteristics to the original data have been provided. Using the function load_data() will load these synthetic data when the original data are unavailable. Note that these synthetic data cannot be used to reproduce the original results. However, it does allow users to run the code and, optionally, generate valid code that can be evaluated using the original data by the project authors.

To request access to the original data, 

<!--Clarify here how users should contact you to gain access to the data, or to submit syntax for evaluation on the original data.-->





