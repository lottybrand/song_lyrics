# Cultural evolution of emotional expression in 50 years of song lyrics

Scripts and data associated to the preprint: "Cultural evolution of emotional expression in 50 years of song lyrics" by Charlotte Brand, Alberto Acerbi, and Alex Mesoudi

This repository contains the R scripts and the data to reproduce the analysis described in the preprint. 

The main analysis script is analysis_final.R 

Here is the content of the various folders:

### data

The processed datasets used in the analysis:
* billboard_analysis.csv
* mxm_analysis.csv

### Results

A summary of the models' results:
* Results_fin

The plots of the results:
* bb_neg_full.png
* bb_pos_full.png
* mxm_neg_full.png
* mxm_pos_full.png

And the script to generate them:
* predPlots.R

### LIWC

The lists of emotion-words used to process the original data:
* negemo.csv
* posemo.csv

The same lists, but without swear words (as used for analyses in supplementary material):
* negemo_no_swear.csv
* posemo_no_swear.csv

The non-stemmed lists (expansions of the original LIWC 2007, that contains wildcards):
* negemo_non_stemmed.csv
* posemo_non_stemmed.csv

### collaboration_processing

An R script used to disambuigate collaborations in the original data (see the preprint from the details):
* disambiguate_collaborations.R


