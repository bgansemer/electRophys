# R-DSE 

This repo contains R scripts for analyzing depolarization-induced presynaptic inhibition and generating plots of electrophysiology trace data.

## Description

This project uses multiple to scripts to read, process, and visualize electrophysiology data. The current version is designed for processing depolarization-induced suppression of excitation (DSE) or depolarization-induced suppression of inhibition (DSI) data. Input data is in the form of .atf files from clampfit (please see `example-data/DSE/` for data format/layout). Additional description on using this functionality will be added in the future. An early version of visualizing example traces/recordings is available using `plotTraces.R`. 

Future plans for development include functionality for single runs of many sweeps, analysis and visualization of spontaneous current frequency and amplitude data, developing the project into an `R` package, and development of a Shiny app.

See below for a list of scripts and short descriptions for each and a description of how to use different functionalities of the project (this description coming soon). 

### Scripts

See below for a general description of the use of each script. Further details on usage, parameters, and output are provided in each individual script.

`renderRmd.R`: This script is used to render Rmd files to save reports of the data. This is the only script that currently needs to be called, as the Rmd file contains all other script calls to process and visualize data.

`dseR.R`: This script reads in, processes, and wrangles raw peak data into organized formats for plotting/visualization and writing out excel files for downstream analysis in other software if desired.

`plotDSE.R`: This script uses `ggplot2` to generate plots of DSE/DSI data.

`plotBasePeak.R`: This script uses `ggplot2` to generate plots of holding current as check for recording quality and cell rundown.

`plotTraces.R`: Early developlment version. This script will be used to plot example recordings/traces. It is currently a standalone script and is not being using in the pipeline.

`theme_BG.R`: This sets a new ggplot theme for use by other plotting functions.

### DSE/DSI data processing and visualization

Description coming soon...
