# NeuroShiny App

### Overview

The NeuroShiny App facilitates the process of running, saving, and plotting neural decoding analyses without prior coding experience using an interactive user interface. The structure of the analysis follows steps outlined in the NeuroDecodeR package (built and developed by Ethan Meyers). To learn more about NeuroDecodeR, visit the [GitHub repository](https://github.com/emeyers/NeuroDecodeR#overview) or [development page](https://emeyers.github.io/NeuroDecodeR/index.html). 

Processes from NeuroDecoder that are implemented in the application include: data binning, neural decoding of `ds_basic` and `ds_generalization` datasources, function parameter alterations, plotting of results, and R script/markdown generation and compiling. 


### Installation 
The current development version of NeuroShiny is only deployable locally and must be forked in order to use. We hope to make the application available through a web browser in the future.

### Folder Structure
The application assumes a standardized folder structure for every analysis. A project can be contained in a folder stored anywhere on a computer and with any desired name. Inside this folder there should be two sub-folders: results and data. The data folder must be split further into two more folders: binned and raster. The binned folder holds all binned .Rda data files and the raster folder holds additional folders for .Rda files of spike data. Finally, the results folder contains a decoding_results sub-folder where future scripts and results are saved depending on what is specified in the application. 
