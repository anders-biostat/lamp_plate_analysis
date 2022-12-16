# LAMP Results Viewer

This script opens in a default web browser an interactive interface to explore the results of multiple LAMP tests to introduce manual adjustments to the automatic classification or add comments, if necessary, and to push results to the server database or to store them locally.

The script was designed to fit the specific workflow and experimental design described in *link to the paper here*, and contains large amount of hard-coded elements. One can use it either as an inspiration to develop a similar interface for another study, or in the exactly same settings. Example data are provided in the `data` directory.

## Installation

The viewer itself does not require installation. The following files has to be copied locally:

- `lamp_result_viewer.R` - the main R script that processes input files and starts the app in the default browser.

- `plateBrowser.html` - contains layout and JavaScript functionality for the script.

In addition, to run the script the following software has to be installed:

- An up-to-date web browser.

- [R](https://www.r-project.org/) (>= 4.0.0)

* several R packages: `hwriter`, `readxl`, `later`, `httr`, `tidyverse`, `rlc`. All these packages are available from CRAN and can be installed by the `install.packages` function from an R session.
```
install.packages(c("hwriter", "readxl", "later", "httr", "tidyverse", "rlc"))
```

## Run

To run the script the following command is used:

```
Rscript lamp_result_viewer.R path/to/the/tecan/output/file.xlsx > log.Rout
```

To try our demo data, please, load and unpack the archive from the `data` directory and use `VT-0016/VT-0016-0019_LAMP.xlsx` file as an input.

On a Windows machine, one can also use the `run_lamp_viewer.bat` file to drag-and-drop the input Excel document to start the app.

### Input files

The viewer requires at least two input files:

- Output from a Tecan plate reader in `xlsx` format with a sheet for each time point measurements from all the wells. One file can contain results from multiple runs. Another sheet named "PrimerSetsUsed" is also required. This sheet must have three columns:

	* `Plate-ID` - ID of the 384-well plate
	* `PrimerSet` - name of the primers that where used for the given well. Some of the primers are considered as control, which influences the automatic classification process. Current controls are `"ACTB"`, `"Actin"` and `"Zika"`. One can edit a list of controls by redefining the `controls` variable in `lamp_result_viewer.R`.
	* `A1 position of 96-well in 384-well` - one of the wells of the 384-well plate to which the content of well "A1" of the 96-well plate was transferred. The current version of the script assumes that these wells are always "A1", "A2", "B1" and "B2".

- List of all wells of the tested 96-well plates and their content in `xlsx` format. The file should contain five columns in its first sheet:

	* `Tube Position` - position on the 96-well plate. 
	* `Rack ID` - ID of the tested plate. Must match the `Barcode` field in the Tecan output file.
	* `Tube ID` - an ID that is used to access a specific sample. For non-sample well can be empty.
	* `Type` - type of the well content. The results only from the wells of type `sample` will be pushed to the server. Other possible options are `"positive control CT32"`, `"positive control CT31"`, `"positive control CT29"`, `"positive control CT28"`, `"positive control CT26"`, `"negative control"`, `"empty"`,. To add other sample types or change the assigned colours, please, edit the `palette` variable in `lamp_result_viewer.R`.
	* `Comment` - optional comment about the sample and its quality. All samples with non-empty comments will be automatically classified as `repeat`, and thus require attention from the experimenter.

The scripts expects a specific file tree to collect all the data. Each plate should have its own directory named by its ID. In each of the directories there should be a barcode file named `plateID_barcodes.xlsx`. The Tecan output file can have any name, but must be located in one of the plates' directories. This directory will be treated as the main one and will be used to store the assigned test results.
Check the `data` directory of this repository for an example.