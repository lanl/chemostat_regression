# chemostat_regression

Analyze chemostat optical density time series data, specifying parameters to automatically detect dilution cycles over which growth rate regression slopes are calculated.





<br /> <br />

# Table of Contents
1. [About](#About)
2. [Dependencies](#Dependencies)
3. [Downloading chemostat_regression](#Downloading)
4. [Parameters](#Parameters)
5. [Execution](#Execution)
    - [Command line](#Command)
    - [R-Studio](#Rstudio)
    - [R-Shiny](#Rshiny)
6. [Output](#Output)





<br /> <br />

# About <a name="About"></a>

Chemostat growth chambers offer controlled and monitored environments from which scientists may define microbial growth conditions and population dynamics. While chemostat systems offer powerful mechanisms for controlling and monitoring growth conditions, few tools exist for analyzing the resulting growth data in a consistent and automated way. Here we introduce the R software package chemostat_regression and describe a protocol for using it to estimate growth rates within chemostat experiments in an automated and customizable fashion. The software uses cutting edge methods, requires minimal dependencies, and implements many execution approaches, making it highly portable and user friendly for all scientists regardless of their familiarity with bioinformatics.

![screenshot](/img/Method.png)

Circles represent individual chemostat data time points (filled white, by default) plotted in one-dimension (top row) or in two dimensions with optical density (OD) measurements on the y-axis (middle and bottom rows). Pump rate is assigned a binary value of on or off at each time point (on = black). Cycles (red) are identified by “on” pump rate values (black) proceeded by a user-defined buffer period during which additional cycles cannot be identified (dark grey. A range of values (orange) surround cycle time points are the screened for local minimum and maximum OD measurement values (local extrema highlighted with bold outlines). Extrema ranges are then modeled for linear regression to produce slope values for each cycle period (blue).





<br /> <br />

# Dependencies <a name="Dependencies"></a>

The chemostat_regression software has three methods of execution: command-line, R-Studio, and R-Shiny. Due to R package incompatibilities, we recommend creating separate environments for executing the tool on command-line / R-Studio and R-Shiny.

This software requires up-to-date installations of R and R-Studio. The software environment managers Mamba or Anaconda are strongly recommended for downloading R packages and handling environment compatibility issues. Mamba is preferred over Anaconda due to its greatly improved speed.

Software:
- [R](#https://www.r-project.org/)
- [R-Studio](#https://posit.co/download/rstudio-desktop/)

Environment manager:
- [Mamba (Preferred)](#https://www.r-project.org/)
- [Anaconda](#https://www.r-project.org/)



### Environment for command line & R-Studio execution

The following environment is required for execution of chemostat_regression:

```
## Create environment & activate
mamba create -n chemostat_regression_cli
mamba activate chemostat_regression_cli

## Install packages
mamba install r::r-rstudioapi
mamba install bioconda::r-argparse
mamba install conda-forge::r-ggplot2
```



### Environment for R-Shiny

Optionally, an R-Shiny implementation of chemostat_regression may also be accessed and requires a seperate environment:

```
## Create environment & activate
mamba create -n chemostat_regression_gui
mamba activate chemostat_regression_gui

## Install packages
mamba install conda-forge::r-shiny
mamba install conda-forge::r-ggplot2
```





# Downloading chemostat_regression <a name="Downloading"></a>

To download the software, on this github page select “code” then “Download ZIP”. This will open a prompt for specifying where to deposit the software on your local machine.

Once the software has been downloaded, open a new terminal and move to the location where the directory was downloaded.

```
cd path/to/downloaded/chemostat_regression
```

Decompress the software and move into the folder:

```
unzip chemostat_regression-main.zip

cd chemostat_regression-main

```

Finally, ensure that the software and dependencies have been properly installed by ensuring the `--help` command returns parameters:

```
## Activate environment
mamba activate chemostat_regression_cli

## Ensure proper installation. This should return parameter details
Rscript chemostat_regression.R --help

```





<br /> <br />

# Parameters <a name="Parameters"></a>

| Parameter | Default | Description |
| --- | --- | -- |
| --help | n/a	Show this help message and exit |
| --infile | n/a | Input file containing chemostat data. tab- and comma-delimited files accepted. |
| --dir_out | n/a | Output directory |
| --col_time | exp_time | Column in input file containing time series values. |
| --convert_time | ms-h | Convert the units of col_time by specifying the starting units and target units, separated by a dash: [h/m/s/ms]-[h/m/s/ms]. Hours (h); minutes (m); seconds (s); milliseconds (ms). Example: ms-h to convert from milliseconds to hours. |
| --col_od | od_measured | Column in input file containing optical density measurement values. |
| --col_pump_fresh | pump_1_rate | Column in input file containing fresh pump rate values. |
| --col_pump_waste | pump_2_rate | Column in input file containing waste pump rate values. |
| --cycle_buffer | 25 | The number of time points that must pass before the detection of another peak may occur. |
| --extrema_range | 20 | Number of time points +/- cycle index to search for local extrema. |
| --color_lines | black | Hexadecimal or R-accepted color value for OD lines. |
| --color_cycles | orange | Hexadecimal or R-accepted color value for cycle range lines. |
| --color_slope | red | Hexadecimal or R-accepted color value for regression slope lines. |
| --linewidth | 0.25 | Line width for figure lines. |
| --textsize | 8 | Text size for figures, in point (pt) scale. |
| --fig_sub_cycles | 5 | The number of cycles to display per sub-figure. |





<br /> <br />

# Execution <a name="Execution"></a>

### Command Line <a name="Command"></a>

To run chemostat_regression from the command line, first open a new terminal window and move to the directory containing the software executables.

```
cd path/to/chemostat_regression-main/chemostat_regression.R
```

Next, load the software environment and execute, specifying parameters when necessary.

Here is a minimal example:

```
mamba activate chemostat_regression_cli
Rscript chemostat_regression.R --infile “example_input.csv”
–-dir_out “output”

```

<br /> <br />

Direct command line execution can become chaotic when many parameters are specified, and recording specific command executions is challenging. For these reasons, it is recommended that command-line execution of chemostat_regression is performed through a bash script. An example executable bash script “bash_script.sh” may be defined as follows:

```
#!/usr/bin/env bash

Rscript chemostat_regression.R \
--infile “example_input.csv” \
--dir_out “output” \
--cycle_buffer 40 \
--extrema_range 30
--textsize 6
```

The above bash script may then be given executable permissions and ran from the command line.

```
## Enable command-line execution of bash script
chmod +x bash_script.sh

## Execute script
./bash_script.sh

```



<br /> <br />

### R-Studio <a name="Rstudio"></a>

Execution of chemostat_regression is also possible through R-Studio when code/variable interactivity, software modification, or troubleshooting is desired.

In a new terminal, move to the directory containing the software executables, load the command line environment, and open R-Studio.

```
## Move to software folder
cd path/to/chemostat_regression-main/

## Activate environment
mamba activate chemostat_regression_cli

## Open R-Studio
open chemostat_regression.R
```

With the software open in R-Studio, navigate to the function `args_rstudio()`. Here, parameters may be specified as with the command line.


The following code displays the `args_rstudio()` function to be used for parameter specification in R-Studio
```
## RStudio execution
args_rstudio <- function() {
	## Define Arguments
	args <- list(
	infile         = "example_input.csv",
        dir_out        = "output",
	col_time       = "exp_time",
	convert_time   = "s-h",
	col_od         = "od_measured",
	col_pump_fresh = "pump_1_rate",
	col_pump_waste = "pump_2_rate",
	cycle_buffer   = 25,
	extrema_range  = 20,
	color_lines    = "black",
	color_cycles   = "orange",
	color_slope    = "red",
	fig_sub_cycles = 3,
	linewidth      = 0.25,
	textsize       = 12 )

	return(args)
	>}
```

With parameters specified, navigate to the end of the chemostat_regression.R file and execute all code. Further control over running specific software functions may be implemented through the functions identify_run_environment_execute() and chemostat_regression() but is outside the scope of this protocol.



<br /> <br />

### R-Shiny <a name="Rshiny"></a>

A R-shiny based graphical user interface implementation of chemostat_regression exists for non-bioinformaticians and users who want real-time tuning of parameters. This mode is intended for exploring a new data set to determine optimal parameters, which may be transferred for execution on the command line.

Open a new terminal, navigate to the directory containing chemostat_regression executables, load the graphical user interface environment, and run the R-Shiny script.

```
## Move to software folder
cd path/to/chemostat_regression-main/

## Activate environment
mamba activate chemostat_regression_gui

## Execute R-Shiny application
open chemostat_regression_Rshiny.R
```

A new window should now open on the default internet browser displaying the initialized graphical user interface. Because an input data frame has not yet been specified, the plot region will be blank. Parameters are displayed on the left-hand side of the screen and are to be used in the same way as command line and R-Studio implementations. As parameters are input and modified, the plot region will actively update to reflect these changes. Additional parameters “Cycle start” and “Cycle stop” are included for specifying custom cycle range subsets to display.

![screenshot](/img/rshiny_gui.png)

Example visualization of slope calculation results displayed within R-Shiny application of chemostat_regression. Figures are intended for verifying output summary file results. Figure and algorithm parameters are displayed on the lefthand side, while the main plotting region displays optical density measurements (y-axis) over time (x-axis). Overlayed in blue are predicted cycle boundaries, with text describing cycle numbers. For each cycle, red overlays show linear regression growth rate estimations across cycle extrema ranges, with text describing growth rate values.

Users should proceed by modifying parameters until visual inspection of cycle windows, local extrema, and slope ranges are all acceptable (figure 2). Inaccurate slope approximations can be caused by an array of experimental and algorithmic issues that may be addressed by following the steps outlined in the troubleshooting section below. Once desired parameter values have been achieved, navigate to the bottom of the parameter panel titled “Command line execution”. Here, text is generated for running chemostat_regression on the command line that reflects user parameter modifications. Copy the “Command line execution” text and execute it as described in the above section Command-line execution.







<br /> <br />

# Output <a name="Output"></a>


**Overview**

Successful execution of chemostat_regression produces a collection of data tables and figures that describe the slope regression calculation process and summarize results. Output files can be found in the directory specified by the `--output` parameter, with each filename beginning with the name of the input chemostat data frame proceeded by a description of the specific output file.

**Figures**

Output ggplot211 figures are presented in the PDF format to allow scalable quality, with files “*__overview.pdf” displaying optical density over time while “*__annotated.pdf” displays this same information, but overlayed with cycle ranges, local extrema callouts, and slope lines so that users can visually inspect the success of these estimates. Further scrutiny is allowed via the “*__subset_cycles_m-n.pdf” figures, where “m” and “n” describe the cycles that have been subset and displayed to provide higher resolution views of specific ranges, as defined by the parameter `—fig_sub_cycles`.

**Data Tables**

Two data tables are returned: “*__annotated.csv/tsv” describes the input chemostat data table with chemostat_regression annotations describing the various metrics analyzed to identify cycles and local extrema, and “*__extrema.csv/tsv” summarizing regression calculations and associated statistics.



<br /> <br />

Output columns present in `*__annotated.csv/tsv`

| Column | Description |
| --- | --- |
| col_time_convert | The experiment time, converted to user-specified units. |
| col_time_convert_units | Time unit conversion implemented. |
| cycles | Delimitations of subsequent chemostat dilution cycles. |
| extrema | Relative minima and maxima identified within cycle ranges. |
| cycle_n | Defines which cycle a given extrema optical density measurement corresponds to. |



<br /> <br />

Output columns present in `*__extrema.csv/tsv`

| Column | Description |
| --- | --- |
| cycle_n | The identified dilution cycle number. |
| row_min | The first time point row number within the cycle. |
| row_max | The last time point row number within the cycle. |
| delta_row | The total number of elapsed time point rows. |
| time_min | The minimum time value. |
| time_max | The maximum time value. |
| od_min | The minimum optical density value. |
| od_max | The maximum optical density value. |
| delta_od | The change in optical density between extrema values. |
| Slope | The calculated linear regression slope between cycle extrema. |
| slope_units | The units for `Slope` value. |
| r2 | Coefficient of determination for calculated slope model. |
| vint | The vertical intercept for calculated slope model. |
| pval | The p-value for calculated slope model. |
| stderr | The standard error for calculated slope model. |




