#!/usr/bin/env Rscript

# ______________________________ ------------------------------------------
# Parameters --------------------------------------------------------------

## RStudio execution
args_rstudio <- function() {
  "
  ______________________________________________________________________________
  Description
    - Define parameters here if running script directly from RStudio.
  "
  
  ## Define Arguments
  args <- list(
    infile         = "example_data/ancestor_dither.csv",
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
  
  ## Export list of arguments, for use when running script from RStudio
  return(args)
}

## Command line execution
args_rscript <- function() {
  "
  ____________________________________________________________________________
  Description
    - Define and retrieve command-line input values
  "
  
  ## Packages
  suppressWarnings( library(argparse) )
  
  ## Define argument parser
  parser <- ArgumentParser(description='CLI input parser for the chemostat_regression program.')
  
  ## Define Arguments
  
  parser$add_argument(
    "--infile",
    help = "Input file containing chemostat data. tab- and comma- delimited files accepted.",
    required = T )
  
  parser$add_argument(
    "--dir_out",
    help = "Output directory.",
    required = T )
  
  parser$add_argument(
    "--col_time",
    help = "Column in input file containing time series values.",
    required = T, default = "exp_time" )
  
  parser$add_argument(
    "--convert_time",
    help = "Convert the units of col_time by specifying the starting units and target units, seperated by a dash: [h/m/s/ms]-[h/m/s/ms]. Hours (h); minutes (m); seconds (s); milliseconds (ms). Example: ms-h to convert from milliseconds to hours.",
    required = F, default = 'ms-h' )
  
  parser$add_argument(
    "--col_od",
    help = "Column in input file containing optical density measurement values.",
    required = T, default = "od_measured" )
  
  parser$add_argument(
    "--col_pump_fresh",
    help = "Column in input file containing fresh pump rate values.",
    required = T, default = "pump_1_rate" )
  
  parser$add_argument(
    "--col_pump_waste",
    help = "Column in input file containing waste pump rate values.",
    required = T, default = "pump_2_rate" )
  
  parser$add_argument(
    "--cycle_buffer",
    help = "The number of time points that must pass before the detection of another peak may occur.",
    required = T, default = 25 )
  
  parser$add_argument(
    "--extrema_range",
    help = "Number of time points +/- cycle index to search for local extrema.",
    required = T, default = 20 )
  
  parser$add_argument(
    "--color_lines",
    help = "Hexadecimal or R-accepted color value for OD lines.",
    required = F, default = "black" )
  
  parser$add_argument(
    "--color_cycles",
    help = "Hexadecimal or R-accepted color value for cycle range deliminations.",
    required = F, default = "orange" )
  
  parser$add_argument(
    "--color_slope",
    help = "Hexadecimal or R-accepted color value for regression slope lines.",
    required = F, default = "red" )
  
  parser$add_argument(
    "--linewidth",
    help = "Line width for figure lines.",
    required = F, default = 0.25 )
  
  parser$add_argument(
    "--textsize",
    help = "Text size for figures.",
    required = F, default = 8 )
  
  parser$add_argument(
    "--fig_sub_cycles",
    help = "The number of cycles to display per sub-figure.",
    required = F, default = 5 )
  
  ## Load parser and format arguments
  args <- parser$parse_args()
  args["cycle_buffer"  ] <- as.numeric(args["cycle_buffer"  ])
  args["extrema_range" ] <- as.numeric(args["extrema_range" ])
  args["fig_sub_cycles"] <- as.numeric(args["fig_sub_cycles"])
  args["linewidth"     ] <- as.numeric(args["linewidth"     ])
  args["textsize"      ] <- as.numeric(args["textsize"      ])
  
  return(args)
}





# ______________________________ ------------------------------------------
# chemostat_regression ----------------------------------------------------

identify_and_set_wd_either <- function(n_back=0) {
  "
  ____________________________________________________________________________
  Description
  - Identify and set the working directory when running code from command line
  
  ____________________________________________________________________________
  Arguments
  - nback  The number of directories to step back into when setting the working directory
  "
  
  ## Scenario: Running file from RStudio
  if ( interactive() == T ) {
    path_current <- strsplit(dirname(rstudioapi::getSourceEditorContext()$path), "/")[[1]] ## Identify the directory of the current R-Studio script
    path_base    <- Reduce(file.path, path_current[1:length(path_current)-n_back])       ## Define the directory to be set
    setwd(path_base)                                                                       ## Set the working directory
  }
  
  ## Scenario: Running file from the command line
  if ( interactive() == F ) {
    initial.options <- commandArgs(trailingOnly=FALSE)
    file.arg.name <- "--file="
    path_current <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)]) ## Identify the directory of the current R-Studio script
    path_base    <- Reduce(file.path, path_current[1:length(path_current)-n_back])       ## Define the directory to be set
    setwd(system("pwd", intern=T) )
  }

  return(path_base)
}

parse_filename_base <- function(infile) {
  "
    ____________________________________________________________________________
    Description
    - Identify input file name (without path & extension) for output file naming usage.
    
    ____________________________________________________________________________
    Arguments
    - infile    Input file. comma-seperated (csv) and tasb-seperated (tsv) accepted
    "
  filename <- gsub(x=basename(infile), pattern="[.].+", replacement="")
  message(paste0("- ", "Identifying local extrema in file: ", basename(infile)))
  return(filename)
}

load_chemostat_data <- function(infile) {
  "
  ____________________________________________________________________________
  Description
  - Load input chemostat data
  
  ____________________________________________________________________________
  Arguments
  - infile  Input file. comma-seperated (csv) and tasb-seperated (tsv) accepted
  "
  if (grepl(x=basename(infile), pattern=".csv")) { df <- read.delim(file=infile, sep=",",  header=T) } ## Scenario: tab-delimited infile
  if (grepl(x=basename(infile), pattern=".tsv")) { df <- read.delim(file=infile, sep="\t", header=T) } ## Scenario: comma-delimited infile
  return(df)
}

identify_min_time <- function(x_chemostat, col_time) {
  "
  ____________________________________________________________________________
  Description
  - Identify minimum time value present in input data file.
  
  ____________________________________________________________________________
  Arguments
  - x_chemostat   Data frame containing chemostat information
  - col_time      x_chemostat column containing data sample times
  "
  return( min(x_chemostat[,col_time]) )
}

plot_od <- function(x_chemostat, col_time, col_od, color_lines, convert_time, textsize, linewidth) {
  "
  ____________________________________________________________________________
  Description
  - Visualize extrema annotation results for user verification of output.
  
  ____________________________________________________________________________
  Arguments
  - x_chemostat     Data frame containing chemostat information
  - col_time        x_chemostat column containing data sample times
  - col_od          x_chemostat column containing od measures
  - color_lines     Hexadecimal or R-accepted color value for OD lines
  - convert_time    Convert the units of col_time by specifying the starting units and target units, seperated by a dash
  - textsize        Text size for figures
  - linewidth       Line width for figure lines
  "
  
  ## Packages
  library(ggplot2)
  
  time_unit <- gsub(x=convert_time, pattern=".+[-]", "")
  
  ## Generate figure & export
  plot <- ggplot()+
    ## [ Line ] OD measurement over time
    geom_line( data=x_chemostat, aes_string(x=col_time, y=col_od), alpha=.85, color=color_lines, linewidth=linewidth)+
    ## [ Format ]
    labs(x=paste0("Time (", time_unit, ")"), y="OD")+
    theme_bw()+
    theme(
      panel.grid.minor = element_blank(),
      text = element_text(color="black", size=textsize),
      axis.text = element_text(color="black", size=textsize))
  
  return(plot)
}

convert_col_time <- function(x_chemostat, col_time, convert_time) {
  "
  ____________________________________________________________________________
  Description
  - Convert time column from existing units to user-defined target units.
  
  ____________________________________________________________________________
  Arguments
  - x_chemostat     Data frame containing chemostat information
  - col_time        x_chemostat column containing data sample times
  - convert_time    Convert the units of col_time by specifying the starting units and target units, seperated by a dash
  "
  
  ## Globals
  unit_conversion <- data.frame(
    row.names     = "unit"                        ,
    unit          = c("ms",  "s",   "m",   "h")   ,
    conversion_ms = c( 60^0,  60^1,  60^2,  60^3) )
  
  ## Parse unit strings
  unit_start <- strsplit(x=convert_time, split="-")[[1]][1]
  unit_final <- strsplit(x=convert_time, split="-")[[1]][2]
  
  ## Convert unit strings to ms values
  conv_start <- unit_conversion[unit_start,]
  conv_final <- unit_conversion[unit_final,]
  
  ratio = conv_start / conv_final
  
  x_chemostat[,"col_time_convert"] <- x_chemostat[,col_time] * ratio
  
  x_chemostat[,"col_time_convert_units"] <- gsub(x=convert_time, pattern="-", replacement=" -> ")
  
  return(x_chemostat)
  
}

annotate_cycle_vertices  <- function(x, col_pump_fresh, col_pump_waste, cycle_buffer) {
  "
  ____________________________________________________________________________
  Description
  - Iterate through chemostat df and identify rows where fresh media pump is activated. When multiple nearby rows are identified, the first instance is captured.
  
  ____________________________________________________________________________
  Arguments
  - x                 Data frame containing chemostat information
  - col_pump_fresh    x column holding fresh media pump rate data
  - col_pump_waste    x column holding waste media pump rate data
  - cycle_buffer      The number of timepoints that must elapse before the next cycle may be identified
  "

  ## Initialize cycles column
  x[,"cycles"] <- F
  
  ## Initialize buffer counter
  counter <- 0
  
  ## Iterate through chemostat df
  for (row in 1:nrow(x)) {
    ## Identify rows containing non-zero pump values
    if( x[row,col_pump_fresh]!=0 | x[row,col_pump_waste]!=0 ) {
      ## Ensure that a nearby previous time point was not flagged as a cycle start
      if (counter<1) {
        ## Define time point as a cycle start
        x[row,"cycles"] <- T
        ## Reset cycle buffer counter to avoid next n time points as cycle starts
        counter <- cycle_buffer
      }
    }
    ## Reduce cycle buffer countdown
    counter <- counter - 1
  }
  ## Export chemostat data frame, annotated with cycle starts
  return(x)
}

identify_frst_cycle_strt <- function(x, col_od, col_cycles) {
  "
  ____________________________________________________________________________
  Description
  - Sometimes, the first few chemostat od measurements can be off. To account for this, the first 10% of od measurements in the first cycle are scanned to find the actual od minimum, which we call the starting point.
  
  ____________________________________________________________________________
  Arguments
  - x                 Data frame containing chemostat information
  - col_od            x column containing od measurement data
  - col_cycles        x column containing cycle annotations (TRUE = cycle start)
  "
  ## Identify indices of cycle indices
  cycle_indices <- which(x[,col_cycles]==T)
  
  ## Identify number of data points in first cycle
  cycle_1_strt <- cycle_indices[1]
  cycle_1_stop <- cycle_indices[2]
  
  ## Determine what 10% of range is
  explore_percent <- ceiling( ( cycle_1_stop - cycle_1_strt + 1 )  / 10 )
  
  ## Revise first cycle start to reflect predicted start od
  cycle_indices[1] <- which.min( x[cycle_1_strt:(cycle_1_strt+explore_percent), col_od] )
  
  ## Update df cycle indices & export
  x[              ,col_cycles] <- F ## Reset cycle indices
  x[cycle_indices ,col_cycles] <- T ## Define cycle indices w/ revised start
  
  message(paste0("- ", "First cycle initial timepoint index adjusted from 1 to ", cycle_indices[1]))
  
  return(x)
}

identify_last_cycle_stop <- function(x, col_od, col_cycles) {
  "
  ____________________________________________________________________________
  Description
  - Because pump flow rate is used to define pump cycles, the last cycle end will not be captured as the pump never turns on again. This function captures the final max value to be used as the final cycle stop od. 
  
  ____________________________________________________________________________
  Arguments
  - x              Data frame containing chemostat information
  - col_od         x column containing od measurement data
  - col_cycles     x column containing cycle annotations (TRUE = cycle start)
  "
  
  ## Identify indices of cycle indices
  cycle_indices <- which(x[,col_cycles]==T)
  
  ## Identify how many data points come after the final index
  end_range <- max(cycle_indices) : nrow(x)
  message(paste0("- ", "Number of timepoints measured after final fresh media pump in: ", length(end_range)))
  
  ## Determine what 10% of range is
  explore_percent   <- ceiling( length(end_range) / 10 )
  end_range_percent <- end_range[ (length(end_range)-explore_percent) : length(end_range) ]
  
  index_final <- end_range_percent[ which.max(x[end_range_percent,col_od]) ]
  
  x[index_final,col_cycles] <- TRUE

  message(paste0("- ", "Last cycle final max od timepoint index identified as: ", index_final, " out of ", nrow(x), " total time points"))
  
  return(x)
}

annotate_cycle_extrema <- function(x, col_od, col_cycles="cycles", extrema_range) {
  "
  ____________________________________________________________________________
  Description
  - Using cleaned cycle annotations, iterate through cycles and find/label extrema.
  
  ____________________________________________________________________________
  Arguments
  - x               Data frame containing chemostat information
  - col_od          x column containing od measurement data
  - col_cycles      x column containing cycle annotations (TRUE = cycle start)
  - extrema_range   Number of time points +/- cycle index to search for local extrema.
  "
  
  ## Define columns for storing extrema value, cycle counter
  x[,"extrema"] <- NA
  x[,"cycle_n"] <- NA
  
  ## Collect data from chemostat df
  cycle_indices <- which(x[,col_cycles]==T) ## Cycle indices
  n_data        <- nrow(x)                  ## Number of data points
  
  ## Report number of cycles identified
  n_cycles <- length(cycle_indices) - 1
  message(paste0("- ", "Number of chemostat cycles identified: ", n_cycles))
  
  ## Initialize cycle counter
  n <- 1
  
  ## Initialize last-minimum tracker
  last_min <- 0
  
  ## Iterate through cycles and annotate cycle number, extrema (min and max)
  for (cycle in cycle_indices) {
    
    ## Define extrema search range
    range <- seq(cycle-extrema_range, cycle+extrema_range)
    
    ## Remove range values outside of data indices
    range <- range[range>last_min & range<=n_data]
    min <- range[which.min( x[range,col_od] )]
    max <- range[which.max( x[range,col_od] )]
    last_min <- min
    
    ## Add extrema and cycle number to chemostat data frame
    
    
    ## Account for first / last cycles
    if      (cycle == min(cycle_indices)) {
      x[min,c("extrema", "cycle_n")] <- c("minimum", 1)
      }
    else if (cycle == max(cycle_indices)) {
      x[max,c("extrema", "cycle_n")] <- c("maximum", n)
      }
    else {
      x[min,c("extrema", "cycle_n")] <- c("minimum", n+1)
      x[max,c("extrema", "cycle_n")] <- c("maximum", n  )
      ## Process cycle number
      n <- n + 1
    }
  }
  return(x)
}

calculate_extrema_slopes_lm <- function(x, col_extrema, col_cycles, col_time, col_od, convert_time) {
  "
  ____________________________________________________________________________
  Description
  - Use linear regression approach to estimate slope across cycle data points ranging between extrema values (inclusive).
  
  ____________________________________________________________________________
  Arguments
  - x_chemostat     Data frame containing chemostat information
  - col_extrema     Data frame produced by annotate_cycle_extrema() summarizing extrema annotations w/ slopes
  - col_cycles     x column containing cycle annotations (TRUE = cycle start)
  - col_time        x_chemostat column containing data sample times
  - col_od          x_chemostat column containing od measures
  - convert_time    Convert the units of col_time by specifying the starting units and target units, seperated by a dash
  "
  ## Subset to extrema rows
  x_ext <- x[which(!is.na(x[,col_extrema])),]
  
  ## Initialize output df
  cols_out <- c("cycle_n", "row_min", "row_max", "delta_row", "time_min", "time_max", "delta_time", "od_min", "od_max", "delta_od", "r2", "yint", "pval", "stderr")
  df_out <- data.frame(matrix(ncol=length(cols_out), nrow=0))
  colnames(df_out) <- cols_out
  
  for (cycle in unique(x_ext[,col_cycles])) {
    
    ## Identify time points defining start and stop of current cycle
    range_min <- x_ext[which(x_ext[,col_cycles]==cycle & x_ext[,col_extrema]=="minimum"), col_time]
    range_max <- x_ext[which(x_ext[,col_cycles]==cycle & x_ext[,col_extrema]=="maximum"), col_time]
    
    ## Subset chemostat data to time points within cycle range
    x_cycle <- x[which(x[,col_time]>=range_min & x[,col_time]<=range_max),]
    
    ## Define number of rows in 
    row_min <- min(as.numeric(row.names(x_cycle)))
    row_max <- max(as.numeric(row.names(x_cycle)))
    
    ## Define linear model, calculate regression, summarize
    model_formula <- as.formula(paste0(col_od, "~", col_time))
    model_fit     <- lm(data=x_cycle, formula=model_formula)
    model_summary <- summary(model_fit)
    
    ## Extract model statistics
    r2     <- model_summary[["adj.r.squared"]]
    slope  <- coef(model_summary)[col_time     , "Estimate"  ]
    yint   <- coef(model_summary)["(Intercept)", "Estimate"  ]
    pval   <- coef(model_summary)[col_time     , "Pr(>|t|)"  ]
    stderr <- coef(model_summary)[col_time     , "Std. Error"]
    
    ## Define hypothetical OD (y-values) based on lm() slope & y-intercept
    od_min <- yint + range_min*slope
    od_max <- yint + range_max*slope
    
    ## Define slope units
    time_unit <- gsub(x=convert_time, pattern=".+[-]", "")
    slope_units <- paste0("OD", " / ", time_unit)
    
    ## Add data to summary
    newrow <- nrow(df_out) + 1
    df_out[newrow,"cycle_n"    ] <- as.numeric(cycle)
    df_out[newrow,"row_min"    ] <- row_min
    df_out[newrow,"row_max"    ] <- row_max
    df_out[newrow,"delta_row"  ] <- row_max - row_min + 1
    df_out[newrow,"time_min"   ] <- range_min
    df_out[newrow,"time_max"   ] <- range_max
    df_out[newrow,"delta_time" ] <- range_max - range_min
    df_out[newrow,"od_min"     ] <- od_min
    df_out[newrow,"od_max"     ] <- od_max
    df_out[newrow,"delta_od"   ] <- od_max - od_min
    df_out[newrow,"slope"      ] <- slope
    df_out[newrow,"slope_units"] <- slope_units
    df_out[newrow,"r2"         ] <- r2
    df_out[newrow,"yint"       ] <- yint
    df_out[newrow,"pval"       ] <- pval
    df_out[newrow,"stderr"     ] <- stderr
  }
  ## Format and export
  return(df_out)
}

plot_extrema <- function(x_chemostat, x_extrema, col_time, col_od, col_cycle_n, color_lines, color_cycle, color_slope, yax_min=NA, yax_max=NA, time_min, convert_time, linewidth, textsize) {
  "
  ____________________________________________________________________________
  Description
  - Visualize extrema annotation results for user verification of output.
  
  ____________________________________________________________________________
  Arguments
  - x_chemostat     Data frame containing chemostat information
  - x_extrema       Data frame produced by annotate_cycle_extrema() summarizing extrema annotations w/ slopes
  - col_time        x_chemostat column containing data sample times
  - col_od          x_chemostat column containing od measures
  - col_cycle_n     x_chemostat column defining cycle group
  - color_lines     Hexadecimal or R-accepted color value for OD lines
  - color_cycle     Hexadecimal or R-accepted color value for cycle deliminations
  - color_slope     Hexadecimal or R-accepted color value for lm slope annotations
  - yax_min         Y-axis minimum
  - yax_max         Y-axis maximum
  - time_min        Minimum time value in x_chemostat
  - convert_time    Convert the units of col_time by specifying the starting units and target units, seperated by a dash
  - linewidth       Line width for figure lines
  - textsize        Text size for figures
  "
  
  ## Packages
  library(ggplot2)
  
  ## Define x axis labels
  xax_brek <- c(x_extrema[,"time_min"], max(x_chemostat[,col_time]))
  xax_labs <- round( xax_brek - time_min, 2 )
  
  time_unit <- gsub(x=convert_time, pattern=".+[-]", "")
  
  ## Define y axis limits
  if ( NA %in% c(yax_min, yax_max) ) {
    time_strt <- head(x_chemostat[which(x_chemostat["extrema"]=="minimum"),col_time], n=1) ## First cycle start time point
    time_stop <- tail(x_chemostat[which(x_chemostat["extrema"]=="maximum"),col_time], n=1) ## Last cycle stop time point
    yax_min <- min( x_chemostat[which(x_chemostat[col_time]>=time_strt & x_chemostat[col_time]<=time_stop),col_od] )
    yax_max <- max( x_chemostat[which(x_chemostat[col_time]>=time_strt & x_chemostat[col_time]<=time_stop),col_od] )
  }
  
  ## Create long version of extrema table
  extrema_min <- x_extrema[,c(col_cycle_n, "time_min", "od_min")]
  extrema_max <- x_extrema[,c(col_cycle_n, "time_max", "od_max")]
  colnames(extrema_min) <- c(col_cycle_n, "time", 'od')
  colnames(extrema_max) <- c(col_cycle_n, "time", 'od')
  x_extrema_l <- rbind(extrema_min, extrema_max)
  
  ## Generate figure & export
  plot <- ggplot()+
    ## [ V-Line ] defining cycles
    geom_vline(xintercept=xax_brek, color=color_cycle, lwd=linewidth)+
    ## [ Points ] Cycle minima & maxima
    geom_point(data=x_extrema, aes(x=time_min, y=od_min), color=color_slope, shape=23, size=5, stroke=linewidth)+
    geom_point(data=x_extrema, aes(x=time_max, y=od_max), color=color_slope, shape=23, size=5, stroke=linewidth)+
    ## [ Line ] Slope lines
    geom_line(data=x_extrema_l, aes_string(x='time', y="od", group=col_cycle_n), color=color_slope, linewidth=linewidth)+
    ## [ Text ] Cycle and slope labels
    geom_text( data=x_extrema, aes(x=(time_max-delta_time/2), y=max(od_max), label=round(slope, 2)), color=color_slope, size=textsize, size.unit="pt", vjust=.5)+
    geom_text( data=x_extrema, aes(x=(time_max-delta_time/2), y=min(od_min), label=cycle_n), color=color_cycle, size=textsize, size.unit="pt", vjust=.5)+
    ## [ Line ] OD measurement over time
    geom_line( data=x_chemostat, aes_string(x=col_time, y=col_od), alpha=.85, color=color_lines, linewidth=linewidth)+
    ## [ X-Axis ]
    scale_x_continuous(breaks=xax_brek, labels=xax_labs)+
    ## [ Y-Axis]
    coord_cartesian(ylim=c(yax_min,yax_max))+
    ## [ Format ]
    labs(x=paste0("Time (", time_unit, ")"), y="OD")+
    theme_bw()+
    theme(
      panel.grid.minor = element_blank(),
      text = element_text(color="black", size=textsize),
      axis.text = element_text(color="black", size=textsize))
  
  return(plot)
  
}

plot_extrema_sub <- function(x_chemostat, x_extrema, col_time, col_od, col_cycle_n, filename, dir_out, cycles_show, color_lines, color_cycle, color_slope, time_min, convert_time, linewidth, textsize) {
  "
  ____________________________________________________________________________
  Description
  - Visualize extrema annotation results within user-defined cycle windows for a closer look.
  
  ____________________________________________________________________________
  Arguments
  - x_chemostat     Data frame containing chemostat information
  - x_extrema       Data frame produced by annotate_cycle_extrema() summarizing extrema annotations w/ slopes
  - col_time        x_chemostat column containing data sample times
  - col_od          x_chemostat column containing od measures
  - col_cycle_n     x_chemostat column defining cycle group
  - filename        Output figure file name
  - dir_out         Output directory
  - cycles_show     The quantity of cycles to show per figure
  - color_lines     Hexadecimal or R-accepted color value for OD lines
  - color_cycle     Hexadecimal or R-accepted color value for cycle deliminations
  - color_slope     Hexadecimal or R-accepted color value for lm slope annotations
  - time_min        Minimum time value in x_chemostat
  - convert_time    Convert the units of col_time by specifying the starting units and target units, seperated by a dash
  - linewidth       Line width for figure lines
  - textsize        Text size for figures
  "
  
  ## Define total number of sub-figures to generate
  n_figs <- ceiling( max(x_extrema[,"cycle_n"]) / cycles_show )
  
  ## Iterate through sub-figure numbers and make sub-figures
  for (n in 1:n_figs) {
    
    ## Identify cycles to display
    cycle_strt <- 1 + cycles_show * ( n - 1 )
    cycle_stop <- cycle_strt + cycles_show - 1
    ## Correct final figure
    if (cycle_stop > max(x_extrema[,"cycle_n"])) { cycle_stop <- max(x_extrema[,"cycle_n"]) }
    
    ## Identify od chemostat data rows associated with specified cycles
    row_start <- x_extrema[which(x_extrema[,"cycle_n"]==cycle_strt),"row_min"]
    row_end   <- x_extrema[which(x_extrema[,"cycle_n"]==cycle_stop),"row_max"]
    
    ## Subset data sets to current ranges
    x_chemostat_sub <- x_chemostat[row_start:row_end,]
    x_extrema_sub   <- x_extrema[cycle_strt:cycle_stop,]
    
    ## Define y axis limits
    time_strt <- head(x_chemostat[which(x_chemostat["extrema"]=="minimum"),col_time], n=1) ## First cycle start time point
    time_stop <- tail(x_chemostat[which(x_chemostat["extrema"]=="maximum"),col_time], n=1) ## Last cycle stop time point
    yax_min <- min( x_chemostat[which(x_chemostat[col_time]>=time_strt & x_chemostat[col_time]<=time_stop),col_od] )
    yax_max <- max( x_chemostat[which(x_chemostat[col_time]>=time_strt & x_chemostat[col_time]<=time_stop),col_od] )
    
    ## Generate sub-figure
    fig_extrema_sub <- plot_extrema(
      x_chemostat  = x_chemostat_sub ,
      x_extrema    = x_extrema_sub   ,
      col_time     = col_time        ,
      col_od       = "od_measured"   ,
      col_cycle_n  = "cycle_n"       ,
      color_lines  = color_lines     ,
      color_cycle  = color_cycle     ,
      color_slope  = color_slope     ,
      yax_min      = yax_min         ,
      yax_max      = yax_max         ,
      time_min     = time_min        ,
      convert_time = convert_time    ,
      linewidth    = linewidth       ,
      textsize     = textsize        )
    
    ## Export
    outfile_fig_sub <- file.path( dir_out, paste0(filename, "__", "subset_cycles", "_", cycle_strt, "-", cycle_stop, ".pdf") )
    ggsave(file=outfile_fig_sub, plot=fig_extrema_sub, width=11, height=8.5)
  }
}

chemostat_regression <- function(infile, dir_out, col_time, col_od, col_pump_fresh, col_pump_waste, cycle_buffer, extrema_range, color_lines, color_cycle, color_slope, linewidth, cycles_show, convert_time, textsize) {
  "
  ____________________________________________________________________________
  Description
  - Core function. Parse pump rate data to define cycles, identify extrema surrounding cycles, and calculate growth rate slopes using regression.
  
  ____________________________________________________________________________
  Arguments
  - infile            Input file. comma-seperated (csv) and tasb-seperated (tsv) accepted
  - x_chemostat       Data frame containing chemostat information
  - x_extrema         Data frame produced by annotate_cycle_extrema() summarizing extrema annotations w/ slopes
  - col_time          x_chemostat column containing data sample times
  - col_od            x_chemostat column containing od measures
  - col_pump_fresh    x column holding fresh media pump rate data
  - col_pump_waste    x column holding waste media pump rate data
  - col_cycle_n       x_chemostat column defining cycle group
  - filename          Output figure file name
  - dir_out           Output directory
  - cycle_buffer      The number of timepoints that must elapse before the next cycle may be identified
  - extrema_range     Number of time points +/- cycle index to search for local extrema.
  - cycles_show       The quantity of cycles to show per figure
  - color_lines       Hexadecimal or R-accepted color value for OD lines
  - color_cycle       Hexadecimal or R-accepted color value for cycle deliminations
  - color_slope       Hexadecimal or R-accepted color value for lm slope annotations
  - time_min          Minimum time value in x_chemostat
  - convert_time      Convert the units of col_time by specifying the starting units and target units, seperated by a dash
  - linewidth         Line width for figure lines
  - textsize          Text size for figures
  "
  
  ## Parse input file name
  filename <- parse_filename_base(
    infile = infile )
  
  ## Load chemostat data
  df_chemostat <- load_chemostat_data(
    infile = infile )
  
  ## Convert time units
  df_chemostat <- convert_col_time(
    x_chemostat  = df_chemostat ,
    col_time     = col_time     ,
    convert_time = convert_time )
  
  ## Identify minimum time value
  time_min <- identify_min_time(
    x_chemostat = df_chemostat       ,
    col_time    = "col_time_convert" )
  
  ## Plot OD curve without extrema annotations
  fig_od <- plot_od(
    x_chemostat  = df_chemostat       ,
    col_time     = "col_time_convert" ,
    col_od       = "od_measured"      ,
    color_lines  = color_lines        ,
    convert_time = convert_time       ,
    linewidth    = linewidth          ,
    textsize     = textsize           )
  
  ## Identify chemostat cycles
  df_chemostat <- annotate_cycle_vertices(
    x              = df_chemostat   ,
    col_pump_fresh = col_pump_fresh ,
    col_pump_waste = col_pump_waste ,
    cycle_buffer   = cycle_buffer   )
  
  ## Revise first cycle index
  df_chemostat <- identify_frst_cycle_strt(
    x              = df_chemostat ,
    col_od         = col_od       ,
    col_cycles     = "cycles"     )
  
  ## Revise last cycle index
  df_chemostat <- identify_last_cycle_stop(
    x              = df_chemostat ,
    col_od         = col_od       ,
    col_cycles     = "cycles"     )
  
  ## Annotate extrema (min, max) and cycle number
  df_chemostat <- annotate_cycle_extrema(
    x             = df_chemostat  ,
    col_od        = col_od        ,
    col_cycles    = "cycles"      ,
    extrema_range = extrema_range )
  
  ## Summarize cycle extrema annotations using linear modeling approach
  df_extrema <- calculate_extrema_slopes_lm(
    x            = df_chemostat       ,
    col_extrema  = "extrema"          ,
    col_cycles   = "cycle_n"          ,
    col_time     = "col_time_convert" ,
    col_od       = col_od             ,
    convert_time = convert_time       )
  
  ## Generate extrema-annotation summary figure
  fig_extrema <- plot_extrema(
    x_chemostat  = df_chemostat  ,
    x_extrema    = df_extrema    ,
    col_time     = "col_time_convert"      ,
    col_od       = "od_measured" ,
    col_cycle_n  = "cycle_n"     ,
    color_lines  = color_lines   ,
    color_cycle  = color_cycle   ,
    color_slope  = color_slope   ,
    time_min     = time_min      ,
    convert_time = convert_time  ,
    linewidth    = linewidth     ,
    textsize     = textsize      )
  
  ## Generate extrema-annotation summary figure, subsets
  plot_extrema_sub(
    x_chemostat = df_chemostat        ,
    x_extrema    = df_extrema         ,
    col_time     = "col_time_convert" ,
    col_od       = "od_measured"      ,
    col_cycle_n  = "cycle_n"          ,
    filename     = filename           ,
    dir_out      = dir_out            ,
    cycles_show  = cycles_show        ,
    color_lines  = color_lines        ,
    color_cycle  = color_cycle        ,
    color_slope  = color_slope        ,
    time_min     = time_min           ,
    convert_time = convert_time       ,
    linewidth    = linewidth          ,
    textsize     = textsize           )
  
  ## Define output files
  outfile_chemostat <- file.path( dir_out, paste0(filename, "__", "annotated", ".csv") )
  outfile_extrema   <- file.path( dir_out, paste0(filename, "__", "extrema"  , ".csv") )
  outfile_fig_ovr   <- file.path( dir_out, paste0(filename, "__", "overview" , ".pdf") )
  outfile_fig_ann   <- file.path( dir_out, paste0(filename, "__", "annotated", ".pdf") )
  
  ## Export results
  write.table(file=outfile_chemostat, x=df_chemostat, sep=",", row.names=F)
  write.table(file=outfile_extrema  , x=df_extrema  , sep=",", row.names=F)
  ggsave(file=outfile_fig_ovr, plot=fig_od     , width=11, height=8.5)
  ggsave(file=outfile_fig_ann, plot=fig_extrema, width=11, height=8.5)
}





# ______________________________ ------------------------------------------
# Main --------------------------------------------------------------------

identify_run_environment_execute <- function() {
  "
  ______________________________________________________________________________
  Description
    - Manage argument parsing and main execution of software
  "
  
  ## _________________________________________________________________________
  ## Initialize Environment
  
  identify_and_set_wd_either()
  
  
  
  ## _________________________________________________________________________
  ## Load parameters
  
  ## [- Interactive -] Use parameters defined in this file
  if ( interactive() == T ) { args <- args_rstudio() }
  
  ## [- Command Line- ] Use parameters defined via command line argparse
  if ( interactive() == F ) {  args <- args_rscript() }
  
  
  
  ## _________________________________________________________________________
  ## Execute
  
  chemostat_regression(
    infile         = args$infile         ,
    dir_out        = args$dir_out        ,
    col_time       = args$col_time       ,
    col_od         = args$col_od         ,
    col_pump_fresh = args$col_pump_fresh ,
    col_pump_waste = args$col_pump_waste ,
    cycle_buffer   = args$cycle_buffer   ,
    extrema_range  = args$extrema_range  ,
    color_lines    = args$color_lines    ,
    color_cycle    = args$color_cycle    ,
    color_slope    = args$color_slope    ,
    cycles_show    = args$fig_sub_cycles ,
    convert_time   = args$convert_time   ,
    linewidth      = args$linewidth      ,
    textsize       = args$textsize       )
  
}

suppressWarnings( identify_run_environment_execute() )
