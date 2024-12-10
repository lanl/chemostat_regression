#!/usr/bin/env Rscript

# ______________________________ ------------------------------------------
# Functions ---------------------------------------------------------------

identify_min_time <- function(x_chemostat, col_time) {
  return(min(x_chemostat[,col_time]))
}

plot_od <- function(x_chemostat, col_time, col_od, color_lines, convert_time) {
  "
  ______________________________________________________________________________
  Description
  - Visualize extrema annotation results for user verification of output.
  
  ______________________________________________________________________________
  Arguments
  -  x_chemostat Data frame containing chemostat information
  -  x_extrema   Data frame produced by annotate_cycle_extrema() summarizing extrema annotations w/ slopes
  -  pal         Figure color palette
  -  col_time    x_chemostat column containing data sample times
  -  col_od      x_chemostat column containing od measures
  -  col_cycle_n x_chemostat column defining cycle group
  "
  
  ## Packages
  library(ggplot2)
  
  time_unit <- gsub(x=convert_time, pattern=".+[-]", "")
  
  ## Generate figure & export
  plot <- ggplot()+
    ## [ Line ] OD measurement over time
    geom_line( data=x_chemostat, aes_string(x=col_time, y=col_od), alpha=.85, color=color_lines)+
    ## [ Format ]
    labs(x=paste0("Time (", time_unit, ")"), y="OD")+
    theme_bw()+
    theme(
      panel.grid.minor = element_blank(),
      text = element_text(color="black"))
  
  return(plot)
}

convert_col_time <- function(x_chemostat, col_time, convert_time) {
  
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
  ______________________________________________________________________________
  Description
  -  Iterate through chemostat df and identify rows where fresh media pump is activated. When multiple nearby rows are identified (within 10), the first instance is captured.
  
  ______________________________________________________________________________
  Arguments
  -  x              Data frame containing chemostat information
  -  col_pump_fresh x column holding fresh media pump rate data
  -  col_pump_waste x column holding waste media pump rate data
  -  cycle_buffer   The number of timepoints that must elapse before the next cycle may be identified
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
  ______________________________________________________________________________
  Description
  -  Sometimes, the first few chemostat od measurements can be off. To account for this, the first 10% of od measurements in the first cycle are scanned to find the actual od minimum, which we call the starting point.
  
  ______________________________________________________________________________
  Arguments
  -  x              Data frame containing chemostat information
  -  col_od         x column containing od measurement data
  -  col_pump_fresh x column containing fresh media pump data
  -  col_cycles     x column containing cycle annotations (TRUE = cycle start)
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
  
  message(paste0("\t", " - ", "First cycle initial timepoint index adjusted from 1 to ", cycle_indices[1]))
  
  return(x)
}

identify_last_cycle_stop <- function(x, col_od, col_cycles) {
  "
  ______________________________________________________________________________
  Description
  -  Because pump flow rate is used to define pump cycles, the last cycle end will not be captured as the pump never turns on again. This function captures the final max value to be used as the final cycle stop od. 
  
  ______________________________________________________________________________
  Arguments
  -  x              Data frame containing chemostat information
  -  col_od         x column containing od measurement data
  -  col_cycles     x column containing cycle annotations (TRUE = cycle start)
  "
  
  ## Identify indices of cycle indices
  cycle_indices <- which(x[,col_cycles]==T)
  
  ## Identify how many data points come after the final index
  end_range <- max(cycle_indices) : nrow(x)
  message(paste0("\t", " - ", "Number of timepoints measured after final fresh media pump in: ", length(end_range)))
  
  ## Determine what 10% of range is
  explore_percent   <- ceiling( length(end_range) / 10 )
  end_range_percent <- end_range[ (length(end_range)-explore_percent) : length(end_range) ]
  
  index_final <- end_range_percent[ which.max(x[end_range_percent,col_od]) ]
  
  x[index_final,col_cycles] <- TRUE
  
  message(paste0("\t", " - ", "Last cycle final max od timepoint index identified as: ", index_final, " out of ", nrow(x), " total time points"))
  
  return(x)
}

annotate_cycle_extrema <- function(x, col_od, col_cycles="cycles", extrema_range) {
  "
  ______________________________________________________________________________
  Description
  -  Using cleaned cycle annotations, iterate through cycles and find/label extrema.
  
  ______________________________________________________________________________
  Arguments
  -  x          Data frame containing chemostat information
  -  col_od     x column containing od measurement data
  -  col_cycles x column containing cycle annotations (TRUE = cycle start)
  "
  
  ## Define columns for storing extrema value, cycle counter
  x[,"extrema"] <- NA
  x[,"cycle_n"] <- NA
  
  ## Collect data from chemostat df
  cycle_indices <- which(x[,col_cycles]==T) ## Cycle indices
  n_data        <- nrow(x)                  ## Number of data points
  
  ## Report number of cycles identified
  n_cycles <- length(cycle_indices) - 1
  message(paste0("\t", " - ", "Number of chemostat cycles identified: ", n_cycles))
  
  
  
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
  
  ## Subset to extrema rows
  x_ext <- x[which(!is.na(x[,col_extrema])),]
  
  ## Initialize output df
  cols_out <- c("cycle_n", "row_min", "row_max", "delta_row", "time_min", "time_max", "delta_time", "od_min", "od_max", "delta_od", "slope_sec", "slope_min", "slope_hr", "slope_day", "r2", "yint", "pval", "stderr")
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

define_cycles_displayed <- function(df_extrema, cycle_range) {
  if ( cycle_range[1] < min(df_extrema[,"cycle_n"]) ) { cycle_range[1] <- min(df_extrema[,"cycle_n"]) }
  if ( cycle_range[2] > max(df_extrema[,"cycle_n"]) ) { cycle_range[2] <- max(df_extrema[,"cycle_n"]) }
  return( cycle_range )
}

plot_extrema <- function(x_chemostat, x_extrema, col_time, col_od, col_cycle_n, color_lines, color_cycle, color_slope, yax_min=NA, yax_max=NA, time_min, convert_time, textsize) {
  "
  ______________________________________________________________________________
  Description
  -  Visualize extrema annotation results for user verification of output.
  
  ______________________________________________________________________________
  Arguments
  -  x_chemostat Data frame containing chemostat information
  -  x_extrema   Data frame produced by annotate_cycle_extrema() summarizing extrema annotations w/ slopes
  -  pal         Figure color palette
  -  col_time    x_chemostat column containing data sample times
  -  col_od      x_chemostat column containing od measures
  -  col_cycle_n x_chemostat column defining cycle group
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
  
  ## Define plot subtitle as mean and standard deviation
  n_cycles <- nrow(x_extrema)
  m <- round(mean(x_extrema$slope), 3)
  standev <- round(sd(x_extrema$slope), 3)
  subtitle <- paste0(n_cycles, " ", "cycles with mean = ", m, " ", "+/-", " ", standev, " ", "SD")
  
  ## Generate figure & export
  plot <- ggplot()+
    ## [ V-Line ] defining cycles
    geom_vline(xintercept=xax_brek, color=color_cycle, lwd=1)+
    ## [ Points ] Cycle minima & maxima
    geom_point(data=x_extrema, aes(x=time_min, y=od_min), color=color_slope, shape=23, size=5, stroke=.5)+
    geom_point(data=x_extrema, aes(x=time_max, y=od_max), color=color_slope, shape=23, size=5, stroke=.5)+
    ## [ Line ] Slope lines
    geom_line(data=x_extrema_l, aes_string(x='time', y="od", group=col_cycle_n), color=color_slope, size=.5)+
    ## [ Text ] Cycle and slope labels
    geom_text( data=x_extrema, aes(x=(time_max-delta_time/2), y=max(od_max), label=round(slope, 2)), color=color_slope, size=textsize, size.unit="pt", vjust=.5)+
    geom_text( data=x_extrema, aes(x=(time_max-delta_time/2), y=min(od_min), label=cycle_n), color=color_cycle, size=textsize, size.unit="pt", vjust=.5)+
    ## [ Line ] OD measurement over time
    geom_line( data=x_chemostat, aes_string(x=col_time, y=col_od), alpha=.85, color=color_lines)+
    ## [ X-Axis ]
    scale_x_continuous(breaks=xax_brek, labels=xax_labs)+
    ## [ Y-Axis]
    coord_cartesian(ylim=c(yax_min,yax_max))+
    ## [ Format ]
    labs(x=paste0("Time (", time_unit, ")"), y="OD", subtitle=subtitle)+
    theme_bw()+
    theme(
      panel.grid.minor = element_blank(),
      text = element_text(color="black", size=textsize),
      axis.text = element_text(color="black", size=textsize))
  
  return(plot)
  
}

plot_extrema_sub <- function(x_chemostat, x_extrema, col_time, col_od, col_cycle_n, filename, dir_out, cycle_start, cycle_stop, color_lines, color_cycle, color_slope, time_min, convert_time, textsize) {
  "
  ______________________________________________________________________________
  Description
  -  Visualize extrema annotation results for user verification of output.
  
  ______________________________________________________________________________
  Arguments
  -  x_chemostat Data frame containing chemostat information
  -  x_extrema   Data frame produced by annotate_cycle_extrema() summarizing extrema annotations w/ slopes
  -  pal         Figure color palette
  -  col_time    x_chemostat column containing data sample times
  -  col_od      x_chemostat column containing od measures
  -  col_cycle_n x_chemostat column defining cycle group
  -  filename Used for file output name
  -  cycles_show The number of cycles to display per sub-figure
  "
    
  ## Correct final figure
  if (cycle_start < min(x_extrema[,"cycle_n"])) { cycle_start <- min(x_extrema[,"cycle_n"]) }
  if (is.na(cycle_stop))                        { cycle_stop  <- max(x_extrema[,"cycle_n"]) }
  if (cycle_stop  > max(x_extrema[,"cycle_n"])) { cycle_stop  <- max(x_extrema[,"cycle_n"]) }
  
  ## Identify od chemostat data rows associated with specified cycles
  row_start <- x_extrema[which(x_extrema[,"cycle_n"]==cycle_start),"row_min"]
  row_end   <- x_extrema[which(x_extrema[,"cycle_n"]==cycle_stop),"row_max"]
  
  ## Subset data sets to current ranges
  x_chemostat_sub <- x_chemostat[row_start:row_end,]
  x_extrema_sub   <- x_extrema[cycle_start:cycle_stop,]
  
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
    textsize     = textsize        )
  
  return(fig_extrema_sub)

}

load_chemostat_data <- function(infile) {
  if (grepl(x=basename(infile), pattern=".csv")) { df <- read.delim(file=infile, sep=",",  header=T) } ## Scenario: tab-delimited infile
  if (grepl(x=basename(infile), pattern=".tsv")) { df <- read.delim(file=infile, sep="\t", header=T) } ## Scenario: comma-delimited infile
  return(df)
}

chemostat_regression <- function(infile, dir_out, col_time, col_od, col_pump_fresh, col_pump_waste, cycle_buffer, extrema_range, color_lines, color_cycle, color_slope, fig_sub_cycles, convert_time, cycle_start, cycle_stop, textsize) {
  "
  ______________________________________________________________________________
  Description
  -  Main function for calculating extrema, returning extrema annotations as a table as well as summary figure.
  
  ______________________________________________________________________________
  Arguments
  - (See previous functions)
  "
  
  ## Create output folder
  filename <- gsub(x=basename(infile), pattern=".csv", replacement="")
  message("________________________________________")
  message("Identifying local extrema in file:")
  message(basename(infile))
  
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
    x_chemostat = df_chemostat  ,
    col_time    = "col_time_convert"      )
  
  ## Plot OD curve without extrema annotations
  fig_od <- plot_od(
    x_chemostat = df_chemostat  ,
    col_time    = "col_time_convert" ,
    col_od      = "od_measured" ,
    color_lines = color_lines   ,
    convert_time = convert_time )
  
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
    x           = df_chemostat ,
    col_extrema = "extrema"    ,
    col_cycles  = "cycle_n"    ,
    col_time    = "col_time_convert"     ,
    col_od      = col_od       ,
    convert_time = convert_time)
  
  ## Generate extrema-annotation summary figure
  fig_extrema <- plot_extrema_sub(
    x_chemostat  = df_chemostat   ,
    x_extrema    = df_extrema     ,
    col_time     = "col_time_convert",
    col_od       = "od_measured"  ,
    col_cycle_n  = "cycle_n"      ,
    filename     = filename       ,
    dir_out      = dir_out        ,
    cycle_start  = cycle_start    ,
    cycle_stop   = cycle_stop     ,
    color_lines  = color_lines    ,
    color_cycle  = color_cycle    ,
    color_slope  = color_slope    ,
    time_min     = time_min       ,
    convert_time = convert_time   ,
    textsize     = textsize       )
  
  return(fig_extrema)
}

display_params_for_cl <- function(infile, dir_out, col_time, col_od, col_pump_fresh, col_pump_waste, cycle_buffer, extrema_range, color_lines, color_cycle, color_slope, fig_sub_cycles, convert_time, cycle_start, cycle_stop, textsize) {
  return(paste0(c(
    "## Command line execution",
    "Rscript chemostat_regression.R",
    paste("--infile"        , infile        ),
    paste("--dir_out"       , dir_out       ),
    paste("--col_time"      , col_time      ),
    paste("--convert_time"  , convert_time  ),
    paste("--col_od"        , col_od        ),
    paste("--col_pump_fresh", col_pump_fresh),
    paste("--col_pump_waste", col_pump_waste),
    paste("--cycle_buffer"  , cycle_buffer  ),
    paste("--extrema_range" , extrema_range ),
    paste("--color_lines"   , color_lines   ),
    paste("--color_cycles"  , color_cycle   ),
    paste("--color_slope"   , color_slope   ),
    paste("--fig_sub_cycles", fig_sub_cycles),
    paste("--textsize"      , textsize      )
    ), collapse=" \\\n"))
}





# ______________________________ ------------------------------------------
# Main --------------------------------------------------------------------

# ______________________________ ------------------------------------------
# User Interface ----------------------------------------------------------

ui <- page_sidebar(
  
  ## App title
  title = "chemostat_regression",
  
  ## Sidebar panel for inputs
  sidebar = sidebar(
    
    position = "left",
    width    = "25%",
    
    ## Input data
    card(
      card_header("Input"),
      
      textInput(
        inputId = "infile"   ,
        label   = "--infile" ,
        value   = ""         ),
      
      textInput(
        inputId = "dir_out"   ,
        label   = "--dir_out" ,
        value   = ""         ),
      
    ),
    
    ## Figure ustomization
    card(
      card_header("Figure customization"),
      
      textInput(
        inputId = "color_lines"   ,
        label   = "--color_lines" ,
        value   = "black"         ),
      
      textInput(
        inputId = "color_cycle"   ,
        label   = "--color_cycle" ,
        value   = "blue"         ),
      
      textInput(
        inputId = "color_slope"   ,
        label   = "--color_slope" ,
        value   = "red"         ),
      
      numericInput(
        inputId = "font_size"   ,
        label   = "Font size" ,
        value   = 16         ),
      
    ),
    
    ## Parameters
    card(
      card_header("Parameters"),
    
      numericInput(
        inputId = "cycle_buffer"   ,
        label   = "--cycle_buffer" ,
        value   = 25               ),
      
      numericInput(
        inputId = "extrema_range"   ,
        label   = "--extrema_range" ,
        value   = 20               ),
      
      numericInput(
        inputId = "fig_sub_cycles"   ,
        label   = "--fig_sub_cycles" ,
        value   = 5               ),
      
      textInput(
        inputId = "convert_time"   ,
        label   = "--convert_time" ,
        value   = "s-h"         ),
      
      textInput(
        inputId = "col_time"   ,
        label   = "--col_time" ,
        value   = "exp_time"         ),
      
      textInput(
        inputId = "col_od"   ,
        label   = "--col_od" ,
        value   = "od_measured"         ),
      
      textInput(
        inputId = "col_pump_fresh"   ,
        label   = "--col_pump_fresh" ,
        value   = "pump_1_rate"         ),
      
      textInput(
        inputId = "col_pump_waste"   ,
        label   = "--col_pump_waste" ,
        value   = "pump_2_rate"         ),
      
    ),
    
    ## Display
    card(
      card_header("Display"),
      
      numericInput(
        inputId = "cycle_start"  ,
        label   = "Cycle start"  ,
        value   = 1             ),
      
      numericInput(
        inputId = "cycle_stop"  ,
        label   = "Cycle stop"  ,
        value   = NA           ),
      
    ),
    
    ## Output, parameters for command line
    card(
      card_header("Command line execution"),
      
      verbatimTextOutput("output_params"),
    ),
    
  ),
  
  # Output: Histogram
  plotOutput(outputId = "distPlot")
)





# ______________________________ ------------------------------------------
# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    ## Define figure
    fig <- chemostat_regression(
      infile         = input$infile         ,
      dir_out        = input$dir_out        ,
      col_time       = input$col_time       ,
      col_od         = input$col_od         ,
      col_pump_fresh = input$col_pump_fresh ,
      col_pump_waste = input$col_pump_waste ,
      cycle_buffer   = input$cycle_buffer   ,
      extrema_range  = input$extrema_range  ,
      color_lines    = input$color_lines    ,
      color_cycle    = input$color_cycle    ,
      color_slope    = input$color_slope    ,
      textsize       = input$font_size      ,
      fig_sub_cycles = input$fig_sub_cycles ,
      convert_time   = input$convert_time   ,
      cycle_start    = input$cycle_start    ,
      cycle_stop     = input$cycle_stop     )
    
    ## Display figure
    fig
    
  })
  
  output$output_params <- renderText({
    
    display_params_for_cl(
      infile         = input$infile         ,
      dir_out        = input$dir_out        ,
      col_time       = input$col_time       ,
      col_od         = input$col_od         ,
      col_pump_fresh = input$col_pump_fresh ,
      col_pump_waste = input$col_pump_waste ,
      cycle_buffer   = input$cycle_buffer   ,
      extrema_range  = input$extrema_range  ,
      color_lines    = input$color_lines    ,
      color_cycle    = input$color_cycle    ,
      color_slope    = input$color_slope    ,
      textsize       = input$font_size      ,
      fig_sub_cycles = input$fig_sub_cycles ,
      convert_time   = input$convert_time   ,
      cycle_start    = input$cycle_start    ,
      cycle_stop     = input$cycle_stop     )
  })
  
  ## Not-implemented params
  "
  
  "
  
}





# ______________________________ ------------------------------------------
# Main --------------------------------------------------------------------

shinyApp(ui = ui, server = server)




