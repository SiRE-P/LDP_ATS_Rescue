# techreport_fig_captions.R
# Figure captions for ATS Technical Report
# Sourced in 00-functions.Rmd

# Results Section
figure_scale_book_caption <-
  "Example of a scale book record form."

figure_sample_caption <-
  "Example of scale samples on a scale book."

bubble_heatmap_caption <- paste(
  "Bubble-plot showing the survey effort (number of distinct ATS survey dates) per lake per year.",
  "Bubble colours specify source dataset (acoustic target or trawl biosampling; dark yellow shows where both overlap);",
  "bubble size is proportional to the number of dates surveyed in a given year.")
  
trip_effort_lakes_caption <- paste(
  "Frequency distribution of ATS trips by month across all available years for key study lakes.", 
  "Each trip may include 1-3 sequential sampling days in the same lake.")

depth_acoustic_caption <-
  "Frequency distribution of lake depth range for acoustic sonar surveys across all lakes and years available."

acoustic_targets_caption <-
  paste(
    "Mean annual number of fish-like targets (FLT) from acoustic sonar surveys for key study lakes by ATS year.",
    "Survey totals summed across depth strata and transects,",
    "then averaged across surveys, weighted by abundance of FLTs.")

acoustic_data_prop_sox_caption <-
  paste(
    "Annual proportion Sockeye (as manually recorded in acoustic source files),",
    "weighted by target abundance, by lake and year."
    )

trawl_sample_label_caption <-
  "Example of a trawl sample label."

trawl_duration_distribution_caption <-
  paste(
    "Frequency distribution of trawl duration for biosample data",
    "(left y-axis) and relative proportions in the dataset (right y-axis), across all lakes and years available.",
    "Duration represents calculated times from recorded start and stop times",
    "where available, otherwise recorded duration time.",
    "Note: Y-axis broken to better portray lower frequencies where duration not equal to the typical 15 minutes."
    )

trawl_depth_caption <-
  paste(
    "Frequency distribution of trawl sampling depth for biosample data, across all lakes and years available.",
    "Due to the fact that the acoustic gear does not detect samples in the top 2 meters,",
    "surface trawls (0-2 m) were used to sample for fish in the upper epilimnion.",
    "Bin width = 2 meters.")

trawl_depth_by_lake_caption <- paste(
  "Frequency distribution of depth (m) data in the trawl dataset for key study lakes.",
  "Bin width for bar is 2 meters.")

trawl_hist_length_weight_by_lake_caption <-
  "Frequency distribution of Sockeye and Stickleback length for key study lakes, all available years."

obsolete_plot_caption <-
  "Distribution of Sockeye and Stickleback length and weight in Great Central Lake."

hist_length_weight_by_year_caption <-
  "Histograms of the distribution of Sockeye and Stickleback length and weight by sampled ATS year."

boxplot_lakes_years_caption <- paste(
  "Size distribution of Sockeye and Stickleback species from trawls in key study lakes by year.",
  "(Note: some upper outliers cropped by Y-axis; statistics not affected.)")

all_spp_length_weight_caption <-
  "Relationship between fish length and weight for all species included in the trawl dataset, across all lakes and years."

separated_spp_length_weight_caption <-
  paste(
    "Scatter plot illustrating the relationship between fish length and weight",
    "for juvenile Chinook, Coho, Sockeye and all Stickleback specimens included",
    "in the dataset."
  )

sockeye_length_weight_key_lakes_caption <-
  paste(
    "Sockeye length and weight data for key study lakes, across all available years.",
    "Note that there are some anomalous values for Sockeye Age +1 in Kennedy Lake.",
    "These unusual length/weight values are flagged in the trawl biosampling dataset.",
    "For Great Central Lake, the Sockeye with long length and light weight are also",
    "flagged in the trawl dataset as having unusual sizes."
  )

stickleback_length_weight_key_lakes_caption <- paste(
  "Stickleback length and weight data for key study lakes, all available years.",
  "Note: life-stage data for Sticklebacks only available for Kennedy Lake.")

trawl_sockeye_proportions_caption <- paste(
  "Annual proportion of Sockeye in Trawl data, weighted by number of fish in sample.",
  "Boxplots show distributions across trawls; red line is the weighted annual mean proportion across all dates.")

# Appendix A
biosample_sox_count <- "Number of Sockeye surveyed in the trawl samples by lake and ATS year."
biosam_nonsox_count <- "Number of non-Sockeye surveyed in the trawl samples by lake and ATS year."
ATS_trips           <- "Number of ATS trips by lake and year."

trawl_biosample_CHE <- "Cheewhat Lake trawl catch summary by survey date for Sockeye and Stickleback."
trawl_biosample_GCL <- "Great Central Lake trawl catch summary by survey date for Sockeye and Stickleback."
trawl_biosample_HEN <- "Henderson Lake trawl catch summary by survey date for Sockeye and Stickleback."
trawl_biosample_HOB <- "Hobiton Lake trawl catch summary by survey date for Sockeye and Stickleback."
trawl_biosample_KEN <- "Kennedy Lake trawl catch summary by survey date for Sockeye and Stickleback."
trawl_biosample_LON <- "Long Lake trawl catch summary by survey date for Sockeye and Stickleback."
trawl_biosample_MEZ <- "Meziadin Lake trawl catch summary by survey date for Sockeye and Stickleback."
trawl_biosample_MUR <- "Muriel Lake trawl catch summary by survey date for Sockeye and Stickleback."
trawl_biosample_NIM <- "Nimpkish Lake trawl catch summary by survey date for Sockeye and Stickleback."
trawl_biosample_OWI <- "Owikeno Lake trawl catch summary by survey date for Sockeye and Stickleback."
trawl_biosample_SKI <- "Skidegate Lake trawl catch summary by survey date for Sockeye and Stickleback."
trawl_biosample_SPR <- "Sproat Lake trawl catch summary by survey date for Sockeye and Stickleback."
trawl_biosample_TAH <- "Tahltan Lake trawl catch summary by survey date for Sockeye and Stickleback."
trawl_biosample_VER <- "Vernon Lake trawl catch summary by survey date for Sockeye and Stickleback."
trawl_biosample_WOS <- "Woss Lake trawl catch summary by survey date for Sockeye and Stickleback."
trawl_biosample_YAK <- "Yakoun Lake trawl catch summary by survey date for Sockeye and Stickleback."

# Appendix B
acoustic_meta_cap   <- "Example lake survey metadata and summary data by ATS date. Includes acoustic sounder type, total transects and fish-like targets; total trawls completed, total trawl effort (in minutes), and total fish in trawl samples. All other lake's data will be available in a supplementary comma-delimited text file AST_survey_summary.csv."
lake_strata_tab_cap <- "Summary of the physical lake parameters for one lake's transects and depth strata as an example of the metadata available for all lakes surveyed. The complete table including all study lakes parameters is available as a supplementary comma-delimited text file lep_lake_parameters.csv."