# techreport_fig_captions.R
# Figure captions for ATS Technical Report

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
  "Frequency distribution of depth range for acoustic sonar surveys across all lakes."

acoustic_targets_caption <-
  paste(
    "Mean annual number of fish-like targets (FLT) from acoustic sonar surveys for key study lakes by ATS year.",
    "Survey totals summed across depth strata and transects,",
    "then averaged across surveys, weighted by abundance of FLTs.")

acoustic_data_prop_sox_caption <-
  paste(
    "Annual proportion Sockeye (as recorded in acoustic source files),",
    "weighted by target abundance, by lake and year.",
    "Note that these proportions were derived from trawl sample data",
    "by observers and applied to the acoustic source data manually,",
    "and are presented here for comparative purposes.",
    "Note also that the resulting proportions may not be valid because the trip trawl proportions were applied",
    "manually to acoustic data across multiple trip transects and depth strata, and thus likely over-representing the true proportions",
    "due to repetitive application -- this should be recalculated",
    "[HS 260821]"
    )

trawl_sample_label_caption <-
  "Example of a trawl sample label."

trawl_duration_distribution_caption <-
  paste(
    "Frequency distribution of trawl duration in minutes",
    "(left y-axis) and their proportion in the dataset (right y-axis).",
    "Duration represents calculated times from recorded start and stop times",
    "where available, otherwise recorded duration time.",
    "Note: Y-axis broken to better portray lower frequencies where Duration less than or greater than 15 minutes."
    )

trawl_depth_caption <-
  paste(
    "Frequency distribution of trawl sample depths across all lakes and years.",
    "Due to the fact that the acoustic gear does not detect samples in the top 2 meters,",
    "surface trawls (0-2 m) were used to sample for fish in the upper epilimnion.",
    "Bin width = 2 meters.")

trawl_depth_by_lake_caption <- paste(
  "Frequency distribution of depth (m) data in the trawl dataset for key study lakes.",
  "Bin width for bar is 2 meters.")

trawl_hist_length_weight_by_lake_caption <-
  "Frequency distribution of Sockeye and Stickleback length for key study lakes."

obsolete_plot_caption <-
  "Distribution of Sockeye and Stickleback length and weight in Great Central Lake."

hist_length_weight_by_year_caption <-
  "Histograms of the distribution of Sockeye and Stickleback length and weight by sampled ATS year."

boxplot_lakes_years_caption <- paste(
  "Size distribution of Sockeye and Stickleback species from trawls in key study lakes by year.",
  "(Note: some upper outliers cropped by Y-axis; statistics not affected.)")

all_spp_length_weight_caption <-
  "Relationship between fish length and weight for all species included in the trawl dataset."

separated_spp_length_weight_caption <-
  paste(
    "Scatter plot illustrating the relationship between fish length and weight",
    "for juvenile Chinook, Coho, Sockeye and all Stickleback specimens included",
    "in the dataset. Additionally, a regression curve is also provided, including",
    "the annotated coefficients for each species. Any adult Sockeye was disregarded",
    "in the statistical calculations. Length-weight relationships were estimated",
    "separately for each species using linear regression on log-transformed data.",
    "For each species, a linear model of the form W=aLb was fitted, i.e.,",
    "log(calculated standardized weight) = log(a) + b × log(length).",
    "The intercept and slope from the regression were used to calculate the",
    "length-weight parameters, where a was obtained by back-transforming",
    "the intercept (exp(intercept)) and b represents the scaling coefficient.",
    "The sample size (N) was calculated as the number of observations included",
    "in each species-specific model, and model fit was assessed using the",
    "coefficient of determination (R²)."
  )

sockeye_length_weight_key_lakes_caption <-
  paste(
    "Sockeye length and weight data for key study lakes.",
    "Note that there are some anomalous values for Sockeye Age +1 in Kennedy Lake.",
    "These unusual length/weight values are flagged in the trawl biosampling dataset.",
    "For Great Central Lake, the Sockeye with long length and light weight are also",
    "flagged in the trawl dataset as having unusual sizes."
  )

stickleback_length_weight_key_lakes_caption <- paste(
  "Stickleback length and weight data for key study lakes.",
  "Note: life-stage data for Sticklebacks only available for Kennedy Lake.")

trawl_sockeye_proportions_caption <-
  "Annual proportion of Sockeye in Trawl data, weighted by number of fish in sample."