# techreport_fig_captions.R
# Figure captions for ATS Technical Report

figure_scale_book_caption <-
  "Example of a scale book record form."

figure_sample_caption <-
  "Example of scale samples on a scale book."

bubble_heatmap_caption <- paste(
  "Survey effort (number of distinct ATS survey dates) per lake per year.",
  "Bubble colours specify source dataset (acoustic target or trawl biosampling; dark yellow shows where both overlap);",
  "bubble size is proportional to the number of dates surveyed in a given year.")
  
trip_effort_lakes_caption <- paste(
  "Frequency distribution of ATS trips by month across all available years for key study lakes (including both source datasets).", 
  "Each trip may include 1-3 sequential sampling days in the same lake.")

depth_acoustic_caption <-
  "Frequency distribution of lake depth range for acoustic sonar surveys across all lakes and years available. No observations were available from the upper 0-2 m of the water column. This reflects limitations of the acoustic equipment rather than an absence of fish occupancy in surface waters. As a result, acoustic observations were concentrated in deeper strata."

acoustic_targets_caption <-
  paste(
    "Mean annual number of fish-like targets (FLT) from acoustic sonar surveys for key study lakes by ATS year.",
    "Survey totals were summed across depth strata and transects,",
    "then averaged across surveys, weighted by abundance of FLTs.")

acoustic_data_prop_sox_caption <-
  paste(
    "Annual proportion of Sockeye Salmon (as recorded in acoustic source files),",
    "weighted by target abundance, by lake and year."
    )

trawl_sample_label_caption <-
  "Example of a trawl sample label."

trawl_duration_distribution_caption <-
  paste(
    "Frequency distribution of trawl duration for biosample data",
    "(left y-axis) and relative proportions in the dataset (right y-axis), across all lakes and years available.",
    "Duration represents calculated times from recorded start and stop times",
    "where available, otherwise recorded duration time. The concentration of 15-minute trawls reflects the standard sampling protocol used during most surveys.",
    "Note: Y-axis broken to better portray lower frequencies where duration not equal to the typical 15 minutes."
    )

trawl_depth_caption <-
  paste(
    "Frequency distribution of trawl sampling depth for biosample data, across all lakes and years available.",
    "Surface (0-2 m) trawls were used to sample portions of the water column not effectively sampled by hydroacoustic equipment.",
    "Bin width = 2 meters.")

trawl_depth_by_lake_caption <- paste(
  "Frequency distribution of depth (m) data in the trawl dataset for key study lakes.",
  "Bin width for bars is 2 meters.")

trawl_hist_length_weight_by_lake_caption <-
  "Frequency distribution of Sockeye Salmon and Stickleback length for key study lakes, all available years."

trawl_sockeye_proportions_caption <- paste(
  "Distributions of annual proportion of Sockeye Salmon across trawls, weighted by number of fish in sample. Red line is the weighted annual mean proportion across all dates in a given year.")

obsolete_plot_caption <-
  "Distribution of Sockeye Salmon and Stickleback length and weight in Great Central Lake."

hist_length_weight_by_year_caption <-
  "Histograms of the distribution of Sockeye Salmon and Stickleback length and weight by sampled ATS year."

boxplot_lakes_years_caption <- paste(
  "Size distribution of Sockeye Salmon and Stickleback species from trawls in key study lakes by year.",
  "(Note: some upper outliers cropped by Y-axis; statistics not affected.)")

all_spp_length_weight_caption <-
  "Relationship between fish length and weight for all species included in the trawl dataset, across all lakes and years."

separated_spp_length_weight_caption <-
  paste(
    "Relationship between fish length and weight",
    "for juvenile Chinook, Coho, Sockeye Salmon and all Stickleback specimens included",
    "in the dataset. The single adult Sockeye Salmon was omitted",
    "from the statistical calculations. Length-weight relationships were estimated separately for each species using log-log regression models described in the Methods section.")

sockeye_length_weight_key_lakes_caption <-
  paste(
    "Sockeye Salmon length and weight data for key study lakes, across all available years. Note that there are some anomalously small values for fish Age +1 in Kennedy Lake. For Great Central Lake, the fish with long length and light weight have unusual sizes. These unusual length/weight values are identified through data-quality flags within the published dataset." )

stickleback_length_weight_key_lakes_caption <- paste(
  "Stickleback length and weight data for key study lakes in all available years.",
  "Note: life-stage data for Sticklebacks available only for Kennedy Lake.")

appendix_survey_trips_caption <- paste(
  "Number of ATS trips by lake and year. NOTE: Values of zero may indicate either no survey was conducted or that no data were recovered for that lake-year combination.")

appendix_trawl_sockeye_caption <- paste(
  "Number of Sockeye Salmon surveyed in the trawl samples by lake and ATS year.")

appendix_trawl_nonsox_caption <- paste(
  "Number of non-Sockeye surveyed in the trawl samples by lake and ATS year.")

appendixB_acoustic_trawl_summary_cpation <- paste(
  "Key lake survey metadata and summary data by ATS date. Includes acoustic sounder type, total transects and fish-like targets; total trawls completed, total trawl effort (in minutes), and total fish in trawl samples. These metadata provide the information required to interpret ATS sampling design and to reproduce historical abundance calculations. The complete dataset including additional rows for the key lakes and metadata for all other lakes data will be available in a supplementary comma-delimited text file ATS_meta_summary_by_survey_date.csv." )

appendixB_lake_strata_tab_caption <- "Summary of the physical lake parameters for key lakes transects and depth strata as an example of the metadata available for all lakes surveyed. The complete dataset including all study lakes parameters is available as a supplementary comma-delimited text file ATS_lake_strata_table.csv."

