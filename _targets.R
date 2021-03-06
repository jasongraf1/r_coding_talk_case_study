# _targets.R file
library(targets)
library(here)
source(here("R", "load_and_clean_functions.R"))
source(here("R", "model_functions.R"))
source(here("R", "plot_functions.R"))
source(here("R", "misc_functions.R"))

options(tidyverse.quiet = TRUE)
# libraries to load
tar_option_set(
  packages = c(
    "tidyverse",
    "here", # for defining relative paths
    "janitor", # for cleaning data
    "vroom", # for fast data loading
    "lme4", # for regression modeling
    "effects", # for calculating partial effects
    ## for mapping
    "sp",
    "rgdal",
    "classInt",
    "plotrix",
    "Hmisc",
    ## for plotting
    "scales",
    "patchwork",
    "ggcharts",
    "ggtext",
    "lemon"
  )
)

# -------------------------------------------------------------------------

## target objects
list(
  # collect files, create dataframe, and clean and annotate it ----------------
  tar_target(files_data_glowbe,
             list.files(here("data_raw"),
                        pattern = "glowbe", full.names = TRUE)),
  tar_target(dataframe_glowbe_raw, map_df(files_data_glowbe, ReadDataset)),
  tar_target(dataframe_glowbe_cleaned, CleanAnnotateData(dataframe_glowbe_raw)),
  # save processed dataset --------------
  tar_target(save_glowbe_data, SaveData(dataframe_glowbe_cleaned, "BE_sat_glowbe_clean.txt"), format = "file"),
  # filter UK data only -------------
  tar_target(dataframe_glowbe_uk, dplyr::filter(dataframe_glowbe_cleaned, country_code == "GB")),
  # create plots --------------------
  # make a dark and light version of the frequency plots
  tar_target(plot_glowbe_freqs_dark_png,
             PlotGlowbeFrequencies(dataframe_glowbe_cleaned, "plot_glowbe_freqs_dark.png", dev = "png"),
             format = "file"),
  tar_target(plot_glowbe_freqs_light_png,
             PlotGlowbeFrequencies(dataframe_glowbe_cleaned, "plot_glowbe_freqs_light.png", dev = "png", theme = "light"),
             format = "file"),
  # plot the bar char illustrating use of sat/sitting with following -ing verb
  tar_target(plot_glowbe_uk_postVPdistance,
             PlotBars(dataframe_glowbe_uk, "plot_glowbe_dist_to_VP.png"),
             format = "file"),
  # Twitter map -------------------
  tar_target(dataframe_twitter_counts,
             vroom(here("data_processed", "BE_sat_Twitter_frequencies.csv"),
                   delim = ",")),
  tar_target(map_uk_blank, rgdal::readOGR(here("data_raw", "SHAPE"), "Areas")),
  # make a dark and light version of the map
  tar_target(plot_twitter_map_sat_dark,
             PlotTwitterMap(dataframe_twitter_counts, map_uk_blank,
                            file = "plot_twitter_map_sit.png",
                            theme = "dark"),
             format = "file"),
  tar_target(plot_twitter_map_sat_blue,
             PlotTwitterMap(dataframe_twitter_counts, map_uk_blank,
                            file = "plot_twitter_map_sit_light.png",
                            theme = "blue"),
             format = "file"),
  # fit regression model ---------------
  tar_target(model_glowbe_dist2VP, FitModelDistVP(dataframe_glowbe_uk)),
  # plot the partial effects of the distance to the following -ing verb
  tar_target(plot_partial_effects,
             PlotPartialEffects(model_glowbe_dist2VP,
                                "plot_partial_effects_dist2VP.png")),
  # print the session information --------------
  tar_target(session_info, PrintSessionInfo())
  )



