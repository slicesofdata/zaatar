# packages 
packages <- c("BayesFactor", "rstanarm",                   # for bayesian modeling
              "car", 
              "cluster", "dendextend", "factoextra",       # for cluster analysis 
              
              "dplyr",                                     # for data manipulation
              # for html tables
              "DT", "stargazer", "formattable",            # for data viewing
              "flextable", "kableExtra",                   # for data viewing
              "gt", "gtsummary", "huxtable", "pixiedust",  # for data viewing
              "ggplot2", "gganimate", "ggcorrplot", "ggExtra", "ggraph",      # for graphics
              "Hmisc", 
              "InformationValue",                          # for model diagnostics 
              "magrittr",                                  # for piping/nesting commands 
              "caret", "parsnip",                          # for model building
              "plotly",
              #"parameters"                                # converts summaries of regression model objects into dataframe
              "modelr",                                    # for modeling
              "pracma",                                    #
              "psych",                                     # for different descriptives
              "pscl",                                      #
              "NlcOptim",                                  #
              "remotes", # "devtools",                       # for development/installing libs from github
              "rstatix",                                   # pipe friendly stats models along with an ANOVA wrapper for car
              "rstudioapi",                                #
              "sjstats", "sjPlot", "sjlabelled",           # for common stats computations and plots built on ggplot2
              "stringr",                                   # for string manipulation 
              "shinydisconnect",                           # for shiny apps; for ggExtra examples
              
              # easystats ecosystem
              "bayestestR", "correlation", "datawizard", "effectsize", 
              "insight", "modelbased", "parameters", "see", "report", 
              
              # tidyverse ecosystem
              "tidyverse",  #"ggplot2", "dplyr", "tidyr", "readr", "purrr", "tibble", "stringr", "forcats",
              "broom", "tidymodels"                       # for tidy modeling
             )
 


not_installed <- packages[!(packages %in% installed.packages()[ , "Package"])]    # Extract not installed packages
#if(length(not_installed)) install.packages(not_installed) # install if not installed

#suppressMessages(remotes::install_github("psyteachr/reprores-v2"))
# suppressMessages(remotes::install_github("easystats/parameters"))
 