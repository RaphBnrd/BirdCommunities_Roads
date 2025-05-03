rm(list = ls())
library(tidyverse)


path_input_data <-  "data/01_a_surveys-raw_data.csv"
path_output_data <- "data/01_b_surveys-clean_data.csv"


# * * * Description of the Raw data * * *

# The raw dataset is a table where each row correspond to an observation of a 
# bird during a visit. The table contains the following columns: 
#   - id: the id of the observation (unique integers)
#   - cfb_plot_num: the id of the plot in the ConFoBi project setting. In our 
#       study, we refer to this id as the site id. 
#   - round: the round of the visit (1 or 2 since we did 2 visits per plot)
#   - day: the date of the visit (dd/mm/yyyy)
#   - time_visit: the time of the start of the 25 min visit (hh:mm:ss)
#   - type_plot: the type of plot (forest, path, road or edge)
#   - observer: the name of the observer (the person who did the survey)
#   - num_surv_5_min: the number of 5 min session during which the bird was
#       observed. We stayed 25 min for each visit. The first 5 min (num_surv_5_min = 0)
#       will be removed from the analysis (preparation, set up, etc.). We keep
#       the 20 min of observation (num_surv_5_min = 1, 2, 3, 4).
#   - species: the species of the bird observed (common name)
#   - distance_more_than_50: a boolean indicating if the bird was observed at
#       more than 50 m from the observer. We remove the birds observed at more 
#       than 50 m from the observer. 
#   - comment: a comment on the observation (if any). 

#              _________________________________________________
# /!\ /!\ /!\ | WE REMOVE num_surv_5_min = 0 AND DISTANT BIRDS |  /!\ /!\ /!\
#            |________________________________________________|

# We select the maximum abundance among each 5 min during the 20 min. 


# Import bird counts ------------------------------------------------------

# Raw data
data_survey_raw <- read.table(path_input_data, header=TRUE, sep = ";")

head(data_survey_raw)
summary(data_survey_raw)
str(data_survey_raw)


# Clean the dataset -------------------------------------------------------

# Copy into clean data
data_survey_clean <- data_survey_raw

# Remove data distant or out of the 4 breaks
data_survey_clean <- data_survey_clean %>% 
  filter(num_surv_5_min != "0", distance_more_than_50 == FALSE)

# Date of the survey in clean format
data_survey_clean$day <- as.POSIXct(
  data_survey_clean$day, format="%d/%m/%Y", tz="UTC"
) %>% 
  strptime(format="%Y-%m-%d")

# Datetime of the beginning of the visit
data_survey_clean$time_visit <- as.POSIXct(
  paste(data_survey_clean$day, data_survey_clean$time_visit), 
  format="%Y-%m-%d %H:%M:%S", tz="UTC"
)

# Plot id (ConFoBi plot number)
data_survey_clean$cfb_plot_num <- as.integer(data_survey_clean$cfb_plot_num)

# Visit ID = "[YYYYmmdd][HHMMSS].[cfb_plot_num]"
data_survey_clean$visit_id <- paste0(
  format(data_survey_clean$day, format="%Y%m%d"), # date
  format(data_survey_clean$time_visit, format="%H%M%S"), # time
  ".",
  formatC(data_survey_clean$cfb_plot_num, width=3, flag="0") # plot id
)

# Session ID = "[YYYYmmdd][HHMMSS for start of the visit].[cfb_plot_num].[num_surv_5_min]"
data_survey_clean$session_id <- paste0(
  format(data_survey_clean$day, format="%Y%m%d"), # date
  format(data_survey_clean$time_visit, format="%H%M%S"), # time
  ".",
  formatC(data_survey_clean$cfb_plot_num, width=3, flag="0"), # plot id
  ".",
  formatC(data_survey_clean$num_surv_5_min, width=1, flag="0") # session number
)


# Deal with factor columns
cols_factor <- c("visit_id", "session_id", "cfb_plot_num", "type_plot", 
                 "observer", "num_surv_5_min", "species")
data_survey_clean[cols_factor] <- lapply(data_survey_clean[cols_factor], as.factor) 

data_survey_clean <- data_survey_clean %>% 
  mutate(type_plot = factor(type_plot, levels = c("forest", "path", "road", "edge") ) )

# Check the data
head(data_survey_clean)
summary(data_survey_clean)
str(data_survey_clean)

levels(data_survey_clean$species)


# Aggregate the dataset per visit -----------------------------------------

# One column per species (1 if the observation is for the corresponding species, else 0)
data_survey_per_visit <- data_survey_clean %>% 
  pivot_wider(names_from = species, values_from = species, 
              values_fn = ~1, values_fill = 0)

# Count per session of 5 min
data_survey_per_visit <- data_survey_per_visit %>% 
  group_by(session_id) %>%
  summarise(across( 
    ( dim(data_survey_clean)[2]-1 ) : # Corresponds to the first species column (session_id is not included)
      ( dim(data_survey_clean)[2]-1 + length(unique(data_survey_clean$species))-1 ), # There are length(unique(data_survey_clean$species)) species
    sum # Applies the sum function to each column
  )) %>%
  ungroup() # -> abundance matrix for each session 

# Add the id of the visit
data_survey_per_visit <- data_survey_per_visit %>% 
  left_join(data_survey_clean %>% select(session_id, visit_id) %>% distinct(), 
            by="session_id")

# Select the maximum number of individuals among the 5 min sessions, for each species
data_survey_per_visit <- data_survey_per_visit %>% 
  group_by(visit_id) %>%
  summarise( across( 2 : ( length(unique(data_survey_clean$species)) + 1 ) , # 1st column is session_id, last column is visit_id
                     max)) %>% # Get the max among the 5 min sessions
  ungroup() # -> abundance matrix for each visit 

# Add all the info of the visitis
data_survey_per_visit <- data_survey_per_visit %>% 
  left_join(
    data_survey_clean %>% 
      select(cfb_plot_num, round, day, time_visit, type_plot, observer, visit_id) %>% 
      distinct, 
    by="visit_id"
  ) 

# Reorder the columns
data_survey_per_visit <- data_survey_per_visit[
  , c("visit_id", "cfb_plot_num", "round", "day", "time_visit", "type_plot", "observer",
      as.character(unique(data_survey_clean$species)) )
]


# Check the data
head(data_survey_per_visit)
summary(data_survey_per_visit)
str(data_survey_per_visit)


# Export the data ---------------------------------------------------------

write.table(data_survey_per_visit, path_output_data, row.names = FALSE, sep = ",")




