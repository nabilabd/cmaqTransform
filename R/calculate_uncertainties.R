##############################################################
## Goal: Calculate uncertainties for non-2006 years
##############################################################

# NB: data can be found here: 
# http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html#_ga=1.198780968.624207471.1428083407


# we proceed by generating linear models of uncertainties to concentration
# then, apply those models for the species concentrations in other years

suppressPackageStartupMessages({
  library(plyr)
  library(dplyr)
  library(magrittr)
})

# Generate the models
aqs_2006 <- readRDS("../../gpfs:pace1:/data_2006/Other_data/year_aqs.rds") %>% tbl_df
aqs_2006 %>% glimpse

# Returns the linear model of uncertainty to concentration, for a species
species_lm <- function(df) lm(sig_c_obs ~ Conc_obs, data = df)

# by-species linear models 
my_model <- aqs_2006 %>% 
  group_by(Species) %>% 
  do(., spec_mod = species_lm(.))

# Check. THIS WORKS
all.equal(
  my_model$spec_mod[[1]], 
  aqs_2006 %>% filter(Species == "Ag") %>% species_lm
)


#########################
# Expand to other years
#########################


all_aqs_data <- saveRDS("/Volumes/My Passport for Mac/gpfs:pace1:/aqs_aggregate_concen_2005_12.rds")
aqs05 <- all_aqs_data %>% filter(Year == "2005")
aqs05 %>% glimpse

all.equal(aqs_2006$Species %>% unique %>% sort, 
          aqs05$Species %>% unique %>% sort)


##### USING RESULTS FROM BOTTOM OF correct_ocec.R

mylist <- interm2_aqs05 %>% dlply(.(Species))

identical( names(mylist), my_model$Species ) # GOOD!


#' Predict Uncertainties in Observed Concentrations
#' 
#' @param model a linear model
#' @param df a dataframe
species_pred <- function(model, df) {
  df2 <- df %>% mutate(sig_c_obs = predict(model, newdata = df))
  df2
}

# COMPLETE 2005 AQS DATA SET
aqs_agg05 <- Map(species_pred, my_model$spec_mod, mylist) %>% 
  ldply %>% tbl_df %>% arrange(SiteID, Date) %>% 
  select(Date, SiteID, Species, Conc_obs, sig_c_obs, len, Year)

aqs_agg05 %>% saveRDS("../../gpfs:pace1:/data_2005/Other_data/year_aqs.rds")


# clean up workspace
rm(all_aqs_data, aqs_2006, my_model)

