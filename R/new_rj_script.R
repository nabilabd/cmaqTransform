





######################################################
###### Part (1): Organizing the files
######################################################

# Here, there are four parts, from the time of obtaining the matlab files of 
# CMAQ output. 

mat_path <- "../../gpfs:pace1:/data_2005/mat_files_2005"
yr <- 2006
num_sources <- ifelse(yr < 2007, yes = 20, no = 16)

# upon completing this first step, manually check the rds folder to make sure 
# that there are as many .mat files as .rds files (i.e., 41)
# (a) matlab files are converted to rds files:
convert_mat_files(mat_path, "2005")

## renaming
# folder containing rds files
for(fname in dir("rds_files_2007/", full=T)) {
  cc <- readRDS(fname); 
  cc2 <- cc %>% rename_rds_files(yr = "2007"); 
  rm(cc)
  saveRDS(cc2, fname)
}

# (b) rds files are broken down into 10-day intervals (~ 63 min)
gen_interval_rds("../../gpfs:pace1:/data_2007/rds_files_2007/", drop_last = 11)

# (c) interval files are aggregated
combine_species_files("../../gpfs:pace1:/data_2006/Interval_Files/")

# (d) a single file with all the simulated concentrations for the year, is made
aggregate_csim("../../gpfs:pace1:/data_2005/Agg_intv_files/")



############################################################################
###### Part (2): Combine concentrations data, both observed and simulated
############################################################################

# useful parameters
yr_data_path <- "../../gpfs:pace1:/data_2005/Other_data/"
agg_files <- "../../gpfs:pace1:/data_2005/Agg_intv_files/"
yr_bef <- "2004"
yr <- as.numeric(yr_bef) + 1
.sources <- names05 #or for 2006: sources, 2007: names07


# Adds csim values to concentration data
concen_agg_init <- aggregate_concentrations(yr_data_path, yr_bef = yr_bef)
concen_agg <- concen_agg_init %>% tbl_df %>% 
  # from_sitedatelist %>% 
  group_by(SiteID, Date) %>% mutate(len = length(Species)) %>% filter(len == 41)

# store sensitivities, then format (~ 12.5 min)
out <- gen_sens_mats(agg_files, yr_bef = yr_bef, .sources = names05) 
saveRDS(out, "../../gpfs:pace1:/data_2005/Other_data/year_SA_matrices.rds")
new_out <- out %>% format_sens_mats(yr = yr, .sources = names05) # save result before returning it here # CHECK THIS

# saveRDS(concen_agg, "../../gpfs:pace1:/data_2005/Other_data/concen_agg.rds")
# saveRDS(new_out, "../../gpfs:pace1:/data_2005/Other_data/new_out.rds")

concen_agg <- readRDS("../../gpfs:pace1:/data_2005/Other_data/concen_agg.rds")
new_out <- readRDS("../../gpfs:pace1:/data_2005/Other_data/new_out.rds")


#################################################################
###### Part (3): Calculate Rj values
#################################################################


## FIX THE SIG_LNR VALUES FOR 2006, 2007

load("../hybridSA/data/sig_lnr06.rda")
load("../hybridSA/data/sig_ctm06.rda")

if(is.data.frame(concen_agg)) concen_agg <- concen_agg %>% to_sitedatelist()

# HERE, WHY NOT JUST A SINGLE INNER JOIN, ON SITEID AND DATE, then call to_sitedatelist ???

# subset so data has common sites; this also orders them
comsites <- base::intersect( names(concen_agg), names(new_out) )
concen_agg2 <- concen_agg[comsites]
new_out2 <- new_out[comsites]

# site names should have same order
library(assertthat)
assert_that( all.equal( names(concen_agg2), names(new_out2) ) )

# FIX THIS!!!
# subset so each site's data has common dates
comdates <- mapply(function(a, b) base::intersect(names(a), names(b)), 
                   concen_agg2, new_out2)
concen_agg2 <- concen_agg2 %>% Map(`[`, ., comdates)
new_out2 <- new_out2 %>% Map(`[`, ., comdates)

# check that all dates are same for each site
assert_that( all.equal( sapply(concen_agg2, names), sapply(new_out2, names) ) )

# up to here, the sens matrices new_out2 and the observations concen_agg2 have 
# corresponding data, i.e., all sites and dates for them are the same

############################
# test for a single site
############################

# problem: concen_agg_df %>% filter(SiteID == "130890002", Date == "2007-11-20") %>% as.data.frame

ind1 <- 25; ind2 <- 5
yr <- 2005
.sources <- names05
myfunc <- function(x, y) get_optim2(x, y, yr = yr, .sources = .sources)

system.time(
  once <- Map(myfunc, concen_agg2[[ind1]], new_out2[[ind1]]) # TOO MUCH INDEXING !?!
  # once <- Map(get_optim2, concen_agg2[[ind1]], new_out2[[ind1]], yr = "2006") # WORKS !?
)

# version for using "Map" in the optimization
once2 <- once %>% 
  ldply %>% tbl_df %>% 
  set_colnames(c("Date", .sources)) %>% # replace "names07" with some ".sources"
  gather(Source, Rj_vals, -Date) %>% 
  mutate(Date = as.character(Date), Source = as.character(Source))



# once2 <- once %>% 
#   as.data.frame(stringsAsFactors=FALSE) %>% 
#   mutate(Source=names07) %>%  # replace "sources" with some ".sources"
#   Filter(function(date_rj) !anyNA(date_rj), .) %>% 
#   gather(Date, Rj_vals, -Source) %>% 
#   mutate(Date = as.character(Date))

# once %>% 
#   as.data.frame(stringsAsFactors=FALSE) %>% 
#   mutate(Source=sources) %>% 
#   gather(Date, Rj_val, -Source) %>% 
#   filter(is.na(Rj_val))

# it works! so proceed to all others
rm(comdates, comsites, once, once2) # de-clutter

############################

# obtain Rj values for entire year. about 70 min.
system.time(
  all_groups <- mcMap(function(a, b) mcMap(myfunc, a, b), concen_agg2, new_out2)
)


# replaced with an easier-to-clean version above
# system.time(
#   nall_ress <- mcmapply(function(a, b) mcmapply(get_optim, a, b), 
#                         concen_agg2, new_out2)
# )

# with how the get_optim function currently implemented, no longer any need
# to re-process any sitedays, or handle errors in among numerical values

# THIS IS SO MUCH EASIER TO WORK WITH, using Map instead of `mapply`
all_groups2 <- all_groups %>% 
  llply(function(datelist) ldply(datelist, .id = "Date")) %>% 
  ldply(.id = "SiteID") %>% 
  set_colnames(c("SiteID", "Date", .sources)) %>% 
  gather(Source, Rj_vals, -c(SiteID:Date)) %>% 
  colwise(as.character)() %>% tbl_df %>% 
  mutate(Rj_vals = as.numeric(Rj_vals)) %>% 
  filter(!is.na(Rj_vals))


# nall_ress2 <- nall_ress %>% 
#   llply(function(mat) as.data.frame(mat, stringsAsFactors=FALSE)) %>% 
#   llply(function(rj_df) mutate(rj_df, Source=sources)) %>% 
#   # llply(function(df) Filter(function(date_rj) !anyNA(date_rj), df)) %>% # this was from before
#   llply(function(df) gather(df, Date, Rj_vals, -Source)) %>% 
#   ldply(.id="SiteID") %>% 
#   mutate(Date = as.character(Date), SiteID = as.character(SiteID)) %>% 
#   filter(!is.na(Rj_vals)) %>% tbl_df

# the following no longer needed: 

# # check that no more NA's:
# nall_ress2 %>% 
#   mutate(Rj_vals = as.numeric(Rj_vals)) %>% filter(!is.na(Rj_vals)) %>% glimpse
# 
# # that is enough to remove the errors:
# nall_ress2 %>% 
#   mutate(Rj_vals = as.numeric(Rj_vals)) %>% filter(!is.na(Rj_vals)) %>% 
#   is.na %>% sum
# 
# # so
# nall_ress2 <- nall_ress2 %>% 
#   mutate(Rj_vals = as.numeric(Rj_vals)) %>% filter(!is.na(Rj_vals))


# 
write_rds(all_groups2, "../../gpfs:pace1:/data_2005/Other_data/year_rj_vals.rds")

# # much more stability, qualitatively, than before
# saveRDS(nall_ress2, "data/nall_results2.rds")

# # these two were calculated with the newer get_optim, that uses both optimizations
# saveRDS(nall_ress2, "../../gpfs:pace1:/data_2006/Other_data/get_optim2_nall_results2.rds") # for 2006, with newer get_optim
# saveRDS(nall_ress2, "../../gpfs:pace1:/data_2005/Other_data/nall_results2.rds") # for 2005. TODO: generalize


####### -----
# comparing newer get_optim method

orig_nall2 <- readRDS("data/nall_results2.rds") %>% tbl_df %>% 
  rename(orig_rj = Rj_vals)

# NO DIFFERENCE IN ALL VALUES OF ORIGINAL OUTPUT OF GET_OPTIM
nall_ress2 %>% 
  left_join(orig_nall2) %>% 
  mutate(diff_method = round(Rj_vals - orig_rj, 10)) %>% 
  filter(diff_method != 0)

# 2060 (or 1000?) values in newer output of get_optim
nall_ress2 %>% 
  anti_join(orig_nall2)

##########################################################################
# part (2) of new_rj_script: find errors and re-optimize those site-days
##########################################################################

# DEFUNCT/OBSOLETE

#' Identify sites and dates with nloptr not converging
#' 
#' Want to find way to automate detection of sites, dates where errors occurred.
#' 
#' The key idea here is that if there's an error, then the result for a site 
#' would be a list, not a matrix.
#' 
#' @param dflist list of dataframes, which is the result of the optimization.
#' @return a dataframe, with columns \code{SiteID} and \code{Date}
#' 
find_error_sitedates <- function(dflist) {
  
  res <- dflist %>% ldply(class, .id = "SiteID") %>% filter(V1 == "list")
  if(nrow(res) == 0) return 
  
  # from here, we presume there are some sites with errors
  sites <- as.character(res$SiteID)
  site_inds <- which(names(dflist) == sites)
  error_sites <- dflist[site_inds]
  
  # produce, for each site, a list of dates for which optimization fails
  # then convert results to a dataframe
  errors <- llply(error_sites, find_errors) %>% llply(which)
  errors %>% 
    unlist %>% names %>% str_split(fixed("."), n = 2) %>% 
    ldply %>% set_names(c("SiteID", "Date"))
}


# re-do optimization for sites-days for which there were problems

err_sitedays <- find_error_sitedates(nall_ress)

concen_agg3 <- concen_agg2 %>% from_sitedatelist %>% 
  right_join(err_sitedays) %>% to_sitedatelist
new_out3 <- new_out2 %>% from_sitedatelist %>% 
  right_join(err_sitedays) %>% to_sitedatelist

# re-compute Rj value for sites, dates where it failed before
system.time(
  redo_rjs <- mcmapply(function(a, b) mcmapply(get_optim, a, b), 
                        concen_agg3, new_out3)
)

err_sitedays


succeeded <- vapply(redo_rjs, is.numeric, logical(1))
if( any (succeeded)) ldply(redo_rjs, `[`, succeeded, .id = "SiteID")
redo_rjs %>% ldply(.id = "SiteID") %>% glimpse

res <- data.frame(
  SiteID = as.character(err_sitedays$SiteID), 
  Source = sources,
  Date = rep(err_sitedays$Date, each = 20), 
  Rj_vals = unname(redo_rjs)
)

res %>% filter(!is.na(Rj_vals)) %>% glimpse

year_rj <- rbind(nall_ress2, res) %>% filter(!is.na(Rj_vals))
year_rj %>% glimpse
year_rj %>% is.na %>% sum

saveRDS(year_rj, "data/year_rj.rds")

##############################
# hack to fix some dates, which aren't handled in the second optimization
##############################

xrows <- which(str_sub(year_rj$Date, 1, 1) == "X")
year_rj[xrows, ] %>% glimpse
year_rj[xrows, ] %>% mutate(Date = str_sub(Date, 2, -1)) %>% 
  mutate(Date = as.character(ymd(Date))) %>% glimpse

year_rj[xrows, ] <- year_rj[xrows, ] %>% mutate(Date = str_sub(Date, 2, -1)) %>% 
  mutate(Date = as.character(ymd(Date)))

year_rj[xrows, ] %>% glimpse
res %>% glimpse

# and no overlap:
year_rj[xrows, ] %>% inner_join(res)

saveRDS(year_rj, "data/year_rj.rds")

# year_rj3 had Rj_vals of NA filtered out, and year_rj4 just had a second 
# optimization performed where there were errors before. The previous year_rj4, 
# then is equivalent to year_rj here.

####### THIS IS WHERE PROCESSING CAN CONTINUE FROM, AFTER NALL_RESS2

year_rj <- all_groups2 # for 2007, 2005

# include only each third day
year_thirds <- year_rj %>% 
  filter(yday(ymd(Date)) %% 3 == 0) %>% # THIS FILTERING SHOULD DEPEND ON YEAR: 2007 - 0, 2005 - 1
  left_join(csn_site_index2) %>% select(SiteID:Ym)
saveRDS(year_thirds, "data/year_thirds.rds")
write_rds(year_thirds, "../../gpfs:pace1:/data_2007/Other_data/year_thirds.rds") # for 2007, so ymd(Date) %% 3 == 0


# for 2005: 
year_thirds <- year_rj %>% 
  filter(yday(ymd(Date)) %% 3 == 1) %>% 
  left_join(csn_site_index2) %>% select(SiteID:Ym)
saveRDS(year_thirds, "../../gpfs:pace1:/data_2005/Other_data/year_thirds.rds")



####################################################################
# part (3) of new_rj_script
####################################################################


#' Extract and format Sunni's Rj values for a source
#' 
#' @note Assumes a workind directory of "hybridSA"
#' 
#' @param source_name name of the source for which to extract Rj values from 
#'  the ST domain
suni_source <- function(source_name) {
  
  message("Reading in Rj file")
  suni_rjs <- readRDS("../../gpfs:pace1:/data_2006/Rjs2006.rds") %>% 
    set_names(sources)
  suni_df <- suni_rjs %>% extract2(source_name)
  rm(suni_rjs)
  
  message("completing Sunni's file conversion...")
  suni_df2 <- suni_df %>% {
    num_days <- dim(.)[3]
    dimnames(.)[[3]] <- as.character(ymd("2005-12-31") + days(1:num_days))
    . } %>% 
    adply(3, slice_to_df) %>% 
    set_names(c("Date", "SiteInd", "Sunni_rj")) %>% 
    left_join(as.data.frame(xxx)) %>% 
    mutate(Date = as.character(Date)) %>% 
    arrange(SiteInd, Date)
  
  suni_df2
}

setL <- list(cn_max = 1e10)

#' ST interpolation of rj values Nabil calculated
nabil_source <- function(source_name) {
  
  message("Pre-processing...")
  if(class(concen_agg2) == "data.frame") concen_agg2 <- concen_agg2 %>% to_sitedatelist
  if(class(new_out2) == "data.frame") new_out2 <- new_out2 %>% to_sitedatelist
  
  # these three parts should be made into a function
  # inc_sitedays <- site_inclusion_df(year_thirds, concen_agg2, new_out2)
  
  # subset for appropriate source 
  source_data_thirds <- year_thirds %>% 
    filter(Source == source_name) %>% 
    # semi_join(inc_sitedays) %>% 
    dlply(.(Date))
  
  message("Spatially interpolating...")
  source_kriged <- source_data_thirds %>% 
    llply(make_spdf) %>% 
    llply(spatial_interp)
  
  # combine all spatially-kriged results
  df_preds <- source_kriged %>% 
    llply( get_preds ) %>% 
    ldply(.id="Date") %>% 
    right_join(xxx %>% as.data.frame) %>% 
    mutate(Date = as.character(Date))
  
  message("temporally interpolating...")
  df_preds2 <- df_preds %>% gen_full_spt_dom %>% ddply(.(SiteInd), interpol)
  
  df_preds2
}


#' Combines Rj values for Nabil and Sunni
#' 
#' 
#' @param src_name name of the source
both_sources <- function(src_name) {
  
  message("Starting rj value collection...")
  suni_rj <- suni_source(src_name)
  nabil_rj <- nabil_source(src_name)
  
  message("matching corresponding data points...")
  nabil_rj2 <- nabil_rj %>% semi_join(suni_rj)
  nabil_rj2[is.na(nabil_rj2$Rj_vals), "Rj_vals"] <- 0
  rm(nabil_rj) # save space
  
  # return results
  nabil_rj2 %>% inner_join(suni_rj)
}


## begin combining all Rj values for the year

all_rj2006 <- vector("list", length = length(sources)) %>% set_names(sources)

start_time <- proc.time()
# loop through all sources
for(k in 1:length(sources)) {
  
  both_df <- both_sources(sources[k])
  correl <- with(both_df, cor(Rj_vals, Sunni_rj))
  
  all_rj2006[[k]] <- both_df %>% rename(Values = Rj_vals) %>% 
    daply(.(Date), df_to_slice) %>% aperm(c(2, 3, 1)) 
  
  attr(all_rj2006[[k]], "correl") <- correl
  beep()
}
end_time <- proc.time()



################################################
# for 2005

# .sources <- sources # for 2005
.sources <- names05 # for 2007

# renamed from: all_rj2005
all_rj2005 <- vector("list", length = length(.sources)) %>% set_names(.sources) 


# loop through all sources
system.time({
  
  for(k in 1:length(.sources)) {
    
    bysource_df <- nabil_source(.sources[k])
    
    all_rj2005[[k]] <- bysource_df %>% rename(Values = Rj_vals) %>% 
      daply(.(Date), df_to_slice) %>% aperm(c(2, 3, 1)) 
   
    rm(bysource_df)
    beep()
  }
  beep(8)
})

write_rds(all_rj2005, "../../gpfs:pace1:/data_2005/Other_data/rj_st_interp2005.rds")

################################################








so_data <- year_thirds %>% 
  filter(Source == "BIOG") %>% 
  dlply(.(Date)) %>% 
  extract2(45)  # 36, 42: fixed, 43: not fixed

# long-term solution: find which csn sites are too close to prediction locations
# to have kriging performed. Then if there is a singularity, remove that 
# close location from the set of observations


locs <- spDists( 
  as.matrix(as.data.frame(so_data)[, c("LON", "LAT")]), 
  as.matrix(as.data.frame(xxx)[, c("LON", "LAT")]),
  longlat = TRUE
  )

# remove five rows closest to points in spatial grid
close_rows <- (order(locs) %% nrow(locs))[1:4]

so_data %>% 
  # extract(-close_rows, ) %>% 
  make_spdf %>% 
  ext_autoKrige(Rj_vals ~ 1, ., new_dat = xxx, model="Exp") %>% 
  str

# here, no singularity if the four closest are removed, but there is if the 
# fourth closest is included. Here are the closest (euclidean) distances between 
# training and prediction locations:
locs[order(locs)][1:5]

# generalizing, let's find the sites that are closest to the prediction 
# locations

csn_dists <- spDists( 
  as.matrix(csn_site_index[, c("LON", "LAT")]), 
  as.matrix(as.data.frame(xxx)[, c("LON", "LAT")]),
  longlat = TRUE
)

# diagnostics
csn_dists %>% str # 188 rows
csn_dists[ order(csn_dists) ][1:10] # 10 closest distances
close_csn <- ( order(csn_dists) %% nrow(csn_site_index) )[1:6] 

close_ids <- csn_site_index[c(close_csn, 188), "SiteID"]

# tidy up
rm(locs, csn_dists, close_ids, close_csn, close_rows, setL)

# this works, and is generalizable
so_data %>% 
  filter(!(SiteID %in% close_ids)) %>% 
  make_spdf %>% 
  autoKrige(Rj_vals ~ 1, ., new_data = xxx, model="Exp") %>% 
  str


# still not able to krige all of them. 

# but, if I set an upper bound for the condition number, then it works
all_rj2006 %>% sapply(function(x) attr(x, "correl")) %>% round(4) %>% 
  as.data.frame() %>% add_rownames() %>% set_names(c("Source", "Correlation"))

saveRDS(all_rj2006, "../../gpfs:pace1:/data_2006/Other_data/rj_st_interp2006.rds")


all_rj2005 <- all_rj2006
saveRDS(all_rj2005, "../../gpfs:pace1:/data_2005/Other_data/rj_st_interp2005.rds")

write_rds(all_rj2007, "../../gpfs:pace1:/data_2007/Other_data/rj_st_interp2007.rds")



####################################################################
# part (4): Calculate Updated SA values and Generate csv files of PM2.5 Source 
#           Impact Fields
####################################################################

# crit_rj2006 <- readRDS("data/rj_st_interp2006.rds")
crit2 <- readRDS("../../gpfs:pace1:/data_2006/Other_data/rj_st_interp2006.rds")
pm_rds_path <- "../../gpfs:pace1:/data_2006/rds_files_2006/PM25_fixed.rds"
rev_simp_dest_path <- "../../gpfs:pace1:/rev_rj"   # for revised source impacts


crit2 <- readRDS("../../gpfs:pace1:/data_2005/Other_data/rj_st_interp2005.rds")
pm_rds_path <- "../../gpfs:pace1:/data_2005/rds_files_2005/PM25.rds"
rev_simp_dest_path <- "../../gpfs:pace1:/rev_rj05"


crit2 <- readRDS("../../gpfs:pace1:/data_2007/Other_data/rj_st_interp2007.rds")
pm_rds_path <- "../../gpfs:pace1:/data_2007/rds_files_2007/PM25_fixed.rds"
rev_simp_dest_path <- "../../gpfs:pace1:/rev_rj07"


revise_pm_fields(pm_rds_path, rev_simp_dest_path, yr = "2005", .sources = .sources) # ~ 45 min



#' 
#' 
#' NB: This function has side effects, and output are not small files.
#' 
#' @param pm_path file path of original CMAQ PM2.5 source impacts
#' @param dest_path_base file path of folder in which to place the csv files of 
#'  output
#'  @param yr year corresponding to the data
#'  
#'  @details previously, there were issues using readr::write_csv instead of 
#'    utils::write.csv, because there was loss of precision with the former, 
#'    resulting in many Rj values being rounded down to zero. So although with 
#'    these files, write_csv was about 11x faster, readRDS is about 3x faster 
#'    than write_csv, and without the loss of precision issues.
#'  
#'  @importFrom dplyr mutate left_join select set_names rename 
#'    inner_join 
#'  @importFrom magrittr %>% extract set_names
#'  @importFrom beepr beep
#'  @importFrom assertthat assert_that
#'  @importFrom stringr str_c
#' 
#' @references Hu et al., 2014, p. 5420
#' 
revise_pm_fields <- function(pm_path, dest_path, yr, .sources = sources) {
  
  pm <- readRDS(pm_path) %>% extract(-1) %>% set_names(.sources)
  assert_that( length(pm) == length(.sources) ) # should work from naming
  on.exit( rm(pm) )
  
  for(m in seq_along(.sources)){
    
    # load and combine data
    pm_df <- pm[[ m ]] %>% dcube_to_df(yr = yr) %>% rename(Source_Impacts = Values)
    rj_df <- crit2[[ m ]] %>% dcube_to_df(yr = yr) %>% rename(Rj_vals = Values)
    grid_centers <- make_grid() %>% select(SiteInd:LAT) %>% round(4) 
    comb_res <- inner_join(pm_df, rj_df)
    
    # calculate revised impacts
    comb_res <- comb_res %>% 
      mutate(Revised_impacts = Source_Impacts * Rj_vals) %>% 
      mutate( Source = names(pm)[m] ) %>% 
      left_join(grid_centers)
    
    # store results
    if( !file.exists(dest_path) ) dir.create(dest_path)
    source_dest <- str_c(dest_path, "/revised_impacts_", yr, "_pm25_", 
                         names(pm)[ m ], ".rds")
    comb_res %>% tbl_df %>% saveRDS(source_dest)
    message(sprintf("Source %s completed", .sources[m])) 
  }
 
  # beep()
}


###########

# save to rish_rev*

revise_pm_fields(pm_rds_path, rev_simp_dest_path, yr = "2005") # ~ 45 min

# for 2006
for(fname in dir("../../gpfs:pace1:/rev_rj", full=T)) {
  aa <- readRDS(fname)
  aa2 <- aa %>% filter(Rj_vals > .1, Source_Impacts > 0); 
  newname <- paste0("../../gpfs:pace1:/rish_rev/", 
                    paste0(str_sub(basename(fname), 1, -4), "csv")) 
  print(summary(aa2)); sprintf("Name is: %s", newname)
  write_csv(aa2, newname)
  }

aa <- readRDS(dir("../../gpfs:pace1:/rev_rj05/", full=T)[1])

# for 2005
for(fname in dir("../../gpfs:pace1:/rev_rj05", full=T)) {
  aa <- readRDS(fname) %>% tbl_df
  aa2 <- aa %>% group_by(Date) %>% 
    mutate(Source_Impacts = round(Source_Impacts, 8), scaled = scale(Source_Impacts)) %>% 
    filter(abs(scaled) < 2.9, Rj_vals > .1, Source_Impacts > 0)
  
  newname <- paste0("../../gpfs:pace1:/rish_rev05/", 
                    paste0(str_sub(basename(fname), 1, -4), "csv")) 
  print(summary(aa2)); message(sprintf("Name is: %s", newname))
  write_csv(aa2, newname)
}

# for 2007
# NB: there are three days with all NA values: Jan 1-2, and Dec 30
val <- 2.5 
# ORDIE and ORGAS have max values over 1000, so those have to be manually adjusted also
for(fname in dir("../../gpfs:pace1:/rev_rj07", full=T)[1:4]) {
  # processed with val = 2.5
  aa <- readRDS(fname) %>% tbl_df
  aa2 <- aa %>% group_by(Date) %>%
    mutate(Source_Impacts = round(Source_Impacts, 8), scaled = scale(Source_Impacts)) %>%
    filter(abs(scaled) < val, Rj_vals > 0, Source_Impacts > 0)
  
  if(unique(aa$Source) %in% c("ORGAS", "ORDIE")) {
    aa2 <- aa2 %>% filter( !(Date %in% c("2007-06-28", "2007-06-29")) )
  }
  
  newname <- paste0("../../gpfs:pace1:/rish_rev07/",
                    paste0(str_sub(basename(fname), 1, -4), "csv"))
  print(summary(aa2)); message(sprintf("Name is: %s", newname))
  
  # # percentage significantly negative
  # ((aa %>% filter(!is.na(Rj_vals)))$Revised_impacts < 0) %>% sum  %>% divide_by(nrow(aa))
  # 
  # # percentage of missing Rj values
  # aa %>% filter(is.na(Rj_vals)) %>% nrow %>% divide_by(nrow(aa))
  
  write_csv(aa2, newname)
}




# For both 2005 and 2006, the sig_lnr06 was used for the Rj uncertainties, 
# despite that the order of their sources differed. PROBLEM (see dataframe belo)

yy <- readRDS("../../gpfs:pace1:/data_2005/rds_files_2005/Ag.rds")
names05 <- dimnames(yy)[[1]][-1] # THIS SHOULD BE .sources FOR 2005
data.frame(
  s06 = sources, 
  s05 = names05,
  sig_lnr = sig_lnr06
) -> early_ordering
# names05 <- as.character(early_ordering$s05)  
rm(yy)


zz <- readRDS("../../gpfs:pace1:/data_2007/rds_files_2007/Ag_fixed.rds")
# NAMES FOR 2007 DDM; WHICH ONE???
data.frame(
  s051 = names(zz)[2:17], # HERE, USE THESE NAMES FOR gen_sens_mats
  s052 = unname(sources07)
)
names07 <- dimnames(zz)[[1]][2:17]


# RJ uncertainties 

library(readxl)
rj_uncs <- read_excel("../../gpfs:pace1:/data_2007/SigmaRj_si_11202015.xls", 
                 sheet = "sigma_rjs_scape2007", col_names = FALSE)
library(readr) 
sources07

rj_uncs07 <- rj_uncs %>% 
  set_colnames(c("sig_lnr07", "orig_sources")) %>% 
  mutate(Source = unname(sources07[match(orig_sources, names(sources07))])) %>% 
  select(Source, sig_lnr07, everything())


# TO CONVERT FROM StateCountySite TO SiteID

dest <- "../../gpfs:pace1:/data_2007/Other_data/year_aqs.rds"
bb <- readRDS(dest)
bb %>% 
  separate(StateCountySite, c("State", "County", "Site"), sep = "-") %>% 
  mutate(
    State = str_pad(State, 2, side="left", pad="0"), 
    County = str_pad(County, 3, side="left", pad="0"),  
    Site = str_pad(Site, 4, side="left", pad="0")
    ) %>% 
  unite(SiteID, State:Site, sep="") %>% 
  write_rds(dest)
rm(dest)


#' Convert From StateCountySite to SiteID
#' 
#' @param df a dataframe
from_statecountysite <- function(df) {
  
  stopifnot("StateCountySite" %in% names(df))
  
  df2 <- df %>% 
    separate(StateCountySite, c("State", "County", "Site"), sep = "-") %>% 
    mutate(
      State  = str_pad(State, 2, side="left", pad="0"), 
      County = str_pad(County, 3, side="left", pad="0"),  
      Site   = str_pad(Site, 4, side="left", pad="0")
    ) %>% 
    unite(SiteID, State:Site, sep="")
    
  df2
}


aqs07 <- readRDS("../../gpfs:pace1:/data_2007/Other_data/year_aqs.rds")

# retain only complete sitedates

aqs07 %>% 
  group_by(SiteID, Date) %>% 
  mutate(len = length(Species)) %>% 
  filter(len >= 41)

