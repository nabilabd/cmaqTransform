

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






