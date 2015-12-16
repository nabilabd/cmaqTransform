
#' Generate daily spatial plots of problematic source impact grid cells
#' 
#' MORE INFO ?
#' 
#' @details simp_range is the value which stores the range of values assumed 
#'  by the CMAQ source impacts, over the whole year. This is visible on 
#'  each day's plot, for context in better understanding that day's values 
#'  within what was attained throughout the year. Also, for a more local 
#'  understanding of the spatial behavior of source impacts, each day's plot 
#'  has 
#' 
#' @param simpacts_df dataframe for the source impacts of a year
#' 
#' @importFrom assertthat assert_that
#' @importFrom plyr d_ply 
#' @importFrom magrittr %>%
#' 
#' @export
year_plot <- function(simpacts_df, yr = "2006") {
  
  assert_that( is.data.frame(simpacts_df) )
  assert_that( all(c("long", "lat", "Date") %in% names(simpacts_df)) )
  
  # define params
  simp_range <- range(simpacts_df$Source_Impacts) %>% round(2)
  doc_path <- sprintf("Extreme_SI_vals/%s/Extreme_%s_ddm_vals.pdf", 
                      yr, unique(simpacts_df$Source))
  simpacts_df <- simpacts_df %>% 
    semi_join( filter(count(simpacts_df, Date), n > 1) )
  
  # doc_path <- "BIOG_rev_impacts.pdf"
  pdf(doc_path, height=6, width=12)
  simpacts_df %>% 
    d_ply(.(Date), day_plot, col_range = simp_range, .print = TRUE)
  dev.off()
}

#' 
#' 
#' @param day_df
#' 
day_plot <- function(day_df, col_range = NULL) {
  
  day_range <- range(day_df$Source_Impacts) %>% round(2)
  if( is.null(col_range) ) col_range <- day_range
  
  plot_title <- sprintf("PM2.5 on %s, Source: %s, Day's Sensitivity Range: %s",
                        unique(day_df$Date), unique(day_df$Source), 
                        paste0("(", day_range[1], ", ", day_range[2], ")") )
  
  # make plot
  res <- states %>% 
    ggplot(aes(long, lat, group = group)) + geom_path() + coord_map() + 
    geom_point(data=day_df, aes(group = NULL), size = 2.3) + 
    geom_point(data=day_df, aes(group = NULL, color = Source_Impacts)) + 
    scale_color_gradient2(limits=col_range, low="red", midpoint=0, high="blue") + 
    ggtitle( plot_title )
  
  res
}

