
#' Geographic data on 188 CSN sites
#'
#' A dataset containing information on the geographic location of Chemical 
#' Speciation Network (CSN) sites. More detailed descriptions of the 
#' fields/codes contained in it can be found via the url below (see "source").
#'
#' @format A data frame with 188 rows and 10 columns
#' \describe{
#'   \item{SiteID}{price, in US dollars}
#'   \item{LON}{Longitude of the site}
#'   \item{LAT}{Latitude of the site}
#'   \item{Xm}{weight of the diamond, in carats, given}
#'   \item{Ym}{price, in US dollars, given}
#'   \item{state_ID}{weight of the diamond, in carats}
#'   \item{county_ID}{price, in US dollars}
#'   \item{site.ID}{weight of the diamond, in carats}
#'   \item{i,j}{price, in US dollars}
#' }
#' @source \url{http://www.epa.gov/ttn/airs/airsaqs/manuals/codedescs.htm}
"csn_site_index"


#' Geographic data on 188 CSN sites
#'
#' A named vector of length 41, with the \eqn{\sigma_{SR_i^CTM}} values for 
#' each chemical species for 2006. These are taken to be constant throughout 
#' the year. Note that these values need to be squared before being used in 
#' the hybrid equation.
#'
#' @source \url{http://www.diamondse.info/}
#' @references \url{http://atmos-chem-phys.net/14/5415/2014/} 
"sig_ctm06"

#' Geographic data on 188 CSN sites
#'
#' An (un)named vector of length 20, with the \eqn{\sigma_{ln R_i}} values for 
#' each source for 2006. These are taken to be constant throughout 
#' the year. Note that these values need to be squared before being used in 
#' the hybrid equation.
#'
#' @source \url{http://www.diamondse.info/}
#' @references \url{http://atmos-chem-phys.net/14/5415/2014/} 
"sig_lnr06"

#' Names of point sources of pollution
#'
#' A vector of length 20, with the names of the sources.
#'
#' @source \url{http://www.diamondse.info/}
#' @references \url{http://atmos-chem-phys.net/14/5415/2014/} 
"sources_06"

#' Names of the forty-one chemical species
#'
#' @source \url{http://www.diamondse.info/}
#' @references \url{http://atmos-chem-phys.net/14/5415/2014/} 
"species"
