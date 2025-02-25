
#' Snowpack Depth on Forest Floor
#' by Kelsey Warren
#' ESM 262 Computing
#' Feb 20 2025
#'
#' This function determines how much snowpack exists on the forest floor (cm) on a given day in the precipitation year in Lake Tahoe, CA
#' 
#' @param prev_pack snowpack depth remaining on the ground from previous precip years (cm)
#' @param snowfall_rate the average rate of snowfall per day during this snow season (cm/day) (in reference location with no canopy)
#' @param runoff_rate the average rate of runoff that leaves the ecosystem per day during this snow season (cm/day)
#' @param interception average percentage of reported snowfall that is intercepted by forest canopy (% total reported snowfall intercepted)
#' @param day day within the snow season in Tahoe (day 1 is the first day of snow season  which spans Nov 1 to April 30 = 180 days total)
#' 
#'
# function definition
forest_snowpack = function(prev_pack, snowfall_rate, runoff_rate, interception, day) {
  
  # error checking: 
  # make sure that the previous year's snowpack depth is not a negative value. If it is negative, assign prev_pack = NA
  prev_pack = ifelse( (prev_pack<0), return("Caution: previous snowpack depth cannot be negative"), prev_pack)
  
  # warn users if their average rate of snowfall (snowfall_rate) is an abnormally large value
  snowfall_rate = ifelse( (snowfall_rate > 100), return("Caution: abnormally high snowfall rate"), snowfall_rate)
  
  # make sure that runoff rate is a positive value
  runoff_rate = ifelse( (runoff_rate<0), return("Caution: negative runoff rate"), runoff_rate)
  
  # make sure that interception rate is between 0% and 100%
  interception = ifelse( (interception<0), return("Caution: interception rate cannot be negative"), interception)
  interception = ifelse( (interception>1), return("Caution: interception rate greater than 100%"), interception)

  # alert users if their day of the snow season is greater than 180
  day = ifelse((day>=181), NA, day)
  
  
  # calculate the snowpack depth on the forest floor on your chosen day
  snowpack_depth = prev_pack + (snowfall_rate*day) - (runoff_rate*day) - (interception*snowfall_rate*day)
    
  
  return(snowpack_depth)
}

