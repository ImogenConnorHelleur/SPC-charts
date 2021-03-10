#function interface for automated SPC function
#' plot_SPC_auto
#'
#' @param data a dataframe or CSV file in the standard format
#' @param chart_typ the type of chart you wish to plot(e.g. "C", "C'", "P", "P'")
#' @param periodMin the minimum number of points per period. (This will then be fed
#' into the first freeze argument)
#' @param runRuleLength the number of points above or below the centre line needed
#' for a rule 2 break
#' @param baselineWait ??
#' @param part a vector of part point returned from the SPC algorithm (could 
#' happen within the function)
#' @param excule a vector of points to exclude from any limit calculations
#' @param r1_col the colour for points causing a rule 1 break 
#' @param r2_col the colour for points causing a rule 2 break 
#' @param cht_title the main title of the chart (could be extracted from the csv?)
#' @param sub_tile can be used for the place that the data has come from (e.g. 
#' which board)(could be extracted from the csv?)
#' @param plot_chart if true, chart is plotted, if false, table of plot data is returned
#' @param write_table if true, the plot data is written to a folder location 
#' 
#'
#'
#' @return the input df with an extra column containing the corresponding occupancy on arrival for each patient
#'
#'
#' @examples
plot_SPC_auto <- function(data, 
                          chart_typ = "C",
                          periodMin = 21,
                          runRuleLength = 8,
                          baselineWait = 0,
                          part,
                          freeze = c(20, 80),
                          exclude,
                          
                          r1_col = "orange",
                          r2_col = "steelblue3",
                          cht_title = "title",
                          sub_title = "Ayrshire and Arran",
                          plot_chart = T,
                          write_table = F,
                          ...
) {
  
}