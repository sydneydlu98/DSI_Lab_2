#' Boxplot of Payments by DRG code
#'
#' This function produces a boxplot of payments by DRG code.
#'
#' @param data a data frame
#' @param type a string name for the type of payments
#'
#' @return A box plot of payments by DRG code
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom dplyr mutate
#'
#' @examples
#' library(readr)
#' drg_data <- read_csv("DRG_data.csv")
#'
#' payments_by_drg_code(drg_data, "Average Medicare Payments")
#'
payments_by_drg_code <- function(data, type){
  # add conditional execution
  if(!is.element(type,
                 c("Average Covered Changes",
                   "Average Total Payments",
                   "Average Medicare Payments"))){
    stop("Argument type should be either 'Average Covered Changes', 'Average Total Payments', 'Average Medicare Payments'.")
  }
  data %>%
    ## create a new variable with drg code
    mutate(drg_code = substr(`DRG Definition`, 1, 3)) %>%
    ggplot(aes(x = get(type), ## make the box plot by using geom_boxplot
               y = drg_code,
               color = drg_code)) +
    geom_boxplot(outlier.shape = 0.2,
                 outlier.size = 0.2) +
    ## make the outliers smaller so the plot is more aesthetically pleasing
    theme(legend.position = "none") + ## remove the legend
    theme(plot.title = element_text(hjust = 0.5)) + ## center the plot title
    labs(title = "Boxplot of Payments by DRG code",
         x = "Average Medicare Payments",
         y = "DRG code") ## change the names for title and x,y labels
}

#' Statistics for all of the DRG codes for average Medicare payments
#'
#' This function calculates the mean, median, standard deviation.
#'
#' @param data a dataframe
#' @param statistics a string name for the calculation
#'
#' @return Mean, median or standard deviation for average medicare payments
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot
#' @import dplyr
#'
#' @examples
#' library(readr)
#' drg_data <- read_csv("DRG_data.csv")
#'
#' statistics_of_drg_code(drg_data, "mean")
#'
statistics_of_drg_code <- function(data, statistics){
  # add conditional execution
  if(!is.element(statistics,
                 c("mean",
                   "median",
                   "standard_deviation"))){
    stop("Argument statistics should be either 'mean', 'median', 'standard_deviation'.")
  }
  data %>%
    ## group the variable "DRG Definition"
    group_by(`DRG Definition`) %>%
    ## calculate the mean, median and standard deviation for "Average Medicare Payments"
    summarise(mean = mean(`Average Medicare Payments`),
              median = median(`Average Medicare Payments`),
              standard_deviation = sd(`Average Medicare Payments`)) %>%
    select(`DRG Definition`, statistics) %>%
    ## change the number of decimal places for the results in the tibble
    mutate_if(is.numeric, scales::number, accuracy = 0.01)
}

