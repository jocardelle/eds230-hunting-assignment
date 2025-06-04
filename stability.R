#' Checking Lotka-Volterra Model Stability
#'
#' @param data a data frame containing a time series of predator and prey population values outputted from a ODE solver
#' @param prey_thresh the minimum desired prey population
#' @param pred_thresh the minimum desired predator population
#'
#' @returns boolean value of whether the model passes the stability check
#' @export
#'
#' @examples
check_stability <- function(data, prey_thresh = 50, pred_thresh = 50) {
  final_prey <- data %>% 
    filter(species == "prey", 
           time == 100) %>% 
    pull(population)
  
  final_pred <- data %>% 
    filter(species == "pred", 
           time == 100) %>% 
    pull(population)
  
  return(final_prey > prey_thresh && final_pred > pred_thresh)
}