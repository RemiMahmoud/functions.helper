
#' Evaluate error metrics
#' evaluate_error
#'
#' @param data a dataset with observed and simulated/fitted values
#' @param observed name of the column with observed values
#' @param simulated name of the column with simulated/fitted values
#' @importFrom dplyr select summarise n
#' @importFrom magrittr %>%
#' @importFrom tidyr drop_na
#' @return a dataset with error metrics
#' @export
#' @examples
#'  x <- rnorm(100)
#' d <- data.frame(observed = x, simulated = x + rnorm(100)  )
#'
#' evaluate_error(d)
#'

evaluate_error <- function (data, observed = "observed", simulated = "simulated")
{
  metrics <- data %>% select(observed = observed, simulated = simulated) %>%
    drop_na() %>%
    summarise(n = n(),
              mean_observed = mean(observed),
                            mean_simulated = mean(simulated),
              bias = mean(observed -  simulated),
              bias_squared = bias^2,
              SSE = sum((observed - simulated)^2), MSE = SSE/n, RMSE = MSE^0.5, RRMSE = RMSE/mean_observed,
                            MAE = mean(abs(observed - simulated)), RMAE = MAE/mean(abs(observed)),
                            RMAEP = mean(abs(observed - simulated)/abs(observed)),
                            EF = 1 - sum((observed - simulated)^2)/sum((observed - mean_observed)^2),
              index_willmott = 1 - sum((observed -  simulated)^2)/sum((abs(simulated - mean(observed)) +  abs(observed - mean(observed)))^2),
              SDSD = (sd(simulated) - sd(observed))^2 * (n - 1)/n,
              LCS = 2 * sd(observed) *   sd(simulated) * (1 - cor(observed, simulated)) *(n - 1)/n,
              NU = (1 - (cov(observed, simulated)/var(simulated)))^2 *  var(simulated) * (n - 1)/n,
              LC = (1 - cor(observed,  simulated)^2) * var(observed) * (n - 1)/n,
              r_pearson = cor(simulated, observed, method = "pearson"),
              p_pearson = ifelse(n >  2, cor.test(simulated, observed, method = "pearson")$p.value, NA),
              r_kendall = cor(simulated, observed, method = "kendall"),
              p_kendall = ifelse(n > 2, cor.test(simulated, observed, method = "kendall")$p.value, NA),
              r_squared = cor(simulated, observed, method = "pearson")^2)

    return(metrics)
}
