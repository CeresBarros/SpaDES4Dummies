## Accessory functions to speciesTempLM module

#' Accessory function to speciesTempLM module that calculates a
#'  linear regression between species abundances and temperature
#'
#' @param Data a data.frame or data.table that contains an \code{abund}
#'   column and a \code{temp} column with abundance and temperature values 
#'   in each location, respectively.
#' @return a linear model (\code{lm}) object fitted with the formula:
#'  \code{abund ~ temp}

linearModel <- function(Data){
  return(lm1 <- lm(abund ~ temp, data = Data))
}


#' Accessory function to speciesTempLM module that plots the
#'  fitted linear regression between species abundances and temperature
#'
#' @param Data a data.frame or data.table that contains an \code{abund}
#'   column and a \code{temp} column with abundance and temperature values 
#'   in each location, respectively.
#' @param model a linear model (\code{lm}) object fitted with the formula:
#'  \code{abund ~ temp}
#' @return a plot

plotLMResults <- function(Data, model){
  plot(Data$abund ~ Data$temp, xlab = "Temp.", ylab = "Species abundance",
       main = paste("From years", min(Data$year), "to", max(Data$year), sep = " "))
  abline(a = model$coefficients["(Intercept)"], b = model$coefficients["temp"], lwd = 2, col = "blue")
}