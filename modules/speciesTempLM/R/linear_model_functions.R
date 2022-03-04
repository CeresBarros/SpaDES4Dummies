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
  lm1 <- lm(abund ~ temp, data = Data)
  return(lm1)
}
