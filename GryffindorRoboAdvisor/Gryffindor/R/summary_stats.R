#' @title Simple portfolio summary statistics
#'
#' @description This package summarizes several functions that allow to compute
#' summary statistics for portfolios. It computes the average yearly return, as
#' well as the annual standard deviation and the maximum drawdown. Moreover, the
#' Sharpe Ratio, which is used for portfolio optimization can be calculated.
#'
#' @param backtest A \code{dataframe} with return time series to be evaluated.
#' @param expectedretrun A \code{dataframe} with expected returns to compute the
#' portfolio expected return.
#' @param covmatrix A \code{matrix} containing the variance of the securities and the
#' repective covariances between the stocks on its diagonale
#' @param par A \code{vector} with the initial weigths of the portfolio. When used with optim these
#' paramerter is changed, such that the resulting portfolio has an optimal risk
#' and return trade-off.
#'
#'
# Average return - takes one column as an input
averagereturn <- function(backtest){
    portfoliologreturns <- data.frame()

    for(r in 1:(nrow(backtest) - 1)){
        portfoliologreturns[r, 1] <- log(backtest[r + 1, 1]/backtest[r, 1])
    }

    averagereturn <- mean(portfoliologreturns[, 1]) * 252
    return(averagereturn)
}

# Calculate max drawdown - takes one column as an input
maxdrawdown <- function(backtest){
    trailingmaxdrawdown = data.frame()

    for(r in 1:(nrow(backtest) - 1)){
        trailingmaxdrawdown[r, 1] <- min(tail(backtest[, 1], -r)) /
            backtest[r, ] - 1
    }

    maxdrawdown <- min(trailingmaxdrawdown[, 1])
    return(maxdrawdown)
}

# Calculate annual standard deviation
yearlystd <- function(backtest){
    portfoliologreturns <- data.frame()

    for(r in 1:(nrow(backtest) - 1)){
        portfoliologreturns[r, 1] <- log(backtest[r + 1, 1] / backtest[r, 1])
    }

    yearlystd <- sd(portfoliologreturns[, 1]) * sqrt(252)
    return(yearlystd)
}

# Calculate sharpe Ratio for maximization
sharpe <- function(expectedreturn, covmatrix, par){

    #optim, used, later needs the parameters as vector, but we need it as matrix
    #here - therefore we have to change back and forth
    par <- as.matrix(par)

    par <- rbind(par, 1 - sum(par[, 1]))

    #Calculate Portfolio return
    pfreturn <- t(expectedreturn) %*% par

    #Calculate Portfolio standard deviation
    pfvar <- t(par) %*% covmatrix %*% par

    #Calculate Sharpe Ratio
    sharperatio <- pfreturn / (pfvar ^ 0.5)

    return(sharperatio)
}
