Packages <- c("quantmod", "doParallel", "foreach", "urca", "dplyr", "ggplot2", "aTSA")
lapply(Packages, library, character.only = TRUE)

setwd("C:/Users/User/Documents/montoring_template")

Sys.setenv(TZ = "UTC")

# Load data
global_equity <- readRDS("C:/Users/User/Documents/montoring_template/Global_equity.RDS")

###########################
# Constructing the spread #
###########################
# Function to calculate the spread
calculate_spread <- function(x, y, beta) {
  return(y - beta * x)
}
# Function to calculate the beta and level
# given start and end dates
calculate_beta_and_level <- function(x, y,
                                     start_date, end_date) {
  require(xts)
  
  time_range <- paste(start_date, "::", end_date, sep = "")
  x <- x[time_range]
  y <- y[time_range]
  
  dx <- diff(x[time_range])
  dy <- diff(y[time_range])
  r <- prcomp( ~ dx + dy)
  
  beta <- r$rotation[2, 1] / r$rotation[1, 1]
  spread <- calculate_spread(x, y, beta)
  names(spread) <- "spread"
  level <- mean(spread, na.rm = TRUE)
  
  outL <- list()
  outL$spread <- spread
  outL$beta <- beta
  outL$level <- level
  
  return(outL)
}

# Function to calculate buy and sell signals
# with upper and lower threshold
calculate_buy_sell_signals <- function(spread, beta,
                                       level, lower_threshold, upper_threshold) {
  
  buy_signals <- ifelse(spread <= level -
                          lower_threshold, 1, 0)
  sell_signals <- ifelse(spread >= level +
                           upper_threshold, 1, 0)
  
  # bind these vectors into a matrix
  output <- cbind(spread, buy_signals,
                  sell_signals)
  colnames(output) <- c("spread", "buy_signals",
                        "sell_signals")
  
  return(output)
}

# Function that we will use to calculate betas
run_regression <- function(dF) {
  return(coef(lm(y ~ x - 1, data = as.data.frame(dF))))
}

rolling_beta <- function(z, width) {
  rollapply(z, width = width, FUN = run_regression,
            by.column = FALSE, align = "right")
}
############################################################################################

####################################
# Signal generation and validation #
####################################
# Rolling window of trading days
window_length <- 12

# Time range
start_date <- "2015-01-01"
end_date <- "2018-12-31"
range <- paste(start_date, "::", end_date, sep = "")

# Our stock pair
x <- global_equity[,1][range]
y <- global_equity[,6][range]

dF <- cbind(x, y)
names(dF) <- c("x", "y")

betas <- rolling_beta(diff(dF), window_length)
data <- merge(betas, dF)
data$spread <- data$y - stats::lag(betas, 1) * data$x

returns <- diff(dF) / dF
return_beta <- rolling_beta(returns, window_length)
data$spreadR <- diff(data$y) / data$y - return_beta * diff(data$x) / data$x

tail(data)
##            betas    x      y      spread     spreadR
## 2011-12-22 2.770586 119.60 383.07 138.70795 -0.002322110
## 2011-12-23 3.094533 120.67 387.66  53.33343  0.003311904
## 2011-12-27 3.450416 120.76 390.74  17.04417  0.007083611
## 2011-12-28 3.364819 119.18 387.00 -24.22055  0.004194527
## 2011-12-29 3.004804 120.41 389.38 -15.77781 -0.003361064

threshold <- sd(data$spread, na.rm = TRUE)

threshold
## [1] 143.7734

plot(data$spread, main = "Spread",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
abline(h = threshold, lty = 2)
abline(h = -threshold, lty = 2)


# Construct the out of sample spread

window_length <- 12

# Time range
start_date <- "2019-01-01"
end_date <- "2020-12-31"
range <- paste(start_date, "::",
               end_date, sep = "")

# Our stock pair
x <- global_equity[,1][range]
y <- global_equity[,6][range]


# Bind these together into a matrix
dF <- cbind(x, y)
names(dF) <- c("x", "y")

# Calculate the out of sample rolling beta
beta_out_of_sample <- rolling_beta(diff(dF), window_length)

# Buy and sell threshold
data_out <- merge(beta_out_of_sample, dF)
#data_out$spread <- data_out$y - stats::lag(beta_out_of_sample, 1) * data_out$x
data_out$spread <- data_out$y - beta_out_of_sample * data_out$x

# Plot the spread with in-sample bands
plot(data_out$spread, main = "Spread",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
abline(h = threshold, lwd = 2)
abline(h = -threshold, lwd = 2)

# Generate sell and buy signals
buys <- ifelse(data_out$spread > threshold, 1, 0)
sells <- ifelse(data_out$spread < -threshold, -1, 0)
data_out$signal <- buys + sells

point_type <- rep(NA, nrow(data_out))
buy_index <- which(data_out$signal == 1)
sell_index <- which(data_out$signal == -1)

point_type[buy_index] <- 22
point_type[sell_index] <- 24
points(data_out$spread, pch = point_type)

num_of_buy_signals <- sum(buys, na.rm = TRUE)
num_of_sell_signals <- sum(abs(sells), na.rm = TRUE)

num_of_buy_signals
num_of_sell_signals


######################
# Trading the spread #
######################
##     beta_out_of_sample     x      y    spread signal
## 2011-01-13         NA 128.37 345.68        NA     NA
## 2011-01-14  1.7511157 129.30 348.48        NA     NA
## 2011-01-18  1.1630714 129.52 340.65 113.84550      1
## 2011-01-19  1.2803161 128.25 338.84 189.67609      1
## 2011-01-20  1.2286891 128.08 332.68 168.69711      1
## 2011-01-21  0.8045108 128.37 326.72 168.99319      1
## 2011-01-24  2.4936855 129.10 337.45 233.58766      1
## 2011-01-25  2.7762163 129.17 341.40  19.29065      0
## 2011-01-26  3.0802946 129.67 343.85 -16.14196      0

prev_x_qty <- 0
position <- 0
trade_size <- 100
signal <- as.numeric(data_out$signal)
signal[is.na(signal)] <- 0
beta <- as.numeric(data_out$beta_out_of_sample)

qty_x <- rep(0, length(signal))
qty_y <- rep(0, length(signal))

for(i in 1:length(signal)) {
  if(signal[i] == 1 && position == 0) {
    # buy the spread
    prev_x_qty <- round(beta[i] * trade_size)
    qty_x[i] <- -prev_x_qty
    qty_y[i] <- trade_size
    position <- 1
  }
  
  if(signal[i] == -1 && position == 0) {
    # sell the spread initially
    prev_x_qty <- round(beta[i] * trade_size)
    qty_x[i] <- prev_x_qty
    qty_y[i] <- -trade_size
    position <- -1
  }
  
  if(signal[i] == 1 && position == -1) {
    # we are short the spread and need to buy
    qty_x[i] <- -(round(beta[i] * trade_size) +
                    prev_x_qty)
    prev_x_qty <- round(beta[i] * trade_size)
    qty_y[i] <- 2 * trade_size
    position <- 1
  }
  
  if(signal[i] == -1 && position == 1) {
    # we are long the spread and need to sell
    qty_x[i] <- round(beta[i] * trade_size) + prev_x_qty
    prev_x_qty <- round(beta[i] * trade_size)
    qty_y[i] <- -2 * trade_size
    position <- -1
  }
}

qty_x[length(qty_x)] <- -sum(qty_x)
qty_y[length(qty_y)] <- -sum(qty_y)

data_out$qty_x <- qty_x
data_out$qty_y <- qty_y

data_out[1:3, ]
##  beta_out_of_sample        x      y  spread signal qty_x qty_y
## 2012-01-17  2.1511279 123.48 408.20     NA     NA    0     0
## 2012-01-18  2.5890817 124.85 412.44  143.87168  1 -259   100
## 2012-01-19  2.0711505 125.51 411.13   86.17435  0    0     0

tail(data_out, 3)
##   beta_out_of_sample       x      y  spread signal qty_x qty_y
## 2012-12-27  6.5051194 138.15 499.45 -404.90307 -1    0     0
## 2012-12-28  5.6770827 136.66 494.14 -394.84962 -1    0     0
## 2012-12-31  6.3934172 138.98 516.04 -272.96095 -1 -668   100

# function for computing the equity curve
compute_equity_curve <- function(qty, price) {
  
  cash_buy <- ifelse(sign(qty) == 1,
                     qty * price, 0)
  cash_sell <- ifelse(sign(qty) == -1,
                      -qty * price, 0)
  position <- cumsum(qty)
  cumulative_buy <- cumsum(cash_buy)
  cumulative_sell <- cumsum(cash_sell)
  
  equity <- cumulative_sell - cumulative_buy +
    position * price
  return(equity)
}

# Add the equity curve columns to the data_out table
data_out$equity_curve_x <- compute_equity_curve(
  data_out$qty_x, data_out$x)
data_out$equity_curve_y <- compute_equity_curve(
  data_out$qty_y, data_out$y)

plot(data_out$equity_curve_x +
       data_out$equity_curve_y, type = 'l',
     main = "out sample_spread", ylab = "P&L",
     cex.main = 0.8,
     cex.axis = 0.8,
     cex.lab = 0.8)

############################
# More on the equity curve #
############################
# Calculates the Sharpe ratio
sharpe_ratio <- function(x, rf) {
  sharpe <- (mean(x, na.rm = TRUE) - rf) /
    sd(x, na.rm = TRUE)
  return(sharpe)
}

# Calculates the maximum drawdown profile
drawdown <- function(x) {
  cummax(x) - x
}

par(mfrow = c(2, 1))
equity_curve <- data_out$equity_curve_x + data_out$equity_curve_y

plot(equity_curve, main = "Equity Curve",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)

plot(drawdown(equity_curve), main = "Drawdown of equity curve",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)

equity <- as.numeric(equity_curve[, 1])
equity_curve_returns <- diff(equity) / equity[-length(equity)]

# Remove any infinities and NaN
invalid_values <- is.infinite(equity_curve_returns)| is.nan(equity_curve_returns)

sharpe_ratio(equity_curve_returns[!invalid_values], 0.03)
#[1] 0.0658528

omega_ratio <- function(r, T) {
  omega <- mean(pmax(r - T, 0)) / mean(pmax(T - r, 0))
  return(omega)
}

#######################
# Strategy attributes #
#######################
# Find out where the trades occur
trade_dates <- data_out$qty_x[data_out$qty_x != 0]
#trade_dates <- data_out$qty_y[data_out$qty_y != 0]

# The trade_dates object is an xts object whose index
# contains the necessary time information
duration <- as.numeric(diff(index(trade_dates)))

# Summary statistics
summary(duration)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## 1.00   13.50   21.00   31.84   44.00  128.00

# Histogram of trade duration
hist(duration, breaks = 20,
     main = "Histogram of trade durations",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)

