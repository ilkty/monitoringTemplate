Packages <- c("readxl", "dplyr","timetk", "ggplot2", "dygraphs")
lapply(Packages, library, character.only = TRUE)

#setwd("C:/Users/User/Documents/montoring_template")
setwd("//172.32.1.177/공용/0 Public_Land/99.Working Room/99.Working Room(LDI사업본부)/TAA(Public)/서승빈/001 LDI day materials")

# 1-1 PMI -----------------------------------------------------------------------------
PMI <- read_excel("주식모니터링_data.xlsx", sheet = "1-1", col_names = FALSE, skip = 6, col_types = c("date", rep("numeric",8))) %>% na.omit %>% tk_xts()
PMI_header <- read_excel("주식모니터링_data.xlsx", sheet = "1-1", col_names = FALSE, range = cell_rows(3))
PMI_header <- replace(PMI_header,  1, values = "date")
colnames(PMI) <- PMI_header[,-1]
# 1-2 ESI -----------------------------------------------------------------------------
ESI <- read_excel("주식모니터링_data.xlsx", sheet = "1-2", col_names = FALSE, skip = 6, col_types = c("date", rep("numeric",7))) %>% na.omit %>% tk_xts()
ESI_header <- read_excel("주식모니터링_data.xlsx", sheet = "1-2", col_names = FALSE, range = cell_rows(3))
ESI_header <- replace(ESI_header,  1, values = "date")
colnames(ESI) <- ESI_header[,-1]
# 1-3 GDP forecast -----------------------------------------------------------------------------
GDP <- read_excel("주식모니터링_data.xlsx", sheet = "1-3", col_names = FALSE, skip = 6, col_types = c("date", rep("numeric",6))) %>% na.omit %>% tk_xts()
GDP_header <- read_excel("주식모니터링_data.xlsx", sheet = "1-3", col_names = FALSE, range = cell_rows(3))
GDP_header <- replace(GDP_header,  1, values = "date")
colnames(GDP) <- GDP_header[,-1]

# 2-1 Global Liquidity  -----------------------------------------------------------------------------
lqdty <- read_excel("주식모니터링_data.xlsx", sheet = "2-1", col_names = FALSE, skip = 6, col_types = c("date", rep("numeric",7))) %>% na.omit %>% tk_xts()
lqdty_header <- read_excel("주식모니터링_data.xlsx", sheet = "2-1", col_names = FALSE, range = cell_rows(3))
lqdty_header <- replace(lqdty_header,  1, values = "date")
colnames(lqdty) <- lqdty_header[,-1]

# 3-1 Earnings  -----------------------------------------------------------------------------
pe <- read_excel("주식모니터링_data.xlsx", sheet = "3-2", col_names = FALSE, skip = 6, col_types = c("date", rep("numeric",16))) %>% na.omit %>% tk_xts()
pe_header <- read_excel("주식모니터링_data.xlsx", sheet = "3-2", col_names = FALSE, range = cell_rows(3))
pe_header <- replace(pe_header,  1, values = "date")
colnames(pe) <- pe_header[,-1]

# 5-1 Global stock Rotation -----------------------------------------------------------------------------
Global_Index <- read_excel("주식모니터링_data.xlsx", sheet = "5", col_names = FALSE, skip = 16, col_types = c("date", rep("numeric",8))) %>% na.omit %>% tk_xts()
Global_Index_header <- read_excel("주식모니터링_data.xlsx", sheet = "5", col_names = FALSE, range = cell_rows(10))
Global_Index_header <- replace(Global_Index_header,  1, values = "date")
colnames(Global_Index) <- Global_Index_header[,-1]

# Glbal Stock PAIRS -----------------------------------------------------------------------------
# Global_equity <- read_excel("Global_indices_Spread.xlsx", sheet = "global_index", col_names = FALSE, skip = 16, col_types = c("date", rep("numeric",8))) %>% na.omit %>% tk_xts()
# Global_equity_header <- read_excel("Global_indices_Spread.xlsx", sheet = "global_index", col_names = FALSE, range = cell_rows(10))
# Global_equity_header <- replace(Global_equity_header,  1, values = "date")
# colnames(Global_equity) <- Global_equity_header[,-1]



# Save R data -----------------------------------------------------------------------------
saveRDS(PMI, file = "C:/Users/User/Documents/montoring_template/PMI.RDS")
saveRDS(ESI, file = "C:/Users/User/Documents/montoring_template/ESI.RDS")
saveRDS(GDP, file = "C:/Users/User/Documents/montoring_template/GDP.RDS")
saveRDS(Global_Index, file = "C:/Users/User/Documents/montoring_template/Global_Index.RDS")
saveRDS(lqdty, file = "C:/Users/User/Documents/montoring_template/lqdty.RDS")
saveRDS(pe, file = "C:/Users/User/Documents/montoring_template/pe.RDS")
#saveRDS(Global_equity, file = "C:/Users/User/Documents/montoring_template/Global_equity.RDS")

a <- readRDS("C:/Users/User/Documents/montoring_template/PMI.RDS")

set.seed(1)
n <- 10
x <- runif(n)
y <- runif(n)
p <- cbind(x,y)
xlim <- c(min(x) - 0.1*diff(range(x)), c(max(x) + 0.1*diff(range(x))))
ylim <- c(min(y) - 0.1*diff(range(y)), c(max(y) + 0.1*diff(range(y))))
plot(p, xlim=xlim, ylim=ylim)
text(p, labels=seq(n), pos=3)
xspline(x, y, shape = c(0,rep(-1, 10-2),0), border="red")
