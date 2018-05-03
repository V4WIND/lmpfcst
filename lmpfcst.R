####----
library(haven)
mysasdata = read_sas("ds_seg.sas7bdat")

######----
# Grouping as per User input - for historical view..sequentially for forecast if and when needed
acct_lvl_test<-read.csv(choose.files(), header = TRUE, stringsAsFactors =  FALSE, strip.white =TRUE)

#In the cases where the user uploads a cl_tid level datafile we create aggregate summary as below
sample <- sqldf(
"
select
per_num, round(count(cl_tid),0) as vol, round(sum(close_bal_amt),2) as bal
from acct_lvl_test
group by 1
")

# Take output files seclection of user and export as csv
save_output_csv<-write.csv(sample, file = "I:/Documents/R/sample.csv", row.names = FALSE)

######----
library(shiny)
library(datasets)
library(forecast)

test<-read.csv(choose.files())

test_per<-test[73:120, 1]
test_bal<-test[73:120, 3]
test_vol<-test[73:120, 2]

test_bal_ts<-ts(test_bal, start=min(test_per), end=max(test_per))
test_fit<-auto.arima(test_bal_ts)

u_plot<-plot(forecast(test_fit, h=12))

test_baln<-log10(test_bal)
test_voln<-log10(test_vol)

test_baln_ts<-ts(test_baln, start=min(test_per), end=max(test_per))
test_nfit<-auto.arima(test_baln_ts)

a_plot<-plot(forecast(test_nfit, h=12))

decomp_bal_ts<-ts(test_bal, frequency = 12)
decomp_baln_ts<-ts(test_baln, frequency = 12)

decomp_u <- decompose(test_bal_ts)
decomp_a <- decompose(test_baln_ts)

ets_fit<-ets(test_bal)
ets_fitn<-ets(test_baln)

u_plot1<-plot(forecast(ets_fit, h=12))
a_plot1<-plot(forecast(ets_fitn, h=12))






######----
library(shiny)
library(forecast)
library(shinydashboard)
library(DT)
library(here)

source(here('src', 'helper_functions.R'))
here()

data_master <- read.csv(here("data", "data_master_1.csv"))

# Create initial time series object 
sp500 <- ts(data_master$sp_500, start = c(1995, 1), frequency = 12)

# Create test set
sp500_test <- window(sp500, 2015, c(2015, 12))

fit_arima <- readRDS(here("models", 'arima.rds'))




######----
# Define UI 
ui <- dashboardPage(
  # Application title
  dashboardHeader(title = "Loss Mitigation Treatment Enrollment Dashboard", titleWidth = 450),
  dashboardSidebar(disable = TRUE),
  dashboardBody()
) 