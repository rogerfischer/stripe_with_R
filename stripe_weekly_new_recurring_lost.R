## By number of payments: stripe_weekly_new_recurring_lost.R
## This R script uses the payments.csv file directly imported from Stripe to your working
## directory and puts out a table with the number of customers 
## It can be found at: 
## 
## Will be followed by:
## By amount of payments: stripe_weekly_new_recurring_lost2.R

## Authors: Roger Fischer, Vitomir Kovanovic


## Set the Working Directory
## Example:
## setwd("/Users/rogerfischer/R/kaywa/public")

## Load all data from CSV file payments.csv into a data.frame
raw_data <- read.csv("payments.csv", stringsAsFactors=FALSE)

## Create a new data frame with a subset of the raw_data data frame
## This is just to make it easier to see only columns that we are interested in
data <- raw_data[, c("id","Customer.ID", "Invoice.ID", "Created..UTC.", "Status", "Amount", "Fee", "Amount.Refunded")]

## Create nicer names for the columns/variables
colnames(data) <- c("id","customer_id", "invoice_id", "time_string", "status", "amount", "fee", "amount_refunded")

## Add a new column/variable "time" as a time object
## strptime converts character vectors to class "POSIXlt"
data$time <- strptime(data$time_string, format="%Y-%m-%d %H:%M")

## Add a new column/variable "period" with YYYY-ww so that we can later split by year-week
## %U
## Week of the year as decimal number (00–53) using Sunday as the first day 1 of the week (and typically with the first 
## Sunday of the year as day 1 of week 1). The US convention.
data$period <- format(data$time,"%y-%U")

## First remove the "," from numbers, then convert amounts from characters to numbers. 
data$amount <- as.numeric(as.character(gsub(',', '', data$amount)))
data$fee <- as.numeric(as.character(gsub(',', '', data$fee)))
data$amount_refunded <- as.numeric(as.character(gsub(',', '', data$amount_refunded)))

## Create new column/variable "amount_net"
data$amount_net <- data$amount - (data$fee + data$amount_refunded)

## Drop the time.string column/variable as we don't need it anymore
data$time_string <- NULL


## Sort by customer, invoice and reversed date order
data_sorted <- data[order(data$customer_id, data$invoice_id, -as.numeric(data$time)),]

## Remove all duplicates where customer_id, invoice_id combination appears
data_f <- data_sorted[!duplicated(data_sorted[,c("customer_id", "invoice_id")]),]

## Mark all duplicates or customer.id as FALSE, all first occurrences as TRUE
data_f$first <- !duplicated(data_f$customer_id)

## Mark all duplicates or customer.id as FALSE, all last occurrences as TRUE
data_f$last <- !duplicated(data_f$customer_id, fromLast=TRUE)


## Recurring, new, lost
## By default all transactions are considered being made by existing/recurring users. New variable "user_type"
data_f$user_type <- "recurring" # TRUE

## If it is the first occurrence, mark it as new (user_type = new)
data_f$user_type[data_f$first] <- "new"

## When the last transaction by the user fails, we consider to have lost that user (user_type = lost)
data_f$user_type[data_f$last & data_f$status == "Failed"] <- "lost"


## Create a table and export to CSV
## Make a table of the user types per period (= week)
user_types_by_period <- table(data_f$user_type, data_f$period)
user_types_by_period # show table

## Write this table to a new csv file
write.table(user_types_by_period, file = "new_recurring_lost.csv", row.names = FALSE, sep = "\t")
