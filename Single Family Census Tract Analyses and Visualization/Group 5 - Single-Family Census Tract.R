library(lubridate)
library(pastecs)
library(DescTools)
library(dplyr)
library(ggplot2)
library(ggpubr)

# Read in data from fhlmc_sf2019c_loans.txt
df <-read.delim(file.choose(), sep = "", header = FALSE,na.strings = "", stringsAsFactors = FALSE)

# Replace column names to actual and utilize make names to avoid future naming catastrophe's
colnames(df) <- make.names(c('Enterprise Flag', 'Record Number', 'US Postal State Code', 'Metropolitan Statistical Area (MSA) Code', 
                  'County - 2010 Census', 'Census Tract - 2010 Census', '2010 Census Tract - Percent Minority',
                  '2010 Census Tract - Median Income', 'Local Area Median Income', 'Tract Income Ratio', 
                  'Borrowers Annual Income', 'Area Median Family Income (2019)', 'Borrower Income Ratio',
                  'Acquisition Unpaid Principal Balance (UPB)', 'Purpose of Loan', 'Federal Guarantee', 'Number of Borrowers',
                  'First-Time Home Buyer', 'Borrower Race 1', 'Borrower Race 2', 'Borrower Race 3', 'Borrower Race 4',
                  'Borrower Race 5', 'Borrower Ethnicity', 
                  'Co-Borrower Race 1', 'Co-Borrower Race 2', 'Co-Borrower Race 3', 'Co-Borrower Race 4', 'Co-Borrower Race 5', 
                  'Co-Borrower Ethnicity',
                  'Borrower Gender', 'Co-Borrower Gender', 'Age of Borrower', 'Age of Co-Borrower', 'Occupancy Code', 'Rate Spread',
                  'HOEPA Status', 'Property Type', 'Lien Status', 'Borrower Age 62 or older', 'Co-Borrower Age 62 or older',
                  'Loan-to-Value Ratio (LTV) at Origination', 'Date of Mortgage Note', 'Term of Mortgage at Origination',
                  'Number of Units', 'Interest Rate at Origination', 'Note Amount', 'Preapproval', 'Application Channel', 
                  'Automated Underwriting System (AUS) Name', 'Credit Score Model - Borrower', 'Credit Score Model - Co-Borrower',
                  'Debt-to-Income (DTI) Ratio', 'Discount Points', 'Introductory Rate Period', 
                  'Manufactured Home - Land Property Interest', 'Property Value', 'Rural Census Tract',
                  'Lower Mississippi Delta County', 'Middle Appalachia County', 'Persistent Poverty County',
                  'Area of Concentrated Poverty', 'High Opportunity Area', 'Qualified Opportunity Zone (QOZ) Census Tract'))

# Get column names used and utilize make names to avoid future naming catastrophe's
selection <- make.names(c('Local Area Median Income', 'Borrowers Annual Income', 'First-Time Home Buyer',
             'Borrower Gender', 'Age of Borrower', 'Property Value'))

# Shrink working dataframe to selection
total_data_df <- subset(df, select = selection)

# Clean Data
data_df <- data.frame(total_data_df)
data_df <- filter(data_df, Local.Area.Median.Income != 999999)
data_df <- filter(data_df, Borrowers.Annual.Income != 999999998)
data_df <- filter(data_df, First.Time.Home.Buyer != 9)
data_df <- filter(data_df, Borrower.Gender != 9)
data_df <- filter(data_df, Borrower.Gender != 4)
data_df <- filter(data_df, Borrower.Gender != 3)
data_df <- filter(data_df, Age.of.Borrower != 9)
data_df <- filter(data_df, Property.Value != 999999999)

data_df$First.Time.Home.Buyer[data_df$First.Time.Home.Buyer == 1] <- 'Yes'
data_df$First.Time.Home.Buyer[data_df$First.Time.Home.Buyer == 2] <- 'No'

data_df$Borrower.Gender[data_df$Borrower.Gender == 1] <- 'Male'
data_df$Borrower.Gender[data_df$Borrower.Gender == 2] <- 'Female'

data_df$Age.of.Borrower[data_df$Age.of.Borrower == 1] <- '0-25'
data_df$Age.of.Borrower[data_df$Age.of.Borrower == 2] <- '25-34'
data_df$Age.of.Borrower[data_df$Age.of.Borrower == 3] <- '35-44'
data_df$Age.of.Borrower[data_df$Age.of.Borrower == 4] <- '45-54'
data_df$Age.of.Borrower[data_df$Age.of.Borrower == 5] <- '55-64'
data_df$Age.of.Borrower[data_df$Age.of.Borrower == 6] <- '65-74'
data_df$Age.of.Borrower[data_df$Age.of.Borrower == 7] <- '74+'

# mean.male <- mean(data_df$Borrowers.Annual.Income[data_df$Borrower.Gender == 'Male'])
# mean.female <- mean(data_df$Borrowers.Annual.Income[data_df$Borrower.Gender == 'Male'])

# Make plot for average borrowers annual income by borrower gender
gender.income.plot <- ggplot(data_df)+
  scale_y_continuous(labels=scales::dollar_format())+
  ggtitle("Average Borrower's Annual Income \nby Borrower Gender") +
  geom_bar(aes(x=Borrower.Gender, y=Borrowers.Annual.Income), stat = "summary", fun.y = "mean")

# Make plot for average borrowers annual income by first time home buyer
home.buyer.income.plot <- ggplot(data_df)+
  scale_y_continuous(labels=scales::dollar_format())+
  ggtitle("Average Borrower's Annual Income \nby First Time Home Buyer") +
  geom_bar(aes(x=First.Time.Home.Buyer, y=Borrowers.Annual.Income), stat = "summary", fun.y = "mean")

# Make plot for average borrowers annual income by age of borrower
age.borrower.income.plot <- ggplot(data_df)+
  scale_y_continuous(labels=scales::dollar_format())+
  ggtitle("Average Borrower's Annual Income \nby Age of Borrower") +
  geom_bar(aes(x=Age.of.Borrower, y=Borrowers.Annual.Income), stat = "summary", fun.y = "mean")

# Drop large incomes
data_df <- filter(data_df, Borrowers.Annual.Income < 300000)

# Make histogram of all borrower's annual income
borrower.income.histogram <- ggplot(data_df, aes(x=Borrowers.Annual.Income)) +
  scale_y_continuous(labels=scales::comma)+
  scale_x_continuous(labels=scales::dollar_format())+
  ggtitle("Histogram of all \nBorrower's Annual Income") +
  geom_histogram(bins = 12)

# Arrange plots into one figure
figure <- ggarrange(gender.income.plot, home.buyer.income.plot, age.borrower.income.plot, borrower.income.histogram,
          ncol = 2, nrow = 2)

# Add title to figure
annotate_figure(figure,
                top = text_grob("Average Borrower's Annual Income by Demographic", color = "blue", face = "bold", size = 14))
