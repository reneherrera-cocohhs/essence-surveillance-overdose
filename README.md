# README


## Abstract 


This project uses data from the NSSP BIoSense ESSENCE API to analyze events of public interest, monitor healthcare data for events that could affect public health, and share data and analyses.


This project intends to surveil emergency department visits related to overdose.


## Objectives 


1. Identify trends (increase or decrease) of unintentional overdose among Coconino County residents. 
2. Display data on a dashboard



## Variables of Interest 


- sex 
- age and age group 
- zip code 
- hospital 
- race and ethnicity 
- suicide ideation and/or suicide attempt 


## Measures for Comparison 


- Number of ED visits per month 
- Calculate rates by dividing the number of ED visits related to suicide by the total number of ED visits each month, multiplied by 100,000.
- Calculate percent change in monthly rate and evaluate for statistical significance 



## R Scripts 


- [00-setup.R](scripts/00-setup.R); List of packages used in project. Checks to see if they are installed and if not, then installs them.
- [01-essence-query.R](scripts/01-essence-query.R); Query and tidy data from Essence API 
- [02-transform-data.R](scripts/02-transform-data.R); Transform tidy data and create new variables (age groups)


## Data Folder 