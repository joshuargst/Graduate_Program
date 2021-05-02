library(dplyr)
library(tidyverse)
library(fivethirtyeight)




'the package "dplyr" has many functions used for data manipulation and provides a set of verbs that allow you to perform helpful transformations. This includes: "Mutate", "select" "filter", "summarise", "arrange","group_by", "order_by" and many more. This Vignette will demonstrate the intent behind these functions and how to apply them.

The dataset we will be working with is from the "FiveThirtyEight" package called "airlinesafety. this data set is shown below.'

airline_safety

'You can see the structure of the dataset as shown below'
str(airline_safety)

'select - This function allows you to select specific a variables from a dataset, for example, if we wanted to select just the "airline" fieldn and "incl_reg_subsidiaries" field we would perform the following operation:'

airline_safety%>% select(airline,incl_reg_subsidiaries)

'Filter allows us to drill deeper into our dataset and retain subsets of that data, for example, if we wish to extract only True values of "incl_reg_subsidiaries we would perform the following operation:'

airline_safety%>% filter(incl_reg_subsidiaries==TRUE)

'summarise is a tool that allows us to compute statistics or custom functions on variables within our dataset. an example of this is shown below by computing both the mean, and the number of observations on safety incidents that occurred between the years 2000 and 2014'


airline_safety %>% summarise(mean = mean(incidents_00_14), n= n())

'Mutate is a tool that allows us to transform our dataset using functions and transformations that we desire. For example, on this airline dataset we can add incidents that occured between 2000 and 2014 (variable "incidents_00_14) with incidents between 1985 and 1999'

airline_safety %>% 
  mutate(All_Incidents = incidents_85_99+incidents_00_14) %>% 
  select(All_Incidents)
airline_safety

'Group_by is a function within Dplyr that allows users to perform summarisations or transformations on groups of data. This is useful in understanding subsets of data rather than the entire dataset. An example is shown below on the airline_safety dataset where we group by the logical vector "incl_reg_subsidiaries, and find the sum of incidents based on companies that include subsidiaries vs not. Mutate does not alter the shape of the dataframe, but the grouping is done in memory and can be shown as a separate column as done below.'

airline_safety %>% 
  mutate(All_Incidents = incidents_85_99+incidents_00_14) %>% 
  group_by(incl_reg_subsidiaries,sum= sum(All_Incidents))

?reorder
'Tidyverse has an enormous library of functions and a powerful visualization library named ggplot2. a sample plot of the dataset is shown below by plotting the dataframe above'

airline_safety %>% ggplot(aes(x=airline,fatal_accidents_00_14, y = fatalities_00_14))+
  geom_col(aes(fill = incl_reg_subsidiaries))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+coord_flip()+
  labs(title = "Fatalities By Airline",
       x = "Airline",
       y = "Fatalities from 2000 to 2014",
       fill = "Has Subsidiary?")

