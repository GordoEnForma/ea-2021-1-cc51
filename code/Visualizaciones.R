# Importamos el dataframe
hotel_df <- read.csv("../data/dataset_clean.csv",header=TRUE,stringsAsFactors = FALSE,sep =";")

# VISUALIZACIONES


# Es necesario installar la siguiente biblioteca 

# install.packages("dplyr")
# library(dplyr)


 barplot(table(hotel_df$hotel))
 
 barplot(table(hotel_df$arrival_date_year))

 grouped <- hotel_df %>% group_by(arrival_date_year, arrival_date_month) %>%tally()

 grouped["faux_date"] <- date_from_separated(1, grouped$arrival_date_month, grouped$arrival_date_year)
 grouped_and_ordered <- grouped[order(as.Date(grouped$faux_date)),]
 
 grouped_and_ordered["year_month"] <- paste(grouped_and_ordered$arrival_date_year, match(grouped_and_ordered$arrival_date_month, month.name), sep="-")

 barplot(grouped_and_ordered$n, names=grouped_and_ordered$year_month, las=2)
 
  result <- c(
    count(hotel_df[hotel_df$babies == 0 | hotel_df$children == 0,])$n[1],
    count(hotel_df[hotel_df$babies > 0 | hotel_df$children > 0,])$n[1]
  )
 
  options(scipen = 5)
  barplot(result)

