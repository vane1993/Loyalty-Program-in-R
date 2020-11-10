### purchase_date : Date when the ticket was purchased (YYYYmmdd).
# - ticket_id : Unique identifier of the purchase.
# - plan_id : Unique identifier of the plan
# - user_id : User who performs that purchase (unique identifier).
# - city_code : City where the event for that ticket will take place.
# - revenue : amount charged to the user for that purchase (USD).
# - margin : amount earned for the company for that purchase (USD).
# - use_loyaty : this column flags whether the user used loyalty or not.

# Reading files
data <- read.csv(file.choose())
head(data)
summary(data)
data$use_loyalty <- as.character(data$use_loyalty)

data$use_loyalty[data$use_loyalty == "f"]  <- 0
data$use_loyalty[data$use_loyalty == "t"]  <- 1
data$use_loyalty <-as.numeric(data$use_loyalty)

sum(data$use_loyalty[data$use_loyalty == 1] )

# Date conversion
library(lubridate)
data[ , 1 ] <- ymd(data[, 1])
class(data$purchase_date)

# Ordering dataset by Date.
newdata <- data[order(data$purchase_date, decreasing = FALSE),] 
tail(newdata)


# Use of Loyalty over the following months. The usage of the loyalty
# program has been increasing from October 2019 to January 2020 which means that our users are
# taking advantage of the deals as we can confirm by having a look at the graph.
install.packages("magrittr ")
library(magrittr)
library(dplyr)

data_loyalty <- newdata[newdata$use_loyalty == 1,]
df_data_loyalty <- data_loyalty[,c(1,7)]

loyalty_by_date <- df_data_loyalty  %>% group_by(month=floor_date(purchase_date, "month")) %>%
  summarize(use_loyalty=sum(use_loyalty))

use_of_loyalty <- ggplot(data=loyalty_by_date, aes(x=month, y=use_loyalty))
use_of_loyalty  + geom_line(color="blue") +
  geom_point( size=2, shape=21, fill="white", colour="blue") + 
  theme_minimal()

# Trends 
#Analysing the trend over the months it is possible to notice that the biggest peaks
# occured at the end of november, december and early January. These dates coincide
# with important holidays in Spain. Hence, people tend to use the discount coupons 
# more often during these dates.
loyalty_by_date <- aggregate(df_data_loyalty["use_loyalty"], by=df_data_loyalty["purchase_date"], sum)
use_of_loyalty <- ggplot(data=loyalty_by_date, aes(x=purchase_date, y=use_loyalty))
use_of_loyalty  + geom_line(color="blue") 



#  Let's have a look at the relantionship between revenue and loyalty grouped by unique User over the
# entired period. 
# As we can see in the table, those customers that present a high revenue also make a higher use of
# the loyalty program.

head(newdata)
library(lubridate)
df_frequent_buyers <- newdata[,c(3,5,7)]
head(df_frequent_buyers)
month(df_frequent_buyers$purchase_date)

big_buyers <- aggregate(. ~ user_id, df_frequent_buyers, sum)
head(big_buyers,10)
# sort by revenue
big_buyers <- big_buyers[order(big_buyers$revenue, decreasing = TRUE),] 
head(big_buyers,10)

# Returning customers. 
# To calculate the returning customer I will calculate the date of first engagement at every row for each customer. 
#This will determine whether they were a new customer or a returning customer at that point in time.
# To categorize each customer as "New" or "Returning" I will substrat the "purchase date" from the 
# "date_of_first_engagement" so if "purchase_date" is equal to the first date of engagement that means
# he is a new customer.

repurchasing_customers <- newdata[,c(1,3,5)]
repurchasing_customers <- repurchasing_customers[order(repurchasing_customers$revenue, decreasing = TRUE),]
head(rrepurchasing_customers)

repurchasing_customers <- repurchasing_customers %>%
  group_by(user_id)%>%
  mutate(date_of_first_engagement=min(purchase_date))%>%
  ungroup()

repurchasing_customers  <- repurchasing_customers %>%
  mutate(Customer_Status = case_when(purchase_date >date_of_first_engagement ~ "Returning",
                                     purchase_date  == date_of_first_engagement ~ "New",
                                     TRUE ~ "Other"))

# Let's see how many "New" and " Returning" customers we have per month.

Customers <-  repurchasing_customers%>%
  group_by(floor_date(purchase_date,unit = 'month'))%>%
  summarise(New_Customers = n_distinct(user_id[Customer_Status=="New"]),
Returning_Customers= n_distinct(user_id[Customer_Status=="Returning"]))
Customers$`floor_date(purchase_date, unit = "month")`
Customers <- as.data.frame(Customers)
colnames(Customers)[1]<-"Date_purchase"


# Plot
# In the following plot we can see a representation between "New" and " Returning" customers.
# The blue line represents New Customers while the black one represents Returning Customers.
# As we can see there is room for improvement regarding the loyalty program, since the aim of the
# company is to retain all the new customer incomers into Returning Customer.
customers_plot <- ggplot(data=Customers, aes(x=Date_purchase, y=New_Customers)) + 
  geom_line(color="blue") 
customers_plot <- customers_plot + geom_line(aes( y=Returning_Customers))
 
# axes label
customers_plot <- customers_plot + xlab("Date of Purchase") +
  ylab("Customers")

# label formating
customers_plot <- customers_plot + xlab("Date of Purchase") +
  ylab("Customers") +
  ggtitle("New/Returning Customers") +
  theme(axis.title.x = element_text(colour="Black", size = 10),
        axis.title.y = element_text(colour = "Black", size=10),
        axis.text.x = element_text(size = 10),
        axis.text.y  = element_text(size = 10))







