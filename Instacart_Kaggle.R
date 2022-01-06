########Data source: https://www.kaggle.com/c/instacart-market-basket-analysis/data
install.packages("data.table")
install.packages("knitr")
install.packages("stringr")
install.packages("DT")
install.packages("plotly")
install.packages("arules")
install.packages("visNetwork")
install.packages("igraph")
install.packages("kableExtra")
install.packages("visNetwork")
install.packages("arulesViz")
library(dplyr)
library(ggplot2)
library(DT)
library(stringr)
library(knitr)
library(data.table)
library(arules)
library(igraph)
library(kableExtra)
library(visNetwork)
library(plotly)
library(arulesViz)

#IMPORTING ALL THE FILES
# ORDERS CONTAINS THE LIST OF ALL THE ORDER IN THE DATRASET. IF DAYS_SINCE_PRIOR_ORDER = NA, THIS MEANS THAT IT WAS USER'S FIRST ORDER
orders <- read.csv("C:\\Users\\Siddharth\\Desktop\\Queens_Coursework\\831\\instacart-market-basket-analysis\\orders.csv")

# INFO ABOUT THE PRODUCT, FURTHERMORE AISLES AND DEPARTMENT
products <- read.csv("C:\\Users\\Siddharth\\Desktop\\Queens_Coursework\\831\\instacart-market-basket-analysis\\products.csv")

# INFO ABOUT WHICH PRODUCTS WERE ORDERED (PRODUCT_ID)
# INFO ABOUT ORDER IN WHICH THE PRODUCT WAS ADDED TO CART
order_products <- read.csv("C:\\Users\\Siddharth\\Desktop\\Queens_Coursework\\831\\instacart-market-basket-analysis\\order_products__train.csv")


order_products_prior<- read.csv("C:\\Users\\Siddharth\\Desktop\\Queens_Coursework\\831\\instacart-market-basket-analysis\\order_products__prior.csv")
aisles <- read.csv("C:\\Users\\Siddharth\\Desktop\\Queens_Coursework\\831\\instacart-market-basket-analysis\\aisles.csv")
department <- read.csv("C:\\Users\\Siddharth\\Desktop\\Queens_Coursework\\831\\instacart-market-basket-analysis\\departments.csv")

str(orders)
str(products)
str(order_products)
str(order_products_prior)
str(aisles)
str(department)


# CONVERTING CHARACTERS TO VARIABLES FOR BETTER VISUALIZATIONS
orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
products <- products %>%mutate(product_name = as.factor(product_name))
aisles  <- aisles %>% mutate(aisle = as.factor(aisle))
department <- department %>% mutate(department = as.factor(department))


####################################### EXPLORATION ########################################

# WHEN ARE MOST OF THE PEOPLE ORDERING ONLINE

max_order_day <- orders %>% ggplot(aes(x = order_hour_of_day)) + 
  geom_histogram(stat = "count", fill = "#60ab59") + theme(
    axis.text.y =element_blank(),
    axis.ticks.y =element_blank(), axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold")) + labs(x = "Hour of the Day", y = "Number of Customers") + xlim(0, 23)  +
  ggtitle("Hourly Traffic") + theme(plot.title = element_text(hjust = 0.5))
# THIS SHOWS THAT MOST OF THE TRAFFIC IS BETWEEN 8-18
max_order_day


# CHECK OF THE DAYS GETTIN THE MAXIMUM NUMBER OF TRAFFIC

max_traffic_days <- orders %>% ggplot(aes(x=order_dow )) +
  geom_histogram(stat = "count", fill = "#eb8921") +  theme(
    axis.text.y =element_blank(),
    axis.ticks.y =element_blank(), axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold") ) + labs(x = "Day of the Week", y = "Number of Customers") +
 
                                                                                                                        
# WE A  RE ASSUMING THAT 0 AND 1ARE THE WEEKEND DAYS, HENCE THE TRAFFIC SEEMS TO BE HIGHER THAN OTHER DAY
max_traffic_days
                

# WHEN ARE THE CUSTOMERS ORDERING AGAIN

order_again <- orders %>% ggplot(aes(x = days_since_prior_order)) +
  geom_histogram(stat = "count", fill = 'chartreuse4') 
#PEOPLE SEEM TO BE ORDEING MORE AFTER 1 WEEK, AND MAX AFTER A MONTH
order_again


# BESTSELLERS
bestsellers <- order_products %>% group_by(product_id) %>%
              summarise(count = n()) %>%
             top_n(8, wt = count) %>%
             left_join(select(products, product_id, product_name), by = "product_id") %>%
             arrange(desc(count))
 
bestsellers   #SHOWS THAT BANANAS ARE THE MOST SOLD PRODUCTS, FOLLOWED BY BAG OF ORGANIC BANANAS

# PRODUCT SALES COUNT
bestseller_chart <- bestsellers %>% ggplot(aes(x = reorder(product_name, -count), y = count)) +
                   geom_bar(stat = "identity", fill = "#60ab59") +
                   theme(axis.text.x = element_text(angle = 35, hjust = 1), axis.title.x = element_blank()) +
                  ggtitle("Bestsellers") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title=element_text(size=14,face="bold")) + labs(y ="Sales Count")

bestseller_chart   #SHOWS THAT BANANAS ARE THE MOST SOLD PRODUCTS, FOLLOWED BY BAG OF ORGANIC BANANAS



# FIND THE PRODUCTS THAT ARE REORDERED THE MOST
reorder_or_not <- order_products %>% group_by(reordered) %>%
                           summarise(count = n()) %>% mutate(reordered = as.factor(reordered)) %>%
                           mutate(proportion = count/sum(count))
reorder_or_not  #THIS SHOWS THAT ALMOST 59.9% PRODUCTS ARE REORDERED AGAIN

# GRAPHICAL REP OF THE ABOVE

reorder_or_not_graph <- reorder_or_not %>% ggplot(aes(x = reordered, y = count, fill = reordered)) +
                                  geom_bar(stat = "identity")
reorder_or_not_graph


# MOST OFTEN REORDERED PRODUCTS - TOP 20

most_reordered_20 <- order_products %>% group_by(product_id) %>%
  summarise(proportion_reordered = mean(reordered), n=n()) %>%
  filter(n>40) %>%
  top_n(8, wt= proportion_reordered) %>%
  arrange(desc(proportion_reordered)) %>%
  left_join(products, by = "product_id")

most_reordered_20


# GRAPHICAL REPRESENTATION OF THE ABOVE

most_reordered_20_graph <- most_reordered_20 %>% ggplot(aes(x = reorder(product_name, -proportion_reordered), y= proportion_reordered))+
                         geom_bar(stat = "identity", fill = "#60ab59") +
                         theme(axis.text.x = element_text(angle = 35, hjust = 1), axis.title.x = element_blank()) + coord_cartesian(ylim = c(0.85, 0.95)) +
theme(axis.text.x = element_text(angle = 35, hjust = 1), axis.title.x = element_blank()) +
  ggtitle("Most Reordered") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(  axis.text=element_text(size=12, face = "bold"),
          axis.title=element_text(size=14,face="bold")) 

most_reordered_20_graph

# ITEMS ADDED FIRST INTO THE CART

first_items <- order_products %>% group_by(product_id, add_to_cart_order) %>%
               summarise(count = n()) %>% mutate(pct = count/sum(count)) %>%
               filter(add_to_cart_order == 1, count > 20) %>%
               arrange(desc(pct)) %>%
               left_join(products, by = "product_id") %>%
               select(product_name, pct, count) %>%
               ungroup() %>%
               top_n(10, wt = pct)

first_items  # WHITE MULTIFOLD TOWELS ARE ADDED TO THE CART AT FIRST MOSTLY

# GRAPHICAL REPRESENTATION FOR THE ABOVE


first_items_graph <- first_items %>% ggplot(aes(x = reorder(product_name, -pct), y = pct)) +
                     geom_bar(stat = "identity", fill = "#eb8921") +
                     theme(axis.text.x = element_text(angle = 51, hjust = 1), axis.title.x = element_blank(), axis.title.y  = element_text(face = "bold")) + coord_cartesian(ylim = c(0.40, 0.75)) +

  ggtitle("First Bought Items") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(  axis.text=element_text(size=12, face = "bold")) + labs(y="Percentage")
  

first_items_graph


# LAST ORDER AND PROBABILITY OF REORDER

reorder_prob <- order_products %>% left_join(orders, by = "order_id") %>%
               group_by(days_since_prior_order) %>%
               summarise(mean_reorder = mean(reordered)) %>%
               ggplot(aes(x = days_since_prior_order, y = mean_reorder)) +
               geom_bar(stat = "identity", fill = "chartreuse4")

reorder_prob # THIS SHOWS THAT IF PEOPLE ORDER ON THE SAME DAY, THEY TEND TO ORDER THE SAME PRODUCTS MORE OFTEN.
# BUT IF MORE DAYS HAVE PASSED, THEY TEND TRY NWW PRODUCTS



# COMPARING PRODUCTS ---> ORGANIC VS INORGANIC

organic_inorganic <- products %>% 
                     mutate(organic = ifelse(str_detect(str_to_lower(products$product_name), "organic"), "organic", "not organic"), organic= as.factor(organic))

org_inorg_comp <- order_products %>%
                  left_join(organic_inorganic, by = "product_id") %>%
                  group_by(organic) %>%
                  summarise(count = n()) %>%
                  mutate(proportion = count/sum(count))
org_inorg_comp

#GRAPHICAL REPRESENTATION OF THE ABOVE

org_inorg_comp_graph <- org_inorg_comp %>% ggplot(aes(x = organic , y = count, fill = organic)) +
                        geom_bar(stat = "identity")
org_inorg_comp_graph


# REORDERING ORGANIC VS INORGANIC

org_inorg_reorder <- order_products %>% left_join(organic_inorganic, by = "product_id") %>%
         group_by(organic) %>% summarise(mean_reordered = mean(reordered))

org_inorg_reorder

#GRAPHIHCAL REPRESENTATION OF THE ABOVE

org_inorg_reorder_graph <- org_inorg_reorder %>% ggplot(aes(x = organic, y = mean_reordered, fill = organic)) +
                           geom_bar(stat = "identity")

org_inorg_reorder_graph


##################################### TREEMAP ###########################################
install.packages("treemap")
library(treemap)

tmp <- products %>% group_by(department_id, aisle_id) %>% summarise(n = n())
tmp <- tmp %>% left_join(department, by = "department_id")
tmp <- tmp %>% left_join(aisles, by = "aisle_id")

tmp2 <- order_products %>%
        group_by(product_id) %>%
        summarise(count = n()) %>%
        left_join(products, by = "product_id") %>%
        ungroup() %>%
        group_by(department_id, aisle_id) %>%
        summarise(sumcount = sum(count)) %>%
        left_join(tmp, by = c("department_id", "aisle_id")) %>%
        mutate(onesize = 1)

tmp2

treemap <- treemap(tmp2, index = c("department", "aisle"), vSize = "onesize" , vColor = "department", palette = "Set3", title = "", sortID = "-sumcount", border.col = "white")

treemap2 <- treemap(tmp2, index = c("department", "aisle"), vSize = "n", palette = "Set3", title = "", border.col = "white")


# TOP 15 AISLES

top_aisles <- order_products %>%
              left_join(products, by = "product_id") %>%
              left_join(aisles, by = "aisle_id") %>%
              left_join(department, by = "department_id") %>%
              group_by(aisle, department) %>%
              tally(sort = TRUE) %>%
              mutate(perc = round(100*n/nrow(order_products),2)) %>%
              ungroup() %>%
              top_n(15, n)

top_aisles_graph <- top_aisles %>% ggplot(aes(x = reorder(aisle, -n), y = n, fill = department)) +
                    geom_bar(stat = "identity") +
                    theme(axis.text.x = element_text(angle = 60, hjust = 1), axis.title.x = element_blank())

top_aisles_graph

###################################Order hour exploration################
#citation: https://www.kaggle.com/shwong/the-instacart-chart

orders<-read_csv("/Users/yunhuixia/Desktop/831 Project/instacart-market-basket-analysis/orders.csv")
#head(orders)
orders<- orders[,c(1,3,6)]
#head(orders)
products<-read_csv("/Users/yunhuixia/Desktop/831 Project/instacart-market-basket-analysis/products.csv")
#head(products)
products<-products[,c(1,2)]
#head(products)
order.prior<-read.csv("/Users/yunhuixia/Desktop/831 Project/instacart-market-basket-analysis/order_products__prior.csv")
#head(order.prior)
order.prior<-order.prior[,c(1,2)]
order.train<-read.csv("/Users/yunhuixia/Desktop/831 Project/instacart-market-basket-analysis/order_products__train.csv")
order.train<-order.train[,c(1,2)]

orders<-orders[which(orders$eval_set=="train" | orders$eval_set=="prior"), ]
orders<-orders[,-2]

#combine the prior and train together
order.products<-bind_rows(order.prior,order.train,.id = NULL)
df <- merge(order.products,orders,by="order_id")

#descend the product by counting the product_id
Top.product <- df %>% count(product_id,sort = TRUE, name = "total_count")

#pick the top 2000 popular products
Top.product <- Top.product[1:2000,]    
Top.product1 <- merge(Top.product,products,by="product_id")
Top.product1 <- Top.product1[order(Top.product1$total_count, decreasing = TRUE),]

library(dplyr)
# keep only observations that have products in top_products
df.top <- inner_join(df, Top.product1,by="product_id")
df.top[,-4]

#For each product_id, count how many orders were placed at each hour and what % this count represents.
df.top1<- df.top %>% group_by(product_id,order_hour_of_day) %>%
  count(product_id, name="count")

df.top2 <- inner_join(df.top1,Top.product1,by="product_id")

options(digits=5)
df.top2$percentage <- df.top2$count/df.top2$total_count*100
df.top2$order_hour_of_day <- as.numeric(df.top2$order_hour_of_day)

#calculate the mean_hour that each top product ordered in the day
options(digits=5)
df.top3<- df.top2 %>% group_by(product_id)%>%
  mutate(mean_hour = sum(order_hour_of_day*count)/total_count)
df.top4 <- df.top3[,-c(2,3,4,6)]
df.top4<- df.top4%>% distinct()

#split morning and afternoon product
morning <- df.top4[order(df.top4$mean_hour),]
morning<-morning[1:25,]

afternoon <- df.top4[order(df.top4$mean_hour,decreasing=TRUE), ] 
afternoon<- afternoon[1:25,]

morning_product <-inner_join(df.top2,morning,by="product_id")
afternoon_product<-inner_join(df.top2,afternoon,by="product_id")

library(ggplot2)
m<-ggplot(morning_product,aes(x=order_hour_of_day, y= percentage))
m+geom_line(aes(linetype = factor(product_name.x)),color="#60ab59")

a<-ggplot(afternoon_product,aes(x=order_hour_of_day, y= percentage))
#a+geom_point(aes(colour = factor(product_id)), size = 4)+geom_line(aes(linetype = factor(product_id)))
a+geom_line(aes(linetype = factor(product_name.x)),color='red')

ggplot()+
  geom_line(data = afternoon_product,aes(x=order_hour_of_day, y= percentage,linetype = factor(product_name.x)),color="#eb8921")+
  geom_line(data = morning_product,aes(x=order_hour_of_day, y= percentage,linetype = factor(product_name.x)),color="#60ab59")


#conclusion : the 24/25 popular products people loved to buy in the afternoon is ice cream

###################################Predictive Modeling##################
# lists all aisles in the store
aisles = read.csv('/Users/yunhuixia/Desktop/831 Project/instacart-market-basket-analysis/aisles.csv')
# Data dict: aisle name and aisle id

# lists all aisles in the store
departments = read.csv('/Users/yunhuixia/Desktop/831 Project/instacart-market-basket-analysis/departments.csv')
# Data dict:department name and department id

# lists all aisles in the store
products = read.csv('/Users/yunhuixia/Desktop/831 Project/instacart-market-basket-analysis/products.csv')
# Data dict:product name, product id, aisle id and department id to which the product belongs

# lists all orders ( previous and current nth orders that need prediction)
orders = read.csv('/Users/yunhuixia/Desktop/831 Project/instacart-market-basket-analysis/orders.csv')
# data dict: 
#   order_id unique id for the order
#   user_id: id of user who placed the order
#   order_number: order number for that user (identifies which order in the past)
#   order_dow: day of the week the order was placed
#   order_hr:  hour of the day the order was placed

#lists all previous orders
order_products_prior = read.csv('/Users/yunhuixia/Desktop/831 Project/instacart-market-basket-analysis/order_products__prior.csv')
# data dict: 
#   order_id unique id for the order
#   product_id: id of the product the user ordered in that order
#   add_to_car_order: order in which the product was added to the cart
#   reordered: 1 indicated user ordered it before, 
#              0 indicates user ordered a new product that was never ordered by this user

# lists all orders that need prediction
order_products_train = read.csv('/Users/yunhuixia/Desktop/831 Project/instacart-market-basket-analysis/order_products__train.csv')
# data dict: 
#   order_id unique id for the order
#   product_id: id of the product the user ordered in that order
#   add_to_car_order: order in which the product was added to the cart
#   reordered: 1 indicated user ordered it before, 
#              0 indicates user ordered a new product that was never ordered by this user

# Loading different libraries
#install.packages("e1071")
#install.packages("randomForest")
#install.packages("ROCR")
#install.packages("kernlab")
library(dplyr)
library(caret)
library(e1071) 
library(randomForest)
library(ROCR)
library(kernlab)

# Option to use 6 significant digits after the decimal point
options(digits=6)

## getting to know the data:
head(aisles)
head(products)
head(departments)
head(products)
head(orders)
head(order_products_train)
head(order_products_prior)

length(unique(aisles$aisle))
length(unique(products$aisle_id))
length(unique(departments$department))
length(unique(products$department_id))
length(unique(orders$user_id))

######
# Counting orders/products different sets

sum(orders$eval_set=="test")
sum(orders$eval_set=="train")
sum(orders$eval_set=="prior")


# Join Products and Aisles
products_all<-products%>%
  left_join(aisles, by.x=aisle_id, by.y=id)%>%
  left_join(departments, by.x=department_id, by.y=id)

# Join the orders in the dataset that has order ids the task wishes to predict with 
# more details from the orders file and join the product descriptions
order_products_train_all<-order_products_train %>%
  left_join(orders, by="order_id")%>%
  left_join(products_all, by="product_id")

# Join the orders in the dataset that has order ids from the past with 
# more details from the orders file and join the product descriptions
order_products_prior_all<-order_products_prior %>%
  left_join(orders, by="order_id")%>%
  left_join(products_all, by="product_id") 

# counting number of orders in the past and present list of orders 
total_orders_prior<-length(unique(order_products_prior_all$order_id))
total_orders_train<-length(unique(order_products_train_all$order_id))


#write.csv(order_products_prior_all, "instacart_table.csv")

##### Are there any user ids that are in train set but not in prior set
length(setdiff(order_products_prior_all$user_id, 
               order_products_train_all$user_id)
) #75000 that were given for test data

length(setdiff(order_products_train_all$user_id, 
               order_products_prior_all$user_id)
)# 0 no ids in train are missing from prior


rm(order_products_prior, order_products_train, products_all)
gc()

# User features
###########################################
# These features define the different user features based on the patterns of the user

# This feature is the average cart size for users
cart_size<-order_products_prior_all%>% 
  group_by(order_id)%>%
  summarize(user_id=first(user_id), cart_size=max(add_to_cart_order), .groups='drop')%>%
  group_by(user_id)%>%
  summarize(user_avg_cart_size=ceiling(mean(cart_size)), .groups='drop')

# These features define the number of items, the number of previous orders, 
# the average hour, day time, how frequently the user orders and what is reorder 
# percentage is for all previous orders & products
user_features<-order_products_prior_all%>%
  group_by(user_id)%>%
  summarize(user_n_items=n(), 
            user_orders_n=max(order_number), 
            user_avg_hr=mean(order_hour_of_day),
            user_avg_day=mean(order_dow),
            user_avg_freq=ceiling(mean(days_since_prior_order, na.rm=TRUE)), 
            user_reord_pct=sum(reordered==1)/n(),
            .groups='drop')


###########################################
# Product features
###########################################

# These features define the different product features based on the patterns 
# of how these products get ordered

# These features define the product popularity by counting the number of times it has been ordered,
# how many got reordered, if the product is organic and average frequency the products get ordered

prod_features<-order_products_prior_all%>%
  group_by(product_id)%>%
  summarize(
    prod_pop_n=n(), 
    prod_reord_tot=sum(reordered==1), 
    prod_org_flag=ifelse(grepl("Organic", first(product_name), fixed=TRUE),1,0),
    prod_avg_freq=ceiling(mean(days_since_prior_order, na.rm=TRUE)),
    .groups='drop')

# changing the NAs to 0. The average frequency has NAs for observations 
# where the product was ordered the 1st time
prod_features$prod_avg_freq<-ifelse(is.na(prod_features$prod_avg_freq), 0, prod_features$prod_avg_freq)

# Looking for top 3 departments
order_products_prior_all%>%
  group_by(department_id, department)%>%
  summarize(n=n(), .groups='drop')%>%arrange(desc(n))%>%slice(1:3)

# merge product names to create flags for the top 3 departments as they are frequently ordered
prod_features<-prod_features%>%
  left_join(products, by.x=product_id, by.y=product_id)

# flags to indicate produce, dairy eggs and snacks 
prod_features$produce_flag<-ifelse(prod_features$department_id==4, 1,0)
prod_features$dairy_eggs_flag<-ifelse(prod_features$department_id==16, 1,0)
prod_features$snacks_flag<-ifelse(prod_features$department_id==19, 1,0)

# removing additonal columns from the join that are not needed anymore
prod_features<-prod_features[,-c(6:8)]

#####################################################################
# User-Product combination features
#####################################################################

# These features define the different user-product combination features 
# based on the patterns of how a user orders these products

# these features define the reorder percentage of a user for a specific product and 
# the product's average position in the cart by the user
user_prod_features<-order_products_prior_all%>%
  group_by(user_id, product_id)%>%
  summarize(user_prod_reord_tot=sum(reordered==1),
            user_prod_cart_order=mean(add_to_cart_order),
            .groups='drop') 

# these features create a flag for products in the last 3 orders and 
# their reorder percentage 
last3order<-order_products_prior_all%>%
  group_by(user_id)%>%
  select(user_id, product_id, order_number, reordered)%>%
  filter(order_number==max(order_number)|order_number==max(order_number)-1|order_number==max(order_number)-2)%>%
  mutate(last3_flag=1)%>%
  group_by(user_id, product_id)%>%
  summarize(last3_reorder_pct=sum(reordered)/3, 
            last3_flag=first(last3_flag), 
            .groups='drop')

# these features create a flag for products in the last order
lastorder<-order_products_prior_all%>%
  group_by(user_id)%>%
  select(user_id, product_id, order_number)%>%
  filter(order_number==max(order_number))%>%
  mutate(prev_flag=1)

# Get all unique combinations of the user and product combinations from prior orders
user_prod_ids<-order_products_prior_all%>%distinct(user_id,product_id)

# merge with nth orders to create the dependent variable "buy"
# buy=1 indicates the user ordered the product in the nth order
# buy=0 indicates the user did not order the product in the nth order
instacart_data<-user_prod_ids%>%
  left_join(order_products_train_all, by=c("user_id", "product_id"))%>%
  select(user_id, product_id, order_id, buy=reordered)

# Since prior order contain all other products the merge will result in NAs
# for products not ordered in the nth order. Hence change those NAs to zeros
instacart_data$buy<-ifelse(is.na(instacart_data$buy),0,1)%>%factor(.)

# Merge all features
instacart_data<-instacart_data%>%
  left_join(cart_size, by.x=user_id, by.y=user_id)%>%
  left_join(prod_features, by.x=product_id, by.y=product_id)%>%
  left_join(user_features, by.x=user_id, by.y=user_id)%>%
  left_join(user_prod_features, by=c("user_id", "product_id"))%>%
  left_join(lastorder, by=c("user_id", "product_id"))%>%
  left_join(last3order, by=c("user_id", "product_id"))

# Merge results in NAs for products not in last order, replace with 0
instacart_data$prev_flag<-ifelse(is.na(instacart_data$prev_flag),0,1)

# Merge results in NAs for products not in last 3 orders, replace with 0
instacart_data$last3_flag<-ifelse(is.na(instacart_data$last3_flag),0,1)
instacart_data$last3_reorder_pct<-
  ifelse(is.na(instacart_data$last3_reorder_pct),
         0,instacart_data$last3_reorder_pct)

# removing redundant column
instacart_data<-instacart_data[,-21] # deleting order number

# changing name to reorder percent from reorder total and calculating the percent by 
# calculating total divided by number of orders
instacart_data<-instacart_data%>%
  rename(
    user_prod_reord_pct= user_prod_reord_tot
  )
instacart_data$user_prod_reord_pct<-instacart_data$user_prod_reord_pct/instacart_data$user_orders_n

# checking any NAs
sum(is.na(instacart_data))
sum(is.na(instacart_data$order_id))
# only NAs are in order ids that are not in nth order

# remove vectors not needed after merge to free up memory
rm(user_prod_features, cart_size, user_features, prod_features, last3order, lastorder)
gc()

# factorize all flags
instacart_data$prod_org_flag<-as.factor(instacart_data$prod_org_flag)
instacart_data$produce_flag<-as.factor(instacart_data$produce_flag)
instacart_data$dairy_eggs_flag<-as.factor(instacart_data$dairy_eggs_flag)
instacart_data$snacks_flag<-as.factor(instacart_data$snacks_flag)
instacart_data$prev_flag<-as.factor(instacart_data$prev_flag)
instacart_data$last3_flag<-as.factor(instacart_data$last3_flag)

# get all unique user ids in the train set provided which is the nth order for users
user_ids<-orders%>%
  filter(eval_set=="train")%>%
  .$user_id

# randomly reducing user ids to 10%
set.seed("310")
index <- createDataPartition(y = user_ids, times = 1, p = 0.1, list = FALSE)
user_ids<-user_ids[index]

# Taking 20% for final testing of the final model
set.seed("630")
test_index <- createDataPartition(y = user_ids, times = 1, p = 0.2, list = FALSE)

temp <- user_ids[-test_index]
user_id_test <- user_ids[test_index]

# Splitting 80% 20% for training the model and fine tuning with evaluation model
set.seed("222")
eval_index <- createDataPartition(y = temp, times = 1, p = 0.2, list = FALSE)

user_id_train <- temp[-eval_index]
user_id_eval <- temp[eval_index]

# checking if any user ids overlapped
intersect(user_id_eval, user_id_test)
intersect(user_id_eval, user_id_train)

rm(temp, test_index, eval_index,index)


# merging user ids to get all features
train_set<-instacart_data%>%
  inner_join(data.frame(user_id=user_id_train), by.x=user_id, by.y=user_id)

eval_set<-instacart_data%>%
  inner_join(data.frame(user_id=user_id_eval), by.x=user_id, by.y=user_id)

test_set<-instacart_data%>%
  inner_join(data.frame(user_id=user_id_test), by.x=user_id, by.y=user_id)


nrow(train_set)
# trainset has 547802 observations

nrow(test_set)
# testset has 166744 observations

nrow(eval_set)
# evalset has 135834 observations


length(unique(train_set$user_id))
# trainset has  8396 users

length(unique(test_set$user_id))
# testset has 2628 users

length(unique(eval_set$user_id))
# evalset has 2100 users

write.csv(instacart_data, "Goodwin.MMA831.csv")

rm(user_id_eval, user_id_test, user_id_train)

# selected specific features based on correlation analysis
xvalues<-c(            
  "buy",                 
  "user_avg_cart_size",           
  "prod_reord_tot",      
  "prod_org_flag",       
  "prod_avg_freq",      
  "produce_flag",        
  "dairy_eggs_flag",     
  "snacks_flag",        
  "user_orders_n",               
  "user_avg_hr",        
  "user_avg_day",        
  "user_avg_freq",       
  "user_reord_pct",     
  "user_prod_reord_pct",
  "user_prod_cart_order",
  "prev_flag",           
  "last3_reorder_pct",  
  "last3_flag"
)

# reducing dataset to contain only features and dependent variable
trainset<-train_set[,xvalues]
evalset<-eval_set[,xvalues]
testset<-test_set[,xvalues]

# write.csv(trainset, "trainset.csv")
# write.csv(testset, "testset.csv")
# write.csv(evalset, "evalset.csv")

get_result_stats<-function(x,y){
  cm<-table(Predict=x, Reference=y)
  acc<-(cm[1,1]+cm[2,2])/sum(cm)
  precision<-cm[2,2]/(cm[1,2]+cm[2,2])
  recall<-cm[2,2]/(cm[2,1]+cm[2,2])
  f1score<-2*precision*recall/(precision+recall)
  
  list(cm=cm, acc=acc, precision=precision, recall=recall, f1score=f1score)
}
#######################################################
# Decision tree model
#######################################################

# use caret package train function
model_tree<-train(trainset[,-1], trainset$buy, method="rpart")

# predict on evalset
buy_tree<-predict(model_tree, evalset)

# summarize metrics
tree_summary<-get_result_stats(buy_tree, evalset$buy)
view(tree_summary)
# use probablity predictions to calculate area under curve
tree_y<- predict(model_tree, evalset, type="prob")[,2]
pred <- prediction(tree_y,evalset$buy)
tree_auc = performance(pred, measure = "auc")

# summary table to add all model results
all_results<-data.frame(method="CART", f1score=tree_summary$f1score, 
                        acc=tree_summary$acc, precision=tree_summary$precision,
                        recall=tree_summary$recall, AUC=round(tree_auc@y.values[[1]],6))

# fine tuning of cp parameter
plot(model_tree)

# Classification Tree plot
plot(model_tree$finalModel)
text(model_tree$finalModel)

# Plot of ROC curve
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)

require(rpart)
fit <- rpart(trainset$buy~., trainset)
plot(fit); text(fit)
df <- data.frame(imp = fit$variable.importance)
df2 <- df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(df2) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()

#######################################################
# Random Forest: RF
#######################################################

#Random Forest function with 100 trees
model_forest<-randomForest(trainset[,-1], trainset$buy, ntree=100)

# predict on evalset
buy_rf<-predict(model_forest, evalset)

# summarize metrics
rf_summary<-get_result_stats(buy_rf, evalset$buy)
view(rf_summary)
# use probablity predictions to calculate area under curve
rf_y<- predict(model_forest, evalset, type="prob")[,2]
pred <- prediction(rf_y, evalset$buy)
forest_auc = performance(pred, measure = "auc")

# summary table to add all model results
all_results<-rbind(all_results, data.frame(method="Random Forest", f1score=rf_summary$f1score, 
                                           acc=rf_summary$acc, precision=rf_summary$precision,
                                           recall=rf_summary$recall, AUC=round(forest_auc@y.values[[1]],6)))



# Fine tuning of mtry
t <- tuneRF(trainset[, -1], trainset[, 1],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 400,
            trace = TRUE,
            improve = 0.01)

t
# gives best mtry as 4


# Variable Importance Plot
imp <- as.data.frame(varImpPlot(model_forest))
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  


ggplot(imp, aes(x=reorder(varnames, -MeanDecreaseGini), weight=MeanDecreaseGini, fill=varnames)) + 2^
  geom_bar() +
  scale_fill_discrete(name="Variable Group") +
  ylab("MeanDecreaseGini") +
  xlab("Variable Name") +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  ggtitle("Variable Importance of Random Forest")

#######################################################
# Logistic Regression: GLM
#######################################################

# use caret package train function
model_glm<-train(trainset[,-1], trainset$buy, method="glm")

# predict on evalset
buy_glm<-predict(model_glm, evalset)

# summarize metrics
glm_summary<-get_result_stats(buy_glm, evalset$buy)

# use probablity predictions to calculate area under curve
glm_y<- predict(model_glm, evalset, type="prob")[,2]
pred <- prediction(glm_y, evalset$buy)
glm_auc = performance(pred, measure = "auc")


# summary table to add all model results
all_results<-rbind(all_results, data.frame(method="Logistic Regression", f1score=glm_summary$f1score, 
                                           acc=glm_summary$acc, precision=glm_summary$precision,
                                           recall=glm_summary$recall, AUC=round(glm_auc@y.values[[1]],6)))

all_results%>%knitr::kable()


################################### Apriori########################

# CREATE SHOPPING BASKET FIRST

shopping_basket <- order_products %>%
                   inner_join(products, by = "product_id") %>%
                   group_by(order_id) %>%
                   summarise(basket = as.vector(list(product_name)))

shopping_basket


# CREATE TRANSACTION DATA

transactions <- as(shopping_basket$basket, "transactions")

transactions_1 <- as(shopping_basket$basket, "transactions")
inspect(transactions[2]) # EACH TRANSACTION CONTAINS DIFFERENT PRODUCTS

#IMPLEMENTING APRIORI ALGORITHM

rules <- apriori(transactions, parameter = list(support = 0.005, confidence = 0.25))


b# REMOVING REDUNDANT RULES
 
rules <- rules[!is.redundant(rules)]
rules_dt <- data.table(lhs= labels(lhs(rules)),
                       rhs = labels(rhs(rules)),
                       quality(rules)) [order(-lift),]
head(rules_dt)
 

#ITEM FREQUECY PLOT
install.packages("RColorBrewer")
library(RColorBrewer)

par(mar = rep(2,4))

arules::itemFrequencyPlot(transactions,
                          topN = 20,
                          col = brewer.pal(8, "Pastel2"),
                          main = "Relative Item Frequency Plot",
                          type = "relative",
                          ylab = "Item Frequency (Relative)")
# THIS SHOWS THAT BANANA, AND BAG OF ORGANIC BANANA CONSTITUTE ALMOST 1/4 OF THE TOTALSALES OF ALL THESE ITEMS


# NETWORK VISUALIZATIONS

subrules <- sort(rules, by = "confidence")
subrules <- head(subrules,15)
ig <- plot(subrules, method = "graph", engine = "igraph", control = list(type="items"))
ig

ig_df <- toVisNetworkData(ig, idToLabel = FALSE)
ig_df



visNetwork(ig_df$nodes, ig_df$edges) %>%
  visNodes(size = 10) %>%
  visLegend() %>%
  visEdges(smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visEdges(arrows = "from") %>%
  visPhysics(
    solver = "barnesHut",
    maxVelocity = 35,
    forceAtlas2Based = list(gravitationConstant = -6000)
  )


transactions.df <- as.data.frame(transactions)

