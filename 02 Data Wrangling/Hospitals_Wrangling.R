require(tidyr)
require(dplyr)
require("jsonlite")
require("RCurl")
require("reshape2")
require("ggplot2")


hospitals3 <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from hospitalpayments"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_ba7433', PASS='orcl_ba7433', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

tbl_df(hospitals)
View(hospitals)

#df <- rename(hospitals, tbl = table) # table is a reserved word in Oracle so rename it to tbl.






moop <- hospitals %>% group_by(STATE) %>% summarise(n = n()) %>% arrange(desc(n)) %>% tbl_df


# select
Texas <- hospitals %>% select(CITY, STATE, PAYMENT, MEASURE_NAME) %>% filter(STATE == "TX") %>% tbl_df

HSD <- hospitals %>% select(CITY, STATE, PAYMENT, MEASURE_NAME) %>% filter(STATE == "TX", CITY %in% c("HOUSTON", "SAN ANTONIO", "DALLAS" )) 

Heart <- hospitals %>% filter(MEASURE_NAME == "Payment for heart attack patients") %>% tbl_df

Pneu <- hospitals %>% filter(MEASURE_NAME == "Payment for pneumonia patients") %>% tbl_df

hospArrange <- hospitals %>% arrange(PAYMENT) %>% tbl_df

#Density plot with a semi-transparent fill
ggplot(HSD, aes(x=PAYMENT, fill=MEASURE_NAME), title="FOO") + geom_density(alpha=.3)

ggplot(hospitals, aes(x=MEASURE_NAME, y=PAYMENT, fill=MEASURE_NAME)) + geom_boxplot() + scale_x_discrete(breaks=NULL) + labs(x=NULL)

#Hospital payments in Texas's biggest cities
ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_continuous() +
  labs(title='Hospital Payments in Texas') +
  labs(x="CITY", y=paste("PAYMENT")) +
  layer(data=HSD, 
        mapping=aes(x=CITY, y=as.numeric(as.character(PAYMENT)), color=as.character(CITY)), 
        stat="identity", 
        stat_params=list(),
        geom="point",
        geom_params=list(), 
        #position=position_identity()
        position=position_jitter(width=0.3, height=0)
  )





overpaid <- hospitals %>% select(HOSPITAL_NAME, STATE, PAYMENT,LOWER_ESTIMATE, LOCATION, MEASURE_NAME) %>% mutate(Payment_percent = cume_dist(PAYMENT),Overpaid_amt = PAYMENT - LOWER_ESTIMATE) %>% filter(Payment_percent <= 0.1)

#
ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  labs(title='Hospital Payments by Disease in Texas') +
  labs(x=paste("PAYMENT"), y="Overpaid Amount") +
  layer(data=overpaid, 
        mapping=aes(x=as.numeric(as.character(PAYMENT)), y=Overpaid_amt, color=MEASURE_NAME), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list(),  
        #position=position_identity()
        position=position_jitter(width=0.3, height=0)
  )




#Hospital payments by disease in Texas
ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_continuous() +
  labs(title='Hospital Payments by Disease in Texas') +
  labs(x="DISEASE", y=paste("PAYMENT")) +
  layer(data=hospitals, 
        mapping=aes(x=MEASURE_NAME, y=as.numeric(as.character(PAYMENT)), color= MEASURE_NAME), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list(),  
        #position=position_identity()
        position=position_jitter(width=0.3, height=0)
  )

hist(hospitals$PAYMENT[hospitals$STATE == "TX"], xlab="Payment in Dollars", main="Frequency Distrubution of Hospital Payments in Texas", col="orange")


ggplot() + 
  coord_cartesian() + 
  scale_x_discrete(breaks=NULL) +
  scale_y_continuous() +
  facet_wrap(~CITY) +
  labs(title='Titanic') +
  labs(x="CITY", y=paste("PAYMENT")) +
  layer(data=HSD, 
        mapping=aes(x=MEASURE_NAME, y=PAYMENT, color=MEASURE_NAME), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list(), 
        #position=position_identity()
        position=position_jitter(width=0.3, height=0)
  )

diamonds %>% select(., cut, clarity) %>% tbl_df
diamonds %>% select(color:price) %>% tbl_df # Equivalent SQL: none
diamonds %>% select(-cut, -clarity) %>% tbl_df # Equivalent SQL: none
x <- diamonds %>% select(cut, clarity) %>% tbl_df 
x

# filter
diamonds %>% select(cut, clarity) %>% filter(cut == "Good") %>% tbl_df # Equivalent SQL: 
# select cut, clarity from diamonds where cut = 'Good'
diamonds %>% select(cut, clarity) %>% filter(cut %in% c("Good", "Fair")) %>% tbl_df
diamonds %>% select(cut, clarity) %>% filter(cut %in% c("Good", "Fair"), clarity == "VS1") %>% tbl_df
diamonds %>% select(cut, clarity) %>% filter(cut %in% c("Good", "Fair"), clarity == "VS1" | is.na(cut)) %>% tbl_df
diamonds %>% select(carat, clarity) %>% filter(carat > 2) %>% tbl_df
diamonds %>% select(cut, clarity, x, y, z) %>% filter(cut %in% c("Good", "Fair"), clarity == "VS1" | is.na(cut)) %>% tbl_df

# arrange
data.frame(x=c(1,1,1,2,2), y=c(5:1), z=(1:5)) %>% arrange(desc(x)) %>% tbl_df
data.frame(x=c(1,1,1,2,2), y=c(5:1), z=(1:5)) %>% arrange(desc(x),y) %>% tbl_df
diamonds %>% arrange(carat) %>% tbl_df
diamonds %>% arrange(desc(carat)) %>% tbl_df

# rename
diamonds %>% rename(tbl= table) %>% tbl_df

# mutate
diamonds %>% select(cut, clarity, x, y, z) %>% filter(cut %in% c("Good", "Fair"), clarity == "VS1" | is.na(cut)) %>% mutate(sum = x+y+z) %>% tbl_df
ndf <- diamonds %>% select(cut, clarity, x, y, z) %>% filter(cut %in% c("Good", "Fair"), clarity == "VS1" | is.na(cut)) %>% mutate(sum = x+y+z) %>% tbl_df
ndf

# Useful mutate functions:
# pmin(), pmax() Parallel, Element-wise min and max
# cummin(), cummax() Cumulative min and max
# cumsum(), cumprod() Cumulative sum and product
# Windowing functions
# between() Are values between a and b?
# cume_dist() Cumulative distribution of values
# cumall(), cumany() Cumulative all and any
# cummean() Cumulative mean
# lead(), lag() Copy with values one position
# ntile() Bin vector into n buckets
# dense_rank(), min_rank(),
# percent_rank(), row_number() Various ranking methods

pmin(c(1:5), (5:1)) # Pairwise min
diamonds %>% mutate(minxy = pmin(x,y)) %>% tbl_df
pmax(c(1:5), (5:1)) # Pairwise max
c(1,1,2,0,4,3,5) %>% cummin()
diamonds %>% mutate(cummin_x = cummin(x)) %>% tbl_df
c(1,1,2,5,4,3,5) %>% cummax()
c(1,1,2,3,4,3,5) %>% cumsum()
diamonds %>% mutate(cumsum_x = cumsum(x)) %>% tbl_df
c(1,1,2,3,4,3,5) %>% cumprod()
c(1,1,2,3,4,3,5) %>% between(2,4)
diamonds %>% mutate(between_x = between(x,4,4.1)) %>% tbl_df
c(1:5) %>% cummean()
c(1:5) %>% lead() - c(1:5)
diamonds %>% mutate(lead_z = lead(z)-z) %>% tbl_df
c(1:5) %>% lag() - c(1:5)
diamonds %>% mutate(lag_z = lag(z)-z) %>% tbl_df
c(1:10)
c(1:10) %>% ntile(4) # bucket edges are rounded
diamonds %>% mutate(ntile_z = ntile(z,100)) %>% arrange(desc(ntile_z)) %>% tbl_df
diamonds %>% mutate(ntile_z = ntile(z,100)) %>% group_by(ntile_z) %>% summarise(n=n()) %>% tbl_df

c(1,1,2,5,4,3,5) %>% cume_dist()
c(1:5) %>% cume_dist()
c(1,1:5) %>% cume_dist()
# c(TRUE, TRUE, FALSE, FALSE, TRUE) %>% cumall()
# c(FALSE, TRUE, FALSE, FALSE, TRUE) %>% cumany()
# Now let's try them in the mutate function
diamonds %>% mutate(price_percent = cume_dist(price)) %>% arrange(desc(price_percent)) %>% tbl_df
# select d.*, cume_dist() OVER (order by price) cume_dist from diamonds d order by 11 desc;
# select d.*, cume_dist() OVER (PARTITION BY cut order by price) cume_dist from (select * from diamonds where rownum < 100) d order by cut desc, 11 desc;

bottom20_diamonds <- diamonds %>% mutate(price_percent = cume_dist(price)) %>% filter(price_percent <= .20) %>% arrange(desc(price_percent)) %>% tbl_df
diamonds %>% mutate(price_percent = cume_dist(price)) %>% filter(price_percent >= .80) %>% arrange(desc(price_percent)) %>% tbl_df
top20_diamonds <- diamonds %>% mutate(price_percent = cume_dist(price)) %>% filter(price_percent >= .80) %>% arrange(desc(price_percent)) %>% tbl_df
diamonds %>% mutate(price_percent = cume_dist(price)) %>% filter(price_percent <= .20 | price_percent >= .80) %>% ggplot(aes(x = price, y = carat)) + geom_point(aes(color=cut))

# summarize (summarise)
diamonds %>% summarize(mean = mean(x), sum = sum(x,y,z), n = n())
# Useful Summary functions:
# min(), max() Minimum and maximum values
# mean() Mean value
# median() Median value
# sum() Sum of values
# var, sd() Variance and standard deviation of a vector
# first() First value in a vector
# last() Last value in a vector
# nth() Nth value in a vector
# n() The number of values in a vector
# n_distinct() The number of distinct values in a vector

diamonds %>% group_by(cut,color) %>% summarise(mean = mean(x), sum = sum(x,y,z), n = n())
diamonds %>% group_by(cut,color) %>% summarise(mean = mean(x), sum = sum(x,y,z), n = n()) %>% ungroup %>% summarize(sum(n))

diamonds %>% group_by(cut,color) %>% summarise(mean = mean(x), sum = sum(x,y,z), n = n()) %>% arrange(n)
diamonds %>% group_by(cut,color) %>% summarise(mean = mean(x), sum = sum(x,y,z), n = n()) %>% arrange(desc(n), cut, color)

diamonds %>% group_by(cut, color, clarity) %>% summarise(mean_carat = mean(carat)) %>% ggplot(aes(x=cut, y=mean_carat, color=color)) + geom_point() + facet_wrap(~clarity)
