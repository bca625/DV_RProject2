require(tidyr)
require(dplyr)
require("jsonlite")
require("RCurl")
require("reshape2")
require("ggplot2")


hospitals <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from hospitalpayments"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_ba7433', PASS='orcl_ba7433', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

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


