hospitals <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from hospitalpayments"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_ba7433', PASS='orcl_ba7433', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

#Hospital payments by disease
ggplot() + 
  coord_cartesian() + 
  scale_x_discrete(breaks=NULL) +
  scale_y_continuous() +
  labs(title='Hospital Payments by Disease') +
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