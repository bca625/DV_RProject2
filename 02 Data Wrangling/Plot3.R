ggplot() + 
  coord_cartesian() + 
  scale_x_discrete(breaks=NULL) +
  scale_y_continuous() +
  facet_wrap(~CITY) +
  labs(title='Hospital Payments in Dallas, Houston, and San Antonio') +
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