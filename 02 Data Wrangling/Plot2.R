ggplot() + 
  coord_cartesian() + 
  scale_x_discrete(breaks=NULL) +
  scale_y_continuous() +
  labs(title='Hospital Payments by Disease in Texas') +
  labs(x="DISEASE", y=paste("PAYMENT")) +
  layer(data=Texas, 
        mapping=aes(x=MEASURE_NAME, y=as.numeric(as.character(PAYMENT)), color= MEASURE_NAME), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list(),  
        #position=position_identity()
        position=position_jitter(width=0.3, height=0)
  )