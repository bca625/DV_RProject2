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