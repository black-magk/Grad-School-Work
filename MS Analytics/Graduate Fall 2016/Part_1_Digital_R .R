#PART 1 Did the views of official brand content rise after the Super Bowl?



#BELOW WE SUBSET THE SHEETS BY DATE; FILTERED TO ONLY INCLUDE THOSE WITH THE OFFICIAL CHANNEL

#Date Vector
observationDates <- (c("2016-01-11","2016-01-16","2016-01-23", "2016-01-30", "2016-02-06", "2016-02-13"))



#SNICKERS

#Below subsets the data based on the given primary channel
jan.11_snickers_a<-subset( jan.11_snickers, channel_id== "UCDviI62w0VbD_9oRNkV1Uig" ) 
#view the subset
jan.11_snickers_a
jan.16_snickers_a<-subset( jan.16_snickers, channel_id== "UCDviI62w0VbD_9oRNkV1Uig" ) 
jan.23_snickers_a<-subset( jan.23_snickers, channel_id== "UCDviI62w0VbD_9oRNkV1Uig" ) 
jan.30_snickers_a<-subset( jan.30_snickers, channel_id== "UCDviI62w0VbD_9oRNkV1Uig" ) 
feb.06_snickers_a<-subset( feb.06_snickers, channel_id== "UCDviI62w0VbD_9oRNkV1Uig" ) 
feb.13_snickers_a<-subset( feb.13_snickers, channel_id== "UCDviI62w0VbD_9oRNkV1Uig" ) 


#REESES
jan.11_reeses_a<-subset( jan.11_reeses, channel_id== "UCc9-kl38p_uzQDVrT06VTxA" ) 
jan.16_reeses_a<-subset( jan.16_reeses, channel_id== "UCc9-kl38p_uzQDVrT06VTxA" ) 
jan.23_reeses_a<-subset( jan.23_reeses, channel_id== "UCc9-kl38p_uzQDVrT06VTxA" ) 
jan.30_reeses_a<-subset( jan.30_reeses, channel_id== "UCc9-kl38p_uzQDVrT06VTxA" ) 
feb.06_reeses_a<-subset( feb.06_reeses, channel_id== "UCc9-kl38p_uzQDVrT06VTxA" ) 
feb.13_reeses_a<-subset( feb.13_reeses, channel_id== "UCc9-kl38p_uzQDVrT06VTxA" ) 

f
#MTDEW
jan.11_mtdew_a<-subset( jan.11_mtdew, channel_id== "UCsdqpqgsSFTRSnyyqHVSgyw" ) 
jan.16_mtdew_a<-subset( jan.16_mtdew, channel_id== "UCsdqpqgsSFTRSnyyqHVSgyw" ) 
jan.23_mtdew_a<-subset( jan.23_mtdew, channel_id== "UCsdqpqgsSFTRSnyyqHVSgyw" ) 
jan.30_mtdew_a<-subset( jan.30_mtdew, channel_id== "UCsdqpqgsSFTRSnyyqHVSgyw" ) 
feb.06_mtdew_a<-subset( feb.06_mtdew, channel_id== "UCsdqpqgsSFTRSnyyqHVSgyw" ) 
feb.13_mtdew_a<-subset( feb.13_mtdew, channel_id== "UCsdqpqgsSFTRSnyyqHVSgyw" ) 


#SHOCKTOP
jan.11_shocktop_a<-subset( jan.11_shocktop, channel_id== "UCpfUlFZlM1yfcJG5dl9fwyg" ) 
jan.16_shocktop_a<-subset( jan.16_shocktop, channel_id== "UCpfUlFZlM1yfcJG5dl9fwyg" ) 
jan.23_shocktop_a<-subset( jan.23_shocktop, channel_id== "UCpfUlFZlM1yfcJG5dl9fwyg" ) 
jan.30_shocktop_a<-subset( jan.30_shocktop, channel_id== "UCpfUlFZlM1yfcJG5dl9fwyg" ) 
feb.06_shocktop_a<-subset( feb.06_shocktop, channel_id== "UCpfUlFZlM1yfcJG5dl9fwyg" ) 
feb.13_shocktop_a<-subset( feb.13_shocktop, channel_id== "UCpfUlFZlM1yfcJG5dl9fwyg" ) 



