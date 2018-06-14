#Read in the Channel IDs of the all the official channels (primary and secondary)
allOfficialChannels<- read.xlsx("data-F16/official-channels.xlsx", sheet=1)
#And then pull out just the primary official channels--there should be one for each brand.
primaryOfficialChannels <- allOfficialChannels[allOfficialChannels$is.primary=="primary","channel.ID"]

#extra the rows of the "alldata" data frame that represent videos posted by a primary channel
officialChannelData<-alldata[is.element(alldata$channel_id,primaryOfficialChannels),]
