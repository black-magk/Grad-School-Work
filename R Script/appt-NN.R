# appt case
# Neural networks
# Dan Zhang, September 2016

# read data
# please download data from D2L and save to the working directory
rm(list=ls())
setwd('C:/Users/dazh1150/Dropbox/Fall2016/Advanced Data Analytics/data/appointments')
appt = read.csv("appointments.csv")
str(appt)
appt$MRN = as.factor(appt$MRN)
appt$cancelled = as.integer(appt$Status=="Cancelled")

# load neuralnet package
#install.packages('neuralnet')
library(neuralnet)

# train the neural network with sample data
# Full training with all data can take considerable amount of time.
# Because this is a classification problem, we set the err.fct = 'ce', which 
# stands for cross entropy; we also set linear.output=FALSE
set.seed(200)
sampleprop = 0.1
train = sample(nrow(appt),nrow(appt)*sampleprop)
appt.net <- neuralnet(cancelled~Lag+Age+TimeSinceReg,data=appt[train,],err.fct='ce',linear.output=FALSE,hidden=3)

# make predictions using the neural network results
# Note that predict function does not work with neuralnet results; instead we use compute
# The columns in the input data have to correspond to the ones used to train the neural network
cancel.pred <- compute(appt.net,appt[,c("Lag","Age","TimeSinceReg")])
sort(cancel.pred$net.result,decreasing=TRUE)[1:50]

# order the predictions by cancellation probabilities
pred = data.frame(custid=1:nrow(appt),cancelled=appt$cancelled,cancel.prob = cancel.pred$net.result)
pred = pred[order(pred$cancel.prob,decreasing=TRUE),]

# create another column in the data frame pred to store the predicted cancellation outcome
pred$cancel.nn = as.numeric(pred$cancel.prob>0.3)

# confusion matrix of the prediction
table(pred$cancelled,pred$cancel.nn)
