#read table into r
datatable = read.table("C:/Users/sarahd_98/OneDrive/Desktop/Thesis/OD readings test.csv",
                       header = TRUE,
                       sep = ",")
#create variables of data
glucosecit = datatable$Glucose.CIT
pyruvatecit = datatable$Pyruvate.CIT
lactatecit = datatable$Lactate.CIT
propionatecit = datatable$Propionate.CIT

glucosek10 = datatable$Glucose.K10
pyruvatek10 = datatable$Pyruvate.K10
lactatek10 = datatable$Lactate.K10
propionatek10 = datatable$Propionate.K10

#set the margins for four graphs
par(mfrow=c(2,2))
#plot empty graphs and add each K10 and CIT data set for each medium to one
plot(NULL, xlim = c(1, 10), ylim=c(0,0.9), main = "Glucose",
     xlab = "Weeks", ylab = "OD values")
points(x = c(0,3,5,8), y = glucosecit, col = "deeppink2", pch = 15, type = "b")
points(x = c(0,3,5,8), y = glucosek10, col = "cornflowerblue", pch = 3, type = "b")

plot(NULL, xlim = c(1, 10), ylim=c(0,0.9), main = "Pyruvate",
     xlab = "Weeks", ylab = "OD values")
points(x = c(0,3,5,8), y = pyruvatecit, col = "deeppink2", pch = 15, type = "b")
points(x = c(0,3,5,8), y = pyruvatek10, col = "cornflowerblue", pch = 3, type = "b")

plot(NULL, xlim = c(1, 10), ylim=c(0,0.9), main = "Lactate",
     xlab = "Weeks", ylab = "OD values")
points(x = c(0,3,5,8), y = lactatecit, col = "deeppink2", pch = 15, type = "b")
points(x = c(0,3,5,8), y = lactatek10, col = "cornflowerblue", pch = 3, type = "b")

plot(NULL, xlim = c(1, 10), ylim=c(0,0.9), main = "Propionate",
     xlab = "Weeks", ylab = "OD values")
points(x = c(0,3,5,8), y = propionatecit, col = "deeppink2", pch = 15, type = "b")
points(x = c(0,3,5,8), y = propionatek10, col = "cornflowerblue", pch = 3, type = "b")
