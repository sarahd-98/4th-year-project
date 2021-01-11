#read data table into r
table = read.table("C:/Users/sarahd_98/OneDrive/Documents/R documents/thesisseqsingle.csv",
                   header = TRUE,
                   sep = ",")

#assign variables to data to make plotting easier
counts = table$no..of.reads
numbers = as.numeric(format(counts,scientific=FALSE))
labels = table$ï..sequence.no.

#create colour pallet
library(RColorBrewer)
coul <- brewer.pal(5, "Set2")

#creating margin for y axis
par(mar=c(4,6,2,2))

#plot bar plot
bar = barplot(counts, main = "Number of Sequence Reads", yaxt = "n", col = coul, xlab = "Sequence ID", ylab = "")

#add appropriate y axis title
mtext(text = "Sequence reads", side = 2, line = 5)

#add y axis numbers
axis(2,at=pretty(counts),labels=format(pretty(counts),big.mark=",", scientific=FALSE),las=1)

#create x axis labels
text(cex=0.58, x=bar[,1], y=-3.7, labels, xpd = TRUE, srt = 50, adj =1)
