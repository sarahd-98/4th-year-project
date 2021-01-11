#read table into r
metatable = read.table("C:/Users/sarahd_98/OneDrive/Documents/Stuff from viktors laptop/scatterversionmeta.csv",
                        header = TRUE,
                        sep = ",",
                        stringsAsFactors = TRUE,
                        check.names = FALSE)

#creates matched vector to assign colours according to cow breed
colourfunc = function(table){
  colourvector = rep(NA, length(nrow(table)))
  for(index in 1:nrow(table)){
    if(table[index, "Breed"] == "Holstein Friesian"){
      colourvector[index] = "coral"
      
    }else if(table[index, "Breed"] == "Limousin Cross"){
      colourvector[index] = "cornflowerblue"
      
    }else if(table[index, "Breed"] == "South Devon"){
      colourvector[index] = "deeppink2"
      
    }else if(table[index, "Breed"] == "Aberdeen Angus cross"){
      colourvector[index] = "aquamarine2"
      
    }else if(table[index, "Breed"] == "Partenaise Cross"){
      colourvector[index] = "darkorchid"
      
    }else if(table[index, "Breed"] == "Jersey Cross"){
      colourvector[index] = "darkolivegreen2"
      
    }
  }
  return(colourvector)
}
colours = colourfunc(metatable)

#create a variable holding all sequnece names for x axis
lbls = metatable$Aliquot

#redefine the margins to help place the axis
par(mar=c(4,6,2,0.3))

#create bar plot
bar = barplot(metatable$DTPos, main = "Days until Positive Shedding Status", 
            xlab = "Days", col = colours, pch = 2, horiz = TRUE, yaxt = "n")

#adds y axis labels and positions them
text(cex=0.68, x=-1, y=bar[,1], lbls, xpd = TRUE, srt = 0, adj =1)

#label y axis
mtext(text = "Sequence ID", side = 2, line = 5)

#creates legends relaying cow breed to colour
legend(x = 32, y = 11.2, legend = c("Holstein Friesian", "Limousin Cross", "South Devon", "Aberdeen Angus cross", "Partenaise Cross","Jersey Cross"),
       fill = c("coral", "cornflowerblue", "deeppink3", "aquamarine2","darkorchid","darkolivegreen2"),
       cex=0.57, bty = "n" )

#adding lines for limits
abline(v=22, col = "red", lty = 2)
abline(v = 23, col = "yellow", lty = 2)
abline(v=31, col = "yellow", lty = 2)
abline(v=32, col = "green", lty = 2)


