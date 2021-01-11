#read data table into r
table = read.table("C:/Users/sarahd_98/OneDrive/Documents/R documents/thesisdoubleseq.csv",
                   header = TRUE,
                   sep = ",")
#create a function which assigns colour according to GC conent number
colourvector = function(table){
  colourvector = c()
  for(index in 1:nrow(table)){
    
    if(table[index, "GC.content"] == 51){
      colourvector[index] = "coral"
      
    }else if(table[index, "GC.content"] == 65){
      colourvector[index] = "cornflowerblue"
      
    }else if(table[index, "GC.content"] == 66){
      colourvector[index] = "aquamarine2"
      
    }else if(table[index, "GC.content"] == 67){
      colourvector[index] = "darkorchid"
      
    }else if(table[index, "GC.content"] == 68){
      colourvector[index] = "deeppink2"
      
    }else if(table[index, "GC.content"] == 69){
      colourvector[index] = "darkolivegreen2"
    }
  }
  return(colourvector)
}

#assign varibles to data to make plotting easier
colours = colourvector(table)
count = table$GC.content
lbls = table$ï..sequence.no.
ticks = c(45:100)

#plot dot chart
dotchart(count,labels=lbls,cex=.45,
         main="% GC content per Sequence",
         xlab="% GC content" ,color = "black", 
         pch = 16, xaxt = "n", ylab = "Sequence ID")
#add x axis ticks
axis(1,at=ticks,pos = 0,las=1)