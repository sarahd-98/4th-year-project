#load packages
library(ape)
library(phytools)
library(scales)

#read in stuff
phylotree = read.tree("C:/Users/sarahd_98/OneDrive/Documents/Stuff from viktors laptop/RAxML_bipartitions.variants")
bryanttable = read.table("C:/Users/sarahd_98/OneDrive/Documents/Stuff from viktors laptop/Bryant 2016 Table S1.csv",
                         header = TRUE,
                         sep = ",",
                         stringsAsFactors = FALSE,
                         check.names = FALSE)
irishtable = read.table("C:/Users/sarahd_98/OneDrive/Documents/Stuff from viktors laptop/newmeta.csv",
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = TRUE,
                      check.names = FALSE)
 
#branchlengths to SNPs
phylotree$edge.length = phylotree$edge.length * 882

#round SNPs
roundedSNPs = round(phylotree$edge.length)
phylotree$edge.length = roundedSNPs

phylotree$tip.label[1]= "14-6278"

#function to get names
irishnames = function(table, tree){
  outputvector = tree$tip.label
  for(index in 1:nrow(table)){
    for(tips in 1:length(tree$tip.label)){
     if(table[index, "AliquotFormat"] == tree$tip.label[tips]){
      outputvector[tips] = paste("(", table[index, "INMV Group"],")",tree$tip.label[tips], table[index, "Herd Identifier"], "-", table[index, "Birth County"])
      } 
    }
  }
  return(outputvector)
}
phylotree$tip.label = irishnames(irishtable, phylotree)


bryantnames = function(table, tree){
  outputvector = tree$tip.label
  for(index in 1:nrow(table)){
    for(tips in 1:length(tree$tip.label)){
      if(table[index, "Accession"] == tree$tip.label[tips] || table[index, "Secondary Accession"] == tree$tip.label[tips]){
        outputvector[tips] = paste("(", table[index, "INMV"], ")", tree$tip.label[tips], "-", table[index, "Country of origin"])
      }
    }
  }
  return(outputvector)
}
phylotree$tip.label = bryantnames(bryanttable,phylotree)

#manually fix some
phylotree$tip.label[8] = "( 1 ) CIT-MAP - Ireland"
phylotree$tip.label[9] ="( 1 ) CITP-MAP - Ireland"
phylotree$tip.label[21] = "MAP-K10"
phylotree$tip.label[13] = "( 1 ) 17-2827 Tyrone 1"
badtips = c(1,21)
phylotree = drop.tip(phylotree, badtips)

SNPs = phylotree$edge.length

#make matched colour vector
irishinmv = function(tree){
  colouroutput = rep(NA,length(tree$tip.label))
  
  
  for(index in 1:length(tree$tip.label)){
    name = NULL
    name = strsplit(tree$tip.label[index]," ")[[1]][2]
    if(is.na(name)== TRUE){
      colouroutput[index] = "black"
        
    }else if(name == "2"){
      colouroutput[index] = "cornflowerblue"
        
    }else if(name == "122"){
      colouroutput[index] = "deeppink3"
        
    }else if(name == "1"){
      colouroutput[index] = "coral"
    }
  }
  return(colouroutput)
}
vntrcol = irishinmv(phylotree)



# Save plot as .pdf file (Ireland)
outputFile <- paste("VNTR_Tree_29-06-20.pdf", sep="")
pdf(outputFile, height=75, width=75)

# Set margins to nothing
currentMar <- par()$mar
par(mar=c(0,0,0,0))
par(bg=NA)


#start plotting
plot.phylo(phylotree, edge.width = 20, font = 3, tip.color = vntrcol,
           label.offset = 0.5,align.tip.label = FALSE,type = "phylogram", cex =8)
edgelabels(SNPs, cex = 5, bg = "azure")
add.scale.bar("bottomleft", cex = 8, lwd = 17)
text(x=75,y=1, cex = 8, label = "SNPs")

legend(x =300,y=22.5, title = "INMV Groups",legend = c("Group 1", "Group 2", "Group 122"), 
       text.col = c("coral","Cornflowerblue", "deeppink3"), title.col = "black", cex = 10)
legend(x=222, y=22.5, legend = c("No. of SNPs"), pch = 0, col = "black", cex = 10)

# Reset the margins
par(mar=currentMar)

dev.off()

