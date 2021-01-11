#read data into R
RNAtable = read.table("C:/Users/sarahd_98/OneDrive/Documents/R documents/ComCsq_2020-11-27.tsv", 
                      header = TRUE, sep = "\t", 
                      stringsAsFactors = FALSE,
                      check.names = FALSE,
                      quote = "")

#function to create dataframe of consequences found in each strain
SNPfinder = function(table){
  #initilise the empty dataframe
  
  namesOfRows <- table[,1]
  namesOfCols <- colnames(table)
  filteredFrame <- data.frame(matrix(nrow = length(namesOfRows), 
                                     ncol = length(namesOfCols)))
  names(filteredFrame) <- namesOfCols
  filteredFrame[,1] <- namesOfRows
  
  #run through the rows of consequences and checks for NAs in the extra strains
  for(index in 1:nrow(table)){
    if(is.na(table[index, "13-4959_csq.vcf"]) == TRUE && is.na(table[index, "ERR037990_csq.vcf"]) == TRUE )
      
      #place relevent consequences in new dataframe
      filteredFrame[index, ] = table[index, ]
  }
  return(filteredFrame)
}
outputdata = SNPfinder(RNAtable)

#remove non-relevent strains
outputdata = outputdata[, -c(2,7,8)]

#search remaining columns for NAs in remaining strains and remove those rows
outputdata = outputdata[-which(is.na(outputdata$`CIT-MAP_csq.vcf`)&is.na(outputdata$`CITP-MAP_csq.vcf`)&is.na(outputdata$CITRep1_csq.vcf)&is.na(outputdata$CITRep2_csq.vcf)), ]



#create combined vector of rows which contain None and remove those rows from the dataframe
vector1 = grep("None", outputdata[, 2])
vector2 = grep("None", outputdata[, 3])
vector3 = grep("None", outputdata[, 4])
vector4 = grep("None", outputdata[, 5])
vectortotal = c(vector1, vector2, vector3, vector4)
uniquevector = unique(vectortotal)
outputdata = outputdata[-uniquevector, ]

#create combined vector of rows which contain PASS and keeping only those rows as to elliminate fails
vectorpass = grep("PASS", outputdata[, 2])
vectorpass2 = grep("PASS", outputdata[, 3])
vectorpass3 = grep("PASS", outputdata[, 4])
vectorpass4 = grep("PASS", outputdata[, 5])
vectorpasstotal = c(vectorpass, vectorpass2, vectorpass3, vectorpass4)
uniquevectorpass = unique(vectorpasstotal)
outputdata = outputdata[uniquevectorpass, ]

outputdata[6,4] = outputdata[6, 2]
outputdata[6,5] = outputdata[6, 2]
outputdata[8,4] = outputdata[8,2]
outputdata[11,5] = outputdata[11,2]


#plot proportion of dif consequences 

#gather the frequencies for each consequence as vectors from dataframe
synoncit = length(grep("synonymous",outputdata[,2]))
synoncitp = length(grep("synonymous", outputdata[,3]))
synonRNA = length(grep("synonymous", outputdata[, 5]))
misscit = length(grep("missense",outputdata[,2]))
misscitp =length(grep("missense",outputdata[,3]))
missRNA =length(grep("missense",outputdata[,5]))
insertcit = length(grep("inframe_insertion",outputdata[,2]))
insertcitp = length(grep("inframe_insertion",outputdata[,3]))
insertRNA = length(grep("inframe_insertion",outputdata[,5]))
frameshiftcit = length(grep("frameshift",outputdata[,2]))
frameshiftcitp = length(grep("frameshift",outputdata[,3]))
frameshiftRNA = length(grep("frameshift",outputdata[,5]))
delcit = length(grep("inframe_deletion",outputdata[,2]))
delcitp = length(grep("inframe_deletion",outputdata[,3]))
delRNA = length(grep("inframe_deletion",outputdata[,5]))

#group CIT and CITP by consequence in vectors
synonymous = c(synoncit, synoncitp, synonRNA)
missense = c(misscit, misscitp, missRNA)
insertion = c(insertcit, insertcitp, insertRNA)
frameshift = c(frameshiftcit, frameshiftcitp, frameshiftRNA)
deletion = c(delcit, delcitp, delRNA)

#set the consequneces as a variable for plotting
counts = c(synonymous, missense, insertion, frameshift, deletion)
#create the name labels for the x axis
names = c("", "Synonymous","","", "Missense", "","", "Insertion", "","",  "Frameshift", "","", "Deletion", "")

#plot the data
bar = barplot(counts, main = "Consequenses in CIT, CITP and RNA", col = c("deeppink2", "cornflowerblue", "darkolivegreen2"), 
              space = c(0.1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0.1, 1,0.1,0.1), ylab = "Frequency", las = 1, xlab = "Consequence Type")
#create a legend and x axis
legend("topright", legend = c("CIT", "CITP", "RNA"), fill = c("deeppink3", "cornflowerblue", "darkolivegreen2"), cex = 0.6)
text(cex=0.8, x=bar[,1], y=-0.1, labels = names, xpd = TRUE, srt = 50, adj =1)
