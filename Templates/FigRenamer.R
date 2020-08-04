setwd("C:/Users/Clare/Dropbox (2° Investing)/2° Investing Team/People/clare/GitHub/Templates/UNPRIFigures")

listoffiles <- list.files(pattern = ".png")
listoffiles <- listoffiles[!grepl("Fig",listoffiles)]

for (x in 1:length(listoffiles)){
  newname <- paste0("Fig",substr(listoffiles[[x]],start=1,stop=2),".png")
  file.rename(listoffiles[[x]],newname)
  
  }
