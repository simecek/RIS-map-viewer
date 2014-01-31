require(RColorBrewer)

plotgeno <- function(tmp) {

  mL <- 0.5 # minimal visualisation length to be visible (=0.5Mb)
  pal <- brewer.pal(5, "Set1")
  
  maxx <- max(tmp$to)  # maximal length of chromosome
  chrs <- c(as.character(1:19),"X","Y","M")
  chr.lengths <- c(197195432, 181748087, 159599783, 155630120,
                   152537259, 149517037, 152524553, 131738871,
                   124076172, 129993255, 121843856, 121257530,
                   120284312, 125194864, 103494974, 98319150,
                   95272651, 90772031, 61342430, 166650296,
                   15902555, 16299)
  
  # sort tmp
  tmp <- tmp[order(tmp$chr, tmp$from),]
  
    if (nrow(tmp)>0) {
      
      plot(0,1, xlim=c(0,maxx)/10^6, ylim=c(20,1), type="n", yaxt="n", xlab="Mb", ylab="chr", main=tmp$strain[1], cex.main=2)
      axis(2, at=1:20, labels=chrs[1:20], tick=FALSE, las=1)
      pal2 = c("AA" = pal[1], "BB" = pal[2], "AB" = pal[3], "CC"=pal[4], "loss" = "black")
      legend(x=152,y=10, unique(tmp$type.explained), fill=pal2, cex=1.5)
      
      for (i in 1:20) {
        lastto<- 0
        for (j in which(tmp$chr==chrs[i])) {
          col <- switch(tmp$type[j], "AA" = pal[1], "BB" = pal[2], "AB" = pal[3], "CC"=pal[4], "loss" = "black", gain="black")
          rect(max(tmp$from[j]/10^6,lastto), i+0.25, max(tmp$to[j]/10^6,tmp$from[j]/10^6+mL), i-0.25, col=col, border=NA)
          lastto <- max(tmp$to[j]/10^6,tmp$from[j]/10^6+mL)
        }
        rect(0, i+0.25, chr.lengths[i]/10^6, i-0.25, col=NA)
      }      
    }  
  
} 

plot2geno <- function(tmp, tmp2) {
  
  mL <- 0.5 # minimal visualisation length to be visible (=0.5Mb)
  pal <- brewer.pal(5, "Set1")
  
  maxx <- max(c(tmp$to,tmp2$to))  # maximal length of chromosome
  chrs <- c(as.character(1:19),"X","Y","M")
  chr.lengths <- c(197195432, 181748087, 159599783, 155630120,
                   152537259, 149517037, 152524553, 131738871,
                   124076172, 129993255, 121843856, 121257530,
                   120284312, 125194864, 103494974, 98319150,
                   95272651, 90772031, 61342430, 166650296,
                   15902555, 16299)
  
  if (nrow(tmp)+nrow(tmp2)>0) {
    
    plot(0,1, xlim=c(0,maxx)/10^6, ylim=c(20,1), type="n", yaxt="n", xlab="Mb", ylab="chr", main=paste(tmp2$strain[1], tmp$strain[1], sep=", "), cex.main=2)
    axis(2, at=1:20, labels=chrs[1:20], tick=FALSE, las=1)
    
    for (i in 1:20) {
      lastto<- 0
      for (j in which(tmp$chr==chrs[i])) {
        col <- switch(tmp$type[j], "AA" = pal[1], "BB" = pal[2], "AB" = pal[3], "CC"=pal[4], "loss" = "black", gain="black")
        rect(max(tmp$from[j]/10^6,lastto), i+0.25, max(tmp$to[j]/10^6,tmp$from[j]/10^6+mL), i, col=col, border=NA)
        lastto <- max(tmp$to[j]/10^6,tmp$from[j]/10^6+mL)
      }
      lastto <- 0
      for (j in which(tmp2$chr==chrs[i])) {
        col <- switch(tmp2$type[j], "AA" = pal[1], "BB" = pal[2], "AB" = pal[3], "CC"=pal[4], "loss" = "black", gain="black")
        rect(max(tmp2$from[j]/10^6,lastto), i, max(tmp2$to[j]/10^6,tmp2$from[j]/10^6+mL), i-0.25, col=col, border=NA)
        lastto <- max(tmp2$to[j]/10^6,tmp2$from[j]/10^6+mL)
      }
      rect(0, i+0.25, chr.lengths[i]/10^6, i-0.25, col=NA)
      lines(x=c(0, chr.lengths[i]/10^6), y=c(i,i))
    }      
  }  
}