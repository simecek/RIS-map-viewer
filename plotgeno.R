# RColorBrewer pallete - Set1
pal <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")

# switch between mm9 and mm10 assembly
aconvert <- function(seglist, assembly) {
  # for mm10 assembly copy mm10from/mm10to to into from/to
  if (assembly=="mm10") {
    seglist$from <- seglist$mm10from
    seglist$to <- seglist$mm10to
  }
  
  # delete mm10from/mm10to columns
  seglist <- seglist[,-grep("mm10",names(seglist))]
  
  # rename from/to -> start/end
  names(seglist)[names(seglist)=="from"] <- "start"
  names(seglist)[names(seglist)=="to"] <- "end"
  
  seglist
}

# switch between mm9 and mm10 assembly
aconvert2 <- function(csvr, assembly) {
  # for mm10 assembly copy mm10from/mm10to to into from/to
  if (assembly=="mm10") {
    csvr$positionBp <- csvr$mm10positionBp
  }
  
  # delete mm10from/mm10to columns
  csvr <- csvr[,-grep("mm10positionBp",names(csvr))]
  
  csvr
}

chr.lengths <- function(assembly="mm9") {
  if (assembly=="mm9") { 
    # length taken as in BSgenome.Mmusculus.UCSC.mm9
    return(c(197195432L, 181748087L, 159599783L, 155630120L, 152537259L, 
      149517037L, 152524553L, 131738871L, 124076172L, 129993255L, 121843856L, 
      121257530L, 120284312L, 125194864L, 103494974L, 98319150L, 95272651L, 
      90772031L, 61342430L, 166650296L, 15902555L, 16299L))
  } 
  if (assembly=="mm10") { 
    # length taken as in BSgenome.Mmusculus.UCSC.mm10
    return(c(195471971L, 182113224L, 160039680L, 156508116L, 151834684L, 
             149736546L, 145441459L, 129401213L, 124595110L, 130694993L, 122082543L, 
             120129022L, 120421639L, 124902244L, 104043685L, 98207768L, 94987271L, 
             90702639L, 61431566L, 171031299L, 91744698L, 16299L))
  } 
  stop("Genome assembly unknown")
}

# plot function for one strain
plotgeno <- function(seglist, chr.lengths) {

  mL <- 0.5 # minimal visualisation length end be visible (=0.5Mb)
  pal2 = c("AA" = pal[1], "BB" = pal[2], "AB" = pal[4], "CC"=pal[3], "loss" = "black")
  
  maxx <- max(seglist$end)  # maximal length of chromosome
  chrs <- c(as.character(1:19),"X","Y","M")
  
  # sort seglist
  seglist <- seglist[order(seglist$chr, seglist$start),]
  
    if (nrow(seglist)>0) {
      
      plot(0,1, xlim=c(0,maxx)/10^6, ylim=c(20,1), type="n", yaxt="n", xlab="Mb", ylab="chr", main=seglist$strain[1], cex.main=2)
      axis(2, at=1:20, labels=chrs[1:20], tick=FALSE, las=1)
      sel.pal <- names(pal2) %in% seglist$type
      sel.pal.explained <- rep("", sum(sel.pal))
      for (i in 1:length(sel.pal.explained))
        sel.pal.explained[i] <- subset(seglist, type == names(pal2)[sel.pal][i])$type.explained[1]
      legend(x=152,y=10, sel.pal.explained, fill=pal2[sel.pal], cex=1.5)
      
      
      for (i in 1:20) {
        lastend<- 0
        for (j in which(seglist$chr==chrs[i])) {
          col <- switch(seglist$type[j], "AA" = pal[1], "BB" = pal[2], "AB" = pal[4], "CC"=pal[3], "loss" = "black", gain="black")
          rect(max(seglist$start[j]/10^6,lastend), i+0.25, max(seglist$end[j]/10^6,seglist$start[j]/10^6+mL), i-0.25, col=col, border=NA)
          lastend <- max(seglist$end[j]/10^6,seglist$start[j]/10^6+mL)
        }
        rect(0, i+0.25, chr.lengths[i]/10^6, i-0.25, col=NA)
      }      
    }  
  
} 

# plot function for two strains comparison
plot2geno <- function(seglist, seglist2, chr.lengths) {
  
  mL <- 0.5 # minimal visualisation length end be visible (=0.5Mb)
  pal2 = c("AA" = pal[1], "BB" = pal[2], "AB" = pal[4], "CC"=pal[3], "loss" = "black")
  
  maxx <- max(c(seglist$end,seglist2$end))  # maximal length of chromosome
  chrs <- c(as.character(1:19),"X","Y","M")
  
  if (nrow(seglist)+nrow(seglist2)>0) {
    
    plot(0,1, xlim=c(0,maxx)/10^6, ylim=c(20,1), type="n", yaxt="n", xlab="Mb", ylab="chr", main=paste(seglist2$strain[1], seglist$strain[1], sep=", "), cex.main=2)
    axis(2, at=1:20, labels=chrs[1:20], tick=FALSE, las=1)
    sel.pal <- names(pal2) %in% seglist$type
    sel.pal.explained <- rep("", sum(sel.pal))
    for (i in 1:length(sel.pal.explained))
      sel.pal.explained[i] <- subset(seglist, type == names(pal2)[sel.pal][i])$type.explained[1]
    legend(x=152,y=10, sel.pal.explained, fill=pal2[sel.pal], cex=1.5)
    
    
    for (i in 1:20) {
      lastend<- 0
      for (j in which(seglist$chr==chrs[i])) {
        col <- switch(seglist$type[j], "AA" = pal[1], "BB" = pal[2], "AB" = pal[4], "CC"=pal[3], "loss" = "black", gain="black")
        rect(max(seglist$start[j]/10^6,lastend), i+0.25, max(seglist$end[j]/10^6,seglist$start[j]/10^6+mL), i, col=col, border=NA)
        lastend <- max(seglist$end[j]/10^6,seglist$start[j]/10^6+mL)
      }
      lastend <- 0
      for (j in which(seglist2$chr==chrs[i])) {
        col <- switch(seglist2$type[j], "AA" = pal[1], "BB" = pal[2], "AB" = pal[4], "CC"=pal[3], "loss" = "black", gain="black")
        rect(max(seglist2$start[j]/10^6,lastend), i, max(seglist2$end[j]/10^6,seglist2$start[j]/10^6+mL), i-0.25, col=col, border=NA)
        lastend <- max(seglist2$end[j]/10^6,seglist2$start[j]/10^6+mL)
      }
      rect(0, i+0.25, chr.lengths[i]/10^6, i-0.25, col=NA)
      lines(x=c(0, chr.lengths[i]/10^6), y=c(i,i))
    }      
  }  
}