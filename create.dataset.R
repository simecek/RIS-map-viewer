dataset <- read.csv("imap.together.csv", as.is=TRUE)
display <- read.csv("master.together.csv", as.is=TRUE)

# rename strains
dataset$strain <- display$display[match(dataset$strain, make.names(display$name))]
dataset$type.explained = NA
dataset$type.explained[dataset$type %in% c("gain", "loss")] <- "CNV"
dataset$type.explained[dataset$type =="CC"] <- "Contamination"
dataset$type.explained[dataset$type =="AB"] <- "Heterozygous"
dataset$type.explained[dataset$type =="AA" & dataset$panel != "LXS"] <- "C57BL/6J"
dataset$type.explained[dataset$type =="AA" & dataset$panel == "LXS" ] <- "ILS"
dataset$type.explained[dataset$type =="BB" & dataset$panel %in% c("AXB","A.B6") ] <- "A/J"
dataset$type.explained[dataset$type =="BB" & dataset$panel == "BXD" ] <- "DBA/2J"
dataset$type.explained[dataset$type =="BB" & dataset$panel == "LXS" ] <- "ISS"
dataset$type.explained[dataset$type =="BB" & dataset$panel == "MSM.B6" ] <- "MSM/Ms"
dataset$type.explained[dataset$type =="BB" & dataset$panel == "PWD.B6" ] <- "PWD/Ph"

# reorder it
dataset <- dataset[order(dataset$strain, dataset$chr, dataset$from) ,]

# panels headers
panels <- c("AXB/BXA RIS", 
            "BXD RIS", 
            "LXS RIS",
            "B6.A CSS",
            "B6.PWD CSS",
            "B6.MSM CSS")
old.panels <- c("AXB", "BXD", "LXS", "A.B6", "PWD.B6", "MSM.B6")
for (i in 1:6) {
  dataset$panel[dataset$panel == old.panels[i]] <- panels[i]
}

# create list of strains per panel
strains = list()
for (p in unique(dataset$panel)) {
  strains[[p]] <- unique(subset(dataset, panel == p)$strain)
  # for CSS remove F1 strain
  if (substr(strains[[p]][1],1,2) == "B6")
    strains[[p]] <- strains[[p]][-1]
}  




# save it
saveRDS(dataset,"dataset.rds")
saveRDS(strains,"strains.rds")
saveRDS(panels, "panels.rds")

