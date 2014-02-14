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

## rename strains and resort them 

# bxd
dataset$strain <- sub("BXD-", "BXD", dataset$strain)
dataset$strain <- sub("(BXD[0-9]*)[!0-9]?.*", "\\1", dataset$strain)
dataset$strain <- sub("PWD/ForeJ", "<PWD/Ph>/ForeJ", dataset$strain)
dataset$strain <- sub("A/J/NaJ", "<A/J>/NaJ", dataset$strain)
dataset$strain <- sub("-MSM", "<MSM/Ms>", dataset$strain)

# reorder it
strain.levels <- unique(dataset$strain)
axbs <- strain.levels[grep("AXB", strain.levels)]
axbs <- axbs[order(as.numeric(sub("AXB","", axbs)))]
bxas <- strain.levels[grep("BXA", strain.levels)]
bxas <- bxas[order(as.numeric(sub("BXA","", bxas)))]
bxds <- strain.levels[grep("BXD", strain.levels)]
bxds <- bxds[order(as.numeric(sub("BXD","", bxds)))]
lxss <- strain.levels[grep("LXS", strain.levels)]
lxss <- lxss[order(as.numeric(sub("LXS","", lxss)))]
pwd.b6 <- strain.levels[grep("ForeJ", strain.levels)]
pwd.b6 <- pwd.b6[order(as.numeric(sub("C57BL/6J-Chr(.*)<.*","\\1", pwd.b6)))]
pwd.b6 <- c(pwd.b6[-24], pwd.b6[24]) # resort mitochondria
a.b6 <- strain.levels[grep("C57BL/6J.*NaJ", strain.levels)]
a.b6 <- a.b6[order(as.numeric(sub("C57BL/6J-Chr(.*)<.*","\\1", a.b6)))]
msm.b6 <- strain.levels[grep("MSM/Ms", strain.levels)]
msm.b6 <- msm.b6[order(as.numeric(sub("C57BL/6J-Chr([0-9]*)[CT]?<.*","\\1", msm.b6)))]
strain.levels <- c(axbs, bxas, bxds, lxss, pwd.b6, a.b6, msm.b6)
dataset$strain <- factor(dataset$strain, levels = strain.levels)

dataset <- dataset[order(dataset$panel, dataset$strain, dataset$chr, dataset$from) ,]
dataset$strain <- as.character(dataset$strain)

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
  strains[[p]] <- unique(subset(dataset, panel == p & !is.na(strain))$strain)
}  

### MISSING - conversion to mm10

# save it
saveRDS(dataset,"dataset.rds")
saveRDS(strains,"strains.rds")
saveRDS(panels, "panels.rds")

