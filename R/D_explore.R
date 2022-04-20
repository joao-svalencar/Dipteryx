#exploring data

head(fates)

fates$removed <- fates$density - fates$intact #creates the "removed" column
fates$found <- fates$removed - fates$lost #creates the "found" column

# sum and percentage of removed endocarps (per remnant) -------------------

# Sao Matheus farm --------------------------------------------------------
fates[fates$remnant=="smat",] #showing data

#sum of removed endocarps
sum(fates$removed[fates$season=="dry"& fates$remnant == "smat"])
sum(fates$removed[fates$season=="wet"& fates$remnant == "smat"])

#percentage of removed endocarps
sum(fates$removed[fates$season=="dry"& fates$remnant == "smat"])/500*100
sum(fates$removed[fates$season=="wet"& fates$remnant == "smat"])/500*100

# Rodeio farm -------------------------------------------------------------
fates[fates$remnant=="rod",] #showing data

#sum of removed endocarps
sum(fates$removed[fates$season=="dry"& fates$remnant == "rod"])
sum(fates$removed[fates$season=="wet"& fates$remnant == "rod"])

#percentage of removed endocarps
sum(fates$removed[fates$season=="dry"& fates$remnant == "rod"])/500*100
sum(fates$removed[fates$season=="wet"& fates$remnant == "rod"])/500*100

# Bom Jardim farm ---------------------------------------------------------
fates[fates$remnant=="bjad",] #showing data

#sum of removed endocarps
sum(fates$removed[fates$season=="dry"& fates$remnant == "bjad"])
sum(fates$removed[fates$season=="wet"& fates$remnant == "bjad"])

#percentage of removed endocarps
sum(fates$removed[fates$season=="dry"& fates$remnant == "bjad"])/500*100
sum(fates$removed[fates$season=="wet"& fates$remnant == "bjad"])/500*100


# removing Bom Jardim farm dry season data --------------------------------
fates <- fates[!(fates$season == "dry" & fates$remnant == "bjad"),]

# sum and percentage of removed endocarps (TOTAL) -------------------------
sum(fates$removed[fates$season=="dry"])
sum(fates$removed[fates$season=="dry"])/1000*100

sum(fates$removed[fates$season=="wet"])
sum(fates$removed[fates$season=="wet"])/1500*100

# sum and percentage of retrieved (found) endocarps  ----------------------
sum(fates$found[fates$season=="dry"])
round(sum(fates$found[fates$season=="dry"])/sum(fates$removed[fates$season=="dry"])*100, 2)

sum(fates$found[fates$season=="wet"])
round(sum(fates$found[fates$season=="wet"])/sum(fates$removed[fates$season=="wet"])*100, 2)


# sum and percentage of preyed endocarps ----------------------------------
sum(fates$preyed[fates$season=="dry"])
round(sum(fates$preyed[fates$season=="dry"])/sum(fates$found[fates$season=="dry"])*100, 2)

sum(fates$preyed[fates$season=="wet"])
round(sum(fates$preyed[fates$season=="wet"])/sum(fates$found[fates$season=="wet"])*100, 2)

