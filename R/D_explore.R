#exploring data

head(fates)

fates$removed <- fates$density - fates$intact #creates the "removed" column

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

# removal totals per season -----------------------------------------------

#sum of removed endocarps - dry season
sum(fates$removed[fates$season=="dry"])

#percentage of removed endocarps - dry season
sum(fates$removed[fates$season=="dry"])/1000*100

#sum of removed endocarps - wet season
sum(fates$removed[fates$season=="wet"])

#percentage of removed endocarps - wet season
sum(fates$removed[fates$season=="wet"])/1500*100
