#DATALOADING
data = read.csv("C:\\Users\\caroh\\Downloads\\flare_catalog_plutino_2023_04__1986_01.csv")
ID = data$multipleID
unID = unique(ID) #set of all unique IDs
dates = data$tstart
pflux = data$peak_flux
categ = data$cat


#Defining variables:
b = vector('numeric', length(unID))
fluxies = vector('numeric', length(unID))
dates1 = vector('numeric', length(unID))
solarclass = vector('numeric', length(unID))

#Find adapted variables
for (i in 1:length(unID)) {
  print(i)
  
  #creating list of all possible values per multiID
  k = pflux[ID == unID[i]]
  v = dates[ID == unID[i]]
  o = categ[ID == unID[i]]
  
  #taking the max as the extreme values are our interest
  fluxies[i] = max(k)
  dates1[i] = min(v)
  solarclass[i] = max(o)
}

