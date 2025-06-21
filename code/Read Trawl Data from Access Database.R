library(RODBC)

# from Athena Ogden

#Connect to your Access database:
setwd("C:\\Users\\StiffH\\Documents\\FISHERIES\\SALMON INDEX STOCKS\\Trawl\\TrawlData")
getwd()

conn <- odbcConnectAccess2007("Trawl Database.accdb") # ("Trawl Database 77-98 (No AGEs).accdb")

#Fetch a table:
TrawlSamples <- sqlFetch(conn, "TrawlSamples")

# Close the connection:
odbcClose(conn)

trawl.samples <- TrawlSamples

write.csv(trawl.samples,"trawlSamples from Access.csv")
