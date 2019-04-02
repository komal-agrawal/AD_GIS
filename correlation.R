

library(dplyr)


#Pearson correlation coefficient
master.data <- read.csv("C:/Users/komal/Documents/AD_GIS_Project/Data/master_data.csv")
CDC.data <- read.csv("C:/Users/komal/Documents/AD_GIS_Project/Data/CDC_ad.csv")

# Subset TNSSS dataset
master.data <- master.data[, c("Analyte", "Region", "Analyte.Conc.")]
master.data <- na.omit(master.data)
CDC.data <- CDC.data[, c("Census.Region", "Crude.Rate")]
colnames(CDC.data) <- c("Region", "Crude.Rate")

#Get number of each region in CDC AD data
nrow(CDC.data[CDC.data$Census.Region == "Northeast", ]) #9
nrow(CDC.data[CDC.data$Census.Region == "West", ]) #13
nrow(CDC.data[CDC.data$Census.Region == "South", ]) #17
nrow(CDC.data[CDC.data$Census.Region == "Midwest", ])#12

# Randomly sample data from each region for each analyte
random_sample <- function(master, analyte)
{
  master <- master[master$Analyte == analyte, ]
  TNSSS.NE <- sample_n(master[master$Region == "Northeast", ], 9)
  TNSSS.W <- sample_n(master[master$Region == "West", ], 13)
  TNSSS.S <- sample_n(master[master$Region == "South", ], 17)
  TNSSS.MW <- sample_n(master[master$Region == "Midwest", ], 12)
  tnsss <- rbind(TNSSS.NE, TNSSS.W, TNSSS.S, TNSSS.MW)
  return(tnsss)
}

cor_multiple <- function(master, analyte, cdc)
{
  iter = 0
  sig.list <- list()
  while (iter < 5)
  {
    random.sample <- random_sample(master = master, analyte = analyte)
    df <- merge(random.sample, cdc, by="Region")
    analyte.cor <- cor(df$Analyte.Conc., df$Crude.Rate, method = "pearson")
    sig.list <- c(sig.list, analyte.cor)
    iter = iter + 1
  }
  return(sig.list)
}

# Aluminum 
al.cor <- cor_multiple(master = master.data, analyte = "ALUMINUM", cdc = CDC.data)

# Arsenic
arsenic.cor <- cor_multiple(master = master.data, analyte = "ARSENIC", cdc = CDC.data)

# Cadmium 
cd.cor <- cor_multiple(master = master.data, analyte = "CADMIUM", cdc = CDC.data)

# Minocycline
mi.cor <- cor_multiple(master = master.data, analyte = "MINOCYCLINE", cdc = CDC.data)

# Doxycycline
do.cor <- cor_multiple(master = master.data, analyte = "DOXYCYCLINE", cdc = CDC.data)

# Ibuprofen
ib.cor <- cor_multiple(master = master.data, analyte = "IBUPROFEN", cdc = CDC.data)

# Caffeine
caf.cor <- cor_multiple(master = master.data, analyte = "CAFFEINE", cdc = CDC.data)

# Fluoxetine
fl.cor <- cor_multiple(master = master.data, analyte = "FLUOXETINE", cdc = CDC.data)

# 17 Alpha Estradiol
es.cor <- cor_multiple(master = master.data, analyte = "17 ALPHA-ESTRADIOL", cdc = CDC.data)

# Progesterone
pro.cor <- cor_multiple(master = master.data, analyte = "PROGESTERONE", cdc = CDC.data)

cor.df <- data.frame(rbind(al.cor, arsenic.cor, cd.cor, do.cor, ib.cor, caf.cor, fl.cor, es.cor, pro.cor))
row.names(cor.df) <-  c("Aluminum", "Arsenic", "Cadmium", "Doxycycline",
              "Ibuprofen", "Caffeine", "Fluoxetine", "17 Alpha Estradiol", "Progesterone")
colnames(cor.df) <- c("r1", "r2", "r3", "r4", "r5")

cor.df$r1 <- unlist(cor.df$r1)
cor.df$r2 <- unlist(cor.df$r2)
cor.df$r3 <- unlist(cor.df$r3)
cor.df$r4 <- unlist(cor.df$r4)
cor.df$r5 <- unlist(cor.df$r5)

write.table(cor.df, file = "C:/Users/komal/Documents/AD_GIS_Project/Analysis/correlation_coef.csv", sep = ",")
