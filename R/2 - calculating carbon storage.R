# Calculating carbon stored in each tree within the master dataset
# date = February 2021 
# Author = George Lloyd, University of Sheffield

# load necessary packages 


#----------------------------------------------------------------------------------------------------------
## tidying equations dataset

# read file with list of equations, relevant parameters and dwd (dry wood densities) for each species
equations<-read.csv("equations.232.sp.csv")

# tidy up the dbh only equation column by removing unnecessary symbols and letters
equations$dbh.equation= gsub("=","", (equations$dbh.equation))
equations$dbh.equation= gsub("\\^","", (equations$dbh.equation))
equations$dbh.equation= gsub("htm","", (equations$dbh.equation))
equations$dbh.equation= gsub("dbhcm","", (equations$dbh.equation))

# split equations column to get parameters a and b 
equations<-equations%>%
  separate(dbh.equation, into = c("a", "b" ), "\\*", remove = TRUE)

# tidy up the height and dbh equation column by removing unnecessary symbols and letters
equations$equation= gsub("=","", (equations$equation))
equations$equation= gsub("\\^","", (equations$equation))
equations$equation= gsub("htm","", (equations$equation))
equations$equation= gsub("dbhcm","", (equations$equation))
 
# split height and dbh equation column to get parameters c, d and e 
equations<-equations%>%
   separate(equation, into = c("c", "d" , "e"), "\\*", remove = TRUE)

# re-order columns
city<- city[ , c("city.name", "species", "diameter", "height", "climate")] 
 
# save this dataset
save(equations, file = "230.sp.parameter.appendix.Rdata", compress = F)
 
 
#----------------------------------------------------------------------------------------------------
 ## add parameter columns onto master_dataset (created in step 1)

# read in master dataset from step 1
load(file = "Outputs/master_dataset.csv")
 
# create parameter columns by matching parameters of each species from the above dataset onto master_dataset
 
a<-as.data.frame( (equations$a[match (master_datset$new.species, equations$new.species)]))
 colnames(a)=c("a")
 
b<-as.data.frame( (equations$b[match (master_dataset$new.species, equations$new.species)]))
 colnames(b)=c("b")
 
c<-as.data.frame( (equations$c[match (master_dataset$new.species, equations$new.species)]))
 colnames(c)=c("c")
 
d<-as.data.frame( (equations$d[match (master_dataset$new.species, equations$new.species)]))
 colnames(d)=c("d")
 
e<-as.data.frame( (equations$e[match (master_dataset$new.species, equations$new.species)]))
 colnames(e)=c("e")
 
dwd<-as.data.frame( (equations$dry.wood.density[match (master_dataset$new.species, equations$new.species)]))
 colnames(dwd)=c("dwd")
 
 
# bind these parameter columns to master_dataset creating master_5
master_5 <- cbind(master_4, a,b,c,d,e,dwd)

# split this database into species with dbh + height measurements 
dbh.and.height<-master_5 %>% subset(!is.na(height)) 
 
# and those with only dbh measurements
dbh.only<-master_5 %>% subset(is.na(height))


 
#----------------------------------------------------------------------------------------------------
## create functions to make calculations easy and fast 

# Function 1 - calculate carbon for trees with height and diameter measurements
carbon.calculation.1 <- function(row){
  #a <- row[grep("a", names(row))]
  c <- as.numeric(row[match("c", names(row))])
  d <-as.numeric(row[match("d", names(row))])
  e <- as.numeric(row[match("e", names(row))])
  diameter <- as.numeric(row[match("diameter", names(row))])
  height <- as.numeric(row[match("height", names(row))])
  dwd <- as.numeric(row[match("dwd", names(row))])
  carbon <- c*diameter^d*height^e*dwd*1.28*0.5
  
}

# Function 2 - calculate carbon for trees with just diameter measurements
carbon.calculation.2 <- function(row){
   #a <- row[grep("a", names(row))]
   a <- as.numeric(row[match("a", names(row))])
   b <-as.numeric(row[match("b", names(row))])
   diameter <- as.numeric(row[match("diameter", names(row))])
   dwd <- as.numeric(row[match("dwd", names(row))])
   carbon <- a*diameter^b*dwd*1.28*0.5
   
 }
 
## actual calculations
 
# apply function 1 (height and dbh)
 
# disable scientific notation
options(scipen=999)
 
# apply function 
carbon.dbh.height <- apply(dbh.and.height,1,carbon.calculation.1)

# attach carbon results onto dataset with species that have both dbh and height
dbh.and.height<-mutate(dbh.and.height, carbon = carbon.dbh.height)
   
# apply function 2 (only dbh)
carbon.dbh <-apply(dbh.only,1,carbon.calculation.2)

# attach carbon results onto dataset with species that only have dbh
dbh.only<-mutate(dbh.only, carbon = carbon.dbh)

## bind these 2 datasets together again to reform master dataset
master_5 <- rbind(dbh.only, dbh.and.height)

 
#--------------------------------------------------------------------------------------------------------------------------------------
## removing trees with wild diameters and height
subset<-master_5 %>%
  subset(!diameter <=1)%>%
  subset (!diameter >=200) %>%
  subset (!height >=70) %>%
  subset(!height <=0.5)
  
# subset cities to remove those with NA's for height
dbh.only.cities<-subset(master_5, city.name == "Bristol" | city.name == "Oslo" | city.name == "Girona" | 
       city.name == "Hamburg")

# remove trees with impossible diameters in these cities that only have dbh measurements 
dbh.only.cities<-dbh.only.cities %>%
   subset(!diameter <=1)%>%
   subset (!diameter >=200)

# bind this subset back to form finished  master_dataset_tidy
master_dataset_tidy<- rbind(subset, dbh.only.cities)

# save dataset as master_dataset_tidy
save(master_dataset_tidy, file = "Outputs/master_dataset_tidy.csv", compress = F)

