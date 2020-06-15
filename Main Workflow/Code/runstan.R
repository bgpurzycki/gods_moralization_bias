
### Prepping CERC Wave II set ###
d <- read.csv("./Data/ALLSITES_V3.6.csv")

labs <- c("SITE", "CID", "SEX", "AGE", "FORMALED", "BGMURDIMP", "BGLIEIMP", 
          "BGSTLIMP", "LGMURDIMP", "LGSTEALIMP", "LGLIEIMP", "LGTYPE")
data <- d[labs]
data <- d[-length(d[,1]),]

data$SITE <- factor(data$SITE)

OutcomeMoralistic <- cbind(data$BGMURDIMP,data$BGLIEIMP,data$BGSTLIMP) + 1
OutcomeLocal <- cbind(data$LGMURDIMP,data$LGLIEIMP,data$LGSTEALIMP) + 1

Site <- as.numeric(data$SITE)
Male <- data$SEX

Age <- data$AGE 
 for( i in 1:length(Age)){
  Age[i] <- Age[i] - mean(data$AGE[which(Site==Site[i])],na.rm=TRUE)
 }
 
Education <- data$FORMALED 
 for( i in 1:length(Education)){
  Education[i] <- Education[i] - mean(data$FORMALED[which(Site==Site[i])],na.rm=TRUE)
 }
 
data2 <- data.frame(Site,Age,Education,Male,OutcomeMoralistic,OutcomeLocal)

data2 <- data2[which(!is.na(data2$Age)),]
data2 <- data2[which(!is.na(data2$Education)),]
data2 <- data2[which(!is.na(data2$Male)),]

Site <- data2$Site
Age <- data2$Age
Education <- data2$Education
Male <- data2$Male
OutcomeMoralistic <- data2[,5:7]
OutcomeLocal <- data2[,8:10]

OutcomeMoralistic[is.na(OutcomeMoralistic)] <- 9
OutcomeLocal[is.na(OutcomeLocal)] <- 9

##################################################### 
N<-length(Age) # Number of respondents
P<-1           # Number of parameters
K<-5           # Number of outcome categories
D<-2           # Number of deities
S<-max(Site)  # Number of sites
Q<-3           # Number of questions

nchains <- 2

model_dat <- list(
N=N,
P=P,
K=K,
D=D,
Q=Q,
S=S,

Site=Site,
Age=Age,
Male=Male,                    
Education=Education,

OutcomeMoralistic=OutcomeMoralistic,
OutcomeLocal=OutcomeLocal
)

 fit0 <- stan(file="Code/modelcode_0.stan",  data = model_dat, thin=1, iter = 2000, warmup=1000, refresh=10, chains=nchains, seed=8675309, control=list(max_treedepth=12))
 save.image("Fit0.RData")                    
                     
                     
##################################################### 
N<-length(Age) # Number of respondents
P<-4           # Number of parameters
K<-5           # Number of outcome categories
D<-2           # Number of deities
S<-max(Site)  # Number of sites
Q<-3           # Number of questions

model_dat <- list(
N=N,
P=P,
K=K,
D=D,
Q=Q,
S=S,

Site=Site,
Age=Age,
Male=Male,                    
Education=Education,

OutcomeMoralistic=OutcomeMoralistic,
OutcomeLocal=OutcomeLocal
)

 fit1 <- stan(file="Code/modelcode_1.stan",  data = model_dat, thin=1, iter = 2000, warmup=1000, refresh=10, chains=nchains, seed=8675309, control=list(max_treedepth=12))
  save.image("Fit1.RData")                      
                          
##################################################### 
N<-length(Age) # Number of respondents
P<-2           # Number of parameters
K<-5           # Number of outcome categories
D<-2           # Number of deities
S<-max(Site)  # Number of sites
Q<-3           # Number of questions

model_dat <- list(
N=N,
P=P,
K=K,
D=D,
Q=Q,
S=S,

Site=Site,
Age=Age,
Male=Male,                    
Education=Education,

OutcomeMoralistic=OutcomeMoralistic,
OutcomeLocal=OutcomeLocal
)

 fit2 <- stan(file="Code/modelcode_2.stan",  data = model_dat, thin=1, iter = 2000, warmup=1000, refresh=10, chains=nchains, seed=8675309, control=list(max_treedepth=12))
  save.image("Fit2.RData")                                           
                     
##################################################### 
N<-length(Age) # Number of respondents
P<-5           # Number of parameters
K<-5           # Number of outcome categories
D<-2           # Number of deities
S<-max(Site)  # Number of sites
Q<-3           # Number of questions

model_dat <- list(
N=N,
P=P,
K=K,
D=D,
Q=Q,
S=S,

Site=Site,
Age=Age,
Male=Male,                    
Education=Education,

OutcomeMoralistic=OutcomeMoralistic,
OutcomeLocal=OutcomeLocal
)

 fit3 <- stan(file="Code/modelcode_3.stan",  data = model_dat, thin=1, iter = 2000, warmup=1000, refresh=10, chains=nchains, seed=8675309, control=list(max_treedepth=12))
  save.image("Fit3.RData")                                           
                     
                     
                     
                     

