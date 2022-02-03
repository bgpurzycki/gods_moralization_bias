### Prepping CERC Wave II set ###
d <- read.table("./Data/ALLSITES_V3.7_tabdel.txt", sep = '\t', header = T)

labs <- c("SITE", "CID", "SEX", "AGE", "FORMALED", "BGMURDIMP", "BGLIEIMP", 
          "BGSTLIMP", "LGMURDIMP", "LGSTEALIMP", "LGLIEIMP", "LGTYPE")
data <- d[labs]

# Dummy code function
mordum <- function(vd, vo){
  vd <- NA
  vd[vo==0] <- 0
  vd[vo>=1] <- 1
  return(vd)
}

data$MURDBIN.BG <-mordum(data$MURDBIN.BG, data$BGMURDIMP) 
data$LIEBIN.BG <- mordum(data$LIEBIN.BG, data$BGLIEIMP)
data$STLBIN.BG <- mordum(data$STLBIN.BG, data$BGSTLIMP)
data$MURDBIN.LG <- mordum(data$MURDBIN.LG, data$LGMURDIMP)
data$LIEBIN.LG <- mordum(data$LIEBIN.LG, data$LGLIEIMP)
data$STLBIN.LG <- mordum(data$STLBIN.LG, data$LGSTEALIMP)

# Code for Christian sites
sit.c <- c("Co.Tanna","Yasawa","Kananga","Samburu","Turkana","Huatasani","Marajo","Sursurunga","Cachoeira")
data$CHRIST[data$SITE %in% sit.c] <- 1
data$CHRIST[!data$SITE %in% sit.c] <- 0

# Recode for LGTYPE (Huatasani and Kananga had two different local deities)
data$site.n <- NA
data$site.n[data$LGTYPE==1] <- "Huatasani.a"
data$site.n[data$LGTYPE==2] <- "Huatasani.s"
data$site.n[data$LGTYPE==3] <- "Kananga.k"
data$site.n[data$LGTYPE==4] <- "Kananga.a"
data$site.n[data$SITE=="Co.Tanna"] <- "Co.Tanna"
data$site.n[data$SITE=="Yasawa"] <- "Yasawa"
data$site.n[data$SITE=="Samburu"] <- "Samburu"
data$site.n[data$SITE=="Turkana"] <- "Turkana"
data$site.n[data$SITE=="Marajo"] <- "Marajo"
data$site.n[data$SITE=="Sursurunga"] <- "Sursurunga"
data$site.n[data$SITE=="Cachoeira"] <- "Cachoeira"
data$site.n[data$SITE=="Tyva Republic"] <- "Tyva Republic"
data$site.n[data$SITE=="Hadza"] <- "Hadza"
data$site.n[data$SITE=="In.Tanna"] <- "In.Tanna"
data$site.n[data$SITE=="Lovu"] <- "Lovu"
data$site.n[data$SITE=="Mauritius"] <- "Mauritius"
data$site.n[data$SITE=="Mysore"] <- "Mysore"

# Center function
centerbysite <- function(v,s) {
  return(v - tapply(v, s, mean, na.rm = T)[s])
}

data$AGE.C <- centerbysite(data$AGE, data$SITE)
data$EDU.C <- centerbysite(data$FORMALED, data$SITE)

# Moral index summations
data$BGMORAL <- data$BGMURDIMP+data$BGLIEIMP+data$BGSTLIMP
data$LGMORAL <- data$LGMURDIMP+data$LGLIEIMP+data$LGSTEALIMP

#write.csv(data, "data.to.stack.csv")

########################
## Table 1: Demographics
########################
demog<-summaryBy(SEX + AGE + FORMALED ~ SITE, 
       data = data, FUN = function(x) { c(N = length(x), 
       n = sum(x, na.rm = TRUE), m = mean(x, na.rm=TRUE), s = sd(x, na.rm=TRUE)) } )

tlabs <- c("SITE", "SEX.N", "SEX.n", "AGE.m", "AGE.s", "FORMALED.m", "FORMALED.s")
demtab <- demog[tlabs]

demtab$SITE <- as.character(demtab$SITE)
demtab$SITE[which(demtab$SITE=="Co.Tanna")] <- "Coastal Tanna"
demtab$SITE[which(demtab$SITE=="In.Tanna")] <- "Inland Tanna"
demtab$SITE[which(demtab$SITE=="Tyva Republic")] <- "Tyva" 
demtab$SITE[which(demtab$SITE=="Marajo")] <- "Maraj\\'o"      
                    
demtab<-demtab[-1,]       

#################################### Final
res <- matrix(NA,ncol=9,nrow=16)             
colnames(res) <- c("Site", "N", "Males", "Age", "Yrs. Ed.", "Moralistic Deity", "Moral Index (MD)", "Local Deity","Moral Index (LD)")  

res[,1] <- c(demtab$SITE, "Mean")  
res[,2] <- c(demtab$SEX.N, round(mean(demtab$SEX.N),1))   
res[,3] <- c(demtab$SEX.n, round(mean(demtab$SEX.n),1))    
res[,4] <- c(paste0(sprintf("%.1f",round(demtab$AGE.m,1))," (",sprintf("%.1f",round(demtab$AGE.s,1)),")"), paste0(sprintf("%.1f",round(mean(data$AGE,na.rm=TRUE),1))," (",sprintf("%.1f",round(sd(data$AGE,na.rm=TRUE),1)),")"))             
res[,5] <- c(paste0(sprintf("%.1f",round(demtab$FORMALED.m,1))," (",sprintf("%.1f",round(demtab$FORMALED.s,1)),")"), paste0(sprintf("%.1f",round(mean(data$FORMALED,na.rm=TRUE),1))," (",sprintf("%.1f",round(sd(data$FORMALED,na.rm=TRUE),1)),")")) 
res[,6] <- c("Christian god","Christian god","Haine","Christian god","Kalpapan","Christian god","Shiva","Christian god","Shiva",
              "Shiva","Christian god","Christian god","Christian god","Buddha Burgan","Christian god","---")
res[,8] <- c("Ogum","Garden spirit","Ishoko","Mountain spirits/saints$^*$","Garden spirit","Kadima/ancestor spirits$^*$","---","Saint Mary","Nam (a spirit)",
              "Chamundeschwari","---","Forest spirit","Ancestor spirits","Spirit-masters","Ancestor spirits","---")
  
 scrap<-cbind(data$BGMURDIMP,data$BGLIEIMP,data$BGSTLIMP)
MI <- c()
MIsd <- c() 
 for( i in 1:15){  
  scrap2 <- ifelse(scrap[which( data$SITE==unique(data$SITE)[i] ),]>0,1,0) 
  MI[i] <- mean(scrap2,na.rm=TRUE)
  MIsd[i] <- sd(scrap2,na.rm=TRUE)               
                }               
                 
m<-cbind(MI,MIsd,as.character(unique(data$SITE)[1:15]))
m<-m[order(m[,3]),]
  
 scrap<-cbind(data$LGMURDIMP,data$LGLIEIMP,data$LGSTEALIMP)
LI <- c()
LIsd <- c() 
 for( i in 1:15){  
   scrap2 <- ifelse(scrap[which( data$SITE==unique(data$SITE)[i] ),]>0,1,0) 
   LI[i] <- mean(scrap2,na.rm=TRUE)
   LIsd[i] <- sd(scrap2,na.rm=TRUE)              
                }  
  
l<-cbind(LI,LIsd,as.character(unique(data$SITE)[1:15]))
l<-l[order(l[,3]),]

res[1:15,7] <- paste0(sprintf("%.2f",round(as.numeric(as.character(m[,1])),2)), " (",sprintf("%.2f",round(as.numeric(as.character(m[,2])),2)),")") 
res[1:15,9] <- paste0(sprintf("%.2f",round(as.numeric(as.character(l[,1])),2)), " (",sprintf("%.2f",round(as.numeric(as.character(l[,2])),2)),")") 

res[c(7,11),9] <- "---"


dHADZA <- read.csv("./Data/CERC Dataset (Wave 1) Version 6.0.csv")
scrap <- cbind(dHADZA$BGSTEALH,dHADZA$BGLYINGH,dHADZA$BGSTEALH)
scrap2 <- ifelse(scrap>0,1,0) 
res[3,7] <- paste0(sprintf("%.2f",round(as.numeric(as.character(mean(scrap2,na.rm=TRUE))),2)), " (",sprintf("%.2f",round(as.numeric(as.character(sd(scrap2,na.rm=TRUE))),2)),")") 


scrap<- cbind(dHADZA$LGSTEALH,dHADZA$LGLYINGH,dHADZA$LGMURDERH)
scrap2 <- ifelse(scrap>0,1,0) 
res[3,9] <- paste0(sprintf("%.2f",round(as.numeric(as.character(mean(scrap2,na.rm=TRUE))),2)), " (",sprintf("%.2f",round(as.numeric(as.character(sd(scrap2,na.rm=TRUE))),2)),")") 

scrap <-rbind(cbind(data$BGMURDIMP,data$BGLIEIMP,data$BGSTLIMP),cbind(dHADZA$BGSTEALH,dHADZA$BGLYINGH,dHADZA$BGSTEALH))
scrap2 <- ifelse(scrap>0,1,0)
res[16,7] <- paste0(sprintf("%.2f",round(as.numeric(as.character(mean(scrap2,na.rm=TRUE))),2)), " (",sprintf("%.2f",round(as.numeric(as.character(sd(scrap2,na.rm=TRUE))),2)),")") 

scrap <-rbind(cbind(data$LGMURDIMP,data$LGLIEIMP,data$LGSTEALIMP),cbind(dHADZA$LGSTEALH,dHADZA$LGLYINGH,dHADZA$LGMURDERH))
scrap2 <- ifelse(scrap>0,1,0)
res[16,9] <- paste0(sprintf("%.2f",round(as.numeric(as.character(mean(scrap2,na.rm=TRUE))),2)), " (",sprintf("%.2f",round(as.numeric(as.character(sd(scrap2,na.rm=TRUE))),2)),")") 
  
write.csv(res,"descriptive.csv", row.names=FALSE, quote = FALSE) 
 
