### Prepping CERC Wave II set ###
d <- read.csv("./Data/ALLSITES_V3.6.csv")

labs <- c("SITE", "CID", "SEX", "AGE", "FORMALED", "BGMURDIMP", "BGLIEIMP", 
          "BGSTLIMP", "LGMURDIMP", "LGSTEALIMP", "LGLIEIMP", "LGTYPE")
data <- d[labs]

 scrap<-cbind(data$BGMURDIMP,data$BGLIEIMP,data$BGSTLIMP)
MI <- matrix(NA,ncol=4,nrow=15)
 for( i in 1:15){
  for(j in 0:3){  
  scrap2 <- ifelse(scrap[which( data$SITE==unique(data$SITE)[i] ),]>j,1,0) 
  MI[i,j+1] <- mean(scrap2,na.rm=TRUE)           
                }}
                
                
 scrap<-cbind(data$LGMURDIMP,data$LGLIEIMP,data$LGSTEALIMP)
LI <- matrix(NA,ncol=4,nrow=15)
 for( i in 1:15){
  for(j in 0:3){  
  scrap2 <- ifelse(scrap[which( data$SITE==unique(data$SITE)[i] ),]>j,1,0) 
  LI[i,j+1] <- mean(scrap2,na.rm=TRUE)           
                }}
                
                
 
dHADZA <- read.csv("./Data/CERC Dataset (Wave 1) Version 6.0.csv")
scrap <- cbind(dHADZA$BGSTEALH,dHADZA$BGLYINGH,dHADZA$BGSTEALH)
MI[11,1] <- mean(ifelse(scrap>-1,1,0),na.rm=TRUE)
MI[11,2] <- mean(ifelse(scrap>0,1,0),na.rm=TRUE)  

scrap<- cbind(dHADZA$LGSTEALH,dHADZA$LGLYINGH,dHADZA$LGMURDERH)
LI[11,1]<- mean(ifelse(scrap>-1,1,0),na.rm=TRUE)
LI[11,2] <- mean(ifelse(scrap>0,1,0),na.rm=TRUE)  

              
Sites <- as.character(unique(data$SITE))  
Sites[which(Sites=="Co.Tanna")] <- "Coastal Tanna"
Sites[which(Sites=="In.Tanna")] <- "Inland Tanna"
Sites[which(Sites=="Tyva Republic")] <- "Tyva"   
Sites[which(Sites=="Marajo")] <- "Marajó"            
Site <- Sites[1:15]  




 df_mg <- data.frame(Site=rep(Site,4), Index=c(MI[,1],MI[,2],MI[,3],MI[,4]),
                     Category=rep(1:4,each=15),Deity="Moral")
                     
 df_lg <- data.frame(Site=rep(Site,4), Index=c(LI[,1],LI[,2],LI[,3],LI[,4]),
                     Category=rep(1:4,each=15),Deity="Local")

df <- rbind(df_mg,df_lg) 

df$Deity <- factor(df$Deity)
df$Deity <- factor(df$Deity,levels(df$Deity)[c(2,1)])
                                        
gg <- ggplot(df, aes(x=Category, y=Index, group=Deity)) +
  geom_line(aes(linetype=Deity))+ xlab("Moralistic Punishment Index")+ ylab("Proportion of Sample")+
  geom_point(aes(shape=Deity)) + facet_wrap( ~ Site, ncol=3) + theme(strip.text.x = element_text(size=16),
    strip.text.y = element_text(size=16),axis.text=element_text(size=12),axis.title.y=element_text(size=18,
    face="bold"),axis.title.x=element_text(size=18,face="bold"))+theme(strip.text.y = element_text(angle = 360),
    legend.text=element_text(size=14)) + theme(panel.spacing = unit(0.75, "lines"))+ geom_segment(aes(x = 1, y = 0.8, xend = 4, yend = 0.2), linetype = "dashed",color="darkred")
gg
 ggsave("DescriptivePlot.pdf",gg,height=11,width=8.5)   

df2 <- df_mg

df2$Index <- df_mg$Index-df_lg$Index
df2$Deity <- rep("Difference", length(df2$Deity))
              
gg2 <- ggplot(df2, aes(x=Category, y=Index, group=Deity)) +
  geom_line(aes(linetype=Deity))+ xlab("Moralistic Punishment Index")+ ylab("Proportion of Sample")+
  geom_point(aes(shape=Deity)) + facet_wrap( ~ Site, ncol=3) + theme(strip.text.x = element_text(size=16),
    strip.text.y = element_text(size=16),axis.text=element_text(size=12),axis.title.y=element_text(size=18,
    face="bold"),axis.title.x=element_text(size=18,face="bold"))+theme(strip.text.y = element_text(angle = 360),
    legend.text=element_text(size=14)) + theme(panel.spacing = unit(0.75, "lines"))
gg2
 ggsave("DescriptivePlot_Diffs.pdf",gg2,height=11,width=8.5)   

              