

################################################################ Predictive Plot
B <- rstan::extract(fit3,"Beta")$Beta
C <- rstan::extract(fit3,"C")$C
H <- rstan::extract(fit3,"C_Shape")$C_Shape

process <- function(x) { return(c(mean(x),HPDI(x,0.9)))}

MD<-array(NA,c(3,S,Q))
LD<-array(NA,c(3,S,Q,K))

for(q in 1:Q){
for(s in 1:S){

MD[,s,q] <- process(logistic(B[ ,s,q,1,1]-C[,1]))
for(k in 1:K){
LD[,s,q,k] <- process(logistic((B[ ,s,q,2,1] + B[ ,s,q,2,5]*H[,s,k])-C[,1]))
}
}
}


MeanMD <- c(MD[1,1:S,1],MD[1,1:S,2],MD[1,1:S,3])
LowerMD <- c(MD[2,1:S,1],MD[2,1:S,2],MD[2,1:S,3])
UpperMD <- c(MD[3,1:S,1],MD[3,1:S,2],MD[3,1:S,3])
SiteMD <- rep(1:S,3)
QuestionMD <- rep(1:3,each=S)

df_MD <- data.frame(Mean=MeanMD,Lower=LowerMD,Upper=UpperMD,Site=SiteMD,Question=QuestionMD,Type="MD")


k<-1
MeanLD <- c(LD[1,1:S,1,k],LD[1,1:S,2,k],LD[1,1:S,3,k])
LowerLD <- c(LD[2,1:S,1,k],LD[2,1:S,2,k],LD[2,1:S,3,k])
UpperLD <- c(LD[3,1:S,1,k],LD[3,1:S,2,k],LD[3,1:S,3,k])
SiteLD <- rep(1:S,3)
QuestionLD <- rep(1:3,each=S)

df_LD_k1 <- data.frame(Mean=MeanLD,Lower=LowerLD,Upper=UpperLD,Site=SiteLD,Question=QuestionLD,Type="LD_0")

k<-2
MeanLD <- c(LD[1,1:S,1,k],LD[1,1:S,2,k],LD[1,1:S,3,k])
LowerLD <- c(LD[2,1:S,1,k],LD[2,1:S,2,k],LD[2,1:S,3,k])
UpperLD <- c(LD[3,1:S,1,k],LD[3,1:S,2,k],LD[3,1:S,3,k])
SiteLD <- rep(1:S,3)
QuestionLD <- rep(1:3,each=S)

df_LD_k2 <- data.frame(Mean=MeanLD,Lower=LowerLD,Upper=UpperLD,Site=SiteLD,Question=QuestionLD,Type="LD_1")

k<-3
MeanLD <- c(LD[1,1:S,1,k],LD[1,1:S,2,k],LD[1,1:S,3,k])
LowerLD <- c(LD[2,1:S,1,k],LD[2,1:S,2,k],LD[2,1:S,3,k])
UpperLD <- c(LD[3,1:S,1,k],LD[3,1:S,2,k],LD[3,1:S,3,k])
SiteLD <- rep(1:S,3)
QuestionLD <- rep(1:3,each=S)

df_LD_k3 <- data.frame(Mean=MeanLD,Lower=LowerLD,Upper=UpperLD,Site=SiteLD,Question=QuestionLD,Type="LD_2")

k<-4
MeanLD <- c(LD[1,1:S,1,k],LD[1,1:S,2,k],LD[1,1:S,3,k])
LowerLD <- c(LD[2,1:S,1,k],LD[2,1:S,2,k],LD[2,1:S,3,k])
UpperLD <- c(LD[3,1:S,1,k],LD[3,1:S,2,k],LD[3,1:S,3,k])
SiteLD <- rep(1:S,3)
QuestionLD <- rep(1:3,each=S)

df_LD_k4 <- data.frame(Mean=MeanLD,Lower=LowerLD,Upper=UpperLD,Site=SiteLD,Question=QuestionLD,Type="LD_3")

k<-5
MeanLD <- c(LD[1,1:S,1,k],LD[1,1:S,2,k],LD[1,1:S,3,k])
LowerLD <- c(LD[2,1:S,1,k],LD[2,1:S,2,k],LD[2,1:S,3,k])
UpperLD <- c(LD[3,1:S,1,k],LD[3,1:S,2,k],LD[3,1:S,3,k])
SiteLD <- rep(1:S,3)
QuestionLD <- rep(1:3,each=S)

df_LD_k5 <- data.frame(Mean=MeanLD,Lower=LowerLD,Upper=UpperLD,Site=SiteLD,Question=QuestionLD,Type="LD_4")

df <- rbind(df_MD,df_LD_k1,df_LD_k2,df_LD_k3,df_LD_k4,df_LD_k5)


df$Question <- factor(df$Question)
levels(df$Question) <- c("Murder","Theft","Lies")
df$Question <- factor(df$Question,levels(df$Question)[c(3,2,1)])

df$Site <- factor(df$Site)
levels(df$Site) <- levels(data$SITE)
levels(df$Site)[which(levels(df$Site)=="Marajo")] <- "Marajó"

df$Mean[which(df$Site=="Hadza")] <- NA
df$Lower[which(df$Site=="Hadza")] <- NA
df$Upper[which(df$Site=="Hadza")] <- NA   

df$Mean[which(df$Site=="Lovu" & df$Type != "MD")] <- NA
df$Lower[which(df$Site=="Lovu" & df$Type != "MD")] <- NA
df$Upper[which(df$Site=="Lovu" & df$Type != "MD")] <- NA 

df$Mean[which(df$Site=="Samburu" & df$Type != "MD")] <- NA
df$Lower[which(df$Site=="Samburu" & df$Type != "MD")] <- NA
df$Upper[which(df$Site=="Samburu" & df$Type != "MD")] <- NA     

df$Type <- factor(df$Type)
df$Type <- factor(df$Type,levels(df$Type)[c(2,3,4,5,6,1)])

scaleFUN <- function(x) sprintf("%.1f", x)

gg <- ggplot(df,aes(Type,Mean)) + facet_wrap( ~ Site, ncol=3) + scale_colour_manual(values = rev(c("darkred","darkorange3","darkslategray")))+
      geom_hline(yintercept=0.5,linetype="dashed") + geom_errorbar(aes(ymin=Lower, ymax=Upper,color=Question), width=0,position = position_dodge(.6)) + 
      geom_point(aes(color=Question),position = position_dodge(.6)) + coord_flip() + theme(strip.text.x = element_text(size=16),
    strip.text.y = element_text(size=16),axis.text=element_text(size=12),axis.title.y=element_text(size=18,
    face="bold"),axis.title.x=element_text(size=18,face="bold"))+theme(strip.text.y = element_text(angle = 360),
    legend.text=element_text(size=14)) + ylab("Probability of Indicating Moral Concern") + xlab("Model")  + theme(panel.spacing = unit(0.75, "lines"))  + 
    scale_y_continuous(labels=scaleFUN)  + guides(color = guide_legend(reverse = TRUE))
 gg
 ggsave("PredictivePlot.pdf",gg,height=11,width=8.5)                                       


########################################################## Res Table
ddd <- 2

resultstabM <- matrix(NA,nrow=4, ncol=5)
resultstabL <- matrix(NA,nrow=4, ncol=5)

################################################ M0
M0M <- rstan::extract(fit0,"MuM")$MuM
M0L <- rstan::extract(fit0,"MuL")$MuL
C0 <- rstan::extract(fit0,"C")$C

g <- sprintf("%.2f",round(process(M0M[ ,1]-C0[,1]),ddd))
resultstabM[1,1] <- paste0(g[1]," (",g[2],"; ",g[3],")")

g <- sprintf("%.2f",round(process(M0L[ ,1]-C0[,1]),ddd))
resultstabL[1,1] <- paste0(g[1]," (",g[2],"; ",g[3],")")


################################################ M1
M1M <- rstan::extract(fit1,"MuM")$MuM
M1L <- rstan::extract(fit1,"MuL")$MuL
C1 <- rstan::extract(fit1,"C")$C

g <- sprintf("%.2f",round(process(M1M[ ,1]-C1[,1]),ddd))
resultstabM[2,1] <- paste0(g[1]," (",g[2],"; ",g[3],")")

g <- sprintf("%.3f",round(process(M1M[ ,2]),ddd+1))
resultstabM[2,2] <- paste0(g[1]," (",g[2],"; ",g[3],")")

g <- sprintf("%.2f",round(process(M1M[ ,3]),ddd))
resultstabM[2,3] <- paste0(g[1]," (",g[2],"; ",g[3],")")

g <- sprintf("%.2f",round(process(M1M[ ,4]),ddd))
resultstabM[2,4] <- paste0(g[1]," (",g[2],"; ",g[3],")")


g <- sprintf("%.2f",round(process(M1L[ ,1]-C1[,1]),ddd))
resultstabL[2,1] <- paste0(g[1]," (",g[2],"; ",g[3],")")

g <- sprintf("%.3f",round(process(M1L[ ,2]),ddd+1))
resultstabL[2,2] <- paste0(g[1]," (",g[2],"; ",g[3],")")

g <- sprintf("%.2f",round(process(M1L[ ,3]),ddd))
resultstabL[2,3] <- paste0(g[1]," (",g[2],"; ",g[3],")")

g <- sprintf("%.2f",round(process(M1L[ ,4]),ddd))
resultstabL[2,4] <- paste0(g[1]," (",g[2],"; ",g[3],")")




################################################ M2
M2M <- rstan::extract(fit2,"MuM")$MuM
M2L <- rstan::extract(fit2,"MuL")$MuL
C2 <- rstan::extract(fit2,"C")$C
H2 <- rstan::extract(fit3,"C_Shape")$C_Shape
H2 <- apply(H2,c(1,3),mean)

g <- sprintf("%.2f",round(process(M2M[ ,1]-C2[,1]),ddd))
resultstabM[3,1] <- paste0(g[1]," (",g[2],"; ",g[3],")")


g <- sprintf("%.2f",round(process(M2L[ ,1]+ M2L[ ,2]*H2[,3] - C2[,1]),ddd))
resultstabL[3,1] <- paste0(g[1]," (",g[2],"; ",g[3],")")

g <- sprintf("%.2f",round(process(M2L[ ,2]),ddd))
resultstabL[3,5] <- paste0(g[1]," (",g[2],"; ",g[3],")")




################################################ M3
M3M <- rstan::extract(fit3,"MuM")$MuM
M3L <- rstan::extract(fit3,"MuL")$MuL
C3 <- rstan::extract(fit3,"C")$C
H3 <- rstan::extract(fit3,"C_Shape")$C_Shape
H3 <- apply(H3,c(1,3),mean)

g <- sprintf("%.2f",round(process(M3M[ ,1]-C3[,1]),ddd))
resultstabM[4,1] <- paste0(g[1]," (",g[2],"; ",g[3],")")

g <- sprintf("%.3f",round(process(M3M[ ,2]),ddd+1))
resultstabM[4,2] <- paste0(g[1]," (",g[2],"; ",g[3],")")

g <- sprintf("%.2f",round(process(M3M[ ,3]),ddd))
resultstabM[4,3] <- paste0(g[1]," (",g[2],"; ",g[3],")")

g <- sprintf("%.2f",round(process(M3M[ ,4]),ddd))
resultstabM[4,4] <- paste0(g[1]," (",g[2],"; ",g[3],")")



g <- sprintf("%.2f",round(process(M3L[ ,1] + M3L[ ,5]*H3[,3] - C3[,1]),ddd))
resultstabL[4,1] <- paste0(g[1]," (",g[2],"; ",g[3],")")

g <- sprintf("%.3f",round(process(M3L[ ,2]),ddd+1))
resultstabL[4,2] <- paste0(g[1]," (",g[2],"; ",g[3],")")

g <- sprintf("%.2f",round(process(M3L[ ,3]),ddd))
resultstabL[4,3] <- paste0(g[1]," (",g[2],"; ",g[3],")")

g <- sprintf("%.2f",round(process(M3L[ ,4]),ddd))
resultstabL[4,4] <- paste0(g[1]," (",g[2],"; ",g[3],")")

g <- sprintf("%.2f",round(process(M3L[ ,5]),ddd))
resultstabL[4,5] <- paste0(g[1]," (",g[2],"; ",g[3],")")

restab <- rbind(resultstabM,resultstabL)
restab <- cbind(rep(NA,8),restab)

 colnames(restab)<- c("Model","Intercept","Age","Male","Education", "Bleeding")
 restab[,1] <- c("Moralistic Deity; M0","Moralistic Deity; M1","Moralistic Deity; M2","Moralistic Deity; M3",
                      "Local Deity; M0","Local Deity; M1","Local Deity; M2","Local Deity; M3")
                      
       restab[which(is.na(restab),arr.ind=TRUE)] <- "---"               

 write.csv(restab,"inferential.csv", row.names=FALSE, quote = FALSE) 







