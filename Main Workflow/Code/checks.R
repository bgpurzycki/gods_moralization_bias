
H <- rstan::extract(fit3,"C_Shape")$C_Shape


sample_eff <- apply(H,2:3,quantile,probs=c(0.05,0.5,0.95))

  df1 <- list("vector",15)

  for(i in 1:15){
	  AA <- i
     df1[[i]] <- data.frame(Level=1:5,
                      LI=sample_eff[1,AA,],
                      Median=sample_eff[2,AA,],
                      HI=sample_eff[3,AA,],
                      Cat=df$Site[AA] )
  }

  df_2 <-do.call(rbind,df1)



  g1xa <- ggplot(df_2,aes(x=as.numeric(Level),y=Median)) + 
        geom_point(aes()) + 
        geom_linerange(aes(ymin=LI,ymax=HI)) + 
        facet_wrap("Cat",nrow=5, scales="free") + xlab("Level") +
        theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold")) + coord_cartesian(ylim = c(0, 1)) +
        theme_light() +
        theme(panel.grid.major.x = element_blank() , panel.grid.minor.x = element_blank()) + 
        ggpubr::rotate_x_text(60) + 
        theme(text = element_text(size = 16))  

        ggsave("curvem3.pdf",g1xa,height=11,width=8.5)       


H <- rstan::extract(fit2,"C_Shape")$C_Shape


sample_eff <- apply(H,2:3,quantile,probs=c(0.05,0.5,0.95))

  df1 <- list("vector",15)

  for(i in 1:15){
	  AA <- i
     df1[[i]] <- data.frame(Level=1:5,
                      LI=sample_eff[1,AA,],
                      Median=sample_eff[2,AA,],
                      HI=sample_eff[3,AA,],
                      Cat=df$Site[AA] )
  }

  df_2 <-do.call(rbind,df1)



  g1xb <- ggplot(df_2,aes(x=as.numeric(Level),y=Median)) + 
        geom_point(aes()) + 
        geom_linerange(aes(ymin=LI,ymax=HI)) + 
        facet_wrap("Cat",nrow=5, scales="free") + xlab("Level") +
        theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold")) + coord_cartesian(ylim = c(0, 1)) +
        theme_light() +
        theme(panel.grid.major.x = element_blank() , panel.grid.minor.x = element_blank()) + 
        ggpubr::rotate_x_text(60) + 
        theme(text = element_text(size = 16))  

        ggsave("curvem2.pdf",g1xb,height=11,width=8.5)       


set.seed(11)
GGt_m1<-traceplot(fit0,pars=sample(names(fit0@sim$samples[[1]]),30))
ggsave("Trace1.pdf",GGt_m1,width=11,height=8.5)

set.seed(11)
GGt_m1<-traceplot(fit1,pars=sample(names(fit1@sim$samples[[1]]),30))
ggsave("Trace2.pdf",GGt_m1,width=11,height=8.5)


set.seed(11)
GGt_m1<-traceplot(fit2,pars=sample(names(fit2@sim$samples[[1]]),30))
ggsave("Trace3.pdf",GGt_m1,width=11,height=8.5)


set.seed(11)
GGt_m1<-traceplot(fit3,pars=sample(names(fit3@sim$samples[[1]]),30))
ggsave("Trace4.pdf",GGt_m1,width=11,height=8.5)
