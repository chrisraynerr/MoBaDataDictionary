rm(list = ls())

###############################################################################
Pkgs   <- c("data.table","dplyr","stringr","tidyr","magrittr","tidytext","heatmaply","ggnetwork")
inPkgs <- Pkgs %in% rownames(installed.packages())
if(any(inPkgs==F)){
  install.packages(Pkgs[!inPkgs],dependencies=T)
}
suppressPackageStartupMessages(suppressMessages(
  lapply(Pkgs, library, character.only=T)
))
###############################################################################

DIR    <- "./HarmonyOutputs/"
CssClu <- fread(paste0(DIR,"ChPsy_HarmonyCosineClusters.csv")) %>% na.omit()
CssTab <- fread(paste0(DIR,"ChPsy_HarmonyCosineScaleItems.csv")) %>% na.omit() #%>% filter(value2 >0.75)
CssMat <- fread(paste0(DIR,"ChPsy_HarmonyCosineMatrix.csv"))[,-1] %>% as.matrix()

rownames(CssMat) <- colnames(CssMat)

CssMatPlot <- CssMat %>% as.matrix() %>% heatmaply()

CssCluVar1 <- CssClu %>% filter(Var1!="MentalHealth"&Var1!="Questionpart2") %>% 
                select(ItemCode1=ItemCode,ScaleItem1=ScaleItem,Wave1=Wave,Subject1=Subject,Centrality1=value,Cluster1=cluster) %>% 
                na.omit() %>% 
                mutate(ScaleItem1=str_replace_all(ScaleItem1,"AdLifEv","ALE"))
  
CssCluVar2 <- CssClu %>% filter(Var1!="MentalHealth"&Var1!="Questionpart2") %>% 
                select(ItemCode2=ItemCode,ScaleItem2=ScaleItem,Wave2=Wave,Subject2=Subject,Centrality2=value,Cluster2=cluster) %>% 
                na.omit()%>% 
                mutate(ScaleItem2=str_replace_all(ScaleItem2,"AdLifEv","ALE")) 

CssTabX    <- CssTab %>% filter(Var1!="MentalHealth"&Var1!="Questionpart2")%>% 
                na.omit() %>% 
                mutate(Similarity=value) %>% select(-value,-value2) %>%
                full_join(CssCluVar1, c("ItemCode1","ScaleItem1")) %>%
                full_join(CssCluVar2, c("ItemCode2","ScaleItem2")) %>% 
                na.omit()%>% 
                mutate(Scale1=str_replace_all(Scale1,"AdLifEv","ALE")) %>% 
                mutate(Scale2=str_replace_all(Scale2,"AdLifEv","ALE"))%>% 
                #filter(QCode1!=QCode2 ) %>% # & abs(Similarity)>0.7) %>% 
                mutate(Similarity = ifelse(abs(Similarity)<0.7,0,Similarity)) %>% 
                mutate(X1 = factor(QCode1,
                                   levels =c('DD','EE','GG','LL','NN'),
                                   labels=c('18m','3yr','5yr','8yr','11yr')))%>% 
                mutate(X2 = factor(QCode2,
                                   levels =c('DD','EE','GG','LL','NN'),
                                   labels=c('18m','3yr','5yr','8yr','11yr'))) # %>% 
                # mutate(Indv1 = ifelse(as.numeric(X1)<8,"Mother","Father"))%>% 
                # mutate(Indv2 = ifelse(as.numeric(X2)<8,"Mother","Father"))%>%
                # mutate(Who  = paste0(Indv1,"_",Indv2))         #%>%
                # mutate(Who  = factor(Who, levels = c("Mother_Mother","Father_Father","Mother_Father","Father_Mother"),
                #                       labels = c("Mother","Father","Both","Both")))         

# testX        <- CssTabX %>% 
#                 select(ItemCode1,ItemCode2,ScaleItem1,ScaleItem2,Similarity,QCode1,QCode2,Scale1,Scale2)

table(CssTabX$X1)
table(CssTabX$X2)
CssTabX %>% filter(ScaleItem1==ScaleItem2)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Full Plot
CssTabX      <- CssTabX %>% arrange(Cluster1) %>% filter(Similarity>.69)
# LabY         <- sort(unique(c(CssTabX$ScaleItem1,CssTabX$ScaleItem2)))
LabY         <- unique(c(CssTabX$ScaleItem1,CssTabX$ScaleItem2))
PosY1        <- as.data.frame(cbind(ScaleItem1=LabY,Y1=c(1:length(LabY))))
PosY2        <- as.data.frame(cbind(ScaleItem2=LabY,Y2=c(1:length(LabY))))

Full <- 
  CssTabX %>% 
  full_join(PosY1, "ScaleItem1") %>% 
  full_join(PosY2, "ScaleItem2")  %>% 
  mutate(X1=as.numeric(X1),X2=as.numeric(X2),Y1=as.numeric(Y1),Y2=as.numeric(Y2)) %>% 
  arrange(Y1,X1)

# exp(abs(Full$Similarity))
FullPlot <- 
  ggplot(Full, aes(x=X1, y=Y1, xend=X2, yend=Y2)) +
    geom_edges(
      aes(size=abs(Similarity)), color="grey50" # size = 0.01, # color = "grey50"
    ) +
    scale_size(range = c(0, 0.71))+
    geom_nodes(aes(color=as.factor(Cluster1))) + #, size = importance)) +
    scale_x_continuous(
      name="Time-point", breaks=c(1:length(unique(Full$X1))), 
      labels=c('18m','3yr','5yr','8yr','11yr'))+
    scale_y_continuous(
      expand = c(.0005, .0005),
      name="Scale-item", breaks=c(1:length(LabY)),
      labels=c(paste0(LabY)),
      trans = "reverse") +
    guides(color="none",size="none")+
    theme(axis.text.y = element_blank())+
  theme_classic()

FullPlot + 
  theme(
    # axis.text.y = element_blank(),
    axis.text.y = element_text(size=3),
    axis.title.y = element_text(size=8),    
    axis.text.x = element_text(size=5),
    axis.title.x = element_text(size=8),
    axis.ticks.y = element_blank()
    )

ggplotly(p=FullPlot)
         
         
table(CssTabX$Cluster1)
table(CssTabX$Cluster2)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CL0 Plot

CL0    <- CssTabX %>% filter(Cluster1==1 & Cluster2==1)
LabY   <- sort(unique(c(CL0$ScaleItem1,CL0$ScaleItem2)))
PosY1  <- as.data.frame(cbind(ScaleItem1=LabY,Y1=c(1:length(LabY))))
PosY2  <- as.data.frame(cbind(ScaleItem2=LabY,Y2=c(1:length(LabY))))

CL0 <- 
  CL0 %>% 
  full_join(PosY1, "ScaleItem1") %>% 
  full_join(PosY2, "ScaleItem2")  %>% 
  mutate(X1=as.numeric(X1),X2=as.numeric(X2),Y1=as.numeric(Y1),Y2=as.numeric(Y2)) %>% 
  arrange(Y1,X1)

CL0Plot <- 
  ggplot(CL0, aes(x=X1, y=Y1, xend=X2, yend=Y2)) +
  geom_edges(
    aes(size=abs(Similarity)), color="grey50" # size = 0.01, # color = "grey50"
  ) +
  scale_size(range = c(0, 0.5))+
  geom_nodes(aes(color=as.factor(Cluster1))) + #, size = importance)) +
  scale_x_continuous(
    name="Time-point", breaks=c(1:length(unique(CL0$X1))), 
    labels=c('18m','3yr','5yr','8yr','11yr'))+
  scale_y_continuous(
    name="Scale-item", breaks=c(1:length(unique(CL0$ScaleItem1))), 
    labels=c(paste0(sort(unique(CL0$ScaleItem1)))),
    trans = "reverse") +
  guides(color="none",size="none")+
  theme(axis.text.y = element_text(size=0.1))+
  theme_minimal()

CL0Plot



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CL1 Plot

CL1    <- CssTabX %>% filter(Cluster1==1 & Cluster2==1)
LabY   <- sort(unique(c(CL1$ScaleItem1,CL1$ScaleItem2)))
PosY1  <- as.data.frame(cbind(ScaleItem1=LabY,Y1=c(1:length(LabY))))
PosY2  <- as.data.frame(cbind(ScaleItem2=LabY,Y2=c(1:length(LabY))))

CL1 <- 
  CL1 %>% 
  full_join(PosY1, "ScaleItem1") %>% 
  full_join(PosY2, "ScaleItem2")  %>% 
  mutate(X1=as.numeric(X1),X2=as.numeric(X2),Y1=as.numeric(Y1),Y2=as.numeric(Y2)) %>% 
  arrange(Y1,X1)

CL1Plot <- 
  ggplot(CL1, aes(x=X1, y=Y1, xend=X2, yend=Y2)) +
  geom_edges(
    aes(size=abs(Similarity)), color="grey50" # size = 0.01, # color = "grey50"
  ) +
  scale_size(range = c(0, 0.5))+
  geom_nodes(aes(color=as.factor(Cluster1))) + #, size = importance)) +
  scale_x_continuous(
    name="Time-point", breaks=c(1:length(unique(CL1$X1))), 
    labels=c('18m','3yr','5yr','8yr','11yr'))+
  scale_y_continuous(
    name="Scale-item", breaks=c(1:length(unique(CL1$ScaleItem1))), 
    labels=c(paste0(sort(unique(CL1$ScaleItem1)))),
    trans = "reverse") +
  guides(color="none",size="none")+
  theme(axis.text.y = element_text(size=0.1))+
  theme_minimal()

CL1Plot

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CL2 Plot

CL2    <- CssTabX %>% filter(Cluster1==2 & Cluster2==2)
LabY   <- sort(unique(c(CL2$ScaleItem1,CL2$ScaleItem2)))
PosY1  <- as.data.frame(cbind(ScaleItem1=LabY,Y1=c(1:length(LabY))))
PosY2  <- as.data.frame(cbind(ScaleItem2=LabY,Y2=c(1:length(LabY))))

CL2 <- 
  CL2 %>% 
  full_join(PosY1, "ScaleItem1") %>% 
  full_join(PosY2, "ScaleItem2")  %>% 
  mutate(X1=as.numeric(X1),X2=as.numeric(X2),Y1=as.numeric(Y1),Y2=as.numeric(Y2)) %>% 
  arrange(Y1,X1)

CL2Plot <- 
  ggplot(CL2, aes(x=X1, y=Y1, xend=X2, yend=Y2)) +
  geom_edges(
    aes(size=abs(Similarity)), color="grey50" # size = 0.01, # color = "grey50"
  ) +
  scale_size(range = c(0, 0.5))+
  geom_nodes(aes(color=as.factor(Cluster1))) + #, size = importance)) +
  scale_x_continuous(
    name="Time-point", breaks=c(1:length(unique(CL2$X1))), 
    labels=c('18m','3yr','5yr','8yr','11yr'))+
  scale_y_continuous(
    name="Scale-item", breaks=c(1:length(unique(CL2$ScaleItem1))), 
    labels=c(paste0(sort(unique(CL2$ScaleItem1)))),
    trans = "reverse") +
  guides(color="none",size="none")+
  theme(axis.text.y = element_text(size=0.1))+
  theme_minimal()

CL2Plot


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CL3 Plot

CL3    <- CssTabX %>% filter(Cluster1==3 & Cluster2==3)
LabY   <- sort(unique(c(CL3$ScaleItem1,CL3$ScaleItem2)))
PosY1  <- as.data.frame(cbind(ScaleItem1=LabY,Y1=c(1:length(LabY))))
PosY2  <- as.data.frame(cbind(ScaleItem2=LabY,Y2=c(1:length(LabY))))

CL3 <- 
  CL3 %>% 
  full_join(PosY1, "ScaleItem1") %>% 
  full_join(PosY2, "ScaleItem2")  %>% 
  mutate(X1=as.numeric(X1),X2=as.numeric(X2),Y1=as.numeric(Y1),Y2=as.numeric(Y2)) %>% 
  arrange(Y1,X1)

CL3Plot <- 
  ggplot(CL3, aes(x=X1, y=Y1, xend=X2, yend=Y2)) +
  geom_edges(
    aes(size=abs(Similarity)), color="grey50" # size = 0.01, # color = "grey50"
  ) +
  scale_size(range = c(0, 0.5))+
  geom_nodes(aes(color=as.factor(Cluster1))) + #, size = importance)) +
  scale_x_continuous(
    name="Time-point", breaks=c(1:length(unique(CL3$X1))), 
    labels=c('18m','3yr','5yr','8yr','11yr'))+
  scale_y_continuous(
    name="Scale-item", breaks=c(1:length(unique(CL3$ScaleItem1))), 
    labels=c(paste0(sort(unique(CL3$ScaleItem1)))),
    trans = "reverse") +
  guides(color="none",size="none")+
  theme(axis.text.y = element_text(size=0.1))+
  theme_minimal()

CL3Plot

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CL4 Plot

CL4    <- CssTabX %>% filter(Cluster1==4 & Cluster2==4)
LabY   <- sort(unique(c(CL4$ScaleItem1,CL4$ScaleItem2)))
PosY1  <- as.data.frame(cbind(ScaleItem1=LabY,Y1=c(1:length(LabY))))
PosY2  <- as.data.frame(cbind(ScaleItem2=LabY,Y2=c(1:length(LabY))))

CL4 <- 
  CL4 %>% 
  full_join(PosY1, "ScaleItem1") %>% 
  full_join(PosY2, "ScaleItem2")  %>% 
  mutate(X1=as.numeric(X1),X2=as.numeric(X2),Y1=as.numeric(Y1),Y2=as.numeric(Y2)) %>% 
  arrange(Y1,X1)

CL4Plot <- 
  ggplot(CL4, aes(x=X1, y=Y1, xend=X2, yend=Y2)) +
  geom_edges(
    aes(size=abs(Similarity)), color="grey50" # size = 0.01, # color = "grey50"
  ) +
  scale_size(range = c(0, 0.5))+
  geom_nodes(aes(color=as.factor(Cluster1))) + #, size = importance)) +
  scale_x_continuous(
    name="Time-point", breaks=c(1:length(unique(CL4$X1))), 
    labels=c('3yr','5yr','11yr'))+
  scale_y_continuous(
    name="Scale-item", breaks=c(1:length(unique(CL4$ScaleItem1))), 
    labels=c(paste0(sort(unique(CL4$ScaleItem1)))),
    trans = "reverse") +
  guides(color="none",size="none")+
  theme(axis.text.y = element_text(size=0.1))+
  theme_minimal()

CL4Plot
