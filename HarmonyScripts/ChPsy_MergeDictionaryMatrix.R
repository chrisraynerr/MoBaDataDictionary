
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

DIR <- "/Users/k2040005/Dropbox/Projects/MoBa/data_dictionary/outputs/"

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## LOADING DATA DICTIONARY
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DatD <- 
  fread(paste0(DIR,"MoBaDataDictionaryWithPhenotoolsScalesTimingSummaryScalesOnly.csv")) %>%
  filter(Subject=="Child") %>% 
  select(ItemCode1=ItemCode, Scale1=Scale,ScaleItem1=ScaleItem) 


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## MATCH DATA DICTIONARY TO CHILD QUESTIONNAIRE HARMONISED ITEMS
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Qs <- list.files(DIR, full.names=T, pattern="MoBaDataDictionaryHarmony")
Qs <- Qs[grep(".csv",Qs)]
ChQs <- Qs[grep("ChQ",Qs)]

ChQDf <- 
  lapply(ChQs,fread)

ChDatD <- 
  bind_rows(ChQDf) %>% #,by="QuestionPart2",type="full") %>% 
  mutate(Var1=str_remove_all(str_to_title(QuestionPart2)," ")) %>% 
  select(Var1,ItemCode1=ItemCode)%>%
  full_join(DatD, "ItemCode1",relationship ="many-to-many")%>% 
  na.omit()


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## LOADING & CLEANING SIMILARITY MATRIX
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ChCssMat <- 
  # read.xlsx2(paste0(DIR,"MoBaDataDictionaryHarmonyChildMatrix.xlsx"), sheetIndex = 1) %>% 
  fread(paste0(DIR,"MoBaDataDictionaryHarmonyChildMatrix.csv")) %>% 
  mutate(across(matches("filename"), str_replace_all, "MoBaDataDictionaryHarmony", ""))%>% 
  mutate(across(matches("filename"), str_replace_all, ".xlsx.*$", "")) 

RowNames <- c(str_remove_all(str_to_title(paste0(ChCssMat$question))," "))
DupRows  <- !duplicated(RowNames)
table(DupRows)
ChCssMat <- ChCssMat %>% select(-filename,-question_no,-question) %>% as.matrix()
ChCssMat <- ChCssMat[DupRows,]
ChCssMat <- ChCssMat[-1,-1]
ColNames <- c(str_remove_all(str_to_title(colnames(ChCssMat))," "))
RowNames <- unique(RowNames)
rownames(ChCssMat) <- c(RowNames)[-1]
colnames(ChCssMat) <- c(ColNames)

for(i in seq_along(colnames(ChCssMat))) {
  for (j in seq_along(rownames(ChCssMat))) {
    if (colnames(ChCssMat)[i] == rownames(ChCssMat)[j]) {
      ChCssMat[j, i] <- 1
}}}

fwrite(
  ChCssMat, 
  paste0(DIR,"MoBaDataDictionaryChildCosineSimilarityMatrix.csv"),
  col.names=T,row.names=T
  )


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CREATING SIMILARITY TABLE
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ChCssTab <- 
  reshape2::melt(ChCssMat) %>% 
  #filter(as.character(Var1)!=as.character(Var2)) %>%
  distinct() %>% 
  inner_join(ChDatD,relationship ="many-to-many") %>% 
  inner_join(ChDatD %>% select(Var2=Var1,ItemCode2=ItemCode1,Scale2=Scale1,ScaleItem2=ScaleItem1),relationship ="many-to-many") %>% 
  mutate(QCode1 = str_remove_all(ItemCode1,"[0-9]*"),QCode2 = str_remove_all(ItemCode2,"[0-9]*")) %>%
  mutate(value2 = round(abs(value),3)) %>%
  na.omit()

fwrite(ChCssTab, paste0(DIR,"MoBaDataDictionaryChildCosineSimilarityScaleItems.csv"))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## LOADING CLUSTERS
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ChCssClu <- fread(paste0(DIR,"MoBaDataDictionaryChildCosineSimilarityClusters.csv")) %>% na.omit()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## MERGING DATA
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# rownames(ChCssMat) <- colnames(ChCssMat)

ChCssCluVar1 <- 
  ChCssClu %>% 
  filter(Var1!="MentalHealth"&Var1!="Questionpart2") %>% 
  select(#ItemCode1=ItemCode,Wave1=Wave,Subject1=Subject,
         ScaleItem1=ScaleItem,Centrality1=value,Cluster1=cluster) %>% 
  na.omit() %>% 
  mutate(ScaleItem1=str_replace_all(ScaleItem1,"AdLifEv","ALE"))

ChCssCluVar2 <- 
  ChCssClu %>% 
  filter(Var1!="MentalHealth"&Var1!="Questionpart2") %>% 
  select(
    #ItemCode2=ItemCode,#Wave2=Wave,Subject2=Subject,
    ScaleItem2=ScaleItem,
    Centrality2=value,
    Cluster2=cluster) %>% 
  na.omit()%>% 
  mutate(ScaleItem2=str_replace_all(ScaleItem2,"AdLifEv","ALE")) 

ChCssTabX    <- 
  ChCssTab %>% 
  filter(Var1!="MentalHealth"&Var1!="Questionpart2")%>% 
  na.omit() %>% 
  mutate(Similarity=value) %>% select(-value,-value2) %>%
  full_join(ChCssCluVar1, "ScaleItem1",relationship ="many-to-many") %>%
  full_join(ChCssCluVar2, "ScaleItem2",relationship ="many-to-many") %>%   
  # full_join(ChCssCluVar1, "ItemCode1") %>%
  # full_join(ChCssCluVar2, "ItemCode2") %>% 
  na.omit()%>% 
  mutate(Scale1=str_replace_all(Scale1,"AdLifEv","ALE")) %>% 
  mutate(Scale2=str_replace_all(Scale2,"AdLifEv","ALE"))

testY        <- 
  ChCssTabX %>% filter(QCode1!=QCode2 ) %>% # & abs(Similarity)>0.7) %>% 
  mutate(Similarity = ifelse(abs(Similarity)<0.7,0,Similarity)) %>% 
  mutate(X1 = factor(QCode1,
                     levels =c('DD','EE','GG','LL','NN'),
                     labels=c(1,2,3,4,5)))%>% 
  mutate(X2 = factor(QCode2,
                     levels =c('DD','EE','GG','LL','NN'),
                     labels=c(1,2,3,4,5)))       

LabY   <- sort(unique(c(testY$ScaleItem1,testY$ScaleItem2)))
PosY1  <- as.data.frame(cbind(ScaleItem1=LabY,Y1=c(1:length(LabY))))
PosY2  <- as.data.frame(cbind(ScaleItem2=LabY,Y2=c(1:length(LabY))))

testY  <- testY %>% full_join(PosY1, "ScaleItem1") %>% full_join(PosY2, "ScaleItem2") #%>% distinct()
testY  <- testY %>% mutate(X1=as.numeric(X1),X2=as.numeric(X2),Y1=as.numeric(Y1),Y2=as.numeric(Y2))
testY  <- testY %>% arrange(Y1,X1)

PosC0  <- testY %>% filter(Cluster1==0) %>% select(ScaleItem1) %>% distinct() %>% arrange()
PosC1  <- testY %>% filter(Cluster1==1) %>% select(ScaleItem1) %>% distinct() %>% arrange()
PosC2  <- testY %>% filter(Cluster1==2) %>% select(ScaleItem1) %>% distinct() %>% arrange()
PosC3  <- testY %>% filter(Cluster1==3) %>% select(ScaleItem1) %>% distinct() %>% arrange()
PosC4  <- testY %>% filter(Cluster1==4) %>% select(ScaleItem1) %>% distinct() %>% arrange()
PosCY1 <- bind_rows(PosC0,PosC1,PosC2,PosC3,PosC4) 
PosCY1 <- PosCY1 %>% mutate(C1 = 1:length(PosCY1$ScaleItem1)) 
PosCY2 <- PosCY1 %>% select(ScaleItem2=ScaleItem1,C2=C1)
testY  <- testY %>% full_join(PosCY1, "ScaleItem1") %>% full_join(PosCY2, "ScaleItem2")   


testY        <- 
  testY %>% 
  mutate(X1 = as.numeric(factor(QCode1,
                     levels =c('DD','EE','GG','LL','NN'),
                     labels=c(1,2,3,4,5)
                     ) ) ) %>% 
  mutate(X2 = as.numeric(factor(QCode2,
                     levels =c('DD','EE','GG','LL','NN'),
                     labels=c(1,2,3,4,5)
                     )))

Plot1 <- 
  # ggplot(testY, aes(x=X1, y=Y1, xend=X2, yend=Y2)) +
  # ggplot(testY %>% filter((Cluster1==0|Cluster2==0)&Similarity>0.8), aes(x=X1, y=C1, xend=X2, yend=C2)) +
  ggplot(testY, aes(x=X1, y=C1, xend=X2, yend=C2)) +
  # ggplot(testY, aes(x=X1, y=ScaleItem1, xend=X2, yend=ScaleItem1)) +
  geom_edges(
    aes(size=abs(Similarity), color=as.factor(Cluster1)),alpha=0.5
  ) +
  scale_size(range = c(0, 0.5))+
  geom_nodes(aes(color=as.factor(Cluster1))) + #, size = importance)) +
  scale_x_continuous(
    name="Time-point", breaks=c(1:length(unique(testY$QCode1))), 
    labels=c(paste0(sort(unique(testY$QCode1))))
    )+
  scale_y_continuous(
    name="Scale-item", breaks=c(1:length(unique(testY$ScaleItem1))), 
    labels=c(paste0(sort(unique(testY$ScaleItem1)))),
    trans = "reverse") +
  guides(color="none",size="none")+
  theme_minimal() #+
  # facet_grid(rows = "Cluster1", scales = "free_y")

ggplotly(Plot)

ggsave("testPlot.png",Plot, width=10, height=80, units="cm",dpi="retina")


testYC <- list()
testYC[["A"]] <- testY %>% filter(Cluster1==0 & Cluster2==0)
testYC[["B"]] <- testY %>% filter(Cluster1==1 & Cluster2==1)
testYC[["C"]] <- testY %>% filter(Cluster1==2 & Cluster2==2)
testYC[["D"]] <- testY %>% filter(Cluster1==3 & Cluster2==3)
testYC[["E"]] <- testY %>% filter(Cluster1==4 & Cluster2==4)

plotList <- list()

for(i in 1:5){
  testC <- 
    testYC[[i]] %>% 
    group_by(Var1) %>% mutate(C1=row.number()) %>% ungroup()
  testC$C1 <- testC$C1 - min(testC$C1)
  testC$C2 <- testC$C2 - min(testC$C2)
  plotList[[i]] <- 
    ggplot(testC, aes(x=X1, y=C1, xend=X2, yend=C2)) +
      geom_edges(aes(size=abs(Similarity), color=as.factor(Scale2)),alpha=0.5) +
      scale_size(range = c(0, 0.5))+
      geom_nodes(aes(color=as.factor(Scale1))) +
      scale_x_continuous(
        name="Time-point", breaks=c(1:length(unique(testC$QCode1))), 
        labels=c(paste0(sort(unique(testC$QCode1)))))+
      scale_y_continuous(
        name="Scale-item", breaks=c(1:length(unique(testC$ScaleItem1))), 
        labels=c(paste0(sort(unique(testC$ScaleItem1)))),trans = "reverse") +
      guides(color="none",size="none")+
      theme_minimal()
}


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PARENTS
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DIR <- "/Users/k2040005/Dropbox/Projects/MoBa/data_dictionary/outputs/"

DatD <- 
  fread(paste0(DIR,"MoBaDataDictionaryWithPhenotoolsScalesTimingSummaryScalesOnly.csv")) %>%
  select(ItemCode1=ItemCode, Scale1=Scale) 

Qs <- list.files(DIR, full.names=T, pattern="MoBaDataDictionaryHarmony")
Qs <- Qs[grep(".csv",Qs)]

PaQs <- Qs[grep("MoQ|FaQ",Qs)]

PaDatD <- lapply(PaQs,fread)

PaDatD <- 
  plyr::join_all(PaDatD,by="QuestionPart2",type="full") %>% 
  mutate(Var1=str_remove_all(str_to_title(QuestionPart2)," ")) %>% 
  select(Var1,ItemCode1=ItemCode)%>%
  inner_join(DatD, "ItemCode1")

PaCssMat <- 
  # read.xlsx2(paste0(DIR,"MoBaDataDictionaryHarmonyAdultMatrix.xlsx"), sheetIndex = 1) %>% 
  fread(paste0(DIR,"MoBaDataDictionaryHarmonyAdultMatrix.csv")) %>% 
  mutate(across(matches("filename"), str_replace_all, "MoBaDataDictionaryHarmony", ""))%>% 
  mutate(across(matches("filename"), str_replace_all, ".xlsx.*$", "")) 

RowNames <- c(str_remove_all(str_to_title(paste0(PaCssMat$question,"_",PaCssMat$filename))," "))
RowNames <- c(str_remove_all(str_to_title(paste0(PaCssMat$question))," "))

PaCssMat <- PaCssMat %>% select(-filename,-question_no,-question) %>% as.matrix()

ColNames <- c(str_remove_all(str_to_title(colnames(PaCssMat))," "))

rownames(PaCssMat) <- c(RowNames)
colnames(PaCssMat) <- c(ColNames)

PaCssMat  <- PaCssMat[-1,-1]

for(i in seq_along(colnames(PaCssMat))) {
  for (j in seq_along(rownames(PaCssMat))) {
    if (colnames(PaCssMat)[i] == rownames(PaCssMat)[j]) {
      PaCssMat[j, i] <- 1
    }
  }
}

fwrite(
  PaCssMat, 
  paste0(DIR,"MoBaDataDictionaryAdultCosineSimilarityMatrix.csv"),
  col.names=T,
  row.names=T
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PaCssMat <- 
  # read.xlsx2(paste0(DIR,"MoBaDataDictionaryHarmonyAdultMatrix.xlsx"), sheetIndex = 1) %>% 
  fread(paste0(DIR,"MoBaDataDictionaryHarmonyAdultMatrix.csv")) %>% 
  mutate(across(matches("filename"), str_replace_all, "MoBaDataDictionaryHarmony", ""))%>% 
  mutate(across(matches("filename"), str_replace_all, ".xlsx.*$", "")) 

RowNames <- c(str_remove_all(str_to_title(paste0(PaCssMat$question))," "))
DupRows  <- !duplicated(RowNames)
table(DupRows)
PaCssMat <- PaCssMat %>% select(-filename,-question_no,-question) %>% as.matrix()
PaCssMat <- PaCssMat[DupRows,]
PaCssMat <- PaCssMat[-1,-1]
ColNames <- c(str_remove_all(str_to_title(colnames(PaCssMat))," "))
RowNames <- unique(RowNames)
rownames(PaCssMat) <- c(RowNames)[-1]
colnames(PaCssMat) <- c(ColNames)

for(i in seq_along(colnames(PaCssMat))) {
  for (j in seq_along(rownames(PaCssMat))) {
    if (colnames(PaCssMat)[i] == rownames(PaCssMat)[j]) {
      PaCssMat[j, i] <- 1
    }}}

fwrite(
  PaCssMat, 
  paste0(DIR,"MoBaDataDictionaryAdultCosineSimilarityMatrix.csv"),
  col.names=T,
  row.names=F
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PaCssTab <- 
  reshape2::melt(PaCssMat) %>% 
  # filter(abs(value)>0.7) %>% 
  filter(as.character(Var1)!=as.character(Var2)) %>%
  distinct() %>% 
  inner_join(PaDatD) %>% 
  inner_join(PaDatD %>% select(Var2=Var1,ItemCode2=ItemCode1, Scale2=Scale1)) %>% 
  # filter(ItemCode1!=ItemCode2) %>%
  mutate(QCode1 = str_remove_all(ItemCode1,"[0-9]*"),QCode2 = str_remove_all(ItemCode2,"[0-9]*")) %>%
  mutate(value2 = round(abs(value),3)) %>%
  na.omit()

fwrite(PaCssTab, paste0(DIR,"MoBaDataDictionaryAdultCosineSimilarityScaleItems.csv"))



## Parents
PaCssClu <- fread(paste0(DIR,"MoBaDataDictionaryAdultCosineSimilarityClusters.csv")) %>% na.omit()
PaCssTab <- fread(paste0(DIR,"MoBaDataDictionaryAdultCosineSimilarityScaleItems.csv")) %>% na.omit() #%>% filter(value2 >0.75)
PaCssMat <- fread(paste0(DIR,"MoBaDataDictionaryAdultCosineSimilarityMatrix.csv")) %>% as.matrix()

rownames(PaCssMat) <- colnames(PaCssMat)

PaCssCluVar1 <- PaCssClu %>% filter(Var1!="MentalHealth"&Var1!="Questionpart2") %>% 
  select(ItemCode1=ItemCode,ScaleItem1=ScaleItem,Wave1=Wave,Subject1=Subject,Centrality1=value,Cluster1=cluster) %>% 
  na.omit() %>% 
  mutate(ScaleItem1=str_replace_all(ScaleItem1,"AdLifEv","ALE"))

PaCssCluVar2 <- PaCssClu %>% filter(Var1!="MentalHealth"&Var1!="Questionpart2") %>% 
  select(ItemCode2=ItemCode,ScaleItem2=ScaleItem,Wave2=Wave,Subject2=Subject,Centrality2=value,Cluster2=cluster) %>% 
  na.omit()%>% 
  mutate(ScaleItem2=str_replace_all(ScaleItem2,"AdLifEv","ALE")) 

PaCssTabX    <- PaCssTab %>% filter(Var1!="MentalHealth"&Var1!="Questionpart2")%>% 
  na.omit() %>% 
  mutate(Similarity=value) %>% select(-value,-value2) %>%
  full_join(PaCssCluVar1, "ItemCode1") %>%
  full_join(PaCssCluVar2, "ItemCode2") %>% 
  na.omit()%>% 
  mutate(Scale1=str_replace_all(Scale1,"AdLifEv","ALE")) %>% 
  mutate(Scale2=str_replace_all(Scale2,"AdLifEv","ALE"))

testY        <- PaCssTabX %>% filter(QCode1!=QCode2 ) %>% # & abs(Similarity)>0.7) %>% 
  mutate(Similarity = ifelse(abs(Similarity)<0.7,0,Similarity)) %>% 
  mutate(X1 = factor(QCode1,
                     levels =c('AA','CC','DD','EE','GG','LL','NN','FF','G'),
                     labels=c(1,2,3,4,5,6,7,8,9)))%>% 
  mutate(X2 = factor(QCode2,
                     levels =c('AA','CC','DD','EE','GG','LL','NN','FF','G'),
                     labels=c(1,2,3,4,5,6,7,8,9))) %>% 
  mutate(Indv1 = ifelse(as.numeric(X1)<8,"Mother","Father"))%>% 
  mutate(Indv2 = ifelse(as.numeric(X2)<8,"Mother","Father"))%>%
  mutate(Who  = paste0(Indv1,"_",Indv2))         %>%
  mutate(Who  = factor(Who, levels = c("Mother_Mother","Father_Father","Mother_Father","Father_Mother"),
                       labels = c("Mother","Father","Both","Both")))         

LabY         <- sort(unique(c(testY$ScaleItem1,testY$ScaleItem2)))
PosY1        <- as.data.frame(cbind(ScaleItem1=LabY,Y1=c(1:length(LabY))))
PosY2        <- as.data.frame(cbind(ScaleItem2=LabY,Y2=c(1:length(LabY))))

testY        <- testY %>% full_join(PosY1, "ScaleItem1") %>% full_join(PosY2, "ScaleItem2") #%>% distinct()
testY        <- testY %>% mutate(X1=as.numeric(X1),X2=as.numeric(X2),Y1=as.numeric(Y1),Y2=as.numeric(Y2))
testY        <- testY %>% arrange(Y1,X1)


Plot <- 
  ggplot(testY, aes(x=X1, y=Y1, xend=X2, yend=Y2)) +
  geom_edges(
    aes(size=abs(Similarity), color=Who), 
  ) +
  scale_size(range = c(0, 0.5))+
  geom_nodes(aes(color=Scale1)) + #, size = importance)) +
  scale_x_continuous(
    name="Time-point", breaks=c(1:length(unique(testY$QCode1))))+ #, 
    #labels=c('AA','CC','DD','EE','GG','LL','NN','FF','G'))+
  scale_y_continuous(
    name="Scale-item", breaks=c(1:length(unique(testY$ScaleItem1))), 
    labels=c(paste0(sort(unique(testY$ScaleItem1)))),
    trans = "reverse") +
  guides(color="none",size="none")+
  theme_minimal()

ggplotly(Plot)
