
rm(list = ls())

###############################################################################
Pkgs   <- c("data.table","dplyr","stringr","tidyr","magrittr","tidytext","heatmaply")
inPkgs <- Pkgs %in% rownames(installed.packages())
if(any(inPkgs==F)){
  install.packages(Pkgs[!inPkgs],dependencies=T)
}
suppressPackageStartupMessages(suppressMessages(
    lapply(Pkgs, library, character.only=T)
  ))
###############################################################################

DIR1 <- "./DictionaryOutputs/"
DIR2 <- "./FilesForHarmony/"

DatD <- 
  fread(paste0(DIR1,"MoBaDataDictionary_PreHarmonyTest.csv")) %>%
  mutate(SubjectQ = paste0(str_sub(Subject,1,2),Wave)) %>%
  select(ItemCode, Scale, SubjectQ, Subject, Wave, ScaleItem) 

Qs <- list.files(DIR2, full.names=T, pattern="Q")
Qs <- Qs[grep(".csv",Qs)]

ChQs <- Qs[grep("ChQ",Qs)]
PaQs <- Qs[grep("MoQ|FaQ",Qs)]

ChDatD <- lapply(ChQs,fread)
PaDatD <- lapply(PaQs,fread)

ChDatD <- 
  plyr::join_all(ChDatD,by="QuestionPart2",type="full") %>% 
  mutate(Var1=str_remove_all(str_to_title(QuestionPart2)," ")) %>% 
  select(Var1,ItemCode)%>%
  inner_join(DatD, "ItemCode")

PaDatD <- 
  plyr::join_all(PaDatD,by="QuestionPart2",type="full") %>% 
  mutate(Var1=str_remove_all(str_to_title(QuestionPart2)," ")) %>% 
  select(Var1,ItemCode)%>%
  inner_join(DatD, "ItemCode")

ChCssClu <- 
  read.xlsx2(paste0(DIR,"MoBaDataDictionaryHarmonyChildClusters.xlsx"), sheetIndex = 1) %>% 
  # fread(paste0(DIR,"MoBaDataDictionaryHarmonyChildClusters.csv")) %>% 
  mutate(across(matches("filename"), str_replace_all, "MoBaDataDictionaryHarmony", ""))%>% 
  mutate(across(matches("filename"), str_replace_all, ".xlsx.*$", "")) %>%
  mutate(Var1 = str_remove_all(str_to_title(question)," ")) %>%
  mutate(SubjectQ = filename) %>%
  select(-filename) %>%
  distinct() %>% 
  inner_join(ChDatD, by=c("Var1","SubjectQ")) %>% 
  mutate(QCode = str_remove_all(ItemCode,"[0-9]*")) %>%
  mutate(value = round(abs(as.numeric(similarity.to.cluster.centre)),3)) %>%
  na.omit()

ChCssClu  <- ChCssClu[-1,]

fwrite(ChCssClu, paste0(DIR,"ChPsy_CosineClusters.csv"))

table(ChCssClu$cluster)

test1 <- ChCssClu %>% filter(cluster==1)
test2 <- ChCssClu %>% filter(cluster==2)


PaCssClu <- 
  read.xlsx2(paste0(DIR,"MoBaDataDictionaryHarmonyAdultClusters.xlsx"), sheetIndex = 1) %>% 
  # fread(paste0(DIR,"MoBaDataDictionaryHarmonyAdultClusters.csv")) %>% 
  mutate(across(matches("filename"), str_replace_all, "MoBaDataDictionaryHarmony", ""))%>% 
  mutate(across(matches("filename"), str_replace_all, ".xlsx.*$", "")) %>%
  mutate(Var1 = str_remove_all(str_to_title(question)," ")) %>%
  mutate(SubjectQ = filename) %>%
  select(-filename) %>%
  distinct() %>% 
  inner_join(PaDatD, by=c("Var1","SubjectQ")) %>% 
  mutate(QCode1 = str_remove_all(ItemCode,"[0-9]*")) %>%
  mutate(value = round(abs(as.numeric(similarity.to.cluster.centre)),3)) %>%
  na.omit()

PaCssClu  <- PaCssClu[-1,]

fwrite(PaCssClu, paste0(DIR,"MoBaDataDictionaryAdultCosineClusters.csv"))

# DONE
