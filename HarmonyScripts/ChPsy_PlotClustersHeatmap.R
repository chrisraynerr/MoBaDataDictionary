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


CssTab <- 
  fread(paste0(DIR,"MoBaDataDictionaryChildCosineSimilarityScaleItems.csv")) %>% 
  # na.omit() %>% 
  select(ScaleItem1,ScaleItem2,value=value2) %>% 
  distinct(ScaleItem1,ScaleItem2,.keep_all = T) %>% 
  mutate(value=round(value,2)) %>% 
  mutate(value=ifelse(value<abs(.70),0,value)) 
  # filter(value2 >0.75) %>% 

CssMat <- CssTab %>% pivot_wider(names_from = ScaleItem2, values_from = value)
CssMat <- CssMat[,-1] %>% as.matrix()
rownames(CssMat) <- colnames(CssMat)

CssMatPlotly <-
  heatmaply_cor(
    CssMat, 
    nodetype="scatter", 
    fontsize_col=1,
    fontsize_row=1,
    hide_colorbar=T,
    dendrogram="none"
    )
