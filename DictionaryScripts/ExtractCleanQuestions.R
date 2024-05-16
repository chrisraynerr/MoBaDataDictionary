remove(list = ls())
packages <- c("tabulizer","dplyr","foreach","stringr","purrr","tidyr","tibble",
  "zoo","reshape2", "splitstackshape","janitor","qdap")

installed_packages <-  packages %in% rownames(installed.packages())

if(any(installed_packages==F)){ install.packages(packages[!installed_packages],
    dependencies = T)}

suppressMessages(invisible( lapply( packages, library,character.only=T)))

#############################################################
# Extract tables
q_folder <- "Q_PDF/"
q <-list.files( path=q_folder)
  
tabs <- list()
foreach(i=seq_along(q)) %do% {
  tabs[[paste0(q[i])]] <- 
    extract_tables(
      paste0(q_folder,q[i]),
      method="lattice"
      # method="stream"
  )
}

#############################################################
# Get variable codes
codes <- list()
foreach(i=seq_along(tabs)) %do% {
  codes[[paste0(q[i])]] <- 
    tabs[[paste0(q[i])]] %>%
    map_dfr(~ as.data.frame(as.matrix(.))) %>%
    as_tibble(.) %>%
    mutate_all(as.character) %>%
    mutate_all(str_remove_all, "_") %>%
    mutate_all(str_extract_all, "[A-Z][A-Z]+[0-9][0-9]+")%>%
    mutate_all(as.character) %>%
    mutate_all(na_if, "character(0)") %>%
    na_if(.,"") %>% 
    filter_all(any_vars(!is.na(.))) %>%
    janitor::remove_empty(., which = "cols") %>%
    mutate_all(str_remove_all, "\\(|\\)") %>%
    mutate_all(str_remove_all, "\"") %>%
    mutate_all(str_remove_all, "c|,") %>% 
    mutate_all(str_replace_all, "\\s+", ";") %>% 
    mutate_all(str_replace_all, " ", ";") %>% 
    pmap(., ~ c(c(...)[!is.na(c(...))], c(...)[is.na(c(...))])) %>%
    exec(rbind, !!!.) %>%
    as_tibble()
}
 
foreach(i=seq_along(tabs)) %do% {
  foreach(j=c(names(codes[[paste0(q[i])]]))) %do% {
      k <- str_count(codes[[paste0(q[i])]][j], ";")
      codes[[paste0(q[i])]] <- 
        codes[[paste0(q[i])]] %>% 
        separate(., col = j, sep = ";", into = c(paste0("C",j,"_",1:k)))
  } 
}

foreach(i=seq_along(tabs)) %do% {
  codes[[paste0(q[i])]] <- 
    codes[[paste0(q[i])]] %>%
    janitor::remove_empty(., which = "cols") %>% 
    pmap(., ~ c(c(...)[!is.na(c(...))], c(...)[is.na(c(...))])) %>%
    exec(rbind, !!!.) %>%
    as_tibble() %>% 
    set_names(c(paste0("C", seq_along(.))))
}

codes <- do.call("bind_rows", codes) 

codes_dx <- 
  codes %>% 
  distinct()  %>%
  janitor::remove_empty(., which = "cols")

#############################################################

quest <- list()
foreach(i=seq_along(tabs)) %do% {
  quest[[paste0(q[i])]] <- 
    tabs[[paste0(q[i])]] %>%
    map_dfr(~ as.data.frame(as.matrix(.))) %>%
    as_tibble(.) %>%
    mutate_all(as.character) %>% 
    select(1,2,3,4) %>%
    #na.omit() %>%
    na_if(.,"") %>%
    filter(! (is.na(V1) & is.na(V2)) ) %>%
    mutate(V1 = str_replace_all(V1, "[A-Za-z]", "")) %>%
    mutate(V1 = as.integer(str_replace_all(V1, "([0-9]+)(.*)","\\1"))) %>%
    mutate_all(str_replace_all, "_","") %>%
    mutate_all(str_replace_all, ",","") %>%
    mutate_all(str_replace_all, "'","") %>%
    mutate_all(str_replace_all, "\\'","") %>%
    mutate_all(str_replace_all, "\\s*\\(.*\\)", "") %>%
    mutate_all(str_replace_all,"\\([^\\)]+\\)","") %>%
    mutate_all(str_replace_all, " ?/ ?", " or ") %>%   
    mutate_all(str_replace_all, "([0-9]+)(-)([0-9]+)","\\1 to \\3") %>%
    mutate_all(str_replace_all, "([0-9]+)(-)","\\1==") %>%
    mutate_all(str_replace_all, "([0-9]+)(\\))","\\1==") %>%
    mutate_all(str_replace_all, "== ","==") %>%
    mutate_all(str_replace_all, "([0-9]+)(\\.)","\\.\\.\\.") %>%
    mutate_all(str_replace_all, "^([a-z])(.*)$","\\.\\.\\.\\1\\2") %>%
    mutate_all(str_replace_all, "([0-9]+)(==)(.*)$","\\.\\.\\.\\1\\2\\3") %>%
    mutate_all(str_replace_all, "(Number)(.*)$","\\.\\.\\.\\1\\2") %>%
    filter(!grepl("response|option|variable|date|20[0-2][0-9]",V2,ignore.case=T)) %>%
    filter(!grepl("response|option|variable|date|20[0-2][0-9]",V3,ignore.case=T)) %>%    
    filter(!grepl("Approx\\.",V2,ignore.case=F)) %>%
    filter(!grepl("Approx\\.",V3,ignore.case=F)) %>%
    mutate_all(str_replace_all, "\\(.*$", "") %>%
    mutate_all(str_replace_all, "^.*\\)", "") %>%
    mutate_all(str_replace_all, "(-)([0-9])", "to \\2") %>%
    mutate_all(str_replace_all, "\\s([0-9]+)( to )([0-9]+)", ";\\1\\2\\3;") %>%
    mutate_all(str_replace_all, "(^0)(\\sto\\s)([0-9]+)", ";\\1\\2\\3") %>%
    mutate_all(str_replace_all, "(\\s+)([A-Z]++[0-9]++$)", ";\\2") %>%
    mutate_all(str_replace_all, "([a-z])([A-Z]++[0-9]++$)", "\\1;\\2") %>%
    mutate_all(str_replace_all, "([A-Z]++[0-9]++)", ";\\1;") %>%
    mutate_all(str_replace_all,  "\\s+", " ") %>%
    mutate_all(str_replace_all,  ";++|;\\s;", ";") %>%
    mutate_all(str_replace_all,  "\\s+;", ";") %>%
    mutate_all(str_replace_all,  "^;", "") %>%
    mutate_all(str_replace_all,  ";;", ";") %>%
    mutate_all(str_replace_all, " ?/ ?", " or ") %>%
    mutate_all(str_replace_all, "^\\s|\\s$", "") %>%
    mutate_all(str_replace_all, ": A", " a") %>%
    mutate_all(str_replace_all, "No\\.", "Number") %>%
    mutate_all(str_replace_all,  "etc\\.", "etc") %>%
    mutate_all(str_replace_all,  "i\\.e\\.", "ie") %>%
    mutate_all(str_replace_all,  "e\\.g\\.", "eg") %>%
    mutate_all(str_replace_all,  "Version A|Version B", "") %>%
    mutate_all(str_replace_all,  "In version A|In version B", "") %>%
    mutate_all(str_replace_all,  "Nor", "nor") %>%
    mutate_all(str_replace_all,  "NOK", "nok") %>%
    mutate_all(str_replace_all,  "IUD", "iud") %>%
    mutate_all(str_replace_all,  "TV", "tv") %>%
    mutate_all(str_replace_all,  "DVD", "dvd") %>%
    mutate_all(str_replace_all,  "PC", "pc") %>%
    mutate_all(str_replace_all,  "X.ray", "xray") %>%
    mutate_all(str_replace_all,  "==;", "==") %>%
    separate(V2, c("Q2_1","Q2_2","Q2_3","Q2_4"), "\\.\\.\\.") %>%
    separate(V3, c("Q3_1","Q3_2","Q3_3","Q3_4"), "\\.\\.\\.") %>%
    separate(Q2_1, c("Q2_1", paste0("Q2_1_C",1:10)), ";") %>%
    separate(Q2_2, c("Q2_2", paste0("Q2_2_C",1:10)), ";") %>%
    mutate_all(str_replace_all, "_","") %>%
    mutate_all(str_replace_all, ",","") %>%
    mutate_all(str_replace_all, "\\'","") %>%
    mutate_all(str_replace_all, "child\\’s", "childs") %>%
    mutate_all(str_replace_all, "Child\\’s", "Childs") %>%
    mutate_all(str_replace_all, "has s or he", "") %>%
    mutate_all(str_replace_all, "^\\s|\\s$", "") %>%
    na_if(.,"") %>%
    na_if(.," ") %>%
    mutate(Q = str_replace(q[i], "\\.pdf", ""))
}

# view(
#   do.call("rbind", quest)  %>%  
#     mutate_all(str_replace_all, "([A-Z]++[0-9]++)", "") %>%
#     na_if(.,"") %>%
#     filter(! (is.na(V1) | is.na(Q2_1))) 
#   ) # %>% distinct(R0, .keep_all = T))

#############################################################
# Get variable text

# Rules: 
# Response: ([0-9]+\\)) 
quest <- list()
foreach(i=seq_along(tabs)) %do% {
  quest[[paste0(q[i])]] <- 
    tabs[[paste0(q[i])]] %>%
    map_dfr(~ as.data.frame(as.matrix(.))) %>%
    as_tibble(.) %>%
    mutate_all(as.character) %>% 
    select(1,2,3,4) %>%
    #na.omit() %>%
    na_if(.,"") %>%
    filter(! (is.na(V1) & is.na(V2)) ) %>%
    mutate(V1 = str_replace_all(V1, "[A-Za-z]", "")) %>%
    mutate(V1 = as.integer(str_replace_all(V1, "([0-9]+)(.*)","\\1"))) %>%
    mutate_all(str_replace_all, "_","") %>%
    mutate_all(str_replace_all, ",","") %>%
    mutate_all(str_replace_all, "'","") %>%
    mutate_all(str_replace_all, "\\s*\\(.*\\)", "") %>%
    mutate_all(str_replace_all,"\\([^\\)]+\\)","") %>%
    mutate_all(str_replace_all, " ?/ ?", " or ") %>%   
    mutate_all(str_replace_all, "([0-9]+)(-)([0-9]+)","\\1 to \\3") %>%
    mutate_all(str_replace_all, "([0-9]+)(-)","\\1==") %>%
    mutate_all(str_replace_all, "([0-9]+)(\\))","\\1==") %>%
    mutate_all(str_replace_all, "== ","==") %>%
    mutate_all(str_replace_all, "([0-9]+)(\\.)","\\.\\.\\.") %>%
    mutate_all(str_replace_all, "^([a-z])(.*)$","\\.\\.\\.\\1\\2") %>%
    mutate_all(str_replace_all, "([0-9]+)(==)(.*)$","\\.\\.\\.\\1\\2\\3") %>%
    mutate_all(str_replace_all, "(Number)(.*)$","\\.\\.\\.\\1\\2") %>%
    filter(!grepl("response|option|variable|date|20[0-2][0-9]",V2,ignore.case=T)) %>%
    filter(!grepl("response|option|variable|date|20[0-2][0-9]",V3,ignore.case=T)) %>%    
    filter(!grepl("Approx\\.",V2,ignore.case=F)) %>%
    filter(!grepl("Approx\\.",V3,ignore.case=F)) %>%
    mutate_all(str_replace_all, "\\(.*$", "") %>%
    mutate_all(str_replace_all, "^.*\\)", "") %>%
    mutate_all(str_replace_all, "(-)([0-9])", "to \\2") %>%
    mutate_all(str_replace_all, "\\s([0-9]+)( to )([0-9]+)", ";\\1\\2\\3;") %>%
    mutate_all(str_replace_all, "(^0)(\\sto\\s)([0-9]+)", ";\\1\\2\\3") %>%
    mutate_all(str_replace_all, "(\\s+)([A-Z]++[0-9]++$)", ";\\2") %>%
    mutate_all(str_replace_all, "([a-z])([A-Z]++[0-9]++$)", "\\1;\\2") %>%
    mutate_all(str_replace_all, "([A-Z]++[0-9]++)", ";\\1;") %>%
    mutate_all(str_replace_all,  "\\s+", " ") %>%
    mutate_all(str_replace_all,  ";++|;\\s;", ";") %>%
    mutate_all(str_replace_all,  "\\s+;", ";") %>%
    mutate_all(str_replace_all,  "^;", "") %>%
    mutate_all(str_replace_all,  ";;", ";") %>%
    mutate_all(str_replace_all, " ?/ ?", " or ") %>%
    mutate_all(str_replace_all, "^\\s|\\s$", "") %>%
    mutate_all(str_replace_all, ": A", " a") %>%
    mutate_all(str_replace_all, "No\\.", "Number") %>%
    mutate_all(str_replace_all,  "etc\\.", "etc") %>%
    mutate_all(str_replace_all,  "i\\.e\\.", "ie") %>%
    mutate_all(str_replace_all,  "e\\.g\\.", "eg") %>%
    mutate_all(str_replace_all,  "Version A|Version B", "") %>%
    mutate_all(str_replace_all,  "In version A|In version B", "") %>%
    mutate_all(str_replace_all,  "Nor", "nor") %>%
    mutate_all(str_replace_all,  "NOK", "nok") %>%
    mutate_all(str_replace_all,  "IUD", "iud") %>%
    mutate_all(str_replace_all,  "TV", "tv") %>%
    mutate_all(str_replace_all,  "DVD", "dvd") %>%
    mutate_all(str_replace_all,  "PC", "pc") %>%
    mutate_all(str_replace_all,  "X.ray", "xray") %>%
    mutate_all(str_replace_all,  "==;", "==") %>%
    separate(V2, c("Q2_1","Q2_2","Q2_3","Q2_4"), "\\.\\.\\.") %>%
    separate(V3, c("Q3_1","Q3_2","Q3_3","Q3_4"), "\\.\\.\\.") %>%
    separate(Q2_1, c("Q2_1", paste0("Q2_1_C",1:10)), ";") %>%
    separate(Q2_2, c("Q2_2", paste0("Q2_2_C",1:10)), ";") %>%
    mutate_all(str_replace_all, "_","") %>%
    mutate_all(str_replace_all, ",","") %>%
    mutate_all(str_replace_all, "\\'","") %>%
    mutate_all(str_replace_all, "child\\’s", "childs") %>%
    mutate_all(str_replace_all, "Child\\’s", "Childs") %>%
    mutate_all(str_replace_all, "has s or he", "") %>%
    mutate_all(str_replace_all, "^\\s|\\s$", "") %>%
    na_if(.,"") %>%
    na_if(.," ") %>%
    mutate(Q2_1T = Q2_1) %>%
    fill(Q2_1) %>%
    mutate(      
      Q1 = 
        ifelse(grepl("^[A-Z].*\\?", Q2_1T), Q2_1T,
               ifelse(grepl("^[A-Za-z].*\\?", Q2_2), paste0(Q2_1, " ", Q2_2), NA
                      ))) %>%
    mutate_all(str_replace_all, "^\\s|\\s$", "") %>%
    mutate(      
      Q2 = 
        ifelse(!grepl("^.*\\?", Q2_1T) & !is.na(Q2_1T), Q2_1T, 
               ifelse(!grepl("^.*\\?|==", Q2_2) & !is.na(Q2_2), Q2_2, NA
      ))) %>%
    mutate_all(str_replace_all, "^\\s|\\s$", "") %>%
    mutate(
      R =
        ifelse(grepl("==|Number",Q2_1T), Q2_1T,
               ifelse(grepl("==|Number",Q2_2), Q2_2,
                      ifelse(grepl("==|Number",Q2_3), Q2_3,
                      ifelse(grepl("==|Number",Q2_4), Q2_4,
                             ifelse(grepl("==|Number",Q3_1), Q3_1,
                                    ifelse(grepl("==|Number",Q3_2), Q3_2,
                                           ifelse(grepl("==|Number",Q3_3), Q3_3,
                                                  ifelse(grepl("==|Number",Q3_4), Q3_4,
                             NA
                             ))))))))) %>%
    mutate(R = str_replace_all(R, "[A-Z]+[0-9]+;", "")) %>%    
    mutate(Q2 = ifelse(grepl("==|Number",Q2), NA, Q2)) %>%
    mutate_all(str_replace_all, "^\\s|\\s$", "") %>%
    na_if(.,"") %>%
    na_if(.," ") %>%
    select(Q1,Q2,R,everything()) %>%
    set_names(c("Q1","Q2","R","V1",paste0("V",2:(length(.)-3)))) %>%
    mutate(
      CODES = 
        paste0(
          Q1,";",Q2,";",R,";",V1,";",
          V2,";",V3,";",V4,";",V5,";",V6,";",V7,";",V8,";",V9,";",V10,";",V11,";",V12,";",
          V13,";",V14,";",V15,";",V16,";",V17,";",V18,";",V19,";",V20,";",V21,";",V22,";",
          V23,";",V24,";",V25,";",V26,";",V27,";",V28,";",V29,";",V30,";",V31)) %>%
    mutate(CODES = str_extract_all(CODES, "[A-Z][A-Z][A-Z]?[0-9]+;|[A-Z][A-Z][A-Z]?[0-9]+")) %>%  
    mutate(CODES = str_replace_all(CODES, ";+", ";")) %>%    
    mutate(CODES = str_replace_all(CODES, "^;|;$", "")) %>%
    mutate(
      OPT = 
        paste0(
          V2,";",V3,";",V4,";",V5,";",V6,";",V7,";",V8,";",V9,";",V10,";",V11,";",V12,";",
          V13,";",V14,";",V15,";",V16,";",V17,";",V18,";",V19,";",V20,";",V21,";",V22,";",
          V23,";",V24,";",V25,";",V26,";",V27,";",V28,";",V29,";",V30,";",V31)) %>%
    mutate(OPT = str_extract_all(OPT, "[0-9]+\\sto\\s[0-9]+;")) %>%  
    mutate(OPT = str_replace_all(OPT, ";+", ";")) %>%    
    mutate(OPT = str_replace_all(OPT, "^;|;$", "")) %>%
    na_if(.,"") %>%
    na_if(.," ") %>%
    select(Q1,Q2,R,V1,CODES,OPT) %>%
    fill(Q1) %>%
    filter(!(is.na(Q1))) %>%
    mutate(Q1 = str_replace_all(Q1, "(^[A-Z].*\\?)(.*$)", "\\1")) %>%
    mutate(Q2 = ifelse(str_count(Q2, "\\w+")>8, NA, Q2))%>%
    mutate(CODES = ifelse(CODES=="character(0)",NA,CODES))%>%
    mutate(CODES = str_replace_all(CODES, "\\(|\\)", "")) %>%
    mutate(CODES = str_replace_all(CODES, "\"", "")) %>%
    mutate(CODES = str_replace_all(CODES, "c|,", "")) %>%
    mutate(OPT = ifelse(OPT=="character(0)",NA,OPT)) %>%
    mutate(OPT = str_replace_all(OPT, "\\(|\\)", "")) %>%
    mutate(OPT = str_replace_all(OPT, "\"", "")) %>%
    mutate(OPT = str_replace_all(OPT, "c|,", ""))%>%
    mutate(OPT = str_replace_all(OPT, "0 to 99+", ""))%>%
    mutate(Q2 = str_remove_all(Q2, "[A-Z]++[0-9]++")) %>%
    mutate(Q1 = str_remove_all(Q1, "[A-Z]++[0-9]++")) %>%
    mutate(Q1 = str_remove_all(Q1, "If yes")) %>%
    mutate(Q1 = str_remove_all(Q1, "^[0-9]+[a-z]")) %>%
    mutate(Q1 = str_remove_all(Q1, "^\\?|^:|^\\."))
}

#######################################################3

resp <- list()
foreach(i=seq_along(tabs)) %do% {
  resp[[paste0(q[i])]] <- 
    quest[[paste0(q[i])]] %>%
    select(Q1,R) %>%
    # mutate_all(as.factor) %>%
    distinct() %>% 
    na.omit() %>%
    mutate(R0 = ifelse(!grepl("==",R), R, NA)) %>%
    mutate(R1 = ifelse(grepl("1==",R), R, NA)) %>%
    mutate(R2 = ifelse(grepl("2==",R), R, NA)) %>%
    mutate(R3 = ifelse(grepl("3==",R), R, NA)) %>%
    mutate(R4 = ifelse(grepl("4==",R), R, NA)) %>%
    mutate(R5 = ifelse(grepl("5==",R), R, NA)) %>%
    mutate(R6 = ifelse(grepl("6==",R), R, NA)) %>%
    mutate(R7 = ifelse(grepl("7==",R), R, NA)) %>%
    mutate(R8 = ifelse(grepl("8==",R), R, NA)) %>%
    mutate(R9 = ifelse(grepl("9==",R), R, NA)) %>%
    mutate(R10 = ifelse(grepl("10==",R), R, NA)) %>%
    mutate(R11 = ifelse(grepl("11==",R), R, NA)) %>%
    mutate(R12 = ifelse(grepl("12==",R), R, NA)) %>%
    mutate(R13 = ifelse(grepl("13==",R), R, NA)) %>%
    mutate(R14 = ifelse(grepl("14==",R), R, NA)) %>%
    mutate(R15 = ifelse(grepl("15==",R), R, NA))

}

unq_resp <- list()
foreach(i=seq_along(tabs)) %do% {
  unq_resp[[paste0(q[i])]] <- 
    resp[[paste0(q[i])]] %>%  select(-R) %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R0) %>% na.omit(), by = "Q1") %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R1) %>% na.omit(), by = "Q1") %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R2) %>% na.omit(), by = "Q1") %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R3) %>% na.omit(), by = "Q1") %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R4) %>% na.omit(), by = "Q1") %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R5) %>% na.omit(), by = "Q1") %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R6) %>% na.omit(), by = "Q1") %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R7) %>% na.omit(), by = "Q1") %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R8) %>% na.omit(), by = "Q1") %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R9) %>% na.omit(), by = "Q1") %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R10) %>% na.omit(), by = "Q1") %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R11) %>% na.omit(), by = "Q1") %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R12) %>% na.omit(), by = "Q1") %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R13) %>% na.omit(), by = "Q1") %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R14) %>% na.omit(), by = "Q1") %>%
    full_join(resp[[paste0(q[i])]] %>% select(Q1,R15) %>% na.omit(), by = "Q1") %>%
    select(Q1,matches(".y")) %>%
    mutate(R = paste0(
      R0.y,";",R1.y,";",R2.y,";",R3.y,";",R4.y,";",
      R5.y,";",R6.y,";",R7.y,";",R8.y,";",R9.y,";",
      R10.y,";",R11.y,";",R12.y,";",R13.y,";",R14.y,";",R15.y)) %>%
    select(Q1,R) %>%
    mutate(R = str_remove_all(R, "NA")) %>%
    mutate(R = str_replace_all(R, ";+", ";")) %>%
    mutate(R = str_remove_all(R, ";$|^;")) %>%
    mutate(R = str_replace_all(R, "= ", "=")) %>%
    mutate(R = str_replace_all(R, "–", "")) %>%
    mutate(R = str_replace_all(R, " ;", ";")) %>%
    mutate(R = str_replace_all(R, "([0-9])(;)(\\s[a-z])", "\\1 \\3")) %>%
    distinct()
  }

Q <- list()
foreach(i=seq_along(tabs)) %do% {
  Q[[paste0(q[i])]] <- 
    quest[[paste0(q[i])]] %>%
    select(Q1,Q2,CODES,OPT) %>%
    filter(!is.na(Q2)) %>%
    full_join(unq_resp[[paste0(q[i])]], by = "Q1") %>%
    mutate(TYPE = ifelse(grepl("==", R), "Categorical",
                         ifelse(grepl("Number", R), "Numeric","Tick"))) %>%
    mutate(CODES = str_replace_all(CODES, "^B1;|^B21;", "\\1")) %>%
    mutate(C1 = str_replace_all(CODES, "^([A-Z]+[0-9]+;)(.*$)", "\\1")) %>%
    mutate(C1 = str_remove_all(C1, ";")) %>%
    mutate(Q1 = str_replace_all(Q1, "If yes", "")) %>%
    mutate(Q1 = str_replace_all(Q1, "Version A|Version B", "")) %>%
    mutate(Q1 = str_replace_all(Q1, "In version A|In version B", "")) %>%
    mutate(Q1 = str_replace_all(Q1, "Nor", "nor")) %>%
    mutate(Q1 = str_replace_all(Q1, "NOK", "nok")) %>%
    mutate(Q1 = str_replace_all(Q1, "IUD", "iud")) %>%
    mutate(Q1 = str_replace_all(Q1, "TV", "tv")) %>%
    mutate(Q1 = str_replace_all(Q1, "DVD", "dvd")) %>%
    mutate(Q1 = str_replace_all(Q1, "X.ray", "xray")) %>%
    mutate(Q1 = str_replace_all(Q1, "Hospi- talised", "Hospitalised")) %>%
    mutate(Q1 = str_replace_all(Q1, "hospi- talised", "hospitalised")) %>%
    mutate(Q1 = str_replace_all(Q1, "Symptoms", "symptoms")) %>%
    mutate(Q1 = str_replace_all(Q1, "Has problem\\? Been", "Has problem been")) %>%
    mutate(Q1 = str_replace_all(Q1, 
         "^Has or.*$", 
         "Does the child have or ever had any of the following illnesses or health problems\\? Give the childs age at the first sign of the illness. If the child no longer has the illness state age of recovery"
          )) %>%
    mutate(Q1 = str_replace_all(Q1, "^.* [a-z] [a-z] [a-z].*$", "")) %>%
    mutate_all(str_remove_all, ",") %>%
    mutate_all(str_remove_all, ",") %>%
    mutate_all(str_remove_all, "^ ") %>%
    mutate(TP = str_replace(q[i], "\\.pdf", "")) %>%
    mutate(Q1 = str_replace_all(Q1, "(\\?)( )","\\?;")) %>%
    mutate(Q1 = str_replace_all(Q1, "(\\?)( )","\\?;")) %>%
    mutate(Q1 = str_replace_all(Q1, "(\\.)( )","\\.;")) %>%
    mutate(Q1 = str_replace_all(Q1, "( )([A-Z])","\\1;\\2")) %>%
    separate(Q1,  c("Q1_1","Q1_2","Q1_3","Q1_4"), ";") %>%  
    mutate(R0 = str_remove_all(R, "–|-")) %>%
    mutate_all(str_replace_all,  "\\s+", " ") %>%
    mutate(R0 = str_remove_all(R0, "0 . 9+")) %>%
    mutate(R0 = str_remove_all(R0, "0 to 9+")) %>%
    mutate(R0 = str_remove_all(R0, "0 9+")) %>%
    mutate(R0 = str_replace_all(R0, "Number 0", "Number")) %>% 
    mutate(R0 = str_replace_all(R0, " ;", ";")) %>% 
    mutate(R0 = str_replace_all(R0, "Yes fy", "")) %>% 
    mutate(R0 = str_replace_all(R0, "^2==No$", "1==Yes;2==No")) %>%
    mutate(R0 = str_replace_all(R0, "1==no 1==no 1==no;", "1==no;")) %>%
    mutate(R0 = str_replace_all(R0, "2Snack in the afternoon", "2")) %>%
    mutate(R0 = str_replace_all(R0, "2==yes a bit 2==yes a bit;", "2==yes a bit;")) %>%
    mutate(R0 = str_replace_all(R0, "3==yes a lot 3==yes a lot 3==yes a lot;", "3==yes a lot;")) %>%
    mutate(R0 = str_replace_all(R0, "1==yes;2==no but.*$", "1==yes;2==no but had previously;3==no never had")) %>%
    mutate(R0 = str_replace_all(R0, "1==Not typical.*$", "1==Not typical;2==Not very typical;3==Quite typical;4==Typical;5==Very typical")) %>%
    mutate(R0 = str_replace_all(R0, "^.*bothered.*$", "1==Not bothered;2==A little bothered;3==Quite bothered;4==Very bothered")) %>%
    mutate(R0 = str_replace_all(R0, "^.*2==Yes but not in the last year;3==Yes during the last year$", "1==Never;2==Yes but not in the last year;3==Yes during the last year")) %>% 
    mutate(R0 = str_replace_all(R0, 
          "1==Never;2==Less than once a week;3==.*$|1==Never;2==1 to 3s times a week.*$", 
          "1==Never;2==Less than once a week;3==1 to 3 times a week;4==4 to 6 times a week;5==1 to 2 times in 24 hours;6==3 to 4 times in 24 hours;7==5 or more times in 24 hours"
          )) %>%
    mutate(R0 = str_replace_all(R0, 
          "^.*Almost never.*$", 
          "1==Never;2==Almost never;3==Sometimes;4==Often;5=Always"
        )) %>%
    mutate(R0 = str_replace_all(R0,
          "^1==Less than 1 minute;2==1 to 10 minutes.*$", 
          "1==Less than 1 minute;2==1 to 10 minutes;3==11 to 30 minutes;4==31 to 60 minutes;5==More than 60 minutes"
        )) %>%
    mutate(R0 = str_replace_all(R0, 
          "^1==Never;2==Less than once per week.*$", 
          "1==Never;2==Less than once per week;3==Once per week;4==2 to 3 times per week;5==4 to 6 times per week;6==Approximately every day"
        )) %>%
    mutate(R0 = str_replace_all(R0,
          "^1==None;2==Less than 1 per week.*$", 
          "1==None;2==Less than 1 per week;3==1 to 2 times per week;4==3 or more times per week"
        )) %>%
    mutate(R0 = str_replace_all(R0,
          "^1==None;2==Less than 1 per week.*$", 
          "1==None;2==Less than 1 per week;3==1 to 2 times per week;4==3 or more times per week"
        )) %>%    
    mutate(R0 = str_replace_all(R0,
              "^1==10 or more;2==7 to 9.*$", 
              "1==10 or more;2==7 to 9;3==5 to 6;4==3 to 4;5==1 to 2;6==Less than 1;7==Never"
            )) %>%
    mutate(R0 = str_replace_all(R0,
          "^.*1==Sick leave;2==Absent due to sick child;3==Mde redundant with compensation;4==Absent with maternity allowance due to the working environment;5==Started maternity leave;6==Service leave;7==Other", 
          "1==Sick leave;2==Absent due to sick child;3==Mde redundant with compensation;4==Absent with maternity allowance due to the working environment;5==Started maternity leave;6==Service leave;7==Other"
        )) %>%
    mutate(R0 = str_replace_all(R0,
          "^.*1==No;2==Yes partly on sick leave;3==Yes completely on sick leave",
          "1==No;2==Yes partly on sick leave;3==Yes completely on sick leave"
        )) %>%    
    mutate(R0 = str_replace_all(R0,
              "^1==Married.*$",
              "1==Married;2==Cohabiting;3==Single;4==Divorced or separated;5==Widowed;6==Other"
            )) %>%
    mutate(R0 = str_replace_all(R0,
          "1==10 or more.*$",
          "1==10 or more;2==7 to 9;3==5 to 6;4==3 to 4;5==1 to 2;6==Less than 1;7==Never"
        )) %>%
    mutate(R0 = str_replace_all(R0,
          "1==Bring packed lunch from home.*$",
          "1==Bring packed lunch from home;2==Buy at school;3==Buy outside of school;4==Don’t eat lunch at school"
        )) %>%
    mutate(R0 = str_replace_all(R0,
          "1==Agree completely.*$",
          "1==Agree completely;2==Agree;3==Agree somewhat;4==Disagree somewhat;5==Disagree;6==Disagree completely"
        )) %>%
    mutate(R0 = str_replace_all(R0,
          "1==Less than 1 minute.*$",
          "1==Less than 1 minute;2==1 to 10 minutes;3==11 to 30 minutes;4==31 to 60 minutes;5==More than 60 minutes"
        )) %>%
    mutate(R0 = str_replace_all(R0,
          "Number of times to |Number of times examined by ",
          "Number of times;"
        )) %>%
    mutate(R0 = str_replace_all(R0,
          "Number of months if more than 3.*$",
          "Number of months if more than 3;1==Less than 1 month;2==1 to 2 months;3==3 months or more"
        )) %>%
    mutate(R0 = str_replace_all(R0,
          "1==0 glass per month;2==1 to 3 glass per month;3==1 glass per week.*$",
          "1==0 glass per month;2==1 to 3 glass per month;3==1 glass per week;4==2 to 6 glass per week;5==1 glass per day;6==2 to 3 glass per day;7==More than 3 glass per day"
        )) %>%
    mutate(R0 = str_replace_all(R0,
          "1==0 to 2 weeks.*$",
          "1==0 to 2 weeks;2==3 to 4 weeks;3==1 to 2 mth;4==3 to 6 mth;5==7 to 12 mth"
        )) %>%
    mutate(
      R0 = str_replace_all(R0,
      "1==1 to 4 times a month.*$",
      "1==1 to 4 times a month;2==1 to 6 times a week;3==Once a day;4==More than once a day"
      )) %>%
    mutate(
      R0 = str_replace_all(R0,
      "1==13\\+ per day.*$",
      "1==13\\+ per day;2==9 to 12 per day;4==7 per day;5==6 per day;6==5 per day;8==3 per day;10==1 per day"
      )) %>%
    mutate(
      R0 = str_replace_all(R0,
      "1==3 or more times every night.*$",
      "1==3 or more times every night;2==Once or twice every night;3==A few times a week;4==Seldom or never"
      )) %>%
    mutate(
      R0 = str_replace_all(R0,
      "1==4\\+.*$",
      "1==4\\+;2==3;3==2;4==1"
      )) %>%
    mutate(R0 = str_replace_all(R0, "( )([1-9]==)",";\\2")) %>%
    separate(R0, c("R_1","R_2","R_3","R_4","R_5","R_6","R_7","R_8","R_9","R_10","R_11","R_12"), ";") 
}


Q0 <- list()
foreach(i=seq_along(tabs)) %do% {
Q0[[paste0(q[i])]] <- 
  Q[[paste0(q[i])]] %>%
  mutate(
    R0 = 
      ifelse(grepl("^[A-Za-z]",R_1), R_1, 
      ifelse(grepl("^[A-Za-z]",R_2), R_2,
      ifelse(grepl("^[A-Za-z]",R_3), R_3,
      ifelse(grepl("^[A-Za-z]",R_4), R_4,
      ifelse(grepl("^[A-Za-z]",R_5), R_5,
      ifelse(grepl("^[A-Za-z]",R_6), R_6,
      ifelse(grepl("^[A-Za-z]",R_7), R_7,
      ifelse(grepl("^[A-Za-z]",R_8), R_8,
      ifelse(grepl("^[A-Za-z]",R_9), R_9,
      ifelse(grepl("^[A-Za-z]",R_10), R_10,NA
      ))))))))))
      ) %>%
  mutate(
    R1 = 
      ifelse(grepl("^1==",R_1), R_1, 
      ifelse(grepl("^1==",R_2), R_2,
      ifelse(grepl("^1==",R_3), R_3,
      ifelse(grepl("^1==",R_4), R_4,
      ifelse(grepl("^1==",R_5), R_5,
      ifelse(grepl("^1==",R_6), R_6,
      ifelse(grepl("^1==",R_7), R_7,
      ifelse(grepl("^1==",R_8), R_8,
      ifelse(grepl("^1==",R_9), R_9,
      ifelse(grepl("^1==",R_10), R_10,NA
      ))))))))))) %>%
    mutate(
      R2 = 
      ifelse(grepl("^2==",R_1), R_1, 
      ifelse(grepl("^2==",R_2), R_2,
      ifelse(grepl("^2==",R_3), R_3,
      ifelse(grepl("^2==",R_4), R_4,
      ifelse(grepl("^2==",R_5), R_5,
      ifelse(grepl("^2==",R_6), R_6,
      ifelse(grepl("^2==",R_7), R_7,
      ifelse(grepl("^2==",R_8), R_8,
      ifelse(grepl("^2==",R_9), R_9,
      ifelse(grepl("^2==",R_10), R_10,NA
      ))))))))))) %>%
  mutate(
    R3 = 
      ifelse(grepl("^3==",R_1), R_1, 
      ifelse(grepl("^3==",R_2), R_2,
      ifelse(grepl("^3==",R_3), R_3,
      ifelse(grepl("^3==",R_4), R_4,
      ifelse(grepl("^3==",R_5), R_5,
      ifelse(grepl("^3==",R_6), R_6,
      ifelse(grepl("^3==",R_7), R_7,
      ifelse(grepl("^3==",R_8), R_8,
      ifelse(grepl("^3==",R_9), R_9,
      ifelse(grepl("^3==",R_10), R_10,NA
      ))))))))))) %>%
  mutate(
    R4 = 
      ifelse(grepl("^4==",R_1), R_1, 
      ifelse(grepl("^4==",R_2), R_2,
      ifelse(grepl("^4==",R_3), R_3,
      ifelse(grepl("^4==",R_4), R_4,
      ifelse(grepl("^4==",R_5), R_5,
      ifelse(grepl("^4==",R_6), R_6,
      ifelse(grepl("^4==",R_7), R_7,
      ifelse(grepl("^4==",R_8), R_8,
      ifelse(grepl("^4==",R_9), R_9,
      ifelse(grepl("^4==",R_10), R_10,NA
      ))))))))))) %>%
  mutate(
    R5 = 
       ifelse(grepl("^5==",R_1), R_1, 
       ifelse(grepl("^5==",R_2), R_2,
       ifelse(grepl("^5==",R_3), R_3,
       ifelse(grepl("^5==",R_4), R_4,
       ifelse(grepl("^5==",R_5), R_5,
       ifelse(grepl("^5==",R_6), R_6,
       ifelse(grepl("^5==",R_7), R_7,
       ifelse(grepl("^5==",R_8), R_8,
       ifelse(grepl("^5==",R_9), R_9,
       ifelse(grepl("^5==",R_10), R_10, NA
       ))))))))))) %>%
  mutate(
    R6 = 
       ifelse(grepl("^6==",R_1), R_1, 
       ifelse(grepl("^6==",R_2), R_2,
       ifelse(grepl("^6==",R_3), R_3,
       ifelse(grepl("^6==",R_4), R_4,
       ifelse(grepl("^6==",R_5), R_5,
       ifelse(grepl("^6==",R_6), R_6,
       ifelse(grepl("^6==",R_7), R_7,
       ifelse(grepl("^6==",R_8), R_8,
       ifelse(grepl("^6==",R_9), R_9,
       ifelse(grepl("^6==",R_10), R_10, NA
       ))))))))))) %>%
  mutate(
    R7 = 
        ifelse(grepl("^7==",R_1), R_1, 
        ifelse(grepl("^7==",R_2), R_2,
        ifelse(grepl("^7==",R_3), R_3,
        ifelse(grepl("^7==",R_4), R_4,
        ifelse(grepl("^7==",R_5), R_5,
        ifelse(grepl("^7==",R_6), R_6,
        ifelse(grepl("^7==",R_7), R_7,
        ifelse(grepl("^7==",R_8), R_8,
        ifelse(grepl("^7==",R_9), R_9,
        ifelse(grepl("^7==",R_10), R_10,NA
        ))))))))))) %>%
  mutate(
    R8 = 
        ifelse(grepl("^8==",R_1), R_1, 
        ifelse(grepl("^8==",R_2), R_2,
        ifelse(grepl("^8==",R_3), R_3,
        ifelse(grepl("^8==",R_4), R_4,
        ifelse(grepl("^8==",R_5), R_5,
        ifelse(grepl("^8==",R_6), R_6,
        ifelse(grepl("^8==",R_7), R_7,
        ifelse(grepl("^8==",R_8), R_8,
        ifelse(grepl("^8==",R_9), R_9,
        ifelse(grepl("^8==",R_10), R_10, NA
        ))))))))))) %>%
  mutate(
    R9 = 
        ifelse(grepl("^9==",R_1), R_1, 
        ifelse(grepl("^9==",R_2), R_2,
        ifelse(grepl("^9==",R_3), R_3,
        ifelse(grepl("^9==",R_4), R_4,
        ifelse(grepl("^9==",R_5), R_5,
        ifelse(grepl("^9==",R_6), R_6,
        ifelse(grepl("^9==",R_7), R_7,
        ifelse(grepl("^9==",R_8), R_8,
        ifelse(grepl("^9==",R_9), R_9,
        ifelse(grepl("^9==",R_10), R_10, NA
        ))))))))))) %>%
  mutate(
    R10 = 
        ifelse(grepl("^10==",R_1), R_1, 
        ifelse(grepl("^10==",R_2), R_2,
        ifelse(grepl("^10==",R_3), R_3,
        ifelse(grepl("^10==",R_4), R_4,
        ifelse(grepl("^10==",R_5), R_5,
        ifelse(grepl("^10==",R_6), R_6,
        ifelse(grepl("^10==",R_7), R_7,
        ifelse(grepl("^10==",R_8), R_8,
        ifelse(grepl("^10==",R_9), R_9,
        ifelse(grepl("^10==",R_10), R_10, NA
        )))))))))))%>%
        select(!matches("R_"))
}
      
# view(do.call("rbind", Q0)) # %>% distinct(R0, .keep_all = T))

Q <- 
  do.call("rbind", Q0) %>% 
  mutate_all(str_replace_all,"9==year","9 year") %>% 
  mutate_all(str_replace_all, "([0-9]+)(==)","") 
  

# remove duplicate codes
Q$CODES <- sapply(Q$CODES, function(x) paste(unique(trimws(unlist(strsplit(x,split=";",fixed=F,perl=T)))), collapse = ";"))
Q$R0 <- sapply(Q$R0, function(x) paste(unique(trimws(unlist(strsplit(x,split=" ",fixed=F,perl=T)))), collapse = " "))

DD <- 
  Q %>%
  mutate(
    R1 = ifelse(!is.na(R1),paste0("1==",R1),""),
    R2 = ifelse(!is.na(R2),paste0("2==",R2),""),
    R3 = ifelse(!is.na(R3),paste0("3==",R3),""),
    R4 = ifelse(!is.na(R4),paste0("4==",R4),""),
    R5 = ifelse(!is.na(R5),paste0("5==",R5),""),
    R6 = ifelse(!is.na(R6),paste0("6==",R6),""),
    R7 = ifelse(!is.na(R7),paste0("7==",R7),""),
    R8 = ifelse(!is.na(R8),paste0("8==",R8),""),
    R9 = ifelse(!is.na(R9),paste0("9==",R9),""),
    R10 = ifelse(!is.na(R10),paste0("1==",R10),"")
  ) %>%
  mutate(R_CAT = paste(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,sep = ";")) %>%
  mutate(R_CAT = ifelse(TYPE=="Tick", "1==Yes", R_CAT)) %>%
  mutate_all(str_replace_all, ";+",";") %>%
  mutate_all(str_replace_all, ";$","") %>%
  filter(CODES!="") %>% 
  filter(!is.na(CODES)) %>% 
  distinct(CODES, .keep_all = T) %>%
  mutate(Q1 = paste(Q1_1,Q1_2,Q1_3,Q1_4, sep = " ")) %>%
  mutate(Q1 = str_remove_all(Q1,"NA")) %>%
  mutate(Q1 = str_remove_all(Q1,"  | $")) %>%
  mutate(Q1 = str_to_sentence(Q1)) %>%
  mutate(Q2 = str_to_sentence(Q2)) %>%
  mutate(C1_ORD = str_replace(C1, "([A-Z]*)([0-9]*)", "\\1 \\2")) %>%
  separate(C1_ORD, c("TP_CODE","QN_CODE")) %>%
  mutate(QN_CODE = str_pad(QN_CODE, 4, pad="0")) %>%
  mutate(C1_ORD = paste0(TP_CODE,QN_CODE)) %>%
  select(Q1,Q2,C1_ORD,CODES,TYPE,R0,R_CAT,TP)

DATE <- Sys.Date()
data.table::fwrite(DD, paste0("Outputs/MoBaDataDictionary_",DATE,".csv", sep=","))
