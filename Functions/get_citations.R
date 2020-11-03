library(easyPubMed)
library(dplyr)
library(kableExtra)
library(readxl)
library(xlsx)
library(parallel)
library(foreach)
library(doParallel)
library(Hmisc)
library(readr)

DF <- read_csv("../Alain search results/In_Alain_Not_In_Micahel.csv")

n = dim(DF)[1]

# Query pubmed and fetch many results
my_query <- c(paste(DF$DOI[1], "[doi]",sep = ""),
              paste(" OR ",DF$DOI[2:n], "[doi]",sep = ""))




for (h in 1: ceiling(length(my_query)/20)){
  # Starting time: record
  t.start <- Sys.time()
  print(h)
  l= (20*(h-1)+1)
  u= 20*(h)
  
  z= my_query[l:u][!is.na(my_query[l:u])]
  
  my_query_mini = paste(z,collapse=" ")
  !is.na(my_query_mini)
  my_pubmed_ids <-  get_pubmed_ids(my_query_mini)
  
  print("Fetching complete")
  
  # Download by 1000-item batches
  my_batches <- seq(from = 1, to = my_pubmed_ids$Count, by = 20)
  my_abstracts_xml <- lapply(my_batches,  function(i) {
    fetch_pubmed_data(my_pubmed_ids, retmax = 1000, retstart = i)  
  })
  
  print("download XMLs complete")
  
  # Store Pubmed Records as elements of a list
  all_xml <- list()
  for(x in my_abstracts_xml) {
    xx <- articles_to_list(x)
    for(y in xx) {
      all_xml[[(1 + length(all_xml))]] <- y
    }  
  }
  
  print("download all XMLs")
  
  
  # Perform operation (use lapply here, no further parameters)
  
  if(h==1){
    final_df <- do.call(rbind, lapply(all_xml, article_to_df, 
                                      max_chars = -1, getAuthors = F, getKeywords = T))
  }else{
    
    intermidiate =  do.call(rbind, lapply(all_xml, article_to_df, 
                                          max_chars = -1, getAuthors = F, getKeywords = T))
    final_df <- rbind(final_df, intermidiate)
    
  }
  # Final time: record
  t.stop <- Sys.time()
  
  #How long did it take?
  print(t.stop - t.start)
  
}





rm(list =ls()[! ls() %in% c("final_df", "DF")])


redo_DF =  DF[which(!(DF$DOI %in% final_df$doi)),]

n = dim(redo_DF)[1]

# Query pubmed and fetch many results
my_query <- c(paste(redo_DF$DOI[1], "[doi]",sep = ""),
              paste(" OR ",redo_DF$DOI[2:n], "[doi]",sep = ""))



for (h in 1: ceiling(length(my_query)/200)){
  # Starting time: record
  t.start <- Sys.time()
  print(h)
  l= (200*(h-1)+1)
  u= 200*(h)
  
  z= my_query[l:u][!is.na(my_query[l:u])]
  
  my_query_mini = paste(z,collapse=" ")
  !is.na(my_query_mini)
  my_pubmed_ids <-  get_pubmed_ids(my_query_mini)
  
  print("Fetching complete")
  
  # Download by 1000-item batches
  my_batches <- seq(from = 1, to = my_pubmed_ids$Count, by = 200)
  my_abstracts_xml <- lapply(my_batches,  function(i) {
    fetch_pubmed_data(my_pubmed_ids, retmax = 1000, retstart = i)  
  })
  
  print("download XMLs complete")
  
  # Store Pubmed Records as elements of a list
  all_xml <- list()
  for(x in my_abstracts_xml) {
    xx <- articles_to_list(x)
    for(y in xx) {
      all_xml[[(1 + length(all_xml))]] <- y
    }  
  }
  
  print("download all XMLs")
  
  
  # Perform operation (use lapply here, no further parameters)
  
  if(h==1){
    final_redo_DF <- do.call(rbind, lapply(all_xml, article_to_df, 
                                      max_chars = -1, getAuthors = F, getKeywords = T))
  }else{
    
    intermidiate =  do.call(rbind, lapply(all_xml, article_to_df, 
                                          max_chars = -1, getAuthors = F, getKeywords = T))
    final_redo_DF <- rbind(final_redo_DF, intermidiate)
    
  }
  # Final time: record
  t.stop <- Sys.time()
  
  #How long did it take?
  print(t.stop - t.start)
  
}


final_df = rbind(final_df, final_redo_DF)


rm(list =ls()[! ls() %in% c("final_df", "DF")])

# Show an excerpt of the results
final_df = final_df[,-which(names(final_df) %in% c("month", "day", "jabbrv","lastname" , "firstname", "address", "email"))]  

names(final_df)[1] =  "PMID"
names(final_df)[2] =  "DOI"
final_df[,1] = as.numeric(final_df[,1])
test = final_df %>% left_join( DF, by = "DOI")



IPD_MA  =  test %>% select(Title, abstract, keywords, Author, journal, DOI, PMID)

colnames(IPD_MA) = capitalize(colnames(IPD_MA))

IPD_MA$Hyperlink = paste("https://www.ncbi.nlm.nih.gov/pubmed/", final_df$EntrezUID,sep = "")


row.names(final_df) = NULL

final_df = as.data.frame(final_df)
IPD_MA = as.data.frame(IPD_MA)
row.names(IPD_MA) = NULL



write.xlsx2(IPD_MA, "Articles/csv-individual-set_till_2020.xlsx",
            col.names=TRUE, row.names=FALSE, append=FALSE)

write_csv(IPD_MA, "Data-set/Covid_systematic_review.csv")



# Perform operation (use lapply here, no further parameters)

final_df2 <- do.call(rbind, lapply(all_xml, article_to_df, max_chars = -1, getAuthors = F, getKeywords = T))

# Final time: record
t.stop <- Sys.time()

#How long did it take?
print(t.stop - t.start)


rm(list =ls()[! ls() %in% c("IPD.MA","IPD.MA","final_df", "final_df2", "DF", "IPD_MA")])



for(i in which(!is.na(as.numeric(IPD_MA$PMID)))){
  
  IPD_MA[i,]$EntrezUID = IPD_MA[i,]$PMID 
  IPD_MA[i,]$PMID =  paste("PMID :",IPD_MA[i,]$PMID )
  
  
}



names(final_df2)[1] =  "EntrezUID"

final_df2[,1] = as.numeric(final_df2[,1])

final_df2 = final_df2 %>%left_join( DF, by = "EntrezUID")


for( i in 1:dim(final_df2)[1]){
  
  IPD_MA[IPD_MA$EntrezUID == final_df2[i,]$EntrezUID,]$EntrezUID 
  IPD_MA[IPD_MA$EntrezUID == final_df2[i,]$EntrezUID,]$doi =  final_df2[i,]$doi
  IPD_MA[IPD_MA$EntrezUID == final_df2[i,]$EntrezUID,]$`Title of trial` = final_df[i,]$title
  IPD_MA[IPD_MA$EntrezUID == final_df2[i,]$EntrezUID,]$Abstract = final_df2[i,]$abstract
  IPD_MA[IPD_MA$EntrezUID == final_df2[i,]$EntrezUID,]$Year = final_df2[i,]$year
  IPD_MA[IPD_MA$EntrezUID == final_df2[i,]$EntrezUID,]$Journal = final_df2[i,]$journal
  IPD_MA[IPD_MA$EntrezUID == final_df2[i,]$EntrezUID,]$Keywords = final_df2[i,]$keywords
  IPD_MA[IPD_MA$EntrezUID == final_df2[i,]$EntrezUID,]$Authors = final_df2[i,]$Description
  IPD_MA[IPD_MA$EntrezUID == final_df2[i,]$EntrezUID,]$Hyperlink = paste("https://www.ncbi.nlm.nih.gov", final_df2[i,]$URL,sep = "")
  
}



IPD_MA.final =  IPD_MA[,-c(28,29) ]


write.xlsx2(IPD_MA.final, "IPD-MA Cochrane papers/6. Data/IPD_test.xlsx")
rm(list =ls())
IPD_MA = readxl::read_xlsx("IPD-MA Cochrane papers/6. Data/IPD_test.xlsx", sheet= 1)




Abstracts = list()


for(i in 1:4137){
  
  Abstracts[[i]] = unlist(strsplit(IPD.MA[i,]$Abstract, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
  
}


names(Abstracts) = paste( "Total sentences: ",sapply(Abstracts, length ))


Abstracts[[1]][1]

lapply(Abstracts, function(x) length(x))

Check =  matrix(NA, nrow = 4137 , ncol = 5 ,dimnames = list(1:4137, c("trial", "randomised", "randomised clinical", "randomised controlled", "RCT")) )
for(i in 1:4137){
  
  Check[i,] = c("trial", "randomised", "randomised clinical", "randomised controlled", "RCT") %in% unlist(strsplit(IPD.MA[i,]$Abstract, split = " "))
  
}
Check = as_data_frame(Check)
head(Check)
Any_TRUE =  apply(Check, MARGIN = 1, any)
Check$Any_TRUE = Any_TRUE
# If interested in specific information,
# you can subset the dataframe and save the
# desired columns/features
RCTs = IPD.MA[which(Any_TRUE),c("PMID", "Abstract")] 



### Check if Cohort Trials are included

Check =  matrix(NA, nrow = 4137 , ncol = 1 ,dimnames = list(1:4137, c("RCT")) )

for(i in 1:4137){
  
  Check[i,] = c("Children") %in% unlist(strsplit(IPD.MA[i,]$Abstract, split = " "))
  
}

IPD.MA[which(Check),]$Abstract
Children = IPD.MA[which(Check),c("PMID", "Journal", "Title of trial","General Medical Field", "Abstract")]
IPD.MA[which(Check),c("PMID", "Abstract")] %>%
  kable() %>% kable_styling(bootstrap_options = 'striped')

