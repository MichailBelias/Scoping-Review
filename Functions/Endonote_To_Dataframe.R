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

#### Load Alain's search results
library(readr)
Alain <- read_csv("Alain search results/Scoping Review Alain.csv")

#### 639 results 
### Check if any DOI is missing


DF =  Alain[which(is.na(Alain$DOI)),]








### Omit the duplicated 
DF = unique(Alain)
DF =  Alain[-which(duplicated(Alain$DOI)),]
DF1 =  Alain[which(duplicated(Alain$Title)),]





n = dim(DF)[1]

# Query pubmed and fetch many results
my_query <- c(paste(DF$Title[1], "[ti]",sep = ""),
              paste(" OR ",DF$Title[2:n], "[ti]",sep = ""))




for (h in 1: ceiling(length(my_query)/n)){
  # Starting time: record
  t.start <- Sys.time()
  print(h)
  l= (n*(h-1)+1)
  u= n*(h)
  
  z= my_query[l:u][!is.na(my_query[l:u])]
  
  my_query_mini = paste(z,collapse=" ")
  !is.na(my_query_mini)
  my_pubmed_ids <-  get_pubmed_ids(my_query_mini)
  
  print("Fetching complete")
  
  # Download by 1000-item batches
  my_batches <- seq(from = 1, to = my_pubmed_ids$Count, by = n)
  my_abstracts_xml <- fetch_pubmed_data(my_pubmed_ids)
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








Michael <- read_csv("ShinyApp/csv-individual-set_till_2020.csv")



Alain_Included =  Alain[which((Alain$`Included (yes/no)` == "Yes")),]
Alain_Excluded =  Alain[which((Alain$`Included (yes/no)` == "No")),]


Common =  Michael[which(Michael$Title %in% Alain$Title),]
Not_common =  Michael[-which(Michael$Title %in% Alain$Title),]



Common_Alain =  Alain[which(Alain$Title %in% Michael$Title),]
In_Alain_NotIn_Michael =  Alain_Included[-which(Alain_Included$Title %in% Michael$Title),]




Common_Included_citations =  Michael[which(Michael$PMID %in% Alain_Included$PMID),]
Common_Exluded_citations =  Michael[which(Michael$Title %in% Alain_Excluded$Title),]




Michael[which(!(Michael$Title %in% Alain_Excluded$Title)),]
