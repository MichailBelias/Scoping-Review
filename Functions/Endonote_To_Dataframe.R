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
Alain <- as.data.frame(read_xlsx("Alain search results/Scoping Review Alain.xlsx"),row.names = NULL )

Alain


### Omit the duplicated 


Michael <- read_xlsx("ShinyApp/Mixalis.xlsx")
Michael$PMID =  as.numeric(Michael$PMID)

Michael =Michael[-which(duplicated(Michael$DOI)),]
Michael =Michael[-which(duplicated(Michael$Title)),]




Common =  Michael[which(Michael$DOI %in% Alain$DOI),]


write.xlsx(Not_common2, "ShinyApp/Maroeska.xlsx")

Not_common1=  Alain[-which(Alain$Title %in% Michael$Title),]
Not_common2=  Michael[-which(Michael$Title %in% Alain$Title),]


which(Not_common1$DOI %in% Not_common2$DOI)



Alain_Included =  Alain[which((Alain$`Included (yes/no)` == "Yes")),]
Alain_Excluded =  Alain[which((Alain$`Included (yes/no)` == "No")),]


Common =  Michael[which(Michael$Title %in% Alain$Title),]
In_Alain_and_In_Michael =  Alain[which(Alain$Title %in% Michael$Title),]
In_Michael_and_In_Alain =  Michael[which(Michael$Title %in% Alain$Title),]

which(duplicated(In_Alain_and_In_Michael$Title))
which(duplicated(In_Michael_and_In_Alain$Title))



Not_common =  Michael[-which(Michael$Title %in% Alain$Title),]




Common_Alain =  Alain[which(Alain$Title %in% Michael$Title),]
In_Alain_Not_In_Michael =  Alain[-which(Alain$Title %in% Michael$Title),]


write.xlsx(In_Alain_Not_In_Michael, "Alain search results/In_Alain_Not_In_Michael.xlsx")

Common_Included_citations =  Michael[which(Michael$PMID %in% Alain_Included$PMID),]
Common_Exluded_citations =  Michael[which(Michael$Title %in% Alain_Excluded$Title),]




Michael[which(!(Michael$Title %in% Alain_Excluded$Title)),]
