library(readr)
csv_individual_set <- read_csv("Articles/csv-individual-set.csv")


papers_read =  list.files(path = "Articles/0 Articles Alain/")
papers_read = sort(papers_read[-c(48:50)])

papers_read = gsub(x = papers_read, pattern = " et al., 2015.pdf" , replacement = "")
papers_read = gsub(x = papers_read, pattern = " et al., 2016.pdf" , replacement = "")
papers_read = gsub(x = papers_read, pattern = " et al., 2017.pdf" , replacement = "")
papers_read = gsub(x = papers_read, pattern = " et al., 2018.pdf" , replacement = "")
papers_read = gsub(x = papers_read, pattern = " et al., 2019.pdf" , replacement = "")

papers_read =gsub(x = papers_read, pattern = ". " , replacement = "")

papers_read =gsub(x = papers_read, pattern = "[0-9]*", replacement = "")



papers_read = sort(papers_read)

papers_read[papers_read== ".Wadeal"] = "Wadeal"
papers_read[papers_read== "Leeal."] = "Leeal"


papers_read %in% csv_individual_set$`First Author`

