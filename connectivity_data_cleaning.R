# Habitat connectivity literature review: Data cleaning

# Load packages
library(ggplot2)

# Import data
papers_all <- read.csv("data/woscc_adjusted-query_all.csv")  # all papers from query
papers_filter <- read.csv("data/woscc_adjusted-query_filter_all.csv")  # date filtered
## removed papers from irrelevant WOSCC categories (listed in supplementary materials)

# Papers per publication year (all time)
range(papers_all$Publication.Year)  # 1986-2027
length(unique(papers_all$Publication.Year))  # 41 years
freq_all <- table(papers_all$Publication.Year)
freq_all <- as.data.frame(freq_all)  # convert to dataframe for easier handling
freq_all[,1] <- as.numeric(1986:2026)  # add 1987 (no publications)
freq_all[,2] <- c(2, 0, freq_all[c(2:40),2])  # added a filler of 0 for 1987
papers_all <- papers_all[-which(papers_all$Publication.Year == 2027),]
## removed 2027 (there may still be upcoming publications)

# Plot publications per year
plot(x = c(1986,1988:2026), y = freq_all$Freq, type = "l",
     xlab = "Publication Year", ylab = "Publication Count")

# Papers per publication year (2014-2026), preliminary filter for relevance
range(papers_filter$Publication.Year)  # 2014-2027

# WOS filtering by date missed some 2027 papers
papers_filter <- papers_filter[-which(papers_filter$Publication.Year == 2027),]
range(papers_filter$Publication.Year)  # 2014-2026
length(unique(papers_filter$Publication.Year))  # 13 years
freq_filter <- as.data.frame(table(papers_filter$Publication.Year))

# # Plot publications per year
# plot(x = c(2014:2026), y = freq_filter$Freq, type = "l",
#      xlab = "Publication Year", ylab = "Publication Count")
# 
# plot(x = c(1986,1988:2026), y = freq_all$Freq, type = "l",
#      xlab = "Publication Year", ylab = "Publication Count")
# lines(x = c(2014:2026), y = freq_filter$Freq, col = "red")

ggplot() +
  geom_bar(data = papers_all, aes(x = Publication.Year)) +
  geom_bar(data = papers_filter, aes(x = Publication.Year), fill = "turquoise3") +
  # geom_line(data = freq_all, aes(x = Var1, y = Freq), alpha = 0.5) +
  labs(x = "Publication Year", y = "Publication Count") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"))
# ggsave(file = "figures/connectivity_publication_year.png",
#        height = 3.63, width = 3.63, units = "in", dpi = 600)  # save figure


# Keep only research articles
## remove book chapters, proceedings papers, data papers, retracted publications
unique(papers_filter$Document.Type)
papers_filter2 <- papers_filter[which(papers_filter$Document.Type != "Article; Book Chapter" &
                                      papers_filter$Document.Type != "Article; Proceedings Paper" &
                                      papers_filter$Document.Type != "Article; Data Paper" &
                                      papers_filter$Document.Type != "Article; Retracted Publication"),]
unique(papers_filter2$Document.Type)
nrow(papers_filter2)  # 12,910 papers

# Check for non-English papers
unique(papers_filter2$Language)
1- (nrow(papers_filter2[which(papers_filter2$Language == "English"),]) / nrow(papers_filter2))
## Non-English papers only comprise ~ 0.01 of the dataset

# Remove non-English papers (cannot determine relevance due to language barrier)
papers_filter2 <- papers_filter2[which(papers_filter2$Language == "English"),]
nrow(papers_filter2)  # 12,782 papers

# Remove non-ecology/non-conservation related papers (mostly medical)
## filter by journal title
unique(papers_filter2$Source.Title)
## "CHEMOSPHERE", "EGYPTIAN JOURNAL OF MEDICAL HUMAN GENETICS", "ATMOSPHERIC POLLUTION RESEARCH"
papers_filter2 <- papers_filter2[which(papers_filter2$Source.Title != "CHEMOSPHERE" &
                                       papers_filter2$Source.Title != "EGYPTIAN JOURNAL OF MEDICAL HUMAN GENETICS" &
                                       papers_filter2$Source.Title != "ATMOSPHERIC POLLUTION RESEARCH"),]
nrow(papers_filter2)  # 12,778

# Save abstracts and specific columns separately
abstracts <- papers_filter2[,c(2,47,9,22,10,48,49,24)]

# Remove articles that do not explicitly state key search terms in abstract
terms <- c("conservation", "connectivity", "corridor", "protected area", 
           "landscape resistance", "fragmentation")

abstracts <- abstracts[Reduce(`|`, lapply(terms, grepl, x = abstracts$Abstract)),]
papers_filter2 <- papers_filter2[Reduce(`|`, lapply(terms, grepl, x = papers_filter2$Abstract)),]

nrow(abstracts)  # 12,031

# Save filtered results as csv files
write.csv(abstracts, file = "data/filtered_abstracts.csv")
write.csv(papers_filter2, file = "data/woscc_adjusted-query_filter_annotated.csv")
## To be manually filtered and annotated for data curation

