# Habitat connectivity literature review: Analysis

# Load packages
library(ggplot2)
library(cowplot)

# Color palettes
palette1 <- c("#264653", "#2A9D8F", "#8AB17D", "#E9C46A", "#F4A261", "#E76F51")
palette2 <- c("#264653", "#8AB17D", "#E9C46A", "#E76F51")
palette3 <- c("#264653", "#287271", "#2A9D8F", "#8AB17D", "#E9C46A", 
              "#F4A261", "#E76F51", "#dc2f02")  # "gray"
palette4 <- c("#734f5a", "#264653", "#2a9d8f", "#8AB17D", "#e9c46a", "#f4a261",
              "#e76f51", "#941c2f")
palette5 <- c("#264653", "#2A9D8F", "#8AB17D", "#E9C46A", "#E76F51")

# Import data ----

# papers_filter2 <- read.csv("data/woscc_adjusted-query_filter_annotated.csv", row.names = 1)
data_full <- read.csv("data/filtered_abstracts_annotated.csv", row.names = 1)
nrow(data_full)  # 12,031 papers

# Remove all rows after the top 65
data_full <- data_full[c(1:65),]

summary(data_full)  # inspect data
str(data_full)

# Keep only entries where "Remove == FALSE"
data <- data_full[which(data_full$Remove == FALSE),c(1:29)]
## last 2 columns ("Remove" and "Notes") not needed for analysis
## some entries flagged for removal (did not meet search criteria)
head(data)  # some columns include multiple entries separated by semicolons


# General Info Analysis ----

## Publication year ----

# Frequency table
year <- as.data.frame(table(data$Publication.Year))
year  # no publications from 2015

year[13,] <- NA  # add empty row for 2015
colnames(year) <- c("year", "count")
year$year <- seq(from = 2014, to = 2026)
year$count <- c(year$count[1], 0, year$count[2:12])  # rearrange to include 2015
year$prop <- year$count/50  # proportion of each year in sample
year

# Plot papers per year
ggplot(data = data) +
  geom_bar(aes(x = Publication.Year), fill = "turquoise3") +
  labs(x = "Publication Year", y = "Count") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"))

test <- arima(x = ts(year[,2], start = c(2014, 1), frequency = 1))
test
predict(test, 5)


## Journals ----

unique(data$Journal)  # 27 journals

# Categorize journals by topic
data$Journal.type <- NA  # empty column
for (i in 1:nrow(data)) {
  if (grepl("CONSERVATION", data$Journal[i])) {
    data$Journal.type[i] <- "Conservation Oriented"
  } else if (grepl("LAND", data$Journal[i])) {
    data$Journal.type[i] <- "Landscape Ecology"
  } else if (grepl("FOREST", data$Journal[i])) {
    data$Journal.type[i] <- "Forest Ecology"
  } else if (grepl("ECO", data$Journal[i]) | grepl("DIVERSITY", data$Journal[i])) {
    data$Journal.type[i] <- "General Ecology"
  } else if (grepl("ENVIRONMENT", data$Journal[i])) {
    data$Journal.type[i] <- "Environmental Management"
  } else { data$Journal.type[i] <- "Specific Animal Taxon" }
}

# Frequency table
table(data$Journal)  # across all journals
journal <- as.data.frame(sort(table(data$Journal.type)))  # sort by count
journal$prop <- journal$Freq/50
journal  # majority in general ecology and conservation

# Plot papers per category
fig_journal1 <- 
  ggplot() +
  geom_bar(data = data, 
           aes(x = reorder(Journal.type, Journal.type, length),  # reorder by freq
           fill = Journal.type)) +
  geom_text(data = journal, 
            aes(x = Var1, y = Freq, label = paste(prop*100, "%", sep = "")), 
            size = 6, size.unit = "pt", vjust = -0.7) +
  labs(x = "Journal Type", y = "Count", tag = "A") +
  scale_fill_manual(values = rev(palette1)) +
  scale_x_discrete(labels = scales::label_wrap(13)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", size = 8),
        axis.title.x = element_text(vjust = 0),
        axis.text = element_text(size = 6),
        plot.tag = element_text(size = 10, family = "sans", face = "bold"),
        plot.tag.position = c(0,0.98)) # horizontal, vertical
# ggsave(fig_journal1, file = "figures/connectivity_journals.png",
#        height = 3.63, width = 4.20, units = "in", dpi = 600)  # save figure

# Plot publication year categorized by journal type
fig_journal2 <-
  ggplot(data = data) +
  geom_bar(aes(x = Publication.Year, fill = Journal.type), 
           position = "stack") +
  labs(x = "Publication Year", y = "Count", fill = "Journal Type", tag = "B") +
  scale_fill_manual(values = rev(palette1)) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 8),
        axis.title.x = element_text(vjust = -0.5),
        axis.text = element_text(size = 6),
        legend.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 6),
        legend.key.size = unit(12, units = "points"),
        legend.position = "inside",
        legend.position.inside = c(0.32,0.8),
        plot.tag = element_text(size = 10, family = "sans", face = "bold"),
        plot.tag.position = c(0,0.98)) # horizontal, vertical
plot(fig_journal2)

# Combine plots
FIG3 <- plot_grid(plotlist = c(fig_journal1, fig_journal2), 
                  ncol = 2, rel_widths = c(4,3))
ggsave(FIG3, file = "figures/connectivity_journals_combined.png", 
              height = 3.63, width = 6.86, units = "in", dpi = 600)  # save figure


## Affiliations and Affiliation countries ----
## First author only

unique(data$Affiliations)  # Mostly universities/academic institutions
data[which(data$Affiliation.Country == "The Netherlands"),10] <- "Netherlands"
data$Affiliation.Country <- gsub("United Kingdom", "UK", data$Affiliation.Country)
## replace with name in `map_data("world")$region` for mapping

length(grep(";", data$Affiliation.Type))  # 5 authors with multiple affiliations

# Reorganize to list all affiliations separately
affil <- data.frame(Type = c(data$Affiliation.Type, rep(NA, times = 5)),
                    Country = c(data$Affiliation.Country, rep(NA, times = 5)))
## Separate affiliations by splitting character strings at the "; "
affil[c(51:55),] <- c(sapply(strsplit(affil[grep(";", affil$Type),1], "; "), `[`, 2),
                      sapply(strsplit(affil[grep(";", affil$Type),2], "; "), `[`, 2))
affil[grep(";", affil$Type),
      ] <- c(sapply(strsplit(affil[grep(";", affil$Type),1], "; "), `[`, 1),
             sapply(strsplit(affil[grep(";", affil$Type),2], "; "), `[`, 1))
nrow(affil)  # 55 affiliations in total

# Frequency table across affiliations
affil2 <- data.frame(sort(table(affil$Type)),  # sort by count
                     prop = as.data.frame(sort(table(affil$Type)))$Freq/55)
affil2  # ~91% academic institutions

# Frequency table across countries
affil3 <- data.frame(sort(table(affil$Country)),  # sort by count
                     prop = as.data.frame(sort(table(affil$Country)))$Freq/55)
colnames(affil3)[1] <- "region"
affil3  # ~36% China, ~16% USA

# Plot affiliations by type
fig_affil1 <- 
  ggplot() +
  geom_bar(data = affil, 
           aes(x = reorder(Type, Type, length),  # reorder by freq
           fill = Type)) +
  geom_text(data = affil2, 
            aes(x = Var1, y = Freq, 
                label = paste(round(prop*100, 1), "%", sep = "")), 
            size = 6, size.unit = "pt", vjust = -0.7) +
  labs(x = "Affiliation Type", y = "Publication Count", tag = "A") +
  scale_x_discrete(labels = scales::label_wrap(10)) +
  scale_fill_manual(values = rev(palette2)) +
  theme_classic() +
  theme(legend.position = "none",
        # plot.background = element_blank(),
        axis.title = element_text(face = "bold", size = 8),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(size = 6),
        plot.tag = element_text(size = 10, family = "sans", face = "bold"),
        plot.tag.position = c(0,0.98)) # horizontal, vertical
plot(fig_affil1)
# ggsave(fig_affil1, file = "figures/connectivity_affiliations.png", 
#        height = 3.63, width = 3.63, units = "in", dpi = 600)  # save figure

# Plot publication year categorized by journal type
fig_affil2 <-
  ggplot(data = affil) +
  geom_bar(aes(x = reorder(Country, Country, length), fill = Type), 
           position = "stack") +
  labs(x = "Affiliation Country", y = "Publication Count", 
       fill = "Affiliation Type", tag = "B") +
  scale_fill_manual(values = rev(palette2)) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 8),
        axis.title.x = element_text(vjust = -0.5),
        axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 45, vjust = 0.65),
        legend.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 6),
        legend.key.size = unit(12, units = "points"),
        legend.position = "inside",
        legend.position.inside = c(0.2,0.8),
        # plot.background = element_blank(),
        plot.tag = element_text(size = 10, family = "sans", face = "bold"),
        plot.tag.position = c(0,0.98)) # horizontal, vertical
plot(fig_affil2)

# Plot on world map
world_map <- dplyr::left_join(map_data("world"), affil3, by = "region")
world_map <- world_map[world_map$lat > -63.2131,]

map_affil <-
  ggplot(world_map, aes(x = long, y = lat, group = group, fill = Freq)) +
  geom_polygon(color = "lightgray", linewidth = 0.1) +
  labs(fill = "Publications", tag = "C") +
  # scale_fill_viridis_c(option = "magma", na.value = "gray", name = "Count") +
  scale_fill_gradient(low = "#A0BF96", high = "#264653", na.value = "gray") +
  coord_fixed(1.3) +
  theme_void() +
  theme(legend.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 8),
        legend.key.size = unit(12, units = "points"),
        legend.background = element_rect(fill = "white"),
        legend.position = "inside",
        legend.position.inside = c(0.15,0.35),
        plot.tag = element_text(size = 10, family = "sans", face = "bold"),
        plot.tag.position = c(0.05,0.98),
        plot.background = element_rect(fill = "white"))
map_affil

# Combine plots
FIG4ab <- plot_grid(fig_affil1, fig_affil2, ncol = 2,
                    nrow = 2, rel_widths = c(2,3), rel_heights = c(1,0.1))
plot(FIG4ab)
FIG4 <- 
  plot_grid(FIG4ab, map_affil, nrow = 2, ncol = 1, rel.heights = c(1,2)) +
  theme(plot.background = element_rect(fill = "white"))
FIG4
ggsave(FIG4, file = "figures/connectivity_affil_combined.png", 
       height = 6.86, width = 6.86, units = "in", dpi = 600)  # save figure


# Study System ----

## Focal Scale, Countries, Continents ----

# Frequency table across study scales
scale <- data.frame(sort(table(data$Scale)),  # sort by count
                    prop = as.data.frame(sort(table(data$Scale)))$Freq/50)
levels(scale) <- c("transnational", "national", "regional", "local")
scale  # 50% regional (majority regional/local, likely hard to do large-scale studies)

# Plot affiliations by type
fig_scale <- 
  ggplot(data = scale) +
  geom_col(aes(x = Var1, y = Freq,  # reorder by freq
               fill = Var1)) +
  geom_text(aes(x = Var1, y = Freq, 
                label = paste(round(prop*100, 1), "%", sep = "")), 
            size = 7, size.unit = "pt", vjust = -1) +
  labs(x = "Study Scale", y = "Studies", tag = "A") +
  # scale_x_discrete(labels = scales::label_wrap(10)) +  
  scale_y_continuous(breaks = c(0,5,10,15,20,25),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = palette2) +
  theme_classic() +
  theme(legend.position = "none",
        # plot.background = element_blank(),
        axis.title = element_text(face = "bold", size = 10),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(size = 7),
        plot.tag = element_text(size = 12, family = "sans", face = "bold"),
        plot.tag.position = c(0,0.98)) # horizontal, vertical
plot(fig_scale)

unique(data$Country)  # 5 transnational sites need to be split up
unique(data$Continent)  # 1 transcontinental site

# Adjust typos
data$Country <- gsub("Monacco", "Monaco", data$Country)

# Label transnational studies by their continent
dat.site <- data[,10:18]
dat.site$Country[grep(";", dat.site$Country)]
dat.site$Continent[grep(";", dat.site$Continent)]

# Separate transcontinental study
dat.site2 <- data.frame(Continent = c(data$Continent, "South America"))
dat.site2$Continent <- gsub("North America; South America", 
                            "North America", dat.site2$Continent)

# Frequency table across continents where studies occurred
site1 <- data.frame(sort(table(dat.site2$Continent)),  # sort by count
                    prop.studies = as.data.frame(sort(table(dat.site2$Continent)))$Freq/50,
                    prop.sites = as.data.frame(sort(table(dat.site2$Continent)))$Freq/51)
site1  
## 40% of studies were conducted in Asia
## ~39% of continents in which studies occurred were in Asia

# Plot publication year categorized by journal type
dat.site2$Scale <- c(dat.site$Scale, "transnational")
dat.site2$Scale <- factor(dat.site2$Scale,
                         levels = c("transnational", "national", "regional", "local"))
fig_site <-
  ggplot(data = dat.site2) +
  geom_bar(aes(x = reorder(Continent, Continent, length), fill = Scale), 
           position = "stack") +
  geom_text(data = site1, 
            aes(x = Var1, y = Freq, 
                label = paste(round(prop.studies*100, 1), "%", sep = "")), 
            size = 7, size.unit = "pt", vjust = -1) +
  labs(x = "Continent", y = "Studies", tag = "B") +
  scale_x_discrete(labels = scales::label_wrap(10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = palette2) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 10),
        axis.title.x = element_text(vjust = -0.5),
        axis.text = element_text(size = 7),
        axis.text.x = element_text(vjust = 0.65),
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 7),
        legend.key.size = unit(12, units = "points"),
        legend.position = "inside",
        legend.position.inside = c(0.21,0.8), # c(0.18,0.8),
        # plot.background = element_blank(),
        plot.tag = element_text(size = 12, family = "sans", face = "bold"),
        plot.tag.position = c(0,0.98)) # horizontal, vertical
plot(fig_site)

# Reorganize to list all countries separately
dat.site3 <- c(data$Country[-which(grepl(";", data$Country))], 
               unlist(strsplit(data$Country[grep(";", data$Country)], "; ")))
unique(dat.site3) 
length(dat.site3)
## 77 unique countries of 117 total countries where studies occurred (incl. duplicates)

# Frequency table across countries where studies occurred
site2 <- data.frame(sort(table(dat.site3)),  # sort by count
                    prop.studies = as.data.frame(sort(table(dat.site3)))$Freq/50,
                    prop.sites = as.data.frame(sort(table(dat.site3)))$Freq/117)
site2
## 40% of studies were conducted in China (20/50 papers)
## All studies conducted in Asia took place either only within or included China

# Was the first author affiliated with the country where the study took place?
dat.site$match <- NA
for (m in 1:nrow(dat.site)) {
  # Check first for authors with multiple affiliations
  if (grepl(";", dat.site$Affiliation.Country[m])) {
    split <- unlist(strsplit(dat.site$Affiliation.Country[m], "; "))
    if (grepl(split[1], dat.site$Country[m]) | grepl(split[2], dat.site$Country[m])){
      dat.site$match[m] <- TRUE
    }
  } else if (grepl(dat.site$Affiliation.Country[m], dat.site$Country[m])) {
    dat.site$match[m] <- TRUE
  } else { dat.site$match[m] <- FALSE }
}
dat.site$match[is.na(dat.site$match)] <- TRUE  # "UK" and "United Kingdom"

table(dat.site$match)  # 45/50 (90%) first authors were associated with their study site

# Adjust country names to match `map_data("world")$region`
unique(dat.site3)[-which(unique(dat.site3) %in% map_data("world")$region)]
site2$dat.site3 <- gsub("Czechia", "Czech Republic", site2$dat.site3)
site2$dat.site3 <- gsub("United Kingdom", "UK", site2$dat.site3)
site2$dat.site3 <- gsub("Vatican City", "Vatican", site2$dat.site3)
colnames(site2)[1] <- "region"

# Plot on world map (countries papers included in their study site)
world_map <- dplyr::left_join(world_map[,1:6], site2, by = "region")  # overwrite

map_site <-
  ggplot(world_map, aes(x = long, y = lat, group = group, fill = Freq)) +
  geom_polygon(color = "lightgray", linewidth = 0.1) +
  labs(fill = "Studies", tag = "C") +
  # scale_fill_viridis_c(option = "magma", na.value = "gray", name = "Count") +
  scale_fill_gradient(low = "#A0BF96", high = "#264653", na.value = "gray") +
  coord_fixed(1.3) +
  theme_void() +
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 8),
        legend.key.size = unit(12, units = "points"),
        legend.background = element_rect(fill = "white"),
        legend.position = "inside",
        legend.position.inside = c(0.15,0.35),
        plot.tag = element_text(size = 12, family = "sans", face = "bold"),
        plot.tag.position = c(0.05,0.98),
        plot.background = element_rect(fill = "white"))
map_site  # includes duplicates due to transnational studies

# Combine plots
FIG5ab <- plot_grid(fig_scale, fig_site, ncol = 2,
                    nrow = 2, rel_widths = c(2,3), rel_heights = c(1,0.1))
plot(FIG5ab)
FIG5 <- 
  plot_grid(FIG5ab, map_site, nrow = 2, ncol = 1, rel.heights = c(1,2)) +
  theme(plot.background = element_rect(fill = "white", color = "white"))
FIG5
ggsave(FIG5, file = "figures/connectivity_sites_combinedA.png", 
       height = 6.86, width = 6.86, units = "in", dpi = 600)  # save figure


## Predominant environment ----

# Fix typos and semicolons for biome
data$Biome <- gsub("Mediterranian", "Mediterranean", data$Biome)
data$Biome <- gsub("termperate", "temperate", data$Biome)
data$Biome <- gsub("connifor", "conifer", data$Biome)
data$Biome <- gsub(";", ",", data$Biome)

# Check for study areas spanning multiple biomes
unique(data$Biome[grepl(",", data$Biome)])  # last 2 span multiple biomes
## separate with semicolon
data$Biome[which(data$Biome == unique(data$Biome[grepl(",", data$Biome)])[5])] <-
  "temperate broadleaf and mixed forests; temperate conifer forests; temperate grasslands, savannas, and shrublands; Mediterranean forests, woodlands, and scrub"
data$Biome[which(data$Biome == unique(data$Biome[grepl(",", data$Biome)])[4])] <-
  "tropical and subtropical moist broadleaf forests; montane grasslands and shrublands"

# Reorganize to list all countries separately
biome <- c(data$Biome[-which(grepl(";", data$Biome))], 
           unlist(strsplit(data$Biome[grep(";", data$Biome)], "; ")))

unique(biome) 
length(biome)
## 9 unique biomes of 54 total biomes where studies occurred (incl. duplicates)

# Frequency table across biomes where studies occurred
biome2 <- data.frame(sort(table(biome)),  # sort by count
                    prop.studies = as.data.frame(sort(table(biome)))$Freq/50,
                    prop.sites = as.data.frame(sort(table(biome)))$Freq/54)
biome2  # mostly in forests

# What types of ecosystems were the studies investigating?
unique(data$Environment)  # only terrestrial and freshwater
## also some avian species, but the focus was on landscape structure

# Frequency table across environments
biome3 <- data.frame(sort(table(data$Environment)),  # sort by count
                     prop = as.data.frame(sort(table(data$Environment)))$Freq/50)
biome3  # majority terrestrial (88%)

# Plot biomes
fig_biome1 <-
  ggplot(data = biome2) +
  geom_col(aes(x = Freq, y = biome),  # reorder by freq
           fill = "gray35") +
  geom_text(aes(x = Freq, y = biome, 
                label = paste(round(prop.studies*100, 1), "%", sep = "")), 
            size = 7, size.unit = "pt", hjust = -0.3) +
  labs(y = "Biome", x = "Study Sites", tag = "A") +
  scale_y_discrete(labels = scales::label_wrap(25)) +
  scale_x_continuous(breaks = c(0,5,10,15,20),
                     expand = expansion(mult = c(0, 0.1))) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", size = 10),
        axis.title.x = element_text(vjust = -0.2),
        axis.title.y = element_text(vjust = 1),
        axis.text = element_text(size = 6),
        plot.tag = element_text(size = 12, family = "sans", face = "bold"),
        plot.tag.position = c(0,0.98)) # horizontal, vertical
plot(fig_biome1)

# Plot ecosystem types
fig_biome2 <- 
  ggplot(data = biome3) +
  geom_col(aes(x = Var1, y = Freq),  # reorder by freq
           fill = c("#84C5BD", "#3E6843")) +
  geom_text(aes(x = Var1, y = Freq, 
                label = paste(round(prop*100, 1), "%", sep = "")), 
            size = 7, size.unit = "pt", vjust = -1) +
  labs(x = "Ecosystem", y = "Studies", tag = "B") +
  scale_y_continuous(breaks = c(0,10,20,30,40,50),
                     expand = expansion(mult = c(0, 0.18))) +
  scale_fill_manual(values = palette2) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", size = 8),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(size = 7),
        plot.tag = element_text(size = 12, family = "sans", face = "bold"),
        plot.tag.position = c(0,0.98)) # horizontal, vertical
plot(fig_biome2)

# Combine plots
ggdraw(fig_biome1) +
  draw_plot(fig_biome2, x = 0.64, y = 0.16, width = 0.3, height = 0.48)

# Combine with focal sites and map
FIG6ab <- plot_grid(fig_biome1, fig_site, ncol = 2,
                  nrow = 2, rel_widths = c(3,2), rel_heights = c(1,0.1))
plot(FIG6ab)
FIG6 <-
  plot_grid(FIG6ab, map_site, nrow = 2, ncol = 1, rel.heights = c(1,2)) +
  theme(plot.background = element_rect(fill = "white", color = "white"))
FIG6
ggsave(FIG6, file = "figures/connectivity_sites_combinedB.png", 
       height = 6.86, width = 7.86, units = "in", dpi = 600)  # save figure


## Target taxa ----

# How many studies had focal species?
unique(data$Target.Taxa)
data$Target.Taxa[is.na(data$Target.Taxa)] <- "none"
taxa <- data.frame(sort(table(data$Target.Taxa)),  # sort by count
                    prop = as.data.frame(sort(table(data$Target.Taxa)))$Freq/50)
t(taxa)  # majority of studies were considering focal species (96%)
## more multispecies studies (26/50, 52%)
## one study mentioned 69 bird and 9 plant species, but did not list them (structural)

# Check if the orders match the taxa
for (i in 1:nrow(data)) {
  if (grepl(";", data$Order[i])){
    species <- unlist(strsplit(data$Specific.Taxon[i], "; "))
    orders <- unlist(strsplit(data$Order[i], "; "))
    print(paste(i, rownames(data)[i], length(species), length(orders), sep = ","))
  } else if (grepl(";", data$Order[i]) == FALSE) {
    print(paste(i, rownames(data)[i], 1, 1, sep = ","))
  } else {print(i, rownames(data)[i], NA, NA, sep = ",")}
}  # some only have general groups (not specific species)

# Reorganize to list all species and their orders separately
taxa1 <- data.frame(Taxa = c(data$Specific.Taxon[-which(grepl(";", data$Specific.Taxon))], 
                             unlist(strsplit(
                               data$Specific.Taxon[grep(";", 
                                                        data$Specific.Taxon)], "; "))),

                    Order = c(data$Order[-which(grepl(";", data$Order))],
                              unlist(strsplit(data$Order[grep(";", data$Order)], "; "))))
# Remove NAs
taxa1 <- taxa1[!is.na(taxa1$Taxa),]
unique(taxa1$Taxa)
length(taxa1$Taxa)  # 364 unique taxa of 395 total (for studies with taxa)

# Frequency table of taxa
taxa2 <- data.frame(sort(table(taxa1$Taxa)),  # sort by count
                    prop = as.data.frame(sort(table(taxa1$Taxa)))$Freq/395,
                    prop.studies = as.data.frame(sort(table(taxa1$Taxa)))$Freq/50)
tail(taxa2, 3)  # top 3 most common taxa studied in our sample (3 studies each)
## Yellow-breasted bunting (Aves, Passeriformes), 
## African bush/savanna elephant (Mammalia, Proboscidea),
## Wild pig/boar (Mammalia, Artiodactyla)

unique(taxa1$Order)
length(taxa1$Order)  # 68 unique Orders of 395 total (for studies with taxa)

# Frequency table of taxonomic orders
taxa3 <- data.frame(sort(table(taxa1$Order)),  # sort by count
                    prop = as.data.frame(sort(table(taxa1$Order)))$Freq/395)
tail(taxa3, 10)  # Orders with >10 focal taxa

# Group all orders with only 1 mention together
taxa4 <- taxa3[which(taxa3$Freq > 3),]
taxa4 <- rbind(taxa4, data.frame(Var1 = c("=1", "=2", "=3"), 
                                 Freq = c(sum(taxa3$Freq[which(taxa3$Freq == 1)]),
                                          sum(taxa3$Freq[which(taxa3$Freq == 2)]),
                                          sum(taxa3$Freq[which(taxa3$Freq == 3)])),
                                 prop = c(sum(taxa3$prop[which(taxa3$Freq == 1)]),
                                          sum(taxa3$prop[which(taxa3$Freq == 2)]),
                                          sum(taxa3$prop[which(taxa3$Freq == 3)]))))
taxa4$Class <- c("Eudicots", "Mammalia", "Monocots", "Actinopterygii", "Aves",
                 "Mammalia", "Aves", "Actinopterygii", "Insecta", "Aves", "Mammalia",
                 "Aves", "Aves", "Aves", "Amphibia", "Mammalia", "Mammalia", "Aves",
                 "Mammalia", "Reptilia", "Amphibia", "Mammalia", "Aves", NA, NA, NA)
taxa4  # prop = proportion of total study species from all papers in each Order

# Plot orders
fig_orders <-
  ggplot(data = taxa4) +
  geom_col(aes(x = Freq, y = Var1, fill = Class)) + #fill = reorder(Class, Class, length))) +  # group by class/clade
  geom_text(aes(x = Freq, y = Var1, 
                label = paste(round(prop*100, 1), "%", sep = "")), 
            size = 7, size.unit = "pt", hjust = -0.3) +
  labs(y = "Taxonomic Order", x = "Study Taxa Count", fill = "Taxonomic Class/Clade") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c(palette4, "gray")) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 10),
        axis.title.x = element_text(vjust = -0.3),
        axis.title.y = element_text(vjust = 1.1),
        axis.text = element_text(size = 7),
        # plot.tag = element_text(size = 12, family = "sans", face = "bold"),
        # plot.tag.position = c(0,0.98), # horizontal, vertical
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 8),
        legend.key.size = unit(13, units = "points"),
        legend.position = "inside",
        legend.position.inside = c(0.82,0.25))
plot(fig_orders)
ggsave(fig_orders, file = "figures/connectivity_taxa_order.png", 
       height = 5, width = 6.86, units = "in", dpi = 600)  # save figure


# Methods Used ----

## Data types and Connectivity ----

# Reorganize to list all data types separately
datcol <- c(data$Data[-which(grepl(";", data$Data))], 
            unlist(strsplit(data$Data[grep(";", data$Data)], "; ")))

# Frequency table of data types used
datcol2 <- data.frame(sort(table(datcol)),  # sort by count
                      prop.studies = as.data.frame(sort(table(datcol)))$Freq/50)
datcol2  # env.survey = environmental survey (environ data not from remote sensing)
## almost all studies use remote sensing data (96%)
## very little use of tracking/camera-trap data, mostly reported occurrence

# Reorganize to list all connectivity types separately
habcon <- c(data$Connectivity.Type[-which(grepl(";", data$Connectivity.Type))], 
           unlist(strsplit(data$Connectivity.Type[grep(";", 
                                                       data$Connectivity.Type)], "; ")))

# Frequency table of connectivity types
habcon2 <- data.frame(sort(table(habcon)),  # sort by count
                     prop.studies = as.data.frame(sort(table(habcon)))$Freq/50)
habcon2  # most studies look at functional connectivity (78%)
## some studies estimate more than 1 form of connectivity


## Main methods ----

# Reorganize to list all data types separately
methods <- c(data$Methods[-which(grepl(";", data$Methods))], 
             unlist(strsplit(data$Methods[grep(";", data$Methods)], "; ")))

# What methods were used for habitat connectivity and suitability modeling
methods2 <- data.frame(sort(table(methods)),  # sort by count
                       prop.studies = as.data.frame(sort(table(methods)))$Freq/50)
methods2  # 15 unique methods, resistance-based methods dominate

# Categorize methods
methods2$group <- NA
methods2$group[which(methods2$methods == "RCM" | 
                 methods2$methods == "LC" | 
                 methods2$methods == "SAMC" |
                 methods2$methods == "RK")] <- "Resistance-based"
methods2$group[which(methods2$methods == "RSF" | 
                       methods2$methods == "SDM")] <- "Habitat suitability"
methods2$group[which(methods2$methods == "GT" | 
                       methods2$methods == "HC")] <- "Graph theory"
methods2$group[which(methods2$methods == "DAPC" | 
                       methods2$methods == "sPCA" | 
                       methods2$methods == "pRDA")] <- "PCA-based"
methods2$group[which(methods2$methods == "GD")] <- "Genetic"
methods2$group[which(is.na(methods2$group))] <- "Other"
levels(methods2$group) <- c("Resistance-based", "Habitat suitability", "Graph theory",
                            "PCA-based", "Genetic", "Other")
## SPOM: stochastic patch occupancy model (metapopulation MC model of patch occupancy)
## MSPA: morphological spatial pattern analysis (mathematical binary image processing)
## ABM: agent-based model (simulated human movement for dynamic "landscape" resistance)
## GD: methods for calculating genetic distance and population structure from GD

# Plot methods
fig_method <- 
  ggplot(data = methods2) +
  geom_col(aes(x = methods, y = Freq, fill = group)) + # group by type of method
  geom_text(aes(x = methods, y = Freq, 
                label = paste(round(prop.studies*100, 1), "%", sep = "")), 
            size = 7, size.unit = "pt", vjust = -0.7) +
  labs(x = "Method", y = "Studies", fill = "Category") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)), 
                     breaks = c(0,5,10,15,20,25,30,35)) +
  scale_fill_manual(values = palette1) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 10),
        axis.title.x = element_text(vjust = -0.3),
        axis.title.y = element_text(vjust = 1.1),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        # plot.tag = element_text(size = 12, family = "sans", face = "bold"),
        # plot.tag.position = c(0,0.98), # horizontal, vertical
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 8),
        legend.key.size = unit(14, units = "points"),
        legend.position = "inside",
        legend.position.inside = c(0.2,0.8))
plot(fig_method)
ggsave(fig_method, file = "figures/connectivity_methods.png", 
       height = 5, width = 5.5, units = "in", dpi = 600)  # save figure

# Did the study involve habitat suitability modeling?
suit <- data.frame(table(data$Habitat.Suitability.Quality),
                   prop.studies = as.data.frame(table(
                     data$Habitat.Suitability.Quality))$Freq/50)
suit 

# Were expert opinion or subjective thresholds used?
eo <- data.frame(table(data$Expert.Opinion),
                 prop.studies = as.data.frame(table(data$Expert.Opinion))$Freq/50)
eo

# Reorganize to list all data types separately
tool <- c(data$Programs.Tools[-which(grepl(";", data$Programs.Tools))], 
             unlist(strsplit(data$Programs.Tools[grep(";", data$Programs.Tools)], "; ")))
tool <- gsub("Ecospat", "ecospat", tool)
tool <- gsub("gDistance", "gdistance", tool)
tool <- gsub("MaxEnt", "Maxent", tool)
tool <- gsub("Python", "python", tool)

# Remove program languages
tool <- tool[-which(tool == "R" | tool == "python" | tool == "C++")]
unique(tool)  # 36 unique software/programs

# What were the main software programs used? 
tool2 <- data.frame(sort(table(tool)),  # sort by count
                       prop.studies = as.data.frame(sort(table(tool)))$Freq/50)

# Categorize methods
tool2$group <- NA
tool2$group[which(tool2$tool == "Circuitscape" | 
                    tool2$tool == "Linkage Mapper" | 
                    tool2$tool == "Omniscape" |
                    tool2$tool == "ResistanceGA" |
                    tool2$tool == "samc" |
                    tool2$tool == "gdistance" |
                    tool2$tool == "UNICOR" |
                    tool2$tool == "ArcGIS")] <- "Resistance-based"
tool2$group[which(tool2$tool == "Maxent" | 
                    tool2$tool == "InVEST HQ" | 
                    tool2$tool == "biomod2" |
                    tool2$tool == "randomForest" |
                    tool2$tool == "ecospat" |
                    tool2$tool == "dismo" |
                    tool2$tool == "mgcv")] <- "Habitat suitability/quality"
tool2$group[which(tool2$tool == "Conefor" | 
                    tool2$tool == "Graphab" |
                    tool2$tool == "Networkx" |
                    tool2$tool == "igraph")] <- "Graph theory"
tool2$group[which(tool2$tool == "ecodist")] <- "PCA/Dissimilarity analysis"
tool2$group[which(tool2$tool == "STRUCTURE" |
                    tool2$tool == "fastSTRUCTURE" |
                    tool2$tool == "Geneland" |
                    tool2$tool == "GenePop" |
                    tool2$tool == "adegenet"|
                    tool2$tool == "GenAlEx" |
                    tool2$tool == "SPAGeDI" |
                    tool2$tool == "polysat" |
                    tool2$tool == "FSTAT" |
                    tool2$tool == "dartR" |
                    tool2$tool == "Arlequin")] <- "Genetic"
tool2$group[which(is.na(tool2$group))] <- "Other"  
levels(tool2$group) <- c("Resistance-based", "Habitat suitability/quality", 
                          "Graph theory", "PCA/Dissimilarity analysis", 
                          "Genetic", "Other")
## "Other" group is mostly spatial pattern analysis/zoning, Marxan for prioritization
## ArcGIS classified as resistance-based because it was generally used for least-cost

tool2  # Circuitscape and derivatives mostly, Maxent widely used for SDMs

# Plot tools and software
fig_tool <- 
  ggplot(data = tool2) +
  geom_col(aes(x = Freq, y = tool, fill = group)) + # group by type of method
  geom_text(aes(x = Freq, y = tool, 
                label = paste(round(prop.studies*100, 1), "%", sep = "")), 
            size = 7, size.unit = "pt", hjust = -0.3) +
  labs(y = "Software Program", x = "Studies", fill = "Category") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = palette1) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 10),
        axis.title.x = element_text(vjust = -0.3),
        axis.title.y = element_text(vjust = 1.1),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 8),
        # plot.tag = element_text(size = 12, family = "sans", face = "bold"),
        # plot.tag.position = c(0,0.98), # horizontal, vertical
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 8),
        legend.key.size = unit(14, units = "points"),
        legend.position = "inside",
        legend.position.inside = c(0.8,0.23))
plot(fig_tool)
ggsave(fig_tool, file = "figures/connectivity_tools.png", 
       height = 5, width = 6.86, units = "in", dpi = 600)  # save figure


# Conservation Application ----

## Primary threats identified for the study system ----

# Reorganize to list all threats separately
threat <- c(data$Threats[-which(grepl(";", data$Threats))], 
            unlist(strsplit(data$Threats[grep(";", data$Threats)], "; ")))
threat <- gsub("small pop", "small population", threat)

# Frequency table of threats identified across studies
threat2 <- data.frame(sort(table(threat)),  # sort by count
                      prop.studies = as.data.frame(sort(table(threat)))$Freq/50)
threat2  # majority highlighted habitat loss and fragmentation (hand-in-hand)
## 8 main threats

# Were these suggested to be human-induced or a direct consequence of human activity?
threat3 <- data.frame(sort(table(data$Human.Induced)),  # sort by count
                      prop.studies = as.data.frame(sort(
                        table(data$Human.Induced)))$Freq/50)
threat3  # mostly yes (92%), no indication of any studies refuting human contribution

# Plot threats
fig_threat <- 
  ggplot(data = threat2) +
  geom_col(aes(x = threat, y = Freq), fill = "gray35") + 
  geom_text(aes(x = threat, y = Freq, 
                label = paste(round(prop.studies*100, 1), "%", sep = "")), 
            size = 6, size.unit = "pt", vjust = -0.7) +
  labs(x = "Major Threats", y = "Studies", tag = "B") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  scale_x_discrete(label = scales::label_wrap(10)) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 10),
        axis.title.x = element_text(vjust = -0.2),
        axis.title.y = element_text(vjust = 1.1),
        axis.text.x = element_text(size = 5.5),
        axis.text.y = element_text(size = 6),
        plot.tag = element_text(size = 10, family = "sans", face = "bold"),
        plot.tag.position = c(0,0.98))
plot(fig_threat)
ggsave(fig_threat, file = "figures/connectivity_threats.png", 
       height = 3.83, width = 5, units = "in", dpi = 600)  # save figure


## Suggested Conservation Strategies ----

# Did the authors suggest conservation recommendations specific to their study system?
cons <- data.frame(table(data$Cons.Strategy), 
                   prop.studies = as.data.frame(table(data$Cons.Strategy))$Freq/50)
cons  # 92% of articles gave specific strategies for conservation management

# What were the main management strategies?
# Reorganize to list all strategies separately
cons1 <- c(data$Suggested.Strategy[-which(grepl(";", data$Suggested.Strategy))], 
            unlist(strsplit(data$Suggested.Strategy[grep(";", 
                                                data$Suggested.Strategy)], "; ")))
# Remove NAs and fix restoration typo
cons1 <- cons1[!is.na(cons1)]
cons1 <- gsub("retoration", "restoration", cons1)
unique(cons1)  # 8 main suggested conservation strategies

# Frequency table of strategies identified across studies
cons2 <- data.frame(sort(table(cons1)),  # sort by count
                      prop.studies = as.data.frame(sort(table(cons1)))$Freq/50)
cons2  # as expected, most gave recommendations for protection/restoration and corridors

# Plot conservation strategies
fig_cons <- 
  ggplot(data = cons2) +
  geom_col(aes(y = cons1, x = Freq), fill = "gray35") + 
  geom_text(aes(y = cons1, x = Freq, 
                label = paste(round(prop.studies*100, 1), "%", sep = "")), 
            size = 7, size.unit = "pt", hjust = -0.3) +
  labs(y = "Conservation Strategy", x = "Studies", tag = "A") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12)), 
                     breaks = c(0,5,10,15,20,25,30,35)) +
  scale_y_discrete(label = scales::label_wrap(10)) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 10),
        axis.title.x = element_text(vjust = -0.3),
        axis.title.y = element_text(vjust = 1.1),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        plot.tag = element_text(size = 10, family = "sans", face = "bold"),
        plot.tag.position = c(0,0.98))
plot(fig_cons)
ggsave(fig_cons, file = "figures/connectivity_conservation.png", 
       height = 4.5, width = 6.86, units = "in", dpi = 600)  # save figure

# Combine plots
FIG8 <- ggdraw(fig_cons) +
  draw_plot(fig_threat, x = 0.44, y = 0.11, width = 0.55, height = 0.46)
ggsave(FIG8, file = "figures/connectivity_conservation_combined.png", 
       height = 5, width = 6.86, units = "in", dpi = 600)  # save figure

# How many studies considered potential legal implications or specific policies?
legal <- data.frame(table(data$Legal.Policy), 
                      prop.studies = as.data.frame(table(data$Legal.Policy))$Freq/50)
legal  # about half each, though leaning towards "TRUE"


