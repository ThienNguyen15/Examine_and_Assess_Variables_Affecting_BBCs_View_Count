library("ggplot2")
library("car")
library("dplyr")
library("readr")
youtube<-read.csv('C:\\thien\\bbc.csv')
youtube["licensed_content"][is.na(youtube["licensed_content"])]<- 0
youtube <-(youtube [, c(9,11,13,15,16,17,18,20)])
sum(is.na(youtube))
youtube<- na.omit(youtube)
sum(is.na(youtube))
freq_table <- table(youtube$video_category_label)
freq_df <- as.data.frame(freq_table)
names(freq_df) <- c("label", "count")
freq_df <- freq_df[order(-freq_df$count),]
head(freq_df, 15)
youtube <- youtube[youtube$video_category_label %in% c("Entertainment", "Comedy", "Music", "Pets & Animals", "Science & Technology", "News & Politics" ), ]
youtube<-youtube[youtube$view_count<=742369,]
# Changing 6 string names of the categories to an integer type to use ANOVA
youtube$label_id[youtube$video_category_label == "Entertainment"] <- 1
youtube$label_id[youtube$video_category_label == "Comedy"] <- 2
youtube$label_id[youtube$video_category_label == "Music"] <- 3
youtube$label_id[youtube$video_category_label == "Pets & Animals"] <- 4
youtube$label_id[youtube$video_category_label == "Science & Technology"] <- 5
youtube$label_id[youtube$video_category_label == "News & Politics"] <- 6
mean_view <- aggregate(cbind(label_id, view_count) ~ video_category_label, data = youtube, FUN = mean)
mean_view <- mean_view[order(mean_view$label_id),]
head(mean_view, 6)
# Changing 2 string names of the definition to an integer type to use ANOVA
youtube$definition[youtube$definition == "hd"] <- 0
youtube$definition[youtube$definition == "sd"] <- 1
cate = youtube[, c('label_id')]
def = youtube[, c('definition')]
licensed = youtube[, c('licensed_content')]
view = youtube[, c('view_count')]
# Use oneway.test() to perform a One-way ANOVA
aonva1 <- oneway.test(view_count ~ licensed, data = youtube, var.equal = TRUE)
# Print the result
print(aonva1)
# Assuming your data is stored in a data frame called "youtube"
# Use oneway.test() to perform a Welch's ANOVA
aonva2 <- oneway.test(view_count ~ cate, data = youtube, var.equal = FALSE)
# Print the result
print(aonva2)
# Assuming your data is stored in a data frame called "youtube"
# Use oneway.test() to perform a Welch's ANOVA
aonva3 <- oneway.test(view_count ~ def, data = youtube, var.equal = FALSE)
# Print the result
print(aonva3)
# Affected factor ~ influencing factor
kruskal.test(view ~ cate, data = youtube)
# Affected factor ~ influencing factor
kruskal.test(view ~ def, data = youtube)
# Affected factor ~ influencing factor
kruskal.test(view ~ licensed, data = youtube)









#install.packages("rcompanion")
#library (rcompanion)
#epsilonSquared ( x = youtube$view, g = youtube$label_id)
#epsilonSquared ( x = youtube$view, g = youtube$definition)
#epsilonSquared ( x = youtube$view, g = youtube$licensed)
#install.packages("FSA")
#library (FSA)

#dunnTest(view ~ def, data = youtube, method="bonferroni") error
