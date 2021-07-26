# Install
install.packages("ggplot2")
install.packages("plotly")
install.packages("dplyr")
library(ggplot2)
library(plotly)
library(dplyr)

# Set working directory
setwd("C:\\Users\\profe\\OneDrive\\Documents\\0 Datasets")

# Unzipping file
file_name <- "social_media.zip"
unzip(file_name)

# Reads file
csv_file<- "Social Media Survey_April 3, 2021_03.13.csv"
social<- read.csv(csv_file)

# Cleaning data
social<-social[,-c(1:17)] # Removing first 17 columns
names<- as.character(as.matrix(social[1, ])) # Identify column names
social<- social[-c(1:2), ] # Removing first two rows, which contained the question & 'import'

names(social)[1:25]<- c("race_ethnicity", "other_race_ethnicity","gender", 
                        "other_gender", "class_year", "fgli_status", 
                        "international_status", "international_country", "usa_state", 
                        "popularity", "regular_apps", "other_regular_apps", 
                        "favorite_apps", "other_favorite_apps", "popular_guess", 
                        "other_popular_guess","time_spent", "posts", 
                        "purpose", "other_purpose", "time_spent_change", 
                        "prepandemic", "other_prepandemic","postpandemic", "other_postpandemic") # Changing column names

levels(social$race_ethnicity)<- c(NA, NA, "Asian", 
                                  "Black", "Latinx/Hispanic", "Middle Eastern/North African", 
                                  "Native American", "Native Hawaiin/Pacific Islander", "Other", 
                                  NA, "White")
levels(social$class_year)<- c(NA, NA, "2021", 
                              "2022", "2023", "2024", 
                              NA, NA)
levels(social$posts)<- c(NA, NA, "About once a day", 
                         "About once a week", "Less often", "Never", 
                         NA, "Once a few weeks", "Several times a day", 
                         "Several times a week")
levels(social$popularity)<- c(NA, NA, "0", "1", "2", "3", "4", NA)

# Data exploration

barplot(table(social$race_ethnicity))
barplot(table(social$gender))
barplot(table(social$class_year))
barplot(table(social$fgli_status))
barplot(table(social$international_status))
barplot(table(social$international_country))
barplot(table(social$usa_state))
barplot(table(social$popularity))
#regular_apps
#favorite_apps
#popular_guess
plot(table(social$time_spent))
barplot(table(social$posts))
#purpose
plot(social$time_spent_change)
#prepandemic
#postpandemic


# GRAPHS

# Change in Usage Time Pie Chart (General)----------------------------------------------------------------------

x = as.numeric(as.character(social$time_spent_change)) # Turn time_spent_change to numeric list
less = x < 0
more = x > 0

decrease_obs = sum(less, na.rm = TRUE)
increase_obs = sum(more, na.rm = TRUE)
no_change_obs = nrow(social[social$time_spent_change == "0",])

labels = c('Increased','Decreased', "No Change")
values = c(increase_obs, decrease_obs, no_change_obs)

pie <- plot_ly(type='pie', labels=labels, values=values, 
               textinfo='label+percent',
               marker = list(colors = c('#C63F3F', '#760000', '#E28073')),
               insidetextorientation='horizontal')

t <- list(family = "helvetica", size = 18) # Creating font

m <- list(
  l = 40,
  r = 40,
  b = 50,
  t = 50,
  pad = 0
) # Creating margins

pie <- pie %>% layout(title="Change in Time Spent on Social Media Since Pandemic", 
                      font=t, titlefont=list(size=30), margin = m) # Applying layout


# Change in Usage Time Pie Chart (Hours/Day)------------------------------------------------------------------

windowsFonts(A = windowsFont("Helvetica"))

hours = na.omit(as.numeric(as.character(social$time_spent_change)))

inc.more.6 = sum(hours > 6)
inc.5.6 = sum(hours == 5 | hours == 6)
inc.3.4 = sum(hours == 3 | hours == 4)
inc.1.2 = sum(hours == 1 | hours == 2)
change.zero = sum(hours == 0)
dec.1.2 = sum(hours == -1 | hours == -2)
dec.3.4 = sum(hours == -3 | hours == -4)
dec.5.6 = sum(hours == -5 | hours == -6)
dec.more.6 = sum(hours < -6)

levels(social$time_spent_change)

change_usage = c(dec.more.6, dec.5.6, dec.3.4, 
                 dec.1.2, change.zero, inc.1.2, 
                 inc.3.4, inc.5.6, inc.more.6)

labels = c('Decreased By >6','Decreased 5-6', "Decreased 3-4", 
           "Decreased 1-2", "No Change", "Increased by 1-2", 
           "Increased by 3-4", "Increased by 5-6", "Increased by >6")

values = change_usage

pie2 <- plot_ly(type='pie', labels=labels, values=values, 
                textinfo='label+percent',
                marker = list(colors = c("#251616", "#FF6961", "#C63F3F", 
                                         "#E28073", "#760000", "#F1D3CF", 
                                         "#E28073", "FF9586", "D84742")),
                insidetextorientation='horizontal', textfont = list(size = 18))

k <- list(
  l = 30,
  r = 30,
  b = 150,
  t = 80,
  pad = 0
) # Creating margins

pie2 <- pie2 %>% layout(title="Change in Time Spent (Hrs/Day) on Social Media Since Pandemic", 
                        font=t, titlefont=list(size=30), margin = k, legend = list(x = 0.78, y = 0.9, font = list(size = 18))) # Applying layout

# Side-to-Side Barplot---------------------------------------------------------------------------


#Proportion (Posts)
posts.prop.barplot = barplot(prop.table(table(social$posts, social$class_year)), 
        legend = TRUE, horiz = T, col=c("#251616", "#FF6961", "#C63F3F", 
                                        "#E28073", "#760000", "#F1D3CF", 
                                        "#E28073"), 
        border = NA, args.legend = list(x = "right", bty = "n"), 
        las = 1, beside=TRUE, family = "A", ylab = "Class Years", 
        font.axis = 2, main = "Class Year Social Media Posting in Hours per Day", cex.main = 2)

#NOT Proportion (Posts)
posts.not.prop.barplot = barplot(table(social$posts, social$class_year), 
        legend = TRUE, horiz = T, col=c("#251616", "#FF6961", "#C63F3F", 
                                        "#E28073", "#760000", "#F1D3CF", 
                                        "#E28073"), 
        border = NA, args.legend = list(x = "right", bty = "n"), 
        las = 1, beside=TRUE, family = "A", ylab = "Class Years", 
        font.axis = 2, main = "Class Year Social Media Posting in Hours per Day", cex.main = 2)

# Side-to-Side Barplot--------------------------------------------------------------------------


#Proportion (Popularity)
pop.prop.barplot = barplot(prop.table(table(social$popularity, social$class_year)), 
                       legend = TRUE, horiz = T, col=c("#251616", "#FF6961", "#C63F3F", 
                                                       "#E28073", "#760000"), 
                       border = NA, args.legend = list(x = "right", bty = "n"), 
                       las = 1, beside=TRUE, family = "A", ylab = "Class Years", 
                       font.axis = 2, main = "Class Year Popularity Self-Rating(s)", cex.main = 2)
#NOT Proportion (Pospularity)
pop.not.prop.barplot = barplot(table(social$popularity, social$class_year), 
                           legend = TRUE, horiz = T, col=c("#251616", "#FF6961", "#C63F3F", 
                                                           "#E28073", "#760000"), 
                           border = NA, args.legend = list(x = "right", bty = "n"), 
                           las = 1, beside=TRUE, family = "A", ylab = "Class Years", 
                           font.axis = 2, main = "Class Year Popularity Self-Rating(s)", cex.main = 2)

# ALL GRAPHS ================================================================================

pie
pie2
posts.prop.barplot
posts.not.prop.barplot
pop.prop.barplot
pop.not.prop.barplot
