install.packages("ggplot2")
install.packages("treemap")
install.packages("hrbrthemes")
library(hrbrthemes)
library(treemap)
library(ggplot2)
library(dplyr)

# Dataset loading ####
imdb_raw <- read.csv("D:\\Studia\\JupyterProjects\\WIZ\\imdb.csv", header=TRUE, sep=",")
imdb_raw %>%
  select(Series_Title, Released_Year, Runtime, Genre, IMDB_Rating, Meta_score, Director, Star1, Star2, Star3, Star4, No_of_Votes, Gross) -> imdb

# Plot 1 ####
# Plot 2 ####
imdb %>%
  select(Genre, Gross) -> imdb_2
imdb_2 %>%
  group_by(Genre) %>%
  summarise(Income = sum(Gross)) %>%
  arrange(desc(Income)) %>%
  top_n(25) %>%
      treemap(imdb_2,
          # data
          index="Genre",
          vSize="Income",
          type="index",
          # Main
          title="",
          palette="Dark2",
          # Borders:
          border.col=c("black"),             
          border.lwds=1,                         
          # Labels
          fontsize.labels=0.7,
          fontcolor.labels="white",
          fontface.labels=1,            
          bg.labels=c("transparent"),              
          align.labels=c("left", "top"),                                  
          overlap.labels=0.5,
          inflate.labels=T  
      ) 
imdb %>%
  count(Genre) -> imdb_2b
# Plot 2b ####
imdb_2b %>%
  arrange(desc(Genre)) %>%
  top_n(25) %>%
  treemap(imdb_2b,
          # data
          index="Genre",
          vSize="n",
          type="index",
          # Main
          title="",
          palette="Dark2",
          # Borders:
          border.col=c("black"),             
          border.lwds=1,                         
          # Labels
          fontsize.labels=0.7,
          fontcolor.labels="white",
          fontface.labels=1,            
          bg.labels=c("transparent"),              
          align.labels=c("left", "top"),                                  
          overlap.labels=0.5,
          inflate.labels=T  
  ) 

# Plot 3 ####

# Plot 4 ####
# Prepare data
imdb_3_d <- imdb %>%
  select(Director) %>%
  count(Director) %>%
  arrange(desc(n)) %>%
  top_n(15) %>%
  cbind(Role='Director')
names(imdb_3_d)[names(imdb_3_d) == "n"] <- "Movies"
names(imdb_3_d)[names(imdb_3_d) == "Director"] <- "Name"

imdb_3_s1 <- imdb %>%
  select(Star1) %>%
  count(Star1)
names(imdb_3_s1)[names(imdb_3_s1) == 'Star1'] <- 'Star'

imdb_3_s2 <- imdb %>%
  select(Star2) %>%
  count(Star2)
names(imdb_3_s2)[names(imdb_3_s2) == 'Star2'] <- 'Star'

imdb_3_s3 <- imdb %>%
  select(Star3) %>%
  count(Star3)
names(imdb_3_s3)[names(imdb_3_s3) == 'Star3'] <- 'Star'

imdb_3_s4 <- imdb %>%
  select(Star4) %>%
  count(Star4)
names(imdb_3_s4)[names(imdb_3_s4) == 'Star4'] <- 'Star'

imdb_3_s <- bind_rows(imdb_3_s1, imdb_3_s2, imdb_3_s3, imdb_3_s4) %>%
  group_by(Star) %>%
  summarise_all(funs(sum(., na.rm = TRUE))) %>% 
  arrange(desc(n)) %>%
  top_n(15) %>%
  cbind(Role='Star')
names(imdb_3_s)[names(imdb_3_s) == "n"] <- "Movies"
names(imdb_3_s)[names(imdb_3_s) == "Star"] <- "Name"

imdb_3 <- bind_rows(imdb_3_d, imdb_3_s) %>%
  #group_by(Role) %>%
  #arrange(desc(Movies))
  arrange(Role)
  
# Set a number of 'empty bar' to add at the end of each group
imdb_3 = rbind(imdb_3[1:15,],NA, NA, NA, imdb_3[16:30,], NA, NA, NA)
#imdb_3 <- imdb_3 %>% arrange(Role)
imdb_3$id <- seq(1, nrow(imdb_3))

# Get the name and the y position of each label
label_imdb_3 <- imdb_3
number_of_bar <- nrow(label_imdb_3)
angle <- 90 - 360 * (label_imdb_3$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_imdb_3$hjust <- ifelse( angle < -90, 1, 0)
label_imdb_3$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot
p <- ggplot(imdb_3, aes(x=as.factor(id), y=Movies, fill=Role)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=Movies, fill=Role), stat="identity", alpha=0.5) +
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.

  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(imdb_3$id)-1,4), y = c(5, 10, 15, 20), label = c("5", "10", "15", "20") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  annotate("text", x = rep(17,4), y = c(5, 10, 15, 20), label = c("5", "10", "15", "20") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=Movies, fill=Role), stat="identity", alpha=0.5) +
  ylim(-20,20) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_imdb_3, aes(x=id, y=Movies+5, label=Name, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_imdb_3$angle, inherit.aes = FALSE )

p

# Plot 5 ####
imdb_4 <- imdb %>%
  select(Runtime, IMDB_Rating, Meta_score, No_of_Votes)
#imdb4_year_rating <- imdb_4 %>% group_by(Runtime) %>% summarise(me = mean(IMDB_Rating))
imdb_4 %>% 
  group_by(Runtime) %>% summarise(Avg_IMDB_Rating = mean(IMDB_Rating)) %>%
    ggplot(aes(x = Runtime, y = Avg_IMDB_Rating)) + 
    geom_bar(stat = "identity")

#png(filename="C:/Users/pkopy/OneDrive/Pulpit/WIZ_plot5_2.png", width=1980, height=1080)

imdb_4 %>% 
  group_by(Runtime) %>% summarise(Avg_IMDB_Rating = mean(IMDB_Rating)) %>% arrange(Avg_IMDB_Rating) %>%
    ggplot( aes(x=Runtime, y=Avg_IMDB_Rating) ) +
      geom_segment( aes(x=Runtime ,xend=Runtime, y=0, yend=Avg_IMDB_Rating), color="grey") +
      geom_point(size=3, color="#69b3a2") +
      #coord_flip() +
      theme_ipsum() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position="none",
      ) +
      xlab("Runtime") +
      ylab("Average IMDB Rating")

#dev.off()
