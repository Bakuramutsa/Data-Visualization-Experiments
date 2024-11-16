# load libraries
library(ggplot2)
library(dplyr)
library(tidyr)

install.packages("patchwork")
library(ggplot2)
library(patchwork)


#import data in R 
wnba_data <- read.csv("All Game Attendance (2).csv", header = TRUE, sep = ",")
wnba_data

# filter the last decade 
wnba_last_decade <- wnba_data %>%
  filter(Year >= 2014 & Year <= 2024)

##################### Creating the line chart to show the change in attendance over the years

# Attendance summary over the years
attendance_summary <- wnba_last_decade %>%
  group_by(Year) %>%
  summarize(Average_Attendance = mean(Attendance, na.rm = TRUE))

# Creating a line chart for average attendance 
ggplot(attendance_summary, aes(x = Year, y = Average_Attendance)) +
  geom_line(color = "#787276", size = 1) +
  geom_point(color = "#FF6A13", size = 3) + 
  geom_text(aes(label = round(Average_Attendance)), 
            vjust = -1,  
            color = "#FF6A13", 
            size = 4) +  
  labs(title = "WNBA Annual Average Attendance Over Time (Last Decade)",
       x = "Year",
       y = "Average Attendance per Game") +
  scale_x_continuous(breaks = seq(min(attendance_summary$Year), 
                                  max(attendance_summary$Year), 
                                  by = 1)) +  
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

  

################## WNBA with the most attendance in the last decade (Home team and Away Team)
######################
library(dplyr)
library(ggplot2)

# Summarizing attendance when the team is at home
home_attendance <- wnba_last_decade %>%
  group_by(Home.Team) %>%
  summarize(Total_Attendance = sum(Attendance, na.rm = TRUE),
            Average_Attendance = mean(Attendance, na.rm = TRUE)) %>%
  mutate(Team = Home.Team, Type = "Home") %>%
  select(Team, Total_Attendance, Average_Attendance, Type)

# Summarizing attendance when the team is away
away_attendance <- wnba_last_decade %>%
  group_by(Away.Team) %>%
  summarize(Total_Attendance = sum(Attendance, na.rm = TRUE),
            Average_Attendance = mean(Attendance, na.rm = TRUE)) %>%
  mutate(Team = Away.Team, Type = "Away") %>%
  select(Team, Total_Attendance, Average_Attendance, Type)

# Combining home and away attendance data
combined_attendance <- rbind(home_attendance, away_attendance)

# Modifying the Total_Attendance for Home games
combined_attendance <- combined_attendance %>%
  mutate(Total_Attendance = ifelse(Type == "Home", -Total_Attendance, Total_Attendance))

# Ordering by best home attendance
ordered_teams <- home_attendance %>%
  arrange((Total_Attendance)) %>%
  pull(Team)

# Creating a butterfly chart, ordering teams by best home attendance
ggplot(combined_attendance, aes(x = factor(Team, levels = ordered_teams), y = Total_Attendance, fill = Type)) +
  geom_bar(stat = "identity", position = "identity") +  
  coord_flip() +  
  labs(title = "WNBA Teams Popularity Over the Last Decade: Home vs. Away games Attendance",
       x = "Team",
       y = "Total Attendance") +
  scale_fill_manual(values = c("Home" = "#FF6A13", "Away" = "#787276"), 
                    name = "Game Type",
                    labels = c("Away", "Home")) +  
  theme(legend.position = "top") +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top")



########## Attendance by season


# Summary of average attendance by game type and order by average attendance 
attendance_by_game_type <- wnba_last_decade %>%
  group_by(Game.Type) %>%
  summarize(Average_Attendance = mean(Attendance, na.rm = TRUE)) %>%
  arrange(desc(Average_Attendance)) 

# Creating a bar chart 
ggplot(attendance_by_game_type, aes(x = reorder(Game.Type, Average_Attendance), y = Average_Attendance)) +
  geom_bar(stat = "identity", fill = "#FF6A13", width=0.7) +  
  labs(title = "Average WNBA Attendance by Game Type Over the Last Decade",
       x = "Game Type",
       y = "Average Attendance") +
  theme_minimal() +
  coord_flip() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top")


###### TOP 10 cities where WNBA is popular 
# theme(panel.background = element_rect(fill =, color = NA))

# Summarizing total attendance by city
top_cities  <- wnba_last_decade %>%
  group_by(City) %>% 
  summarize(Total_Attendance = sum(Attendance, na.rm = TRUE))%>%
  arrange(desc(Total_Attendance)) %>%
  top_n(10, Total_Attendance)

top_cities

# Creating a bar chart 
ggplot(top_cities, aes(x = reorder(City, Total_Attendance), y = -Total_Attendance)) +  # Use '-' for descending order
  geom_bar(stat = "identity", fill = "#FF6A13", width = 0.5) +  
  labs(title = "WNBA's Popularity: Top 10 Cities by Attendance Over the Last Decade",
       y = "City",
       x = "Total Attendance") +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


        
        
        
      







       







