library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))

#setwd("C:\Users\Edutab User\Documents\IFERB-Data")

library(ggplot2)
library(forcats)
library(tidyr)
library(mudata2)
library(tidyverse)
library(openxlsx)
library("xlsx")
library(dplyr)
library(readxl)
library(plotly)
library(leaflet)
library(scales)

Level1_Assessment <- read_excel("Iferb_data.xlsx",sheet = "Level1_Assessment")
Level2_Assessment <- read_excel("Iferb_data.xlsx",sheet = "Level2_Assessment")
Teacher_Assessment <- read_excel("Iferb_data.xlsx",sheet = "Teacher_survey")
################################################################################

# Level 1 processing

level1_freq <- as.data.frame(Level1_Assessment %>%
  group_by(School, Gender) %>%
  summarise(Total = n()))
write.csv (level1_freq,"level1_freq.csv")

level2_freq <- as.data.frame(Level2_Assessment %>%
                               group_by(School, Gender) %>%
                               summarise(Total = n()))
write.csv (level2_freq,"level2_freq.csv")


All_combined <- Level1_Assessment %>%
  select(Gender,School,MathQ1,MathQ2,MathQ3,MathQ4,MathQ5,MathQ6,MathQ7,MathQ8,
         MathQ9,MathQ10,LitQ1,LitQ2,LitQ3,LitQ4,LitQ5,SciQ1,SciQ2,SciQ3,SciQ4,
         SciQ5,SciQ6,SciQ7)

All_combined <- All_combined %>%
  pivot_longer (!c(Gender, School), names_to = "Question", values_to = "Score")

All_combined <- All_combined %>%
  mutate(Broad_categories = case_when(Question == "MathQ8" ~ "Conceptual",
                                       Question == "MathQ5" ~ "Skill",
                                       Question == "MathQ3" ~ "Knowledge",
                                       Question == "MathQ10" ~ "Skill",
                                       Question == "MathQ1" ~ "Knowledge",
                                       Question == "MathQ2" ~ "Knowledge",
                                       Question == "MathQ9" ~ "Conceptual",
                                       Question == "MathQ6" ~ "Knowledge",
                                       Question == "MathQ7" ~ "Conceptual",
                                       Question == "MathQ4" ~ "Knowledge",
                                       Question == "SciQ1" ~ "Knowledge",
                                       Question == "SciQ2" ~ "Skill",
                                       Question == "SciQ3" ~ "Skill",
                                       Question == "SciQ4" ~ "Skill",
                                       Question == "SciQ5" ~ "Conceptual",
                                       Question == "SciQ6" ~ "Conceptual",
                                       Question == "SciQ7" ~ "Conceptual",
                                       Question == "LitQ1" ~ "Knowledge",
                                       Question == "LitQ2" ~ "Knowledge",
                                       Question == "LitQ3" ~ "Knowledge",
                                       Question == "LitQ4" ~ "Knowledge",
                                       Question == "LitQ5" ~ "Skill"))
                                      


All_combined <- All_combined %>%
  mutate(Out_of = as.numeric(case_when(Question == "MathQ8" ~ "1.5",
                                       Question == "MathQ5" ~ "2",
                                       Question == "MathQ3" ~ "1.5",
                                       Question == "MathQ10" ~ "2.5",
                                       Question == "MathQ1" ~ "1",
                                       Question == "MathQ2" ~ "1.5",
                                       Question == "MathQ9" ~ "1",
                                       Question == "MathQ6" ~ "1.5",
                                       Question == "MathQ7" ~ "1.5",
                                       Question == "MathQ4" ~ "2",
                                       Question == "SciQ1" ~ "1.5",
                                       Question == "SciQ2" ~ "3",
                                       Question == "SciQ3" ~ "2.5",
                                       Question == "SciQ4" ~ "1.5",
                                       Question == "SciQ5" ~ "1.5",
                                       Question == "SciQ6" ~ "1",
                                       Question == "SciQ7" ~ "2",
                                       Question == "LitQ1" ~ "3.5",
                                       Question == "LitQ2" ~ "1",
                                       Question == "LitQ3" ~ "1",
                                       Question == "LitQ4" ~ "1",
                                       Question == "LitQ5" ~ "1")))
                                       


All_combined <- All_combined %>%
  mutate(Category = case_when(Question == "MathQ8" ~ "Numeracy",
                              Question == "MathQ5" ~ "Numeracy",
                              Question == "MathQ3" ~ "Numeracy",
                              Question == "MathQ10" ~ "Numeracy",
                              Question == "MathQ1" ~ "Numeracy",
                              Question == "MathQ2" ~ "Numeracy",
                              Question == "MathQ9" ~ "Numeracy",
                              Question == "MathQ6" ~ "Numeracy",
                              Question == "MathQ7" ~ "Numeracy",
                              Question == "MathQ4" ~ "Numeracy",
                              Question == "SciQ1" ~ "World_around_us",
                              Question == "SciQ2" ~ "World_around_us",
                              Question == "SciQ3" ~ "World_around_us",
                              Question == "SciQ4" ~ "World_around_us",
                              Question == "SciQ5" ~ "World_around_us",
                              Question == "SciQ6" ~ "World_around_us",
                              Question == "SciQ7" ~ "World_around_us",
                              Question == "LitQ1" ~ "Literacy",
                              Question == "LitQ2" ~ "Literacy",
                              Question == "LitQ3" ~ "Literacy",
                              Question == "LitQ4" ~ "Literacy",
                              Question == "LitQ5" ~ "Literacy"))
                              




All_combined <- All_combined %>%
  mutate(Percentage = Score/Out_of*100)

All_combined$Percentage <- round(All_combined$Percentage, digits = 2)

#con <- All_combined %>%
  #filter(Percentage > 100)

#as.data.frame(All_combined)
#write.csv(All_combined, "All_questions.csv")

################################################################################

# Level 1 explorations
ggplot(Level1_Assessment, aes(y = Total_Score, x = Gender, fill = Gender)) +
  geom_boxplot() +
  geom_point(size = 1.5) +
  scale_fill_manual(values = c("lavenderblush","darkturquoise")) +
  theme_light() +
  labs(y = "Total Score",
       x = "School",
       title = "Level 1 Assessment:Score comparison with respect to Gender") +
  scale_y_continuous(breaks=seq(0,30,5)) +
  theme(axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
      axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
      axis.title.x = element_text(colour="black", size = 15,face = "bold"),
      axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
      legend.title = element_text(color = "black", size = 15,face = "bold"),
      legend.text = element_text(color = "black", size = 15,face = "bold"),
      plot.title = element_text(face = "bold",hjust = 0.5))


ggplot(Level1_Assessment, aes(y = Total_Score, x = Gender, fill = School)) +
  geom_boxplot(width = .5) +
  scale_fill_manual(values = c("gold","forestgreen")) +
  theme_bw() +
  labs(y = "Total Score",
       x = "Gender",
       title = "Level 1 Assessment: Score comparison with respect to gender and school") +
  scale_y_continuous(breaks=seq(5,35,3)) +
  theme(axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

ggplot(Level1_Assessment, aes(y = Total_Score, x = School , fill = Gender)) +
  geom_boxplot(width = 0.3) +
  scale_fill_manual(values = c("lavenderblush","darkturquoise")) +
  theme_bw() +
  labs(y = "Score",
       x = "School",
       title = "Level 1 Assessment: Score comparison with respect to gender and school") +
  scale_y_continuous(breaks=seq(0,30,5)) +
  theme(axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

################################################################################  

# Level 1_Maths

All_combined %>%
  filter(Category == "Numeracy") %>%
  group_by(Gender) %>%
  summarise (math_mean1 = mean(Percentage)) %>%
  ggplot(aes(y = math_mean1, x = Gender, fill = Gender)) +
  geom_bar(position="dodge", stat="identity",width = 0.3) +
  scale_fill_manual(values = c("lightsalmon2","darkturquoise")) +
  labs(y = "Math_mean",
       x = "Gender",
       title = "Level 1 Assessment: Math mean score by Gender") +
  theme_classic() +
  coord_flip() +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

All_combined %>%
  filter(Category == "Numeracy") %>%
  group_by(School) %>%
  summarise (math_mean1 = mean(Percentage)) %>%
  ggplot(aes(y = math_mean1, x = School, fill = School)) +
  geom_bar(position="dodge", stat="identity",width = 0.3) +
  scale_fill_manual(values = c("gold","forestgreen")) +
  theme_classic() +
  coord_flip() +
  labs(y = "Math_mean",
       x = "School",
       title = "Level 1 Assessment: Math mean score by School") +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

sharon <- All_combined %>%
  filter(Category == "Numeracy") %>%
  group_by(Question) %>%
  summarise(mean_percentage = mean(Percentage))

as.data.frame(sharon)
write.csv(sharon, "Numeracy perfomance.csv")


# level 1 Science
 All_combined %>%
  filter(Category == "World_around_us") %>%
  group_by(Gender) %>%
  summarise (science_mean1 = mean(Percentage)) %>%
  ggplot(aes(y = science_mean1, x = Gender, fill = Gender)) +
  geom_bar(position="dodge", stat="identity",width = 0.3) +
  scale_fill_manual(values = c("lightsalmon2","darkturquoise")) +
  labs(y = "science_mean",
       x = "Gender",
       title = "Level 1 Assessment: science mean score by Gender") +
  theme_classic() +
  coord_flip() +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

All_combined %>%
  filter(Category == "World_around_us") %>%
  group_by(School) %>%
  summarise (science_mean1 = mean(Percentage)) %>%
  ggplot(aes(y = science_mean1, x = School, fill = School)) +
  geom_bar(position="dodge", stat="identity",width = 0.3) +
  scale_fill_manual(values = c("gold","forestgreen")) +
  theme_classic() +
  coord_flip() +
  labs(y = "science_mean",
       x = "School",
       title = "Level 1 Assessment: Science mean score by School") +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

sharon <- All_combined %>%
  filter(Category == "World_around_us") %>%
  group_by(Question) %>%
  summarise(mean_percentage = mean(Percentage))

as.data.frame(sharon)
write.csv(sharon, "World_around_us perfomance.csv")


#Level 1_Literacy

All_combined %>%
  filter(Category == "Literacy") %>%
  group_by(Gender) %>%
  summarise (literacy_mean1 = mean(Percentage)) %>%
  ggplot(aes(y = literacy_mean1, x = Gender, fill = Gender)) +
  geom_bar(position="dodge", stat="identity",width = 0.3) +
  scale_fill_manual(values = c("lightsalmon2","darkturquoise")) +
  labs(y = "literacy_mean",
       x = "Gender",
       title = "Level 1 Assessment: literacy mean score by Gender") +
  theme_classic() +
  coord_flip() +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

All_combined %>%
  filter(Category == "Literacy") %>%
  group_by(School) %>%
  summarise (literacy_mean1 = mean(Percentage)) %>%
  ggplot(aes(y = literacy_mean1, x = School, fill = School)) +
  geom_bar(position="dodge", stat="identity",width = 0.3) +
  scale_fill_manual(values = c("gold","forestgreen")) +
  theme_classic() +
  coord_flip() +
  labs(y = "literacy_mean",
       x = "School",
       title = "Level 1 Assessment: Literacy mean score by School") +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

sharon <- All_combined %>%
  filter(Category == "Literacy") %>%
  group_by(Question) %>%
  summarise(mean_percentage = mean(Percentage))

as.data.frame(sharon)
write.csv(sharon, "Literacy perfomance.csv")


# mean skill
Level1_Assessment %>%
  group_by(Gender) %>%
  summarise(mean_skill = mean(SkillsQ1)) %>%
  ggplot(aes(y = mean_skill, x = Gender, fill = Gender)) +
  scale_fill_manual(values = c("lightsalmon2","darkturquoise")) +
  geom_col(width = 0.2) +
  labs(title = "Level 1 Assessment: Mean Skill by Gender") +
  theme_classic() +
  scale_y_continuous(breaks=seq(0,2,0.2)) +
  theme(axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

Level1_Assessment %>%
  group_by(School) %>%
  summarise(mean_skill = mean(SkillsQ1)) %>%
  ggplot(aes(y = mean_skill, x = School, fill = School)) +
  scale_fill_manual(values = c("gold","forestgreen")) +
  geom_col(width = 0.2) +
  labs(title = "Level 1 Assessment: Mean Skill by School") +
  theme_classic() +
  scale_y_continuous(breaks=seq(0,2,0.2)) +
  theme(axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

#general subject perfomance level 1
All_combined %>%
  group_by (School) %>%
  summarise (mean_school = mean(Percentage))




ggplot(All_combined, aes(x = Category, y = Percentage, fill = Category)) +
  geom_boxplot() +
  facet_wrap(~Gender)

All_combined %>%
  group_by(Category) %>%
  summarise(mn = mean(Percentage)) %>%
  ggplot(aes(x = "", y = mn, fill = Category))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") + 
  scale_fill_brewer("Blues") +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = mn/3 + c(0, cumsum(mn)[-length(mn)]), 
                label = percent(mn/100)), size=5) +
  theme_void() +
  labs(title = "Level 1: General mean perfomance",
       y ="",
       x = "",
       fill = "Category") +
  theme(axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

All_combined %>%
  group_by(Gender,Category) %>%
  summarise(mean_percentage = mean(Percentage)) %>%
  ggplot(aes(x = Gender, y = mean_percentage,fill = Gender)) +
  geom_col(width = 0.4) +
  facet_wrap(~Category) +
  scale_fill_manual(values = c("lightsalmon2","darkturquoise")) +
  theme_bw() +
  labs(y = "Mean percentage",
       x = "Gender",
       title = "Level 1 Assessment: Mean percentage perfomance per subject") +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(legend.position = "none",
    axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

All_combined %>%
  group_by(Question, Category) %>%
  summarise(mean_percentage = mean(Percentage)) %>%
  ggplot(aes(y = mean_percentage, x = Question, fill = Category)) +
  geom_col() +
  theme_bw() +
  labs(y = "Mean percentage",
       x = "Question",
       title = "Level 1 Assessment: Mean percentage perfomance per Category question") +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(axis.text.x = element_text(color = "black", size = 10,angle = 90, face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))


All_combined$Broad_categories <- fct_relevel(All_combined$Broad_categories,
                                            "Knowledge","Skill","Conceptual")


# mutate(Variety = fct_reorder(Variety, desc(times_mentioned))) %>%



All_combined %>%
  group_by(Broad_categories) %>%
  summarise(mean_percentage = mean(Percentage)) %>%
mutate(Broad_categories = fct_reorder(Broad_categories, desc(mean_percentage))) %>%
  ggplot(aes(y = mean_percentage, x = Broad_categories, fill = Broad_categories)) +
  geom_col(width = 0.3) +
  scale_fill_manual(values = c("lightsalmon2","darkturquoise","gold","forestgreen")) +
  theme_bw() +
  labs(y = "Mean percentage",
       x = "Category",
       title = "Level 1 Assessment: Mean percentage perfomance per Category") +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black", size = 10,angle = 0, face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))



All_combined %>%
  group_by(Broad_categories,Question) %>%
  summarise(mean_percentage = mean(Percentage)) %>%
  mutate(Question = fct_reorder(Question, desc(mean_percentage))) %>%
  ggplot(aes(y = mean_percentage, x = Question, fill = Broad_categories)) +
  geom_col() +
  scale_fill_manual(values = c("gold","lightsalmon2","forestgreen","darkturquoise")) +
  theme_bw() +
  labs(y = "Mean percentage",
       x = "Question",
       title = "Level 1 Assessment: Mean percentage perfomance per Category") +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(axis.text.x = element_text(color = "black", size = 10,angle = 90, face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))


All_combined %>%
  group_by(Question, Category, School) %>%
  summarise(mean_percentage = mean(Percentage)) %>%
  ggplot(aes(y = mean_percentage, x = Question, fill = Category)) +
  facet_wrap(~School, nrow = 2) +
  geom_col(width = 0.5) +
  theme_bw() +
  labs(y = "Mean percentage",
       x = "Question",
       title = "Level 1 Assessment: Mean percentage perfomance per Category question") +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(axis.text.x = element_text(color = "black", size = 10,angle = 90, face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

################################################################################

# Level 2 processing

All_combined2 <- Level2_Assessment %>%
  select(Gender,School,MathQ1,MathQ2,MathQ3,MathQ4,MathQ5i,MathQ5ii,MathQ6,
         MathQ7,MathQ8,MathQ9,MathQ10a,MathQ10b,LitQ1,LitQ2,LitQ3i,LitQ3ii,
         LitQ4,LitQ5,SciQ1,SciQ2,SciQ3,SciQ4,SciQ5,SciQ6,SciQ7,SkillsQ1)

All_combined2 <- All_combined2 %>%
  pivot_longer (!c(Gender, School), names_to = "Question", values_to = "Score")

All_combined2 <- All_combined2 %>%
  mutate(Out_of = as.numeric(case_when(Question == "MathQ8" ~ "2",
                                       Question == "MathQ5i" ~ "1",
                                       Question == "MathQ5ii" ~ "1",
                                       Question == "MathQ3" ~ "1",
                                       Question == "MathQ10a" ~ "0.5",
                                       Question == "MathQ10b" ~ "0.5",
                                       Question == "MathQ1" ~ "1",
                                       Question == "MathQ2" ~ "3",
                                       Question == "MathQ9" ~ "2.5",
                                       Question == "MathQ6" ~ "1",
                                       Question == "MathQ7" ~ "1.5",
                                       Question == "MathQ4" ~ "2.5",
                                       Question == "SciQ1" ~ "1",
                                       Question == "SciQ2" ~ "1",
                                       Question == "SciQ3" ~ "1.5",
                                       Question == "SciQ4" ~ "2",
                                       Question == "SciQ5" ~ "2",
                                       Question == "SciQ6" ~ "1",
                                       Question == "SciQ7" ~ "1.5",
                                       Question == "LitQ1" ~ "1",
                                       Question == "LitQ2" ~ "2",
                                       Question == "LitQ3i" ~ "0.5",
                                       Question == "LitQ3ii" ~ "0.5",
                                       Question == "LitQ4" ~ "1",
                                       Question == "LitQ5" ~ "1",
                                       Question == "SkillsQ1" ~ "1")))

All_combined2 <- All_combined2 %>%
  mutate(Category = case_when(Question == "MathQ8" ~ "Numeracy",
                              Question == "MathQ5i" ~ "Numeracy",
                              Question == "MathQ5ii" ~ "Numeracy",
                              Question == "MathQ3" ~ "Numeracy",
                              Question == "MathQ10a" ~ "Numeracy",
                              Question == "MathQ10b" ~ "Numeracy",
                              Question == "MathQ1" ~ "Numeracy",
                              Question == "MathQ2" ~ "Numeracy",
                              Question == "MathQ9" ~ "Numeracy",
                              Question == "MathQ6" ~ "Numeracy",
                              Question == "MathQ7" ~ "Numeracy",
                              Question == "MathQ4" ~ "Numeracy",
                              Question == "SciQ1" ~ "World_around_us",
                              Question == "SciQ2" ~ "World_around_us",
                              Question == "SciQ3" ~ "World_around_us",
                              Question == "SciQ4" ~ "World_around_us",
                              Question == "SciQ5" ~ "World_around_us",
                              Question == "SciQ6" ~ "World_around_us",
                              Question == "SciQ7" ~ "World_around_us",
                              Question == "LitQ1" ~ "Literacy",
                              Question == "LitQ2" ~ "Literacy",
                              Question == "LitQ3i" ~ "Literacy",
                              Question == "LitQ3ii" ~ "Literacy",
                              Question == "LitQ4" ~ "Literacy",
                              Question == "LitQ5" ~ "Literacy",
                              Question == "SkillsQ1" ~ "21_c_skill"))


All_combined2 <- All_combined2 %>%
  mutate(More_categories = case_when(Question == "MathQ8" ~ "Conceptual",
                                     Question == "MathQ5i" ~ "Knowledge",
                                     Question == "MathQ5ii" ~ "Knowledge",
                                     Question == "MathQ3" ~ "Skill",
                                     Question == "MathQ10a" ~ "Conceptual",
                                     Question == "MathQ10b" ~ "Conceptual",
                                     Question == "MathQ1" ~ "Knowledge",
                                     Question == "MathQ2" ~ "Knowledge",
                                     Question == "MathQ9" ~ "Skill",
                                     Question == "MathQ6" ~ "Skill",
                                     Question == "MathQ7" ~ "Skill",
                                     Question == "MathQ4" ~ "Skill",
                                     Question == "SciQ1" ~ "Knowledge",
                                     Question == "SciQ2" ~ "Skill",
                                     Question == "SciQ3" ~ "Skill",
                                     Question == "SciQ4" ~ "Skill",
                                     Question == "SciQ5" ~ "Skill",
                                     Question == "SciQ6" ~ "Skill",
                                     Question == "SciQ7" ~ "Conceptual",
                                     Question == "LitQ1" ~ "Knowledge",
                                     Question == "LitQ2" ~ "Knowledge",
                                     Question == "LitQ3i" ~ "Knowledge",
                                     Question == "LitQ3ii" ~ "Knowledge",
                                     Question == "LitQ4" ~ "Skill",
                                     Question == "LitQ5" ~ "Conceptual",
                                     Question == "SkillsQ1" ~ "21_c_skill"))


All_combined2 <- All_combined2 %>%
  mutate(Percentage = Score/Out_of*100)

All_combined2$Percentage <- round(All_combined2$Percentage, digits = 2)

con <- All_combined2 %>%
filter(Percentage > 100)

#as.data.frame(All_combined)
#write.csv(All_combined, "All_questions.csv")

##############################################################################

# Level 2 explorations
ggplot(Level2_Assessment, aes(y = Total_Score, x = Gender, fill = Gender)) +
  geom_boxplot() +
  geom_point(size = 1.5) +
  scale_fill_manual(values = c("lavenderblush","darkturquoise")) +
  theme_light() +
  labs(y = "Total Score",
       x = "Gender",
       title = "Level 2 Assessment:Score comparison with respect to Gender") +
  scale_y_continuous(breaks=seq(0,30,5)) +
  theme(legend.position = "none",
    axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))


ggplot(Level2_Assessment, aes(y = Total_Score, x = Gender, fill = School)) +
  geom_boxplot(width = .5) +
  scale_fill_manual(values = c("gold","forestgreen")) +
  theme_bw() +
  labs(y = "Total Score",
       x = "Gender",
       title = "Level 2 Assessment: Score comparison with respect to gender and school") +
  scale_y_continuous(breaks=seq(5,35,3)) +
  theme(axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

ggplot(Level2_Assessment, aes(y = Total_Score, x = School , fill = Gender)) +
  geom_boxplot(width = 0.3) +
  scale_fill_manual(values = c("lavenderblush","darkturquoise")) +
  theme_bw() +
  labs(y = "Score",
       x = "School",
       title = "Level 2 Assessment: Score comparison with respect to gender and school") +
  scale_y_continuous(breaks=seq (0,30,5)) +
  theme(axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

# numeracy level 2
All_combined2 %>%
  filter(Category == "Numeracy") %>%
  group_by(Gender) %>%
  summarise (math_mean2 = mean(Percentage)) %>%
  ggplot(aes(y = math_mean2, x = Gender, fill = Gender)) +
  geom_bar(position="dodge", stat="identity",width = 0.3) +
  scale_fill_manual(values = c("lightsalmon2","darkturquoise")) +
  labs(y = "Math_mean",
       x = "Gender",
       title = "Level 2 Assessment: Math mean score by Gender") +
  theme_classic() +
  coord_flip() +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(legend.position = "none",
    axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

All_combined2 %>%
  filter(Category == "Numeracy") %>%
  group_by(School) %>%
  summarise (math_mean2 = mean(Percentage)) %>%
  ggplot(aes(y = math_mean2, x = School, fill = School)) +
  geom_bar(position="dodge", stat="identity",width = 0.3) +
  scale_fill_manual(values = c("gold","forestgreen")) +
  theme_classic() +
  coord_flip() +
  labs(y = "Math_mean",
       x = "School",
       title = "Level 2 Assessment: Math mean score by School") +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(legend.position = "none",
    axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

sharon2 <- All_combined2 %>%
  filter(Category == "Numeracy") %>%
  group_by(Question) %>%
  summarise(mean_percentage = mean(Percentage))

as.data.frame(sharon2)
write.csv(sharon2, "Numeracy perfomance 2.csv")


# level 2 Science
All_combined2 %>%
  filter(Category == "World_around_us") %>%
  group_by(Gender) %>%
  summarise (science_mean2 = mean(Percentage)) %>%
  ggplot(aes(y = science_mean2, x = Gender, fill = Gender)) +
  geom_bar(position="dodge", stat="identity",width = 0.3) +
  scale_fill_manual(values = c("lightsalmon2","darkturquoise")) +
  labs(y = "science_mean",
       x = "Gender",
       title = "Level 2 Assessment: science mean score by Gender") +
  theme_classic() +
  coord_flip() +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

All_combined2 %>%
  filter(Category == "World_around_us") %>%
  group_by(School) %>%
  summarise (science_mean2 = mean(Percentage)) %>%
  ggplot(aes(y = science_mean2, x = School, fill = School)) +
  geom_bar(position="dodge", stat="identity",width = 0.3) +
  scale_fill_manual(values = c("gold","forestgreen")) +
  theme_classic() +
  coord_flip() +
  labs(y = "science_mean",
       x = "School",
       title = "Level 2 Assessment: Science mean score by School") +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

sharon2 <- All_combined2 %>%
  filter(Category == "World_around_us") %>%
  group_by(Question) %>%
  summarise(mean_percentage = mean(Percentage))

as.data.frame(sharon2)
write.csv(sharon2, "World_around_us perfomance 2.csv")


# Level 2 Literacy

All_combined2 %>%
  filter(Category == "Literacy") %>%
  group_by(Gender) %>%
  summarise (literacy_mean2 = mean(Percentage)) %>%
  ggplot(aes(y = literacy_mean2, x = Gender, fill = Gender)) +
  geom_bar(position="dodge", stat="identity",width = 0.3) +
  scale_fill_manual(values = c("lightsalmon2","darkturquoise")) +
  labs(y = "literacy_mean",
       x = "Gender",
       title = "Level 2 Assessment: literacy mean score by Gender") +
  theme_classic() +
  coord_flip() +
  scale_y_continuous(breaks=seq(0,30,5)) +
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

All_combined2 %>%
  filter(Category == "Literacy") %>%
  group_by(School) %>%
  summarise (literacy_mean2 = mean(Percentage)) %>%
  ggplot(aes(y = literacy_mean2, x = School, fill = School)) +
  geom_bar(position="dodge", stat="identity",width = 0.3) +
  scale_fill_manual(values = c("gold","forestgreen")) +
  theme_classic() +
  coord_flip() +
  labs(y = "literacy_mean",
       x = "School",
       title = "Level 2 Assessment: Literacy mean score by School") +
  scale_y_continuous(breaks=seq(0,30,5)) +
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

sharon2 <- All_combined2 %>%
  filter(Category == "Literacy") %>%
  group_by(Question) %>%
  summarise(mean_percentage = mean(Percentage))

as.data.frame(sharon2)
write.csv(sharon2, "Literacy perfomance 2.csv")


# mean skill 2
Level2_Assessment %>%
  group_by(Gender) %>%
  summarise(mean_skill = mean(SkillsQ1)) %>%
  ggplot(aes(y = mean_skill, x = Gender, fill = Gender)) +
  scale_fill_manual(values = c("lightsalmon2","darkturquoise")) +
  geom_col(width = 0.2) +
  labs(title = "Level 2 Assessment: Mean Skill by Gender") +
  theme_classic() +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

Level2_Assessment %>%
  group_by(School) %>%
  summarise(mean_skill = mean(SkillsQ1)) %>%
  ggplot(aes(y = mean_skill, x = School, fill = School)) +
  scale_fill_manual(values = c("gold","forestgreen")) +
  geom_col(width = 0.2) +
  labs(title = "Level 2 Assessment: Mean Skill by School") +
  theme_classic() +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  theme(legend.position = "none",
    axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

#general subject perfomance

ggplot(All_combined2, aes(x = Category, y = Percentage, fill = Category)) +
  geom_boxplot() +
  facet_wrap(~Gender)

All_combined2 %>%
  group_by(School) %>%
  summarise(mn = mean(Percentage))


 # ggplot(aes(x = "", y = mn, fill = Category))+
  #geom_bar(width = 1, stat = "identity") +
  #coord_polar("y") + 
  #scale_fill_brewer("Blues") +
  #theme(axis.text.x=element_blank()) +
  #geom_text(aes(y = mn/3 + c(0, cumsum(mn)[-length(mn)]), 
                #label = percent(mn/100)), size = 5) +
  #theme_void() +
  #labs(title = "Level 2: General mean perfomance",
       #y ="",
       #x = "",
       #fill = "Category") +
 # theme(axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        #axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        #axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        #legend.title = element_blank(),
        #legend.text = element_text(color = "black", size = 15,face = "bold"),
       # plot.title = element_text(face = "bold",hjust = 0.5))

All_combined2 %>%
  group_by(Gender,Category) %>%
  summarise(mean_percentage = mean(Percentage)) %>%
  ggplot(aes(x = Gender, y = mean_percentage,fill = Gender)) +
  geom_col(width = 0.4) +
  facet_wrap(~Category) +
  scale_fill_manual(values = c("gold","forestgreen")) +
  theme_bw() +
  labs(y = "Mean percentage",
       x = "Gender",
       title = "Level 2 Assessment: Mean percentage perfomance per subject") +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black", size = 15,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

All_combined2 %>%
  group_by(Question, Category) %>%
  summarise(mean_percentage = mean(Percentage)) %>%
  ggplot(aes(y = mean_percentage, x = Question, fill = Category)) +
  geom_col() +
  theme_bw() +
  labs(y = "Mean percentage",
       x = "Question",
       title = "Level 2 Assessment: Mean percentage perfomance per Category question") +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(axis.text.x = element_text(color = "black", size = 10,angle = 90,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

All_combined2 %>%
  group_by(Question, Category, Gender) %>%
  summarise(mean_percentage = mean(Percentage)) %>%
  ggplot(aes(y = mean_percentage, x = Question, fill = Category)) +
  facet_wrap(~Gender, nrow = 2) +
  geom_col() +
  theme_bw() +
  labs(y = "Mean percentage",
       x = "Question",
       title = "Level 2 Assessment: Mean percentage perfomance per Category question") +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(axis.text.x = element_text(color = "black", size = 10,angle = 90,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))

All_combined2 %>%
  group_by(More_categories) %>%
  summarise(mean_percentage = mean(Percentage)) %>%
  mutate(More_categories = fct_reorder(More_categories, desc(mean_percentage))) %>%
  ggplot(aes(y = mean_percentage, x = More_categories, fill = More_categories)) +
  geom_col(width = 0.3) +
  scale_fill_manual(values = c("lightsalmon2","darkturquoise","gold","forestgreen")) +
  theme_bw() +
  labs(y = "Mean percentage",
       x = "Category",
       title = "Level 2 Assessment: Mean percentage perfomance per Category") +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black", size = 10,angle = 0, face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))



All_combined2 %>%
  group_by(More_categories,Question) %>%
  summarise(mean_percentage = mean(Percentage)) %>%
  mutate(Question = fct_reorder(Question, desc(mean_percentage))) %>%
  ggplot(aes(y = mean_percentage, x = Question, fill = More_categories)) +
  geom_col() +
  scale_fill_manual(values = c( "lightsalmon2","darkturquoise","forestgreen","gold")) +
  theme_bw() +
  labs(y = "Mean percentage",
       x = "Question",
       title = "Level 2 Assessment: Mean percentage perfomance per Category") +
  scale_y_continuous(breaks=seq(0,70,10)) +
  theme(axis.text.x = element_text(color = "black", size = 10,angle = 90, face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))##

################################################################################

##Teacher survey

Teacher_Assessment1 <- Teacher_Assessment %>%
  pivot_longer (!c(Gender,School,Location,Trained_PBL,Trained_Developed), 
                names_to = "Questions", values_to = "Response")

composition <- as.data.frame(Teacher_Assessment %>%
  group_by(Gender,School) %>%
  summarise(n = n()))
write.csv(composition,"composition.csv")

Teacher_Assessment %>%
  filter(Trained_PBL == "Yes") %>%
  group_by(Trained_PBL,Trained_Developed) %>%
  summarise(n = n())

Teacher_Assessment1 <- Teacher_Assessment1 %>%
  mutate(Response = case_when(Response == "1" ~ "Never",
                              Response == "2" ~ "Very rarely",
                              Response == "3" ~ "Rarely",
                              Response == "4" ~ "Sometime",
                              Response == "5" ~ "Often",
                              Response == "6" ~ "Very often"))

Teacher_Assessment1 <- Teacher_Assessment1 %>%
  mutate(Category = case_when(Questions == "Learn_Engage1" ~ "Learner_engagement",
                              Questions == "Learn_Engage2" ~ "Learner_engagement",
                              Questions == "Learn_Engage3" ~ "Learner_engagement",
                              Questions == "Learn_Engage4" ~ "Learner_engagement",
                              Questions == "Role_ed5" ~ "Role_of_educator",
                              Questions == "Role_ed6" ~ "Role_of_educator",
                              Questions == "Diff_resp7" ~ "Differentiated_response",
                              Questions == "Diff_resp8" ~ "Differentiated_response",
                              Questions == "Diff_resp9" ~ "Differentiated_response",
                              Questions == "Fasc_disc10" ~ "Facilitation_disussion",
                              Questions == "Fasc_disc11" ~ "Facilitation_disussion",
                              Questions == "Fasc_Disc12" ~ "Facilitation_disussion",
                              Questions == "Fasc_Disc13" ~ "Facilitation_disussion",
                              Questions == "Fasc_Disc14" ~ "Facilitation_disussion",
                              Questions == "Type_Quiz15" ~ "Type_question",
                              Questions == "Type_Quiz16" ~ "Type_question",
                              Questions == "Supp_learn_en17" ~ "Supportive_environment",
                              Questions == "Supp_learn_en18" ~ "Supportive_environment",
                              Questions == "Supp_learn_en19" ~ "Supportive_environment",
                              Questions == "Supp_learn_en20" ~ "Supportive_environment",
                              Questions == "Feedback21" ~ "Feedback",
                              Questions == "Feedback22" ~ "Feedback",
                              Questions == "Feedback23" ~ "Feedback",
                              Questions == "Feedback24" ~ "Feedback",
                              Questions == "Att_learn25" ~ "Attitude_learning",
                              Questions == "Att_learn26" ~ "Attitude_learning",
                              Questions == "Att_learn27" ~ "Attitude_learning",
                              Questions == "Att_Learn28" ~ "Attitude_learning",
                              Questions == "Att_Learn29" ~ "Attitude_learning"))

Teacher_Assessment1$Response <- fct_relevel(Teacher_Assessment1$Response,
                                            "Never","Very rarely","Rarely",
                                            "Sometime","Often","Very often")


ggplot(Teacher_Assessment1, aes(x = Response,fill = Category)) +
  geom_bar(width = 0.5, position = "fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2",
                               "gold","forestgreen")) +
  labs(y = "Frequency count",
  title = "Teacher feedback on the different sections") +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black",angle = 0,size = 10),
        axis.text.y = element_text(color="black", size = 10, angle = 0),
        axis.title.x = element_text(colour="black", size = 10),
        axis.title.y = element_text(colour="black", size = 10,hjust = 0.5),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10),
        plot.title = element_text(hjust = 0.5))

# ggplot2::ggsave(filename = c("E:/IFERB Data/Teacher_feedback.png"))



ggplot(Teacher_Assessment1, aes(x = Category, fill = Response)) +
  geom_bar(width = 0.3, position = "fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(y = "Percentage",
       title = "Teacher feedback on the different sections") +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black",angle = 0,size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold"))

#learner engagement
Teacher_Assessment1 %>%
  filter(Category == "Learner_engagement") %>%
  ggplot(aes(x = School, fill = Response)) +
  geom_bar(width = 0.4, position="fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  facet_wrap(~Questions) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Learner Engagement per school",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold",))
  
Teacher_Assessment1$Response <- fct_relevel(Teacher_Assessment1$Response,
                                            "Never","Very rarely","Rarely",
                                            "Sometime","Often","Very often")

Teacher_Assessment1 %>%
  filter(Category == "Learner_engagement") %>%
  ggplot(aes(x = Questions, fill = Response)) +
  geom_bar(width = 0.4, position = "fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Learner Engagement",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold",))

#Role_of_educator
Teacher_Assessment1 %>%
  filter(Category == "Role_of_educator") %>%
  ggplot(aes(x = School, fill = Response)) +
  geom_bar(width = 0.4, position="fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  facet_wrap(~Questions) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Role of educator per school",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold",))

Teacher_Assessment1$Response <- fct_relevel(Teacher_Assessment1$Response,
                                            "Never","Very rarely","Rarely",
                                            "Sometime","Often","Very often")

Teacher_Assessment1 %>%
  filter(Category == "Role_of_educator") %>%
  ggplot(aes(x = Questions, fill = Response)) +
  geom_bar(width = 0.4, position = "fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Role of educator",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold",))

##Differentiated_response
Teacher_Assessment1 %>%
  filter(Category == "Differentiated_response") %>%
  ggplot(aes(x = School, fill = Response)) +
  geom_bar(width = 0.4, position="fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  facet_wrap(~Questions) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Differentiated response per school",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold",))

Teacher_Assessment1$Response <- fct_relevel(Teacher_Assessment1$Response,
                                            "Never","Very rarely","Rarely",
                                            "Sometime","Often","Very often")

Teacher_Assessment1 %>%
  filter(Category == "Differentiated_response") %>%
  ggplot(aes(x = Questions, fill = Response)) +
  geom_bar(width = 0.4, position = "fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Differentiated response",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold"))

#Facilitation_disussion
Teacher_Assessment1 %>%
  filter(Category == "Facilitation_disussion") %>%
  ggplot(aes(x = School, fill = Response)) +
  geom_bar(width = 0.4, position="fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  facet_wrap(~Questions) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Facilitation disussion per school",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold",))

Teacher_Assessment1$Response <- fct_relevel(Teacher_Assessment1$Response,
                                            "Never","Very rarely","Rarely",
                                            "Sometime","Often","Very often")

Teacher_Assessment1 %>%
  filter(Category == "Facilitation_disussion") %>%
  ggplot(aes(x = Questions, fill = Response)) +
  geom_bar(width = 0.4, position = "fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Facilitation disussion",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold"))

#Type_question
Teacher_Assessment1 %>%
  filter(Category == "Type_question") %>%
  ggplot(aes(x = School, fill = Response)) +
  geom_bar(width = 0.4, position="fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  facet_wrap(~Questions) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Type of question per school",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold",))

Teacher_Assessment1$Response <- fct_relevel(Teacher_Assessment1$Response,
                                            "Never","Very rarely","Rarely",
                                            "Sometime","Often","Very often")

Teacher_Assessment1 %>%
  filter(Category == "Type_question") %>%
  ggplot(aes(x = Questions, fill = Response)) +
  geom_bar(width = 0.4, position = "fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Type of question",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold"))

#Supportive_environment
Teacher_Assessment1 %>%
  filter(Category == "Supportive_environment") %>%
  ggplot(aes(x = School, fill = Response)) +
  geom_bar(width = 0.4, position="fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  facet_wrap(~Questions) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Supportive environment per school",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold",))

Teacher_Assessment1$Response <- fct_relevel(Teacher_Assessment1$Response,
                                            "Never","Very rarely","Rarely",
                                            "Sometime","Often","Very often")

Teacher_Assessment1 %>%
  filter(Category == "Supportive_environment") %>%
  ggplot(aes(x = Questions, fill = Response)) +
  geom_bar(width = 0.4, position = "fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Supportive environment",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold"))

#Feedback
Teacher_Assessment1 %>%
  filter(Category == "Feedback") %>%
  ggplot(aes(x = School, fill = Response)) +
  geom_bar(width = 0.4, position="fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  facet_wrap(~Questions) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Feedback per school",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold",))

Teacher_Assessment1$Response <- fct_relevel(Teacher_Assessment1$Response,
                                            "Never","Very rarely","Rarely",
                                            "Sometime","Often","Very often")

Teacher_Assessment1 %>%
  filter(Category == "Feedback") %>%
  ggplot(aes(x = Questions, fill = Response)) +
  geom_bar(width = 0.4, position = "fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Feedback",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold"))

#Attitude_learning
Teacher_Assessment1 %>%
  filter(Category == "Attitude_learning") %>%
  ggplot(aes(x = School, fill = Response)) +
  geom_bar(width = 0.4, position="fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  facet_wrap(~Questions) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Attitude to learning per school",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold",),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold",),
        legend.title = element_text(color = "black", size = 15,face = "bold",),
        legend.text = element_text(color = "black", size = 15,face = "bold",),
        plot.title = element_text(hjust = 0.5,face = "bold",))

Teacher_Assessment1$Response <- fct_relevel(Teacher_Assessment1$Response,
                                            "Never","Very rarely","Rarely",
                                            "Sometime","Often","Very often")

Teacher_Assessment1 %>%
  filter(Category == "Attitude_learning") %>%
  ggplot(aes(x = Questions, fill = Response)) +
  geom_bar(width = 0.4, position = "fill",aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= scales::percent) +
  theme_classic() +
  scale_fill_manual(values = c("cadetblue2","chartreuse2","chocolate2",
                               "antiquewhite2","darkgoldenrod2","darkolivegreen2")) +
  labs(title = "Attitude to learning",
       y = "Percentage") +
  theme(axis.text.x = element_text(color = "black",angle = 0, face = "bold", size = 10),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold",),
        axis.title.y = element_text(colour="black", size = 15,hjust = 0.5,face = "bold"),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(hjust = 0.5,face = "bold"))

















