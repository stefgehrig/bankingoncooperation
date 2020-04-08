#### code for tables, figures and summary statistics describing
#### the results of the literature review in
####
#### "Banking on cooperation: An evolutionary analysis of microfinance loan repayment behaviour"
#### Gehrig, S., Mesoudi, A., Lamba, L.
####
#### required data "litreview_results.csv" are supplied in a separate file

#empty environment
rm(list = ls(all.names = TRUE))

#load libraries and fonts
library(readr)
library(tidyr)
library(dplyr)
library(zoo)
library(janitor)
library(ggplot2)
library(extrafont)
loadfonts(device = "win")

#import dataset with extracted associations from literature (= Table S1)
df <- read_csv("litreview_results.csv")

#clean
df <- janitor::remove_empty(df)

#fill NAs in column with category of predictor
df$`Category of predictor variable` <-  na.locf(df$`Category of predictor variable`, fromLast = FALSE)

#########################
#### Create Table S2 ####
#########################

#transform and order dataset
df %>% group_by(`Category of predictor variable`) %>% 
  summarise("Negative"        = sum(`Effect on repaymentb`=="neg"),      #stands for "negative association"
            "Non-significant" = sum(`Effect on repaymentb`=="nons"),     #stands for "non-significant association"
            "Inverted U"      = sum(`Effect on repaymentb`=="curve"),    #stands for "inverted U-shaped association"
            "Positive"        = sum(`Effect on repaymentb`=="pos")) %>%  #stands for "positive association"  
  mutate("Total N" = Negative+`Non-significant`+`Inverted U`+Positive) -> df2
df2 <- df2[order(match(df2$`Category of predictor variable`, unique(df$`Category of predictor variable`))),]

#calculate proportions
df2 %>% 
  mutate_at(c(2,3,4,5), function(x) x/df2$`Total N`) -> df3

#formatting of character strings
df3 <- matrix(paste(round(as.matrix(df3[,2:5]*100,2)),
             "% (",
             as.matrix(df2[,2:5]),
             "/",
             df2$`Total N`,
             ")",
             sep=""),
       nrow=nrow(as.matrix(df2[,2:5])), dimnames=dimnames(as.matrix(df2[,2:5])))
df3 <- as.data.frame(cbind(df2$`Category of predictor variable`, df3))
names(df3)[1] <- "Category of predictor variable"

#write
write.csv(df3, "TableS2.csv", row.names = FALSE)

#########################
#### Create Figure 2 ####
#########################

#transform data for plotting
df2 %>% 
  pivot_longer(cols = c(Negative, `Non-significant`, `Inverted U`, Positive), 
               names_to = "eff",
               values_to = "sum") %>% 
  group_by(`Category of predictor variable`) %>% 
  mutate(perc         = sum/sum(sum)*100,
         sum_position = cumsum(sum) - 0.5 * sum) -> df_plot

#shorten name of one predictor category
df_plot$`Category of predictor variable`[
  df_plot$`Category of predictor variable` == "Personal and business contacts between group members"
  ] <- "Personal and business contacts betw. group members"

#order factors for plotting
df_plot$eff <- factor(df_plot$eff, ordered = TRUE, 
                      levels = c("Positive",
                                "Inverted U",
                                "Non-significant",
                                "Negative"))

#order factors for plotting
df_plot$`Category of predictor variable` <- factor(df_plot$`Category of predictor variable`, ordered = TRUE, 
                      levels = rev(unique(df_plot$`Category of predictor variable`)))

#write
png("Figure2.png", width = 2150, height = 1000, res = 205)
df_plot %>% 
  ggplot(., aes(x    = `Category of predictor variable`,
                y    = sum,
                fill = eff)) + 
  geom_bar(stat = "identity") +
  geom_text(data = df_plot, size = 3, 
            aes(
            x     = `Category of predictor variable`,
            y     = ifelse(sum == 0, NA, sum_position),
            label = paste0(round(perc,0), "%")),
            color = "white") +
  coord_flip() +
  theme_minimal(base_size=14) +
  labs(y = "No of. associations with repayment",
       fill = "Direction (qualitative)") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    text=element_text(family="Segoe UI")
  ) +
  scale_fill_manual(values=c("#addd8e", 
                             "#9ecae1", 
                             "lightgrey",
                             "#fdbb84"))
dev.off()

#########################
#### Create Figure 3 ####
#########################

#transform data for plotting
df2 %>% 
  mutate(N                        = Negative + `Non-significant` + `Inverted U` + Positive,
         score                    = (Negative*(-1) + `Inverted U` + Positive) / N,
         "Common ancestry"        = c(1,0,0,0,0,0,0,0,0,0,0,0,0), #defines manually which evolutionary mechanisms...
         "Repeated interaction"   = c(1,1,1,1,1,1,1,0,0,0,1,0,1), #...should be linked to which predictors
         "Partner choice"         = c(0,0,0,0,0,0,0,0,1,1,0,0,0),
         "Similarity of tags"     = c(0,0,0,0,0,0,0,0,0,0,1,1,0),
         "Social learning"        = c(1,1,1,1,1,1,0,1,0,0,1,0,0),
         "Demography and ecology" = c(1,0,1,0,0,0,0,0,0,0,1,1,1)) -> df_plot2

df_plot2 %>% 
  pivot_longer(cols      = c("Common ancestry",
                             "Repeated interaction",
                             "Partner choice",
                             "Similarity of tags",
                             "Social learning",
                             "Demography and ecology"), 
               names_to  = "evo",
               values_to = "indicator") %>% 
  filter(indicator==1) -> df_plot2

#shorten name of one predictor category
df_plot2$`Category of predictor variable`[
  df_plot2$`Category of predictor variable` == "Personal and business contacts between group members"
  ] <- "Personal and business contacts betw. group members"

#order factors for plotting
df_plot2$evo <-factor(
  df_plot2$evo,
  ordered = TRUE,
  levels = c("Common ancestry",
             "Repeated interaction",  
             "Partner choice",        
             "Similarity of tags",    
             "Social learning",       
             "Demography and ecology")
  )

#order factors for plotting
df_plot2$`Category of predictor variable` <- factor(
  df_plot2$`Category of predictor variable`, 
  ordered = TRUE, 
  levels = rev(unique(df_plot$`Category of predictor variable`)))

#write
png("Figure3.png", width = 2200, height = 1000, res = 210)
ggplot(df_plot2, aes(x = `Category of predictor variable`,
                     y = evo)) +
  geom_point(aes(color = score,
                 size  = N)) +
  coord_flip() +
  scale_size_continuous() +
  scale_color_gradient2(high = "#addd8e", 
                        mid  = "lightgrey",
                        low  = "#fdbb84",
                        labels = c("Negative", "Positive"),
                        breaks = c(-0.6, 0.6),
                        limits = c(-1, 1)) +
  theme_minimal(base_size=14) +
  labs(y = "Evolutionary mechanism of cooperation",
       color = "Aggregate association\nwith repayment") +
  theme(
    legend.position = "right",
    text=element_text(family="Segoe UI"),
    axis.text.x = element_text(angle = 50,
                               hjust=1),
    legend.key.size  = unit(0.3, "cm"),
    legend.key.width = unit(0.3,"cm"),
    legend.text  = element_text(size = 10),
    legend.title = element_text(size = 11))
dev.off()

############################
#### Summary statistics ####
############################

length(unique(df$Reference))                      #40 studies
length(unique(df$Country))                        #31 countries
nrow(df)                                          #135 extracted associations, oh which...
apply(df2[,-1], 2, FUN = function(x) sum(x))      #...27 negative
                                                  #...63 non-significant
                                                  #...1 inverted U-shaped
                                                  #...44 positive