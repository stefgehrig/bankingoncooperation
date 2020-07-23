#### code for tables, figures and summary statistics describing
#### the results of the literature review in
####
#### "Banking on cooperation: An evolutionary analysis of microfinance loan repayment behaviour"
#### Gehrig, S., Mesoudi, A., Lamba, L.
####
#### required data "litreview_results.csv" are supplied in a separate file

# load packages and data---------------------------

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
# loadfonts(device = "win")
library(ggforce)

#import dataset with extracted associations from literature (= Table S1)
df <- read_csv("litreview_results.csv")

#clean
df <- janitor::remove_empty(df)

#fill NAs in column with category of predictor
df$`Category of predictor variable` <-  na.locf(df$`Category of predictor variable`, fromLast = FALSE)

# Table S2------------------------------------

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

# change order of categories
df3 <- df3[c(1,2,4,5,6,3,8,7,9,10,11,12,13),]
df2 <- df2[c(1,2,4,5,6,3,8,7,9,10,11,12,13),]

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

# Summary statistics --------------------------------

length(unique(df$Reference))                      #40 studies
length(unique(df$Country))                        #31 countries
nrow(df)                                          #139 extracted associations, of which...
apply(df2[,-1], 2, FUN = function(x) sum(x))      #...29 negative
                                                  #...63 non-significant
                                                  #...2 inverted U-shaped
                                                  #...45 positive

# Figure 2---------------------------------------------------

#transform data for plotting
df2 %>% 
  pivot_longer(cols = c(Negative, `Non-significant`, `Inverted U`, Positive), 
               names_to = "eff",
               values_to = "sum") %>% 
  group_by(`Category of predictor variable`) %>% 
  mutate(perc         = sum/sum(sum)*100,
         sum_position = cumsum(sum) - 0.5 * sum) -> df_plot

#change names of two predictor categories
df_plot$`Category of predictor variable`[
  df_plot$`Category of predictor variable` == "Personal and business relations between group members"
] <- "Personal and business relations betw. group members"
df_plot$`Category of predictor variable`[
  df_plot$`Category of predictor variable` == "Cultural homogeneity of group"
] <- "Socio-cultural homogeneity of group"

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
png("Figure2.png", width = 2500, height = 1200, res = 205)
df_plot %>% 
  ggplot(., aes(x    = `Category of predictor variable`,
                y    = sum,
                fill = eff)) + 
  geom_bar(stat = "identity") +
  geom_text(data = df_plot, size = 3, 
            aes(
              x     = `Category of predictor variable`,
              y     = ifelse(sum == 0, NA, sum_position),
              label = paste0(round(perc,0), "%"))) +
  scale_y_continuous(breaks = seq(0, 25, by=2)) +
  coord_flip(ylim = c(0, 25), clip="off") +
  theme_minimal(base_size=14) +
  labs(y = "Number of associations with repayment",
       fill = "Direction of association:") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    #text=element_text(family="Segoe UI")
    axis.title.x = element_text(vjust = -2),
    axis.title.y = element_text(vjust = 1.5)
  ) +
  scale_fill_manual(values=c("#addd8e", 
                             "#9ecae1", 
                             "lightgrey",
                             "#fdbb84"),
                    guide = guide_legend(reverse = TRUE)
  ) +
  annotate("rect",
           xmin=c(12.5,5.5,3.5,0.5),
           xmax=c(13.7,12.4,5.4,3.4),
           ymin=rep(-14.7,4),
           ymax=rep(-1,4),
           alpha=0.1,
           color=rep("grey",4),
           fill=c("blue","yellow","green","red")
  ) +
  annotate("label", 
           label = c("COMMON\nANCESTRY", "PRIOR\nINTERACTION", "PARTNER\nCHOICE", "OTHER/\nMULTIPLE\nMECHANISMS"), 
           x = c(13.1,9,4.5,2), 
           y = rep(-12.8,4), 
           size = rep(3,4),
           fontface = 2
  )
dev.off()

