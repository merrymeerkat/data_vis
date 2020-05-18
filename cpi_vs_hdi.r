dat1 <- dat
dat1

countries_shown = c("Afghanistan", "Congo", "Sudan", "Myanmar", "Iraq", "India", "Venezuela", "Russia", "Argentina", "Greece", "Brazil", "Greece", "Italy", "Spain", "France", "US", "Germany", "Norway", "New Zealand", "Singapore", "Japan", "Britain", "Barbados", "Botswana", "Cape Verde", "Buthan", "Rwanda", "South Africa", "China")


library(ggplot2)
library("ggrepel")
#geom_line(aes(y = pred.HDI)) + geom_smooth()

dat1$pred.HDI <- predict(lm(HDI ~ CPI, data = dat1))

our_y_lab <- expression(italic("Human Development Index, 2011 (1 = best)"))
our_x_lab <- expression(italic("Corruption Perception Index, 2011 (10 = best)"))
our_title <- expression(bold("Corruption and human development\n"))
dim(dat1)
country_sub <- dat1$Country[]
                      
#geom_line(aes(y = dat1$pred.HDI)) + geom_smooth(color = "black")7
#aes(color = Region) + geom_point(shape = 'o', size = 5)

ggplot(data = dat1, aes(x = CPI, y = HDI)) + 
    geom_point(shape = 'o', size = 5, aes(color = Region)) + 
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray70"), axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.line.x = element_line(color = "gray65"), axis.ticks.length.x = unit(-.12, "cm"), axis.text.x = element_text(vjust = -2), legend.justification = c(0,0.2), legend.position = c(0,1), legend.title = element_blank(), legend.direction = "horizontal", plot.margin = margin(.8, 0, 0, .2, "cm"), plot.caption = element_text(hjust = -0.16), axis.title.x = element_text(vjust = -1, hjust = 0.4)) +
  scale_colour_manual(labels = c("OECD", "Americas", str_wrap("Asia & Oceania", width = 10), str_wrap("Central & Western Europe", width = 16), str_wrap("Middle East & North Africa", width = 14), str_wrap("Sub-Saharan Africa", width = 14)), values = c("red", "green", "#2b1900", 'turquoise4', "pink", "orange")) +
  scale_x_continuous(limits = c(1,10), breaks = seq(1,10,1)) +
   scale_y_continuous(limits = c(0.2,1), breaks = seq(0.2,1,.1)) + labs(title = our_title, y = our_y_lab, x = our_x_lab, caption = "Sources: Transparency International; UN Human Development Report") + geom_smooth(method = lm, formula = y ~ log(x), se = FALSE, color = "red", fullrange = TRUE) + guides(colour = guide_legend(nrow = 1)) +
geom_text_repel(aes(dat1$CPI, dat1$HDI, label = ifelse(is.element(dat1$Country, countries_shown), dat1$Country, "")), size = 4)
