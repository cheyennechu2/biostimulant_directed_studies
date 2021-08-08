library(dplyr)
library(ggplot2)
library(rstatix)

###########   EDA    ################

########### BIWEEKLY ################

biweekly <- read.csv('biweekly.csv')
biweekly$Final.Dry.Weight <- as.numeric(as.character(biweekly$Final.Dry.Weight))
biweekly$Dilution.Rates_trt <- gsub(" ", "", biweekly$Dilution.Rates_trt, fixed = TRUE)

biweekly.processed <- biweekly %>% 
  group_by(Dilution.Rates_trt) %>% 
  dplyr::summarize(Mean = mean(Final.Dry.Weight, na.rm=TRUE), SD = sd(Final.Dry.Weight, na.rm=TRUE))
                   
biweekly.processed$Dilution.Rates_trt <- c('D0 (no product applied)',
                                           'D1 (1:25)',
                                           'D2 (1:50)',
                                           'D3 (1:100)',
                                           'D4 (1:200)',
                                           'D5 (1:400)')
View(biweekly.processed)

ggplot(biweekly.processed, aes(x=Dilution.Rates_trt, y=Mean, fill=Dilution.Rates_trt)) + 
  geom_bar(stat="identity", color="black", position=position_dodge(), width=0.5) +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  labs(fill='Dilution Rates') +
  xlab('Dilution Rates') +
  ylab('Final Dry Weight') +
  ggtitle('Biweekly Treatment') +
  scale_x_discrete(labels = c('D0', 'D1', 'D2', 'D3', 'D4', 'D5')) +
  ylim(0,15)






########### WEEKLY ################

weekly <- read.csv('weekly.csv')
weekly$Final.Dry.Weight <- as.numeric(weekly$Final.Dry.Weight)
weekly$Dilution.Rates_trt <- gsub(" ", "", weekly$Dilution.Rates_trt, fixed = TRUE)

weekly.processed <- weekly %>% 
  group_by(Dilution.Rates_trt) %>% 
  dplyr::summarize(Mean = mean(Final.Dry.Weight, na.rm=TRUE), SD = sd(Final.Dry.Weight, na.rm=TRUE))

weekly.processed$Dilution.Rates_trt <- c('D0 (no product applied)',
                                         'D1 (1:25)',
                                         'D2 (1:50)',
                                         'D3 (1:100)',
                                         'D4 (1:200)',
                                         'D5 (1:400)')
View(weekly.processed)

ggplot(weekly.processed, aes(x=Dilution.Rates_trt, y=Mean, fill=Dilution.Rates_trt)) + 
  geom_bar(stat="identity", color="black", position=position_dodge(), width=0.5) +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                position=position_dodge(.9)) +
  labs(fill='Dilution Rates') +
  xlab('Dilution Rates') +
  ylab('Final Dry Weight') +
  ggtitle('Weekly Treatment') +
  scale_x_discrete(labels = c('D0', 'D1', 'D2', 'D3', 'D4', 'D5')) +
  ylim(0,15)


########### ANOVA ################

# Biweekly - Dilution Rates
aov(Final.Dry.Weight ~ Dilution.Rates_trt, data=biweekly)

# Weekly - Dilution Rates
aov(Final.Dry.Weight ~ Dilution.Rates_trt, data=weekly)


# Check if difference between biweekly and weekly

