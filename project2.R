dat <- read_csv("~/Documents/Fall25/Stats4520/social_media_data (2).csv")

#cleaning
colnames(dat)[5] = "Daily_Usage"
dat$Gender = as.factor(dat$Gender)
dat$Dominant_Emotion = factor(dat$Dominant_Emotion, levels = c("Anger", "Anxiety", "Boredom", "Happiness", "Neutral", "Sadness"))
dat$Platform = as.factor(dat$Platform)
dat$Dominant_Emotion = as.factor(dat$Dominant_Emotion)
which(dat$Dominant_Emotion == "Agression")
which(is.na(dat$Dominant_Emotion))
dat = dat[-1163,]
which(dat$User_ID == 27)
dat = dat[-1197,]


#initial boxplot
boxplot(Daily_Usage ~ Dominant_Emotion, data=dat, 
        col = "lightblue",
        xlab = "Dominant Emotion",
        ylab = "Daily Usage (min)")



#testing effects
library(lme4)
library(pbkrtest)
lmod <- aov(Daily_Usage ~ . - User_ID, data=dat)
summary(lmod)

#full model:
op <- options(contrasts = c("contr.sum", "contr.poly"))
mod_tot <- lmer(Daily_Usage ~ Age + Gender + (1|Platform) + Posts_Per_Day + Likes_Received_Per_Day + Comments_Received_Per_Day + Messages_Sent_Per_Day + (1|Dominant_Emotion), dat)
summary(mod_tot)

#remove age
lmodb <- lmer(Daily_Usage ~ Gender + (1|Platform) + Posts_Per_Day + Likes_Received_Per_Day + Comments_Received_Per_Day + Messages_Sent_Per_Day + (1|Dominant_Emotion), dat)
KRmodcomp(mod_tot, lmodb)
#no significant age term

#remove Gender
lmodc <- lmer(Daily_Usage ~ Age + (1|Platform) + Posts_Per_Day + Likes_Received_Per_Day + Comments_Received_Per_Day + Messages_Sent_Per_Day + (1|Dominant_Emotion), dat)
KRmodcomp(mod_tot, lmodc)
#significant gender term

#remove posts
lmode <- lmer(Daily_Usage ~ Age + Gender + (1|Platform) + Likes_Received_Per_Day + Comments_Received_Per_Day + Messages_Sent_Per_Day + (1|Dominant_Emotion), dat)
KRmodcomp(mod_tot, lmode)
#significant posts term

#remove likes
lmodf <- lmer(Daily_Usage ~ Age + Gender + (1|Platform) + Posts_Per_Day + Comments_Received_Per_Day + Messages_Sent_Per_Day + (1|Dominant_Emotion), dat)
KRmodcomp(mod_tot, lmodf)
#significant likes term

#remove comments
lmodg <- lmer(Daily_Usage ~ Age + Gender + (1|Platform) + Posts_Per_Day + Likes_Received_Per_Day + Messages_Sent_Per_Day + (1|Dominant_Emotion), dat)
KRmodcomp(mod_tot, lmodc)
#significant comments term

#remove messages
lmodh <- lmer(Daily_Usage ~ Age + Gender + (1|Platform) + Posts_Per_Day + Likes_Received_Per_Day + Comments_Received_Per_Day + (1|Dominant_Emotion), dat)
KRmodcomp(mod_tot, lmodh)
#significant messages term

newMod <- lmer(Daily_Usage ~ Gender + (1|Platform) + Posts_Per_Day + Likes_Received_Per_Day + Comments_Received_Per_Day + Messages_Sent_Per_Day + (1|Dominant_Emotion), dat)
summary(newMod)

#plotting
library(ggplot2)
ggplot(dat, aes(x=Dominant_Emotion, y=Daily_Usage)) + xlab("Dominant Emotion") + ylab("Daily Usage")
geom_boxplot()

ggplot(dat, aes(y = Daily_Usage,
                x = Platform,
                color = Dominant_Emotion)) + 
  geom_point(size = 3) + 
  xlab("Platform") + 
  ylab("Daily Usage (min)") +
  scale_color_manual(
    name = "Dominant Emotion",
    values = c("tomato", "lightsalmon", "lightyellow", "lightgreen", "lightblue", "steelblue"))


