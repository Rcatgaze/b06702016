
library(dplyr)
library(ggplot2)


train <- read.csv("./all/train.csv", stringsAsFactors=FALSE)
test  <- read.csv("./all/test.csv",  stringsAsFactors=FALSE)
gender_submission  <- read.csv("./all/gender_submission.csv",  stringsAsFactors=FALSE)

full_test <- merge(test, gender_submission ,by= "PassengerId", all = T)
all <- bind_rows(train, full_test)

head(all)
tail(all)


# Part.A


all$Survived <- as.factor(all$Survived)
ggplot(data = all, aes(x = Survived, y = Fare)) +
  geom_boxplot() + coord_flip() +
  labs( y = 'Fare', x = 'Survived', 
        title = 'Mathematical Score Box')


all <- na.omit(all)

with(all, 
     tapply(Fare , Survived,
            function(x) 
              c(mean(x) + c(-2, 2) * sd(x)/sqrt(length(x)))))



t.test(Fare ~ Survived, data = all)
t.test(Fare ~ Survived, data = all, var.equal = TRUE)



#小結：以票價來說，購買越高價位的人存活度更高，

# Part.B


library(Hmisc)

all$Embarked <- as.factor(all$Embarked) 
tapply(all$Fare, all$Embarked, mean)

去掉資料中的NA

all <- na.omit(all)

ggplot(data = all, 
       aes(x = Embarked, y = Fare)) +
  stat_summary(fun.data = 'mean_cl_boot', size = 1) +
  scale_y_continuous(breaks = seq(500, 660, by = 20)) +
  geom_hline(yintercept = mean(all$Fare) , 
             linetype = 'dotted') +
  labs(x = '上船地點', y ='票價') +
  coord_flip()

anova(m1 <- lm(Fare ~ Pclass, data = all))
ggplot(data = all, 
       aes(group = Pclass, 
           y = Fare, x = Pclass)) +
  geom_point() +
  stat_smooth(method = 'lm', se = F) +
  stat_smooth(aes(group = Embarked, 
                  y = Fare, x = Pclass), 
              method = 'lm', se = F) + 
  facet_grid( . ~  Embarked) +
  labs(x = '社經地位', y = '票價')




anova(m2 <- update(m1, . ~ . + 
                     Embarked, data = all))



res_lm <- lapply(list(m1, m2, m3), summary)
(res_lm[[2]]$r.sq - res_lm[[3]]$r.sq)/res_lm[[2]]$r.sq
anova(m3, m2)
(res_lm[[2]]$r.sq - res_lm[[1]]$r.sq)/res_lm[[1]]$r.sq
anova(m1, m2)

library(coefplot)
m2 <- lm(Fare ~ Pclass + Embarked, 
         data = all)
coefplot(m2, xlab = '估計值', ylab = '迴歸變項', title = '反應變項 = 票價')



fit_m2 <- data.frame(all[, c(3, 10, 12)], fitted = fitted(m2), resid = resid(m2),
                     infl = influence(m2)$hat )


ggplot(data = fit_m2, aes(x = scale(resid)), group = Embarked ) +
  stat_density(geom = 'path', position = 'identity', aes(linetype = Embarked)) +
  scale_linetype_manual(values = 4:1) +
  guides(linetype = guide_legend(reverse = TRUE)) +
  labs(x = '標準化殘差', y = '機率密度') +
  theme(legend.position = c(.50, .8))

library(lattice)
qqmath(~ scale(resid) | Embarked, data = fit_m2, type = c('p', 'g', 'r'),
       xlab = '常態位數', ylab = '標準化殘差', layout = c(2, 3),
       pch = '.', cex = 2)

library(MASS)
library(ggplot2)
ggplot(data = fit_m2, aes(x = fitted, y = scale(resid), group = Embarked )) +
  geom_point(pch = 20, size = 1) +
  stat_smooth(method = 'rlm', se = F) +
  facet_grid(Embarked ~ .) +
  labs(x = '數學預測值', y = '標準化殘差')

ggplot(data = fit_m2, aes(x = infl, y = scale(resid), group = Embarked)) +
  geom_text(aes(label = rownames(fit_m2)), cex = 2) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  facet_grid(Embarked ~ .) +
  labs(x = '影響值', y = '標準化殘差')

summary(influence(m2)$hat)

a <- sub("male","0",all$Sex)
b <- sub("fe0","1",a)
all$Sex <- as.numeric(b)
all$SibSp <- as.numeric(all$SibSp)
all$Parch <- as.numeric(all$Parch)
all_Fare <- all[, c('Fare', 'Sex', 

colMeans(all_Fare)

library(heplots)
scatterplotMatrix(~ Fare + Sex + Age + SibSp + Parch, data= all_Fare,
                  pch = '.', cex = 3, smooth = FALSE, ellipse = TRUE,
                  diagonal = 'none', lower.panel = NULL)

library(corrplot)
corrplot(cor(all_Fare), method = 'ellipse', order = 'hclust', addrect = 4,
         type = 'upper', tl.pos = 'tp')
corrplot(cor(all_Fare), add = TRUE, type = 'lower', method = 'number',
         order = 'hclust', col = 'black', diag = FALSE, tl.pos = 'n', cl.pos = 'n')

summary(m4 <- lm(Fare ~ + Sex + Age + SibSp + Parch, data= all_Fare))

coefplot(m4, predictors = c('Sex', 
                            'Age', 'SibSp', 'Parch'),
         xlab = '估計值', ylab = '迴歸變項(去除截距)', title = '反應變項是票價')

library(effects)
plot(allEffects(m4), main = '', ylim = c(550, 670), grid = T)

library(lm.beta)
summary(lm.beta(m4))

summary(m5 <- update(m4, . ~ . - Age , data = all_Fare))

anova(m5, m4)


