#####################################################################################
library(lattice)
library(plyr)
library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)
theme_update(text = element_text(size=11),
             axis.text.x = element_text(angle=90, hjust=1))
      
library(sciplot)


data = read.csv("data.csv", header = TRUE)
################################################################
summary(data) 

keycol <- "model"
valuecol <- "accuracy"
gathercols <- c("presp_distillbert_accuracy", "presp_ft_distillbert_accuracy")

longdata = gather_(data, keycol, valuecol, gathercols)

write.csv(longdata,"data", row.names = TRUE)
###################Summarize and Error Bar plot##################
longdata = read.csv("data", header = TRUE)
data=group_by(longdata, model, condition)%>% 
  summarise(mean=mean(accuracy),se=se(accuracy))
head(data)

p=ggplot(data = data, aes(x = model, y = mean)) + 
  geom_bar(fill = rep(clrs_hcl(4), 1), position = "dodge", stat = "identity") + 
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width = 0.3,position = "dodge",alpha = 0.7) +
  facet_wrap(~ condition) 
p

p + scale_x_discrete(name ="BERT and Pragmatics Understanding") +
  scale_y_continuous(name="Accuracy Mean") + 
  theme(axis.text.x = element_text(face="bold", color="#993333", 
                                   size=9, angle=70))


path = 'data'
filename = 'bert_pragmatics_accuarcy.png'
ggsave(path = path, width = 7, height = 5, device='png', filename = filename, dpi=550)

########################GPT3########################
longdata = gather_(data, keycol, valuecol, gathercols)

write.csv(longdata,"data", row.names = TRUE)

###################Summarize and Error Bar plot##################
longdata = read.csv("data", header = TRUE)
data=group_by(longdata, model, condition)%>% 
  summarise(mean=mean(accuracy),se=se(accuracy))
head(data)

p=ggplot(data = data, aes(x = model, y = mean)) + 
  geom_bar(fill = rep(clrs_hcl(2), 1), position = "dodge", stat = "identity") + 
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width = 0.3,position = "dodge",alpha = 0.7) +
  facet_wrap(~ condition) 
p

p + scale_x_discrete(name ="GPT and Pragmatics Understanding") +
  scale_y_continuous(name="Accuracy Mean") + 
  theme(axis.text.x = element_text(face="bold", color="#993333", 
                                   size=9, angle=70))


path = 'data'
filename = 'gpt_pragmatics_accuarcy.png'
ggsave(path = path, width = 7, height = 5, device='png', filename = filename, dpi=550)

