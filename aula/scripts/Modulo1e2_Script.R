### Script para Aula 1 - Curso de Introducao ao R ###
##################### --SETS--  #####################

# September 30, 2022
# Carina Isabella Motta

#1 LOAD PACKAGES----------------------------------------------------------------

# a vector listing package names needed for importing the DNA sequences,
#calculating genetic distance, calculated geographic distance, and performing
#a Mantel test

rm(list=ls())

package.list <- c("here", #so I don't have to deal with setting a WD
                  "ggplot2", 
                  "vegan",
                  "ExpDes.pt",
                  "jtools",
                  "nlme",
                  "MASS",
                  "car",
                  "dunn.test",
                  "r2glmm",
                  "dplyr",
                  "tidyverse",
                  "ggpubr",
                  "gplots",
                  "FactoMineR",
                  "factoextra",
                  "emmeans",
                  "MuMIn",
                  "bblme",
                  "PerformanceAnalytics",
                  "lme4"
)


#installing the packages if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#and loading the packages into R with a for loop
for(i in package.list){library(i, character.only = T)}

rm(list=ls())
getwd()

?load

load(here::here("dados", "Modulo1.RData"))

load(here::here("dados", "Modulo2.RData"))

#load(file.choose())
#save(flores, file="Modulo2.RData")

library(ggplot2)
laranbox = 
  ggplot(laranjas, aes(x = Tratamento, y = Biomassa)) +
  geom_boxplot()
laranbox

laranbox = 
  ggplot(laranjas, aes(x = as.factor(Tratamento), y = Biomassa, fill= as.factor(Tratamento))) +
  geom_boxplot(width=0.2,position=position_dodge(1))+   
  scale_fill_manual(values=c("black", "darkgrey", "white"))+
  ylab("")+xlab("Tratamentos")+ 
  theme_classic()+theme(text=element_text(size=20),legend.position = "none")
laranbox

# ylim(c(0,150))+

##################### --Exercicio 4--  #####################
hist()
plot()
boxplot()
sample()
order()
t.test()
lm()
cor()
prcomp() ou princomp()
matrix()
shapiro.test()

##################### --Exercicio 3--  #####################
bla = boxplot(ba~dis, data = exemplo, col = c("grey","red"), main="Dispersão",
              xlab="Tipo de Dispersão", ylab="Área Basal")

# Aumente a janela do plot para ver melhor a figura
boxplot(exemplo$ba~exemplo$dis,main = "Título", xlab="Eixo X", ylab = "Eixo Y", col = "darkgreen" )

ble  = plot(exemplo$ba)

plot(woo~sla, pch = 5 ,data = exemplo, col ="red")
# main = "ScatterPlot"

hist(exemplo$tgh, main = "Dureza Foliar")

mean(exemplo$tgh)
sd(exemplo$tgh)
aggregate(.~species, FUN="mean", data=exemplo[,c(3:15)])
hist(exemplo$tgh, main="Dureza")

####---------------------- MODULO 2 ------------------------ ##########
##################### -- Sets --  #####################

#setwd("SET HERE")
#getwd()
#setwd("/Users/priscila/Desktop/DataAnalysis/YouandR2021/CursoR2021/Pheno/Aula 2")
#save(flores, exemplo,
#     file="Modulo2.RData")

#load(file.choose())
#read.csv(file.choose(), header = T)

##################### --Pacotes--  #####################
#if(!require(vegan)) install.packages("vegan")
#if(!require(ggplot2)) install.packages("ggplot2")
#if(!require(ExpDes.pt)) install.packages("ExpDes.pt")
#if(!require(jtools)) install.packages("jtools")
#if(!require(nlme)) install.packages("nlme")
#if(!require(MASS)) install.packages("MASS")
#if(!require(car)) install.packages("car")
#if(!require(dunn.test)) install.packages("dunn.test")
#if(!require(r2glmm)) install.packages("r2glmm")
#if(!require(dplyr)) install.packages("dplyr")
#if(!require(tidyverse)) install.packages("tidyverse")
#if(!require(ggpubr)) install.packages("ggpubr")
#if(!require(gplots)) install.packages("gplots")
#if(!require(FactoMineR)) install.packages("FactoMineR")
#if(!require(factoextra)) install.packages("factoextra")
#if(!require(emmeans)) install.packages("emmeans")
#if(!require(MuMIn)) install.packages("MuMIn")
#if(!require(bbmle)) install.packages("bbmle")
#if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
#if(!require(lme4)) install.packages("lme4")

##################### --VERIFICANDO OS DADOS --  #####################
plot(exemplo$h)

plot(exemplo$h,
     ylab="Altura (m)", 
     xlab = "Ocorrência", 
     pch = 8, main = "Bla")

hist(exemplo$sla, main = "Histogram SLA")

shapiro.test(exemplo$h)  

hist(exemplo$dis)
#O que aconteceu?
boxplot(h~dis, data=exemplo)

##################### --Normalidade--  #####################
?shapiro.test
# Running examples:
#normal
shapiro.test(rnorm(100, mean = 5, sd = 3))
# P > 0.05

#não normal
shapiro.test(runif(100, min = 2, max = 4))
# P < 0.05

# Calculando a normalidade para todas as colunas numéricas da planilha de exemplo:
lapply(exemplo[, 5:15], shapiro.test)

##################### --TESTE T (Ex 2 / Lista 2)--  #####################
?t.test
# note que sao necessarios vetores numericos - ou duas colunas da planilha, ou 
# como exemplo abaixo:

t.test(exemplo[exemplo$dis == 'ane', exemplo$h] , 
       exemplo[exemplo$dis == 'zoo', exemplo$h])

a = exemplo[exemplo$dis == 'ane', exemplo$h]
b= exemplo[exemplo$dis == 'zoo', exemplo$h]
t.test(a, b)

#Option 2
trata = laranjas[1:15,]
tratab = laranjas[16:35,]

t.test(trata$Biomassa, tratab$Biomassa)
t.test(trata$Biomassa, mu=100)

##################### --BOXPLOTS --  #####################
boxplot(exemplo$h ~ exemplo$dis)

#Option 2 / Exercício 3
library(ggplot2)
altbox = 
  ggplot(exemplo, aes(x = as.factor(dis), y = h, fill= dis)) +
  geom_boxplot(width=0.2,position=position_dodge(1))+   
  scale_fill_manual(values=c("orange", "darkblue"))+
  ylab("")+xlab("Tratamentos")+ 
  theme_classic()+theme(text=element_text(size=20))
altbox

##################### --ANOVA (Ex 4/2)--  #####################
solo.fogo = data.frame(
  fogo = rep(c("alto", "intermediario", "baixo"), each = 5),
  N = c(1623, 1534, 1582, 1652, 1633, 910, 1120, 921, 980, 1110,
        1148, 1288, 1414, 1456, 1448))

mod=aov(N~fogo, data=solo.fogo)
summary(mod)
mod
TukeyHSD(mod)

aggregate(N~fogo, data=solo.fogo, FUN="mean")
aggregate(N~fogo, data=solo.fogo, FUN="sd")

library(ggplot2)
nfogobox = 
  ggplot(solo.fogo, aes(x = as.factor(fogo), y = N, fill= fogo)) +
  geom_boxplot(width=0.2,position=position_dodge(1))+   
#  scale_fill_manual(values=c("orange", "darkblue", "black"))+
  ylab("")+xlab("")+ 
  theme_light()+theme(text=element_text(size=20))
nfogobox

install.packages("ExpDes.pt")
library(ExpDes.pt)
dic(solo.fogo$fogo, solo.fogo$N)

##################### --CORRELAÇÃO (Ex 5/2) --  #####################
cor(exemplo$h, exemplo$ba)
cor(exemplo[,5:15])

plot(exemplo$ba, exemplo$h)
cor.test(exemplo$h, exemplo$ba)

?pairs
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt)
}

a = exemplo[,5:15]
pairs (a, upper.panel = panel.cor)

##################### --REGRESSÕES: 1. SIMPLES (Ex 6/2)--  #####################
hist(exemplo$h)
shapiro.test(exemplo$h)

mod = lm (h ~ ba, data=exemplo)
mod
summary(mod)

##################### --SCATTER PLOTS --  #####################
#Forma mais simples
plot(h ~ ba, data=exemplo)
abline(mod)
#No ggplot2:
ggplot(exemplo, aes(x=ba, y=h)) + geom_point()

#Podemos plotar em função de uma terceira variável;
ggplot(exemplo, aes(x=ba, y=h)) + 
  geom_point(aes(size=K))+theme_classic()

#Adicionando a linha da regressao no ggplot:
ggplot(exemplo, aes(x=ba, y=h)) + 
  geom_point()+
  geom_smooth(method=lm)+theme_classic()

### Exercicio de Regressao Linear Simples #2 (exercicio 7/2)
riqueza = c(3, 2, 4, 4, 6, 6, 7, 6, 8, 7, 10, 12, 8, 9)
tamanho = c(20, 19, 21, 21, 23, 23, 24, 23, 25, 24, 27, 29, 25, 26)

mod2=lm(riqueza~tamanho)
summary(mod2)
hist(resid(mod2))

scatalt=
  ggplot(exemplo, aes(x=ba, y=h)) + geom_point()+
  geom_smooth(method=lm)+ylab("Altura (m)")+xlab("Área Basal (mm)")+
  theme_classic()+theme(text=element_text(size=15))
scatalt

##################### -- MULTIPLA (Ex 8/2)--  #####################
cor(exemplo[,7:15])
pairs(exemplo[,7:15])
pairs(exemplo[,7:15], upper.panel = panel.cor)
sum(cor(exemplo[,7:15]) > 0.8)
#K - N
#sla - P
# decidi remover o K e o P
dataf = exemplo[,c(5:15)]

mod3 = lm(h ~ tor+brk+woo+N+sla+tgh, data=exemplo)
summary(mod3)
step(mod3)

mod4 = lm(h ~ tor+woo+N+sla+tgh, data=exemplo)
summary(mod4)

plot(h ~ woo, data=dataf)

mod5 = lm(h ~ woo, data=dataf)
summary(mod5)
#abline(mod5)

##################### --MODELOS MISTOS--  #####################
##################### --distribuiçao--  #####################
shapiro.test(exemplo$h)
qqp(log(exemplo$h), "lnorm")
poisson <- fitdistr(exemplo$h, "Poisson", na.rm=TRUE)
qqp(exemplo$h, "pois", poisson$estimate, lambda=1.57)
gamma <- fitdistr(exemplo$h, "gamma")
qqp(exemplo$h, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])
hist(log(exemplo$h))

mmodel = glm((h) ~ as.factor(pol)+as.factor(dis)+(1|plot), family=Gamma, data=exemplo)
anova(mmodel)
summary(mmodel)

mmodel1 = glm(h ~ as.factor(pol)+as.factor(dis)+(1|plot), data=exemplo)
anova(mmodel1)
summary(mmodel1)

mmodel2 = lmer(h ~ as.factor(pol)+as.factor(dis)+(1|plot), data=exemplo)
anova(mmodel2)
summary(mmodel2)
## Modelo não converge!

# AIC comparison chooses this one:
mmodel3 = glm(h ~ as.factor(dis)+(1|plot), data=exemplo)
anova(mmodel1)
summary(mmodel3)

mmodel4 = glm(h ~ as.factor(dis), data=exemplo)
anova(mmodel1)
summary(mmodel3)

anova(mmodel1,mmodel2,mmodel3)

resultado<-ICtab(mmodel, mmodel1, mmodel2,mmodel3, mmodel4,type="AICc", sort=T, weights=T, base=T, nobs=nrow(exemplo))
resultado

emm.bmc = emmeans(mmodel3,~as.factor(dis),data=exemplo)
contrast(regrid(emm.bmc), method = "pairwise")
##Graph
hbox = ggplot(exemplo, aes(x = as.factor(dis), y =h, fill= as.factor(dis))) +
  geom_boxplot(width=0.2,alpha=0.7,position=position_dodge(0.35))+   
  ylab("Altura (m)")+xlab("")+
  theme_classic()+theme(text=element_text(size=18), legend.position = "none")+
  annotate("text", x = 1, y = 3, label = "a", size=6)+
  annotate("text", x = 2, y = 3, label = "b", size=6)+
  scale_fill_manual(values=c("white","black"))
hbox
