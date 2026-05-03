################################################################################
################################################################################
################################################################################
library(ggplot2)
library(splines)
library(nlme)
library(lattice)
library(latticeExtra)
require(gamm4)
library(RColorBrewer)
library(ggpmisc)
library(dplyr)
################################################################################
################################################################################
################################################################################
dados1 = read.table("df_psplines.csv", header = T, sep=";", dec=",")
dados2 <- reshape(cbind(id=1:nrow(dados1), dados1), 
                  varying=5:45, 
                  v.names="massaseca",
                  timevar="Tempo", 
                  times=as.numeric(gsub("X", "", tail(names(dados1), -3))), 
                  direction="long", sep="")
dados=dados2[order(dados2$id), ]
dados = na.omit(dados)
dados$Teor  <- factor(dados$Teor)
dados$Fator  <- factor(dados$Fator)
################################################################################
################################################################################
################################################################################
dados[dados$Teor == 0 & dados$Fator == "Am", "Trat"] = "Am1:0%"
dados[dados$Teor == 6 & dados$Fator == "Am", "Trat"] = "Am2:6%"
dados[dados$Teor == 8 & dados$Fator == "Am", "Trat"] = "Am3:8%"
dados[dados$Teor == 10 & dados$Fator == "Am", "Trat"] = "Am4:10%"
dados[dados$Teor == 12 & dados$Fator == "Am", "Trat"] = "Am5:12%"
dados[dados$Teor == 0 & dados$Fator == "As", "Trat"] = "As1:0%"
dados[dados$Teor == 6 & dados$Fator == "As", "Trat"] = "As2:6%"
dados[dados$Teor == 8 & dados$Fator == "As", "Trat"] = "As3:8%"
dados[dados$Teor == 10 & dados$Fator == "As", "Trat"] = "As4:10%"
dados[dados$Teor == 12 & dados$Fator == "As", "Trat"] = "As5:12%"
################################################################################
################################################################################
################################################################################
dados$Trat <- factor(dados$Trat)
#########################################################################
#########################################################################
#################### Gráfico de dispersão ###############################
#########################################################################
#########################################################################
x11()
ggplot(dados, aes(x = Tempo, y = massaseca)) + facet_wrap(~Trat,ncol=5,nrow=2) +
  xlab("Time (Minutes)") +
  geom_point(size=3, color="#969696") +
  ylab("Weight (mg)") + 
  geom_smooth(method="loess", se = F, color="#556B2F") +
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text.x = element_text(size = 22,color="black"),
        axis.text.x = element_text(size = 18, colour = "black"),
        axis.text.y = element_text(size = 18, colour = "black"),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 25)) 
################################################################################
################################ Box-plot por Tratamento #######################
##################################### ggplot2 ##################################
###############################################################################
x11()
p = ggplot(data=dados, aes(x=Trat, y=massaseca)) + 
  stat_boxplot(geom = "errorbar", width = .33) +  
  geom_boxplot(width = .66, color="black", fill="#556B2F") +
  labs(fill="Tratamentos:", x="Treatments", 
       y='Weight (mg)') + 
  stat_summary(aes(linetype = "Median"),
               geom = "errorbar",
               fun.ymin = median, fun.ymax = median,
               width = .66, size = 1) +
  scale_linetype_manual(NULL, values = 1) +
  stat_summary(aes(shape = "Mean"),
               geom = "point",
               fun.y = mean,
               size = 5) +
  scale_shape_manual(NULL, values = 18) +
  theme(legend.position=c(1,0.8), legend.justification = c(1,0),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text.x = element_text(size = 18, colour = "black"),
        axis.text.y = element_text(size = 18, colour = "black"),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 25)) 
p 
################################################################################
####################### Gráfico de  Perfis #####################################
################################################################################
interaction <- dados %>%
  dplyr::select(massaseca, Tempo, id) %>%
  group_by(id, Tempo) %>%
  summarise(Average = mean(massaseca))
x11()
ggplot(interaction, aes(x=Tempo, y=Average, group=id)) +   
  geom_line() +
  xlab("Time (Minutes)") +
  ylab("Weight (mg)")  +
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text.x = element_text(size = 18, colour = "black"),
        axis.text.y = element_text(size = 18, colour = "black"),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 25)) 
################################################################################
################################################################################
################################## Model #######################################
################################################################################
################################################################################
fit4.gamm <- gamm(massaseca~factor(Trat)+s(Tempo,k=10,bs="ps",m=2,
                                           by=factor(Trat)),
                  random=list(id=pdSymm(~Tempo)),data=dados)
################################################################################
################################################################################
#################################### Ajuste ####################################
################################################################################
x11()
my.formula <- y ~ x
ylim_sup <- 1.1 * max(dados$massaseca)
ylim_inf <- min(dados$massaseca)
shape_brks <- unique(dados$Trat)
shape_vals <- rep(1, 10)

label_y_npc <- rep(1.0, 10)
label_x_npc <- rep(0.91, 10)

dados %>%
  group_by(Tempo, Fator, Trat) %>%
  summarise(massaseca = mean(massaseca, na.rm = TRUE),.groups = 'drop') %>%
  ggplot(aes(x = Tempo, y = massaseca,color = Trat)) +
  geom_point(data= dados, aes(x=Tempo,y=fitted(fit4.gamm$lme), group=id), color="#969696") +
  geom_line(data= dados, aes(x=Tempo,y=fitted(fit4.gamm$lme), group=id),
            col = "black") +
  stat_smooth(method = "lm", se = FALSE,
              formula = y ~ poly(x, 2, raw = TRUE),
              linetype = 1, 
              size = 0.7) + scale_shape_manual(name = "Trat", 
                                               breaks = shape_brks, 
                                               values = shape_vals) +
  geom_point() + 
  coord_cartesian(xlim=c(0,1420)) + 
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), 
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., sep = "*plain(\", \")~")),
               label.x.npc = label_x_npc,
               y = 1.0,  angle=90, 
               label.y.npc = label_y_npc,
               parse = TRUE, size = 4.3) +
  ylim(ylim_inf, ylim_sup) +
  labs(title = "",
       x = "Time (Minutes)",
       y = "Weight (mg)",
       color = "Trat") + 
  theme(legend.position="none",legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text.x = element_text(size = 22,color="black"),
        axis.text.x = element_text(size = 18, colour = "black"),
        axis.text.y = element_text(size = 18, colour = "black"),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 25),
        text = element_text(size = 20,color="black"))+ 
  facet_wrap(~Trat,ncol=5,nrow=2)
###############################################################################
################################### Ajuste ####################################
###############################################################################
modelpoly = lm(massaseca~Trat + Tempo + I(Tempo^2), data=dados)
summary(modelpoly)
BIC(modelpoly)
###############################################################################
############################## Diagnóstico - Polinomial #######################
###############################################################################
x11()
par(mfrow=c(2,2))
plot(model1)
Graph1=dados %>%
  mutate(fitted = predict(modelpoly),
         resid = residuals(modelpoly, type = "pearson")) %>%
  ggplot(aes(fitted, resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color="red",size=1) +
  labs(x = "Fitted Values", y = "Residuals", title = "(a)") + 
  theme(legend.position = "none", legend.title = element_text(size = 17),
        legend.text = element_text(size = 17),
        axis.title = element_text(size = 30),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 25),
        plot.title = element_text(size = 32)) 
Graph1
###############################################################################
############################## Diagnóstico - Polinomial #######################
################################# ggplot 2 ####################################
###############################################################################
qplot(massaseca, predict(model1), data = dados) +
  geom_abline(yintercept = 0, colour="red", size=1) + 
  labs(x = "Fitted Values", y = "Weight (mg)",title = "(a)") + 
  theme(legend.position = "none", legend.title = element_text(size = 17),
        legend.text = element_text(size = 17),
        axis.title = element_text(size = 30),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 25),
        plot.title = element_text(size = 32)) 
###############################################################################
################################### Ajuste ####################################
###############################################################################
model1 <- lme(massaseca~factor(Trat)+Tempo,random=list(id=pdIdent(~1)),
              data=dados)
summary(model1)
###############################################################################
############################## Diagnóstico ####################################
######################## Modelo (b) ###########################################
###############################################################################
x11()
Graph1=dados %>%
  mutate(fitted = predict(model1),
         resid = residuals(model1, type = "pearson")) %>%
  ggplot(aes(fitted, resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color="red",size=1) +
  labs(x = "Fitted Values", y = "Residuals", title = "(b)") + 
  theme(legend.position = "none", legend.title = element_text(size = 17),
        legend.text = element_text(size = 17),
        axis.title = element_text(size = 30),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 25),
        plot.title = element_text(size = 32)) 
Graph1
###############################################################################
############################## Diagnóstico ####################################
######################## Modelo (b) ###########################################
###############################################################################
x11()
qplot(massaseca, predict(model1), data = dados) +
  geom_abline(yintercept = 0, colour="red", size=1) + 
  labs(x = "Fitted Values", y = "Weight (mg)",title = "(b)") + 
  theme(legend.position = "none", legend.title = element_text(size = 17),
        legend.text = element_text(size = 17),
        axis.title = element_text(size = 30),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 25),
        plot.title = element_text(size = 32)) 
###############################################################################
################################### Ajuste ####################################
###############################################################################
fit2.gamm <- gamm(massaseca~factor(Trat)+s(Tempo,k=10,bs="ps",m=2),
                  random=list(id=pdIdent(~1)),data=dados)
summary(fit2.gamm$gam)
comp_lme3 = fit2.gamm$lme
fitted.values3 = fit2.gamm$gam$fitted.values
predictions3 = predict(fit2.gamm$gam,se.fit=TRUE)
###############################################################################
############################## Diagnóstico ####################################
######################## Modelo (c) ###########################################
###############################################################################
Graph2=dados %>%
  mutate(fitted = predict(fit2.gamm$lme),
         resid = residuals(fit2.gamm$lme, type = "pearson")) %>%
  ggplot(aes(fitted, resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color="red",size=1) +
  labs(x = "Fitted Values", y = "Residuals", title = "(c)") + 
  theme(legend.position = "none", legend.title = element_text(size = 17),
        legend.text = element_text(size = 17),
        axis.title = element_text(size = 30),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 25),
        plot.title = element_text(size = 32)) 
Graph2
###############################################################################
############################## Diagnóstico ####################################
######################## Modelo (c) ###########################################
###############################################################################
x11()
qplot(massaseca, predict(fit2.gamm$lme), data = dados) +
  geom_abline(yintercept = 0, colour="red", size=1) + 
  labs(x = "Fitted Values", y = "Weight (mg)",title = "(c)") + 
  theme(legend.position = "none", legend.title = element_text(size = 17),
        legend.text = element_text(size = 17),
        axis.title = element_text(size = 30),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 25),
        plot.title = element_text(size = 32)) 
###############################################################################
################################### Ajuste ####################################
###############################################################################
fit3.gamm <- gamm(massaseca~factor(Trat)+s(Tempo,k=10,bs="ps",m=2),
                  random=list(id=pdSymm(~Tempo)),data=dados)
summary(fit3.gamm$gam)
comp_lme2 = fit3.gamm$lme
fitted.values2 = fit3.gamm$gam$fitted.values
predictions2 = predict(fit3.gamm$gam,se.fit=TRUE)
###############################################################################
############################## Diagnóstico ####################################
######################## Modelo (d) ###########################################
###############################################################################
Graph3=dados %>%
  mutate(fitted = predict(fit3.gamm$lme),
         resid = residuals(fit3.gamm$lme, type = "pearson")) %>%
  ggplot(aes(fitted, resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color="red",size=1) +
  labs(x = "Fitted Values", y = "Residuals", title = "(d)") + 
  theme(legend.position = "none", legend.title = element_text(size = 17),
        legend.text = element_text(size = 17),
        axis.title = element_text(size = 30),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 25),
        plot.title = element_text(size = 32)) 
Graph3
###############################################################################
############################## Diagnóstico ####################################
######################## Modelo (d) ###########################################
###############################################################################
x11()
qplot(massaseca, predictions2$fit, data = dados) +
  geom_abline(yintercept = 0, colour="red", size=1) + 
  labs(x = "Fitted Values", y = "Weight (mg)",title = "(d)") + 
  theme(legend.position = "none", legend.title = element_text(size = 17),
        legend.text = element_text(size = 17),
        axis.title = element_text(size = 30),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 25),
        plot.title = element_text(size = 32)) 
###############################################################################
################################### Ajuste ####################################
###############################################################################
fit4.gamm <- gamm(massaseca~factor(Trat)+s(Tempo,k=10,bs="ps",m=2,
                                           by=factor(Trat)),
                  random=list(id=pdSymm(~Tempo)),data=dados)
summary(fit4.gamm$lme)
comp_lme = fit4.gamm$lme
fitted.values = fit4.gamm$gam$fitted.values
predictions = predict(fit4.gamm$gam,se.fit=TRUE)
###############################################################################
############################## Comparação #####################################
###############################################################################
anova(fit2.gamm$lme,fit3.gamm$lme,fit4.gamm$lme)

minus2.RLRT.4vs3 <- -2*(logLik(fit3.gamm$lme,REML=TRUE)-logLik(fit4.gamm$lme,REML=TRUE))
minus2.RLRT.4vs3
0.5*qchisq(.90,0)+0.5*qchisq(.90,1)

minus2.RLRT.4vs2 <- -2*(logLik(fit2.gamm$lme,REML=TRUE)-logLik(fit4.gamm$lme,REML=TRUE))
minus2.RLRT.4vs2
0.5*qchisq(.90,0)+0.5*qchisq(.90,1)

minus2.RLRT.4vs1 <- -2*(logLik(model1,REML=TRUE)-logLik(fit4.gamm$lme,REML=TRUE))
minus2.RLRT.4vs1
0.5*qchisq(.90,0)+0.5*qchisq(.90,1)  
###############################################################################
############################## Diagnóstico ####################################
######################## Modelo (e) ###########################################
###############################################################################
Graph4=dados %>%
  mutate(fitted = predict(fit4.gamm$lme),
         resid = residuals(fit4.gamm$lme, type = "pearson")) %>%
  ggplot(aes(fitted, resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color="red",size=1) +
  labs(x = "Fitted Values", y = "Residuals", title = "(e)") + 
  theme(legend.position = "none", legend.title = element_text(size = 17),
        legend.text = element_text(size = 17),
        axis.title = element_text(size = 30),
        axis.text.x = element_text(size = 25,color = "black", hjust=1),
        axis.text.y = element_text(size = 25,color = "black", hjust=1),
        axis.text = element_text(size = 35),
        plot.title = element_text(size = 32)) 
x11()
Graph4
###############################################################################
############################## Diagnóstico ####################################
######################## Modelo (e) ###########################################
###############################################################################
x11()
qplot(massaseca, predictions$fit, data = dados) +
  geom_abline(yintercept = 0, colour="red", size=1) + 
  labs(x = "Fitted Values", y = "Weight (mg)",title = "(e)") + 
  theme(legend.position = "none", legend.title = element_text(size = 17),
        legend.text = element_text(size = 17),
        axis.title = element_text(size = 30),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 25),
        plot.title = element_text(size = 32)) 
table(dados$Teor)