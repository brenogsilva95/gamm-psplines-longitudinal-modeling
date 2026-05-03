rm(list=ls())
########################################################################
#################################### Pacotes ###########################
########################################################################
library(nlme)
library(ggplot2)
library(GGally)
library(splines)
library(fields)
library(lattice)
require(ISLR)
library(grid)
library(dplyr)
library(MASS)
library(mgcv)
library(RVAideMemoire)
library(SemiPar)
library(goftest)
library(mvShapiroTest)
library(hnp)
library(lme4)
require(gamm4)
#########################################################################
######################## Organização do conjunto de dados ###############
#########################################################################
dados = read.table("df_gamm.csv", header = T, sep=";", dec=",")
dados$G  <- factor(dados$G)
dados$S  <- factor(dados$S)
dados$S1 <- factor(dados$S1)
########################################################################
###################### Análise exploratória gráfica dos dados ##########
########################################################################
########################################################################
# Gráfico de painéis por individuo sendo suas cores para cada grupo#####
########################################################################
x11()
ggplot(dados, aes(x = Tempo, y = Peso)) +
  geom_line(aes(colour = G)) +
  facet_wrap(~ S) +
  scale_x_continuous(breaks = seq(3, 12, 3))
########################################################################
########################## Gráfico de perfis ###########################
########################################################################
x11()
par(mfrow=c(2,3))
xyplot(Peso ~ (Tempo)|factor(G),groups=S, scale= list(alternating=F),
       type = "a", between = list(x=1,y=1),
       space = "right", points = FALSE, lines = TRUE, 
       main = " Evolução do peso no tempo para cada tratamento",
       xlab = "Semanas" )

#####################################################################
#####################################################################
####################### Evolução do peso individual no tempo ########
#####################################################################
x11()
xyplot(Peso ~ (Tempo)|S, data = dados, between = list(x=1,y=1),
       space = "right", main = " Evolução do peso no tempo para cada individuo",
       xlab = "Tempo (Semanas)")
update(trellis.last.object(),
       strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
       par.strip.text = list(cex = 0.75),
       aspect = "iso")

##########################################################################
##########################################################################
################Evolução do peso no tempo de cada individuo por grupo para 
############################cada intercepto diferente ####################
meu.panel = function(x,y,subscripts) {
  panel.xyplot(x,y,pch=19, col="gray")
  panel.loess(x, y,col="black",lty=1,lwd=2)
}

trellis.par.set(strip.background=list(col="gray"),
                box.rectangle=list(col="black"),
                add.text=list(font=3),
                box.umbrella=list(col="black"),
                box.dot=list(col="black"),
                plot.symbol=list(col="black",pch=1,cex=1))
x11()
xyplot(Peso ~ (Tempo)|G, data = dados,
       prepanel = function(x, y) prepanel.loess(x, y),scales=list(cex=1.7),
       panel = meu.panel,xlab = list("Tempo", cex=1.6),
       ylab = list("Variável Resposta", cex=1.6), text = list(cex=1.6))



##################################################################
##################Gráfico de interação sujeitos ##################
##################################################################
interaction <- dados %>%
  dplyr::select(Peso, Tempo, S) %>%
  group_by(S, Tempo) %>%
  summarise(Average = mean(Peso))

x11()
ggplot(interaction, aes(x=Tempo, y=Average, group=S)) + 
  theme(axis.title = element_text(size=12, face = "bold", vjust=10)) +   
  geom_line() +
  xlab("Tempo (Semanas)") +
  ylab("Peso")  +
  theme_bw() + theme(axis.title = element_text(size = 30),
axis.text = element_text(size = 26))

####################################################################
############ Colocar linha na 6º Semana#############################
#gr2 + geom_vline(aes(xintercept=6))
####################################################################

##################################################################
#######################Gráfico de interação gaiolas ##############
##################################################################
x11()
xyplot(Peso ~ (Tempo),groups=S1,
       type = "a", auto.key=list(space = "right", 
                                  points = FALSE, lines = TRUE, colorkey = S),
       main = " Graf. de perfis individuais das gaiolas em função do (tempo)")

##################################################################
########################Gráfico de interação grupos ##############
##################################################################
x11()
xyplot(Peso ~ (Tempo),groups=G,
       type = "a", auto.key=list(colorkey = G,
       space = "right", points = FALSE, lines = TRUE),
       main = " Graf. de perfis dos grupos da evolução do peso em função do tempo",xlab="Tempo(Semanas)")


#####################################################################
##################### BoxPlot variação do peso para cada individuo.##
#####################################################################
x11()
ggplot(data=dados, aes(x=factor(S), y=Peso, fill = G)) + geom_boxplot(fill="black",color="gray") +
  xlab("Tempo (Semanas)") +
  ylab("Peso") + theme_bw() + theme(axis.title = element_text(size = 18),
                                    axis.text = element_text(size = 14)) 


#####################################################################
##################### BoxPlot variação do peso para cada Tempo.##
#####################################################################

x11()
ggplot(data=dados, aes(x=factor(Tempo), y=Peso)) + geom_boxplot(fill="black",color="gray", show.legend = FALSE) +
  xlab("Tempo (Semanas)") +
  ylab("Peso") + 
  stat_summary(fun.y=mean, colour="white", geom="point", 
               shape=18, size=5,show_guide = FALSE) +
  theme_bw() + theme(axis.title = element_text(size = 30),
                     axis.text = element_text(size = 26)) 

#######################################################################
#######################Variação do peso no tempo para cada grupo#######
#######################################################################
mediasCona = tapply(dados$Peso, dados$G, mean)

x11()
ggplot(data=dados, aes(x=G, y=Peso), mediasCona)+geom_boxplot(fill="black",color="gray", show.legend = FALSE) +
  xlab("Tratamentos")  +
  stat_summary(fun.y=mean, colour="white", geom="point", 
               shape=18, size=5,show_guide = FALSE) +
  ylab("Peso") +
  theme_bw() + theme(axis.title = element_text(size = 30),
                     axis.text = element_text(size = 26))


######################################################################
#####################Gráfico de dispersão para os dados###############
######################################################################

disp=ggplot(dados, aes(x = Tempo, y = Peso)) + geom_point(fill="black",color="black", show.legend = FALSE)+
  xlab("Tempo (Semanas)") +
  ylab("Peso") +
  theme_bw() + theme(axis.title = element_text(size = 18),
                     axis.text = element_text(size = 14))
x11()
disp + theme(legend.position="none")
######################################################################
##########Histograma da variavel resposta para os tratamentos#########
######################################################################
x11()
ggplot(data = dados, aes(x = Peso)) +
  geom_histogram(fill="black",color="gray",show.legend = FALSE) +
  xlab("Peso") +
  ylab("Densidade")+
  facet_wrap(~G) +
  theme_bw() + theme(axis.title = element_text(size = 18),
                     axis.text = element_text(size = 14))

###################################################################################
################3 Histograma da variável resposta para os anos#####################
###################################################################################
x11()
ggplot(data = dados, aes(x = Peso)) +
  geom_histogram(fill="black",color="gray",show.legend = FALSE) +
  xlab("Peso") +
  ylab("Densidade") +
  facet_wrap(~Tempo) +
  theme_bw() + theme(axis.title = element_text(size = 18),
                     axis.text = element_text(size = 14))

##################################################################################
###############################MEDIDAS RESUMO#####################################
##################################################################################
##### Estatisticas resumo Variavel resposta para cada tratamento ####
tapply(Peso,G,summary)
##### Desvio da Variavel resposta para cada tratamento ####
tapply(Peso,G,sd)

##### Estatisticas resumo Variavel resposta para cada gaiola ####
tapply(Peso,S1,summary)
##### Desvio da Variavel resposta para cada gaiola ####
tapply(Peso,S1,sd)

######################################################################################
######################### Escolha das matrizes de correlação #########################
######################################################################################

gamm0 <- gamm(Peso~factor(G)+s(Tempo,k=40,bs="ps",m=2,
                               by=factor(G)),
              random=list(S=pdSymm(~Tempo)),data=dados)
summary(gamm0$gam)

gamm1 <- gamm(Peso~factor(G)+s(Tempo,k=40,bs="ps",m=2,
                                   by=factor(G)),
                  random=list(S=pdSymm(~Tempo)),correlation = corAR1(form = ~ 1 | S ),data=dados)
summary(gamm1$gam)

gamm2 <- gamm(Peso~factor(G)+s(Tempo,k=40,bs="ps",m=2,
                               by=factor(G)),
              random=list(S=pdSymm(~Tempo)),correlation = corCompSymm(0.3, form= ~ 1 | S ),data=dados)
summary(gamm2$gam)

gamm3 <- gamm(Peso~factor(G)+s(Tempo,k=40,bs="ps",m=2,
                               by=factor(G)),
              random=list(S=pdSymm(~Tempo)),correlation = corARMA(p=1, q=1, form= ~ 1 | S ),data=dados)
summary(gamm3$gam)

anova(gamm0$lme,gamm1$lme,gamm2$lme,gamm3$lme)

model1 <- lme(Peso~factor(G)+factor(Tempo),random=list(S=pdIdent(~1)),
               data=dados)
 summary(model1)

##############################################################################
##############################Modelo com intercepto aleatório#################
##############################################################################
model1 <- lme(Peso~factor(Tempo),random=list(S=pdIdent(~1)),
              data=dados)

summary(model1)
fit.model1=model1$fitted
# o modelo acima não é capaz de descrever as trajetorias dos pesos dos sujeitos.
# o modelo acima assume que todos os sujeitos tem a mesma taxa de crescimento sendo
# este linear.
## Modelo com intercepto aleatório por tratamentos
# plot do modelo ajustado
x11()
xyplot(fitted(model1) ~ Tempo|factor(G),groups=S,
       col=1,
       lwd=1,t="b",pch=19,data=dados,main="Modelo com intercepto aleatório por tratamentos",cex=.35)
# modelo aditivo, em que a função de suavização que explica a trajetoria do peso
# dos sujeitos é uma p-splines, por meio da representação do modelo misto.
# Matriz positiva definida com todos elementos da diagonal igual a um (identidade).
#Modelo de interceptação aleatória suavizado.
# Ao inves de linhas retas, ajustamos curvas suavez que só diferem em suas interceptações.
fit2.gamm <- gamm(Peso~factor(G)+s(Tempo,k=25,bs="ps",m=3),
                  random=list(S=pdIdent(~1)),data=dados)
summary(fit2.gamm$gam)


# Tendências ajustadas para o modelo de interceptação aleatória suave por tratamento
a2 <- xyplot(Peso ~ Tempo|factor(G),groups=S,col=4,
             lwd=1,t="p",pch=19,data=dados,main="Ajustes do modelo de interceptação aleatória suave por tratamento",cex=.35)
b2 <- xyplot(fitted(fit2.gamm$lme) ~ Tempo|factor(G),groups=S,col=1,
             lwd=1,pch=19,data=dados,main="Ajustes do modelo de interceptação aleatória suave por tratamento",cex=.35,type="a")
X11()
a2 + as.layer(b2)
#O grafico anterior evidencia que mesmo considerando as curvas suavizadas, 
#não conseguimos capturar as trajetórias individuais

#Assim, partimos então para o modelo linear com diferenças individuais (Com interecepto e inclinação)
# este modelo é uma extensão do anterior.
#Agora permitimos não apenas interceptar aleatoriamente, mas também
# as inclinações aleaotorias. Este modelo é bem mais flexiviel, permitindo
# o ajuste de curvas não tão paralelas.

fit3.gamm <- gamm(Peso~factor(G)+s(Tempo,k=25,bs="ps",m=3),
                  random=list(S=pdSymm(~Tempo)),data=dados)
summary(fit3.gamm$gam)

#pdSymm é utilizado para representar a a estrutura da matriz de variancias
# e covariancia dos efeitos aleatorios (Matriz geral positiva definida)
# Interceptações aleatórias e inclinações e efeito suave para o tempo


## Tendências individuais ajustadas para o modelo aleatório de interceptação 
#e inclinação suave por tratamento
a2 <- xyplot(Peso ~ Tempo|factor(G),groups=S,col=tim.colors((c(1:5))),
             lwd=1,t="p",pch=19,data=dados,main=" ",cex=.35)

b3 <- xyplot(fitted(fit3.gamm$lme) ~ Tempo|factor(G),groups=S,col=1,
             lwd=1,pch=19,data=dados,main=" ",cex=.35,type="a")
x11()
a2 + as.layer(b3)

#Curva por modelo de tratamento
#Curva por interação do fator: O objetivo final do estudo era os efeitos a longo prazo de
#cinco tratamentos diferentes, visando a qualidade de vida dos individuos. 
#Portanto, curvas individuais para cada tratamento são de interesse. Podemos
#estender o modelo permitindo uma interação de uma variável de fator com um preditor contínuo. o
#modelo é da forma:

fit4.gamm <- gamm(Peso~factor(G)+s(Tempo,k=25,bs="ps",m=3,
                                             by=factor(G)),
                  random=list(S=pdSymm(~Tempo)),data=dados)
summary(fit4.gamm$gam)

#########################################################################
################ Valores do Parâmetro Lambda ############################
#########################################################################
fit4.gamm$gam$sp
###################################################################################

###################################################################################
##########  Modelo sem considerar a estrutura de correlação nos dados##############
###################################################################################
prod.gam3<-gamm(Peso ~ s(Tempo,bs="ps",m=2,k=8), data = dados) 

##################################################################################
############################### Medidas Resumo ###################################
##################################################################################
summary(prod.gam3$gam)
#################################################################################
############# Gráfico Suavização sem considerar a estrutura######################
#################################################################################
x11()
plot(prod.gam3$gam,residuals = TRUE,scheme=1 ,ylab = "s(Tempo - Semanas,2.637)", xlab = "Tempo - Semanas")

#################################################################################
##################################### Qualidade do Modelo #######################
#################################################################################
x11()     
par(mfrow=c(2,2))
gam.check(prod.gam3$gam)
################################################################################
################################ Checando pressuposições dos resíduos###########
################################################################################
correl<-prod.gam3$gam$resid
x11()
par(cex=2)
acf(correl, main="")  
prod.gam3$gam$sp
################################################################################
################ Modelo considerando a estrutura de correção ###################
################################################################################

prod.gam3.3<-gamm(Peso ~ s(Tempo,bs="ps",m=2,k=8), data = dados,  
                  correlation=corARMA(form= ~ Tempo|S,p=1,q=0))   #correlation=corAR1())
(prod.gam3.3)

residuos=residuals(prod.gam3.3$lme,type="n")

anova(prod.gam3.3)


#################################
x11()
plot(residuos)

#################################################################################
##################################### Qualidade do Modelo #######################
#################################################################################
x11()     
par(mfrow=c(2,2))
gam.check(prod.gam3.3$gam)

################################################################################
################################### Checando pressuposicoes dos residuos #######
################################################################################
x11()
par(cex=2)
acf(residuos, main= "", ylab="FCA")
################################################################################
###################### Suavização com a estrutura ##############################
################################################################################
x11()
par(cex=2)
plot(prod.gam3.3$gam,residuals = FALSE,ylab = "s(Tempo (Semanas),5.89)",scheme=1 ,xlab = "Tempo (Semanas)")
summary(prod.gam3.3$gam)
###############################################################################
########################################### Maior tempo #######################
###############################################################################
prod.gam3.3$gam$sp
###############################################################################
################################# Graus de Liberdade Efetivos #################
###############################################################################
gam.check(prod.gam3.3$gam)

######################################################################################
#####Comparando modelos para determinar qual desses modelos trabalha melhor com ######
# a estrutura de correlação. Neste caso com AR(1)#####################################
######################################################################################
anova(prod.gam3$lme,prod.gam3.3$lme,prod.gam3.4$lme)


########################################################################################
##############################SUAVIZAÇÃO NO GAMM########################################
########################################################################################
fit11 <- gamm4(Peso ~ s(Tempo,bs="ps",m=2,k=8), data = dados,  
               correlation=corARMA(form= ~ Tempo|S,p=1,q=0))  
fit11$gam$sp
summary(fit11$gam)
x11()
plot(fit11$gam, scheme = 1)

############ Tendências individuais ajustadas para o modelo aleatório 
####de interceptação e declive suave por tratamento ###################

b4 <- xyplot(fitted(fit4.gamm$lme) ~ Tempo|factor(G),groups=S,col=tim.colors(c(1:5)),
             lwd=1.8,pch=19,data=dados,main="Tratamentos",cex=.35,type="a")
x11()
a2 + as.layer(b4)
## Tendências globais ajustadas para o modelo aleatório 
#de interceptação e inclinação suave por tratamento
x11()
par(cex=2)
plot(fit4.gamm$gam,1,scheme=1, main = "Função de suavização estimada", xlab="Tempo (Semanas)")

# O modelo acima pode ser feito utilizando LME.
##------------------------------------------------------------------##
##### Construindo a matriz X e Z ####################
##------------------------------------------------------------------##
##------------------------------------------------------------------##

bspline <- function(x,xl,xr,ndx,bdeg){
  dx <- (xr-xl)/ndx
  knots <- seq(xl-bdeg*dx,xr+bdeg*dx,by=dx)
  B <- spline.des(knots,x,bdeg+1,0*x,outer.ok=TRUE)$design
  output <- list(knots=knots,B=B)
  return(output)
}


mixed.model.B<-function(x,xl,xr,ndx,bdeg,pord,type="Eilers"){
  Bbasis=bspline(x,xl,xr,ndx,bdeg)
  B <- Bbasis$B
  m=ncol(B)
  D=diff(diag(m),differences=pord)

  if(type=="Eilers"){
    Z <- B%*%t(D)%*%solve(D%*%t(D))
  }else if(type=="SVD"){ print("SVD method")
    P.svd=svd(t(D)%*%D)
    U=(P.svd$u)[,1:(m-pord)]
    d=(P.svd$d)[1:(m-pord)]
    Delta=diag(1/sqrt(d))
    Z=B%*%U%*%Delta
  }
  X=NULL
  for(i in 0:(pord-1)){
    X=cbind(X,x^i)
  }
  output <- list(X=X,Z=Z)
  return(output)
}

#Para os dados, vamos ajustar dois modelos aninhados 
#com 5 e 6 componentes de variância, modelos (4.3) e (4.5), mas 
#incluindo uma interação(curva) do fator G: 
# onde y_ij denota o peso do iésimo sujeito em relação ao tempo j,
# para i=1,...,57 e j está  entre 1 e 12. 
# f1 denota a média da curva para os índividuos no grupo 1
# f2 denota a média da curva para os índividuos no grupo 2
# f3 denota a média da curva para os índividuos no grupo 3
# f4 denota a média da curva para os índividuos no grupo 4
# f5 denota a média da curva para os índividuos no grupo 5

###### ai1 e ai2 
#são os interceptos e declives aleatórios e y gi (xij) é o específico
#desvio do sujeito com relação à curva média de seu grupo de tratamento.
#O interesse neste estudo são os efeitos do tratamento no peso ao longo do tempo e a
# resposta dos indivíduos aos tratamentos.

X=model.matrix(Peso~factor(G)*Tempo)
 treatment=factor(G)
 
 MM=mixed.model.B(Tempo,min(Tempo)-0.5,max(Tempo)+0.5,25,3,3,type="Eilers")
 
 Z=MM[[2]]
 dim(Z)
 
 Id=factor(rep(1,length(Peso)))
 
 Z.block4=list(G=pdIdent(~Z-1),S=pdSymm(~Tempo))
 
 data.fr <- groupedData(Peso ~ X[,-1] | Id, data = data.frame(Peso,X,Z,S,Tempo))
 model4 <- lme(Peso~X[,-1],data=data.fr,random=Z.block4) 
 summary(model4)
##------------------------------------------------------------------##

##------------------------------------------------------------------##
##------------------------------------------------------------------##

#O modelo mais flexível é aquele que permite modelar as
#diferenças específicas entre indivíduos usando termos suaves não paramétricos.
# Assim, o modelo sugerido é o curvas especificas por individuos.

#Este modelo é muito complexo para caber usando gamm, porque precisaríamos 
#definir 57 variáveis(sujeitos) para obter uma curva individual para cada sujeito 
#No entanto, isso é muito simples usando lme.

X=model.matrix(Peso~factor(G)*Tempo)
MM=mixed.model.B(Tempo,min(Tempo)-0.5,max(Tempo)+0.5,10,3,3,type="Eilers")
 
Z=MM[[2]]
dim(Z)
Id=factor(rep(1,length(Peso)))

MM.sujeito=mixed.model.B(Tempo,min(Tempo)-0.5,max(Tempo)+0.5,10,3,3,type="Eilers")
treatment=factor(G)
Z.sujeito=MM.sujeito[[2]] # create a specific sub-matrix per each individual
Z.block5=list(treatment=pdIdent(~Z-1),S=pdSymm(~Tempo),S=pdIdent(~Z.sujeito-1))

data.fr5 <- groupedData(Peso ~ X[,-1] | Id,
                        data = data.frame(Peso,X,Z,Z.sujeito,S,Tempo))
model5 <- lme(Peso~X[,-1],data=data.fr5,random=Z.block5)
summary(model5)

########################################################################
#################################### Checando efeitos ###################
########################################################################
names(ranef(model5))

apply(ranef(model5)$S, 2, shapiro.test)
apply(ranef(model5)$treatment, 2, shapiro.test)


lapply(1:2, function(i){media=mean(ranef(model5)$S[,i])
dp=sd(ranef(model5)$S[,i])
ks.test(ranef(model5)$S[,i],"pnorm",media,dp)})

lapply(1:2, function(i){media=mean(ranef(model5)$S[,i])
dp=sd(ranef(model5)$S[,i])
cvm.test(ranef(model5)$S[,i],"pnorm",media,dp)})

lapply(1:2, function(i){media=mean(ranef(model5)$S[,i])
dp=sd(ranef(model5)$S[,i])
ad.test(ranef(model5)$S[,i],"pnorm",media,dp)})

lapply(1:25, function(i){media=mean(ranef(model5)$treatment[,i])
dp=sd(ranef(model5)$treatment[,i])
ks.test(ranef(model5)$treatment[,i],"pnorm",media,dp)})

lapply(1:25, function(i){media=mean(ranef(model5)$treatment[,i])
dp=sd(ranef(model5)$treatment[,i])
cvm.test(ranef(model5)$treatment[,i],"pnorm",media,dp)})

lapply(1:25, function(i){media=mean(ranef(model5)$treatment[,i])
dp=sd(ranef(model5)$treatment[,i])
ad.test(ranef(model5)$treatment[,i],"pnorm",media,dp)})

#############################################################################
###################Histograma quanto a suposição de distribuição normal #####
#############################################################################
x11()
hist(ranef(model5)$S[,2])
hist(ranef(model5)$S[,1])
####################################################################
##############################aleatorios ###########################
####################################################################
random.effects(model5)
## Tendências globais ajustadas para o modelo aleatório 
#de interceptação e inclinação suave por tratamento

b5 <- xyplot(fitted(model5) ~ Tempo|factor(G),groups=S,col=(c(1,2,4)),
             lwd=1.8,pch=19,data=dados,main="Treatment",cex=.35,type="a")
x11()
a2 + as.layer(b5)
######################################################################################
######################################## Curvas Médias ###############################
######################################################################################
x11()
par(cex=2)
xyplot(model5$fitted[,2] ~ Tempo,groups=G,col=1,lty=1:5,
       lwd=2,pch=19,data=dados,main="  ",
       cex=.35,type="a",ylab="Peso",key=list(corner=c(0,1),cex=1,lines=list(col=1, lty=1:5, lwd=2),
                                 text=list(c("Controle infecção","Tratados (13cH)","Tratados (6cH)","Controle infecção","Tratados (3cH)"))))


#Os graficos mostram os efeitos estimados da curva média da amostra por tratamento. Podemos observar
#que nos grupos 2,3,4,5 o efeito é menor em relação ao controle
# Ja para o controle o efeito é maior do que os outros  grupos.
#anos.

###################################################################

## ------------------------------------------------------------------------------
# without treatment random effects (i.e. a single curve for all treatment levels) 
Z.block5.2=list(Id=pdIdent(~Z-1),S=pdSymm(~Tempo),S=pdIdent(~Z.sujeito-1))
data.fr <- groupedData(Peso ~ X[,-1] | Id,
                       data = data.frame(Peso,X,Z,Z.sujeito,S,Tempo))
model5.2 <- lme(Peso~X[,-1],data=data.fr,random=Z.block5.2)
summary(model5.2)
## Average mean curves per treatment for models 4.5 (with no treatment random effect)
x11()
par(cex=2)
xyplot(model5.2$fitted[,2]~ Tempo,groups=G,col=1,lty=c(2,1,3,4,5),
       lwd=2,pch=19,data=dados,
       main="",cex=10,type="a",ylab="Peso",
       key=list(corner=c(0,1),cex=1,lines=list(col=1, lty=c(2,1,3,4,5), lwd=2),
                text=list(c("Controle infecção","Tratados (13cH)","Tratados (6cH)","Controle infecção","Tratados (3cH)"))))

x11()
xyplot(model5.2$fitted[,1]~ Tempo,groups=G,col=1:5,lty=1:5,
       lwd=3,pch=19,data=dados,main="Efeitos Fixos",cex=.35,type="a",
       key=list(corner=c(0,1),cex=1,lines=list(col=1:5, lty=1:5, lwd=3),
                text=list(c("Controle infecção","Tratados (13cH)","Tratados (6cH)","Controle infecção","Tratados (3cH)"))))
x11()
xyplot(model5.2$fitted[,1]-model5.2$fitted[,2]~ Tempo,groups=G,col=1:3,lty=1:3,
       lwd=3,pch=19,data=dados,main="Efeitos Fixos - População com Efeitos Aleatorios",cex=.35,type="a",
       key=list(corner=c(0,1),cex=1,lines=list(col=1:5, lty=1:5, lwd=3),
                text=list(c("Controle infecção","Tratados (13cH)","Tratados (6cH)","Controle infecção","Tratados (3cH)"))))


