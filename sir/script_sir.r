# Simulações numéricas no modelo SIR
# https://biologiadeprogramas.wordpress.com/2020/03/22/o-modelo-sir-e-o-achatamento-da-curva/

# packages
library(deSolve)
library(tidyverse)

#----------------------------------------------------------

# SIR
# mais informacoes aqui: https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology

SIR = function(t, y, parms)
{
  with(as.list(c(y, parms)), 
       {
         N = S + I + R
         
         dS = -beta * I * S / N
         
         dI = beta * I * S / N - gamma * I
         
         dR = gamma * I
         
         return(list(c(S = dS, I = dI, R = dR)))
         
       })
}



# -------------------------------------------------------------------------



#vamos rodar dois cenarios

#aqui, cada pessoa infecta, em media, uma pessoa por dia
t1 = deSolve::ode(y = c(S = 22e4, I = 3, R = 0),
                  times =  seq(from = 1, to = 200, by = .1),
                  func = SIR,
                  parms = c(beta = 1, gamma = 1/15))
head(t1)

#convertendo os dados para porcentagem
dat1 = as.data.frame(t1)
head(dat1)
dat1[,-1] = dat1[,-1]/(22e4+3)*100
head(dat1)

#pico da infeccao
peak1 = which.max(t1[,"I"])
t1[peak1,]
round(dat1[peak1,])

# plot
dat1 %>% 
  tibble::as_tibble() %>% 
  ggplot() + 
  geom_line(aes(x = time, y = S), col = "dodgerblue") +
  geom_line(aes(x = time, y = I), col = "red") +
  geom_line(aes(x = time, y = R), col = "gray10") +
  theme_bw()

par(las=1, bty="l", mgp=c(3,1,0), mar=c(5,5,3, 2),
      oma=c(0,0,0,0),
      cex.lab=1.5, mfrow=c(1,1), cex.main=2)
  cols = c("dodgerblue","red","grey10")
  matplot(x=dat1$time,y=dat1[,-1], type="l", lty=1, 
          col=cols, lwd=2, xlab="Tempo (dias)", 
          ylab="Porcentagem da população",
          main="Cenário de alto contato")
  legend("right", col=cols, lty=1, lwd=2, bty="n",
         legend=c("Saudáveis", "Infectados (doentes)", "Recuperados"))


#comparando cenarios
library(plotrix)
library(scales)
fcols = alpha(c("red", "dodgerblue"), 0.75)
  par(las=1, bty="l", mgp=c(3,1,0), mar=c(5,5,3, 2),
      oma=c(0,0,0,0),
      cex.lab=1.5, mfrow=c(1,1), cex.main=2)
  
  plot(0, type="n", ylim=c(0,80), xlim=c(0,200),
     xlab="Tempo (dias)", 
     ylab="Porcentagem de infectados",
     main="Comparação entre cenários")
  
  #altas gambiarras pra desenhar uma area colorida
  polygon(x=c(0, dat1$time,200),
          y=c(0, dat1$I, 0), col = fcols[1], border=NA)
  
  polygon(x=c(0, dat2$time,200),
          y=c(0, dat2$I, 0), col = fcols[2], border=NA)

  legend("topright", col=fcols, pch=15, bty="n",
         legend=c("Alto contato", 
                "Contato reduzido"))

#um grafico soh com os dois cenarios
par(las=1, bty="l", mgp=c(3,1,0), mar=c(4,5,3, 2),
    oma=c(2,2,0,0),
    cex.lab=2, mfrow=c(2,1), cex.main=2)
cols = c("dodgerblue","red","grey10")
matplot(y=t1[,-1],x=t1[,1], type="l", lty=1, 
        col=cols, yaxt="n",
        lwd=2, xlab="", ylab="",
        main="Alto contato")
legend("right", col=cols, lty=1, lwd=2, bty="n",
        legend=c("Saudáveis", "DOENTES", "Recuperados"))

axis(side=2, at=seq(from=0, to=10, by=2)*1e5, 
     labels = c("zero", "200 mil", "400 mil", "600 mil",
                "800 mil", "1 milhão"))

matplot(y=t2[,-1],x=t2[,1], type="l", lty=1, 
        col=c("dodgerblue","red","grey10"), yaxt="n",
        lwd=2, xlab="", ylab="",
        main="Contato reduzido")

axis(side=2, at=seq(from=0, to=10, by=2)*1e5, 
    labels = c("zero", "200 mil", "400 mil", "600 mil",
                "800 mil", "1 milhão"))

legend("right", col=cols, lty=1, lwd=2, bty="n",
       legend=c("Saudáveis", "DOENTES", "Recuperados"))

par(las=3)
mtext(side=2, outer=TRUE, cex=2.5, line = 0, 
      text = "Número de pessoas")
par(las=1)
mtext(side=1, outer=TRUE, cex=2.5, line = 0, 
      text = "Tempo (dias)")

#==========================================================
