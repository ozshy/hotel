### R packages needed
library(nleqslv)# package numerical solution of a system of equations
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot

#### Hotelling paper begins ####


####### integrating prices and profits into a single chart
## b=0 => 1-b=0 ##
# plotting pa and pb as functions of a\in[0,1-b]
L = 1 #Length of the linear street
b = 0.0*L # distance of firm B from 1 (from the right) expressed as a fraction of the interval
tau = 1 # transportation cost parameter
#
# vector of "a" as firm A gets closer to firm B located at "L-b"
(a.vec = seq(0, L-b, (L-b)/100))
# vectors of pa and pb solutions to be plotted
pa.vec = rep(NA, length(a.vec))# initialize
pb.vec = rep(NA, length(a.vec))# initialize
xhat.vec = rep(NA, length(a.vec))# initialize
profita.vec = rep(NA, length(a.vec))# initialize
profitb.vec = rep(NA, length(a.vec))# initialize

for (i in 1:length(a.vec)){
  # the function is a 2-dim vector corresponding to equations (8a) and (8b) in the paper (B does not undercut A and A does not undercut B). x[1] is pa, x[2] is pb
  upe.fn = function(x){c(x[2]*(L-(tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[1]-tau*(L-b-a.vec[i]))*L, x[1]*((tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[2]-tau*(L-b-a.vec[i]))*L)
  }
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb.vec = unlist(nleqslv(c(0,0), upe.fn)[1]);
  # place it in pa.vec and pb.vec to be plotted
  pa.vec[i] = papb.vec[1]; # UPE pa
  pb.vec[i] = papb.vec[2]  # UPE pb
  # market share of firm A
  xhat.vec[i] = (tau*(L+a.vec[i]-b)-papb.vec[1] + papb.vec[1])/(2*tau)
}# end loop on all i (all a.vec)
# profits firm A and firm B
profita.vec = pa.vec*xhat.vec
profitb.vec = pb.vec*(L-xhat.vec)# profit firm A
# plot pa.vec and pb.vec as functions of a.vec
# dataframe for pa and pb solution when b=0 (firm B is located at L)
papb_00.df = data.frame(a.vec, pa.vec, pb.vec, xhat.vec, profita.vec, profitb.vec)
#
# plot of UPE prices, profits, and A's market share for b=0
ggplot(papb_00.df, aes(x=a.vec)) + geom_line(aes(y=pa.vec), linetype="solid", size=1.2) + geom_line(aes(y=pb.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xhat.vec), linetype="dotted", size=1.5) +labs(x="a = location of firm A", y="UPE prices, profits, and firm A's market share")+ scale_x_continuous(breaks = seq(0,L,0.1*L)) + scale_y_continuous(breaks = seq(0,2*L,0.1*L)) + theme(axis.text.x = element_text(size = 18, color = "black"),   axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.52*L, y = 0.57*L, label =TeX("$p_A^u$"), size = 10) + annotate("text", x = 0.73*L, y = 0.65*L, label =TeX("$p_B^u$"), size = 10) + annotate("text", x = 0.8*L, y = 0.98*L, label =TeX("$\\hat{x}^u$"), size = 10)+ geom_line(aes(y=profita.vec), linetype="solid", size=1.2, color="blue") + geom_line(aes(y=profitb.vec), linetype="longdash", size=1.2, color="blue") + annotate("text", x = 0.4*L, y = 0.49*L, label =TeX("$\\pi_A^u$"), size = 10, color="blue") + annotate("text", x = 0.6*L, y = 0.07*L, label =TeX("$\\pi_B^u$"), size = 10, color="blue")
#

## b=0.25*L => 1-b=0.75*L ##
# plotting pa and pb as functions of a\in[0,1-b]
L = 1 #Length of the linear street
b = 0.25*L # distance of firm B from 1 (from the right) expressed as a fraction of L
tau = 1 # transportation cost parameter
#
# vector of "a" as firm A gets closer to firm B located at "L-b"
(a.vec = seq(0, L-b, (L-b)/100))
# vectors of pa and pb solutions to be plotted
pa.vec = rep(NA, length(a.vec))# initialize
pb.vec = rep(NA, length(a.vec))# initialize
xhat.vec = rep(NA, length(a.vec))# initialize
profita.vec = rep(NA, length(a.vec))# initialize
profitb.vec = rep(NA, length(a.vec))# initialize

for (i in 1:length(a.vec)){
  # the function is a 2-dim vector corresponding to equations (8a) and (8b) in the paper (B does not undercut A and A does not undercut B). x[1] is pa, x[2] is pb
  upe.fn = function(x){c(x[2]*(L-(tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[1]-tau*(L-b-a.vec[i]))*L, x[1]*((tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[2]-tau*(L-b-a.vec[i]))*L)
  }
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb.vec = unlist(nleqslv(c(0,0), upe.fn)[1]);
  # place it in pa.vec and pb.vec to be plotted
  pa.vec[i] = papb.vec[1]; # UPE pa
  pb.vec[i] = papb.vec[2]  # UPE pb
  # market share of firm A
  xhat.vec[i] = (tau*(L+a.vec[i]-b)-papb.vec[1] + papb.vec[1])/(2*tau)
}# end loop on all i (all a.vec)
# profits firm A and firm B
profita.vec = pa.vec*xhat.vec
profitb.vec = pb.vec*(L-xhat.vec)# profit firm A
# plot pa.vec and pb.vec as functions of a.vec
# dataframe for pa and pb solution when b=0 (firm B is located at L)
papb_025.df = data.frame(a.vec, pa.vec, pb.vec, xhat.vec, profita.vec, profitb.vec)
#
# plot of UPE prices, profits, and A's market share for b=0.25 => 1-b = 0.75
ggplot(papb_025.df, aes(x=a.vec)) + geom_line(aes(y=pa.vec), linetype="solid", size=1.2) + geom_line(aes(y=pb.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xhat.vec), linetype="dotted", size=1.5) +labs(x="a = location of firm A", y="UPE prices, profits, and firm A's market share")+ scale_x_continuous(breaks = seq(0,0.75*L,0.05*L)) + scale_y_continuous(breaks = seq(0,2*L,0.1*L)) + theme(axis.text.x = element_text(size = 18, color = "black"),   axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.30*L, y = 0.76*L, label =TeX("$p_A^u$"), size = 10) + annotate("text", x = 0.4*L, y = 0.83*L, label =TeX("$p_B^u$"), size = 10) + annotate("text", x = 0.65*L, y = 0.76*L, label =TeX("$\\hat{x}^u$"), size = 10)+ geom_line(aes(y=profita.vec), linetype="solid", size=1.2, color="blue") + geom_line(aes(y=profitb.vec), linetype="longdash", size=1.2, color="blue") + annotate("text", x = 0.5*L, y = 0.31*L, label =TeX("$\\pi_A^u$"), size = 10, color="blue") + annotate("text", x = 0.55*L, y = 0.08*L, label =TeX("$\\pi_B^u$"), size = 10, color="blue")
#

## b=0.5*L => 1-b=0.5*L ##
# plotting pa and pb as functions of a\in[0,1-b]
L = 1 #Length of the linear street
b = 0.5*L # distance of firm B from 1 (from the right) expresses as a fraction of L
tau = 1 # transportation cost parameter
#
# vector of "a" as firm A gets closer to firm B located at "L-b"
(a.vec = seq(0, L-b, (L-b)/100))
# vectors of pa and pb solutions to be plotted
pa.vec = rep(NA, length(a.vec))# initialize
pb.vec = rep(NA, length(a.vec))# initialize
xhat.vec = rep(NA, length(a.vec))# initialize
profita.vec = rep(NA, length(a.vec))# initialize
profitb.vec = rep(NA, length(a.vec))# initialize

for (i in 1:length(a.vec)){
  # the function is a 2-dim vector corresponding to equations (8a) and (8b) in the paper (B does not undercut A and A does not undercut B). x[1] is pa, x[2] is pb
  upe.fn = function(x){c(x[2]*(L-(tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[1]-tau*(L-b-a.vec[i]))*L, x[1]*((tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[2]-tau*(L-b-a.vec[i]))*L)
  }
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb.vec = unlist(nleqslv(c(0,0), upe.fn)[1]);
  # place it in pa.vec and pb.vec to be plotted
  pa.vec[i] = papb.vec[1]; # UPE pa
  pb.vec[i] = papb.vec[2]  # UPE pb
  # market share of firm A
  xhat.vec[i] = (tau*(L+a.vec[i]-b)-papb.vec[1] + papb.vec[1])/(2*tau)
}# end loop on all i (all a.vec)
# profits firm A and firm B
profita.vec = pa.vec*xhat.vec
profitb.vec = pb.vec*(L-xhat.vec)# profit firm A
# plot pa.vec and pb.vec as functions of a.vec
# dataframe for pa and pb solution when b=0 (firm B is located at L)
papb_05.df = data.frame(a.vec, pa.vec, pb.vec, xhat.vec, profita.vec, profitb.vec)
#
# plot of UPE prices, profits, and A's market share for b=0.5*L => 1-b = 0.5*L
ggplot(papb_05.df, aes(x=a.vec)) + geom_line(aes(y=pa.vec), linetype="solid", size=1.2) + geom_line(aes(y=pb.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xhat.vec), linetype="dotted", size=1.5) +labs(x="a = location of firm A", y="UPE prices, profits, and firm A's market share")+ scale_x_continuous(breaks = seq(0,0.5*L,0.05*L)) + scale_y_continuous(breaks = seq(0,2*L,0.1*L)) + theme(axis.text.x = element_text(size = 18, color = "black"),   axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.15*L, y = 0.81*L, label =TeX("$p_A^u$"), size = 10) + annotate("text", x = 0.15*L, y = 0.55*L, label =TeX("$p_B^u$"), size = 10) + annotate("text", x = 0.4*L, y = 0.50*L, label =TeX("$\\hat{x}^u$"), size = 10)+ geom_line(aes(y=profita.vec), linetype="solid", size=1.2, color="blue") + geom_line(aes(y=profitb.vec), linetype="longdash", size=1.2, color="blue") + annotate("text", x = 0.25*L, y = 0.15*L, label =TeX("$\\pi_A^u$"), size = 10, color="blue") + annotate("text", x = 0.28*L, y = 0.28*L, label =TeX("$\\pi_B^u$"), size = 10, color="blue")
#

### I don't get Byford's SSRN result that equilibrium location is at 3/8 from the edges of town. It may be because he takes the limit. 
## b=3/8*L => 1-b=5/8*L ## (Byford model predict optimal location at 3/8L from the edges)
# plotting pa and pb as functions of a\in[0,1-b]
L = 1 #Length of the linear street
b = (3/8)*L # distance of firm B from 1 (from the right) expresses as a fraction of L
tau = 1 # transportation cost parameter
#
# vector of "a" as firm A gets closer to firm B located at "L-b"
(a.vec = seq(0, L-b, (L-b)/100))
# vectors of pa and pb solutions to be plotted
pa.vec = rep(NA, length(a.vec))# initialize
pb.vec = rep(NA, length(a.vec))# initialize
xhat.vec = rep(NA, length(a.vec))# initialize
profita.vec = rep(NA, length(a.vec))# initialize
profitb.vec = rep(NA, length(a.vec))# initialize

for (i in 1:length(a.vec)){
  # the function is a 2-dim vector corresponding to equations (8a) and (8b) in the paper (B does not undercut A and A does not undercut B). x[1] is pa, x[2] is pb
  upe.fn = function(x){c(x[2]*(L-(tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[1]-tau*(L-b-a.vec[i]))*L, x[1]*((tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[2]-tau*(L-b-a.vec[i]))*L)
  }
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb.vec = unlist(nleqslv(c(0,0), upe.fn)[1]);
  # place it in pa.vec and pb.vec to be plotted
  pa.vec[i] = papb.vec[1]; # UPE pa
  pb.vec[i] = papb.vec[2]  # UPE pb
  # market share of firm A
  xhat.vec[i] = (tau*(L+a.vec[i]-b)-papb.vec[1] + papb.vec[1])/(2*tau)
}# end loop on all i (all a.vec)
# profits firm A and firm B
profita.vec = pa.vec*xhat.vec
profitb.vec = pb.vec*(L-xhat.vec)# profit firm A
# plot pa.vec and pb.vec as functions of a.vec
# dataframe for pa and pb solution when b=0 (firm B is located at L)
papb_3over8.df = data.frame(a.vec, pa.vec, pb.vec, xhat.vec, profita.vec, profitb.vec)
#
# plot of UPE prices, profits, and A's market share for b=(3/8)*L => 1-b = (5/8)*L
ggplot(papb_3over8.df, aes(x=a.vec)) + geom_line(aes(y=pa.vec), linetype="solid", size=1.2) + geom_line(aes(y=pb.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xhat.vec), linetype="dotted", size=1.5) +labs(x="a = location of firm A", y="UPE prices, profits, and firm A's market share")+ scale_x_continuous(breaks = seq(0,(5/8)*L,(0.5/8)*L)) + scale_y_continuous(breaks = seq(0,2*L,0.1*L)) + theme(axis.text.x = element_text(size = 18, color = "black"),   axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.15*L, y = 1.1*L, label =TeX("$p_A^u$"), size = 10) + annotate("text", x = 0.15*L, y = 0.77*L, label =TeX("$p_B^u$"), size = 10) + annotate("text", x = 0.5*L, y = 0.61*L, label =TeX("$\\hat{x}^u$"), size = 10)+ geom_line(aes(y=profita.vec), linetype="solid", size=1.2, color="blue") + geom_line(aes(y=profitb.vec), linetype="longdash", size=1.2, color="blue") + annotate("text", x = 0.25*L, y = 0.3*L, label =TeX("$\\pi_A^u$"), size = 10, color="blue") + annotate("text", x = 0.29*L, y = 0.39*L, label =TeX("$\\pi_B^u$"), size = 10, color="blue")
#

####### unused code #######
##################
# ## b=0 => 1-b=0 ##
# # plotting pa and pb as functions of a\in[0,1-b]
# L = 1 #Length of the linear street
# b = 0.0 # distance of firm B from 1 (from the right)
# tau = 1 # transportation cost parameter
# #
# # vector of "a" as firm A gets closer to firm B located at "L-b"
# (a.vec = seq(0, L-b, (L-b)/100))
# # vectors of pa and pb solutions to be plotted
# pa.vec = rep(NA, length(a.vec))# initialize
# pb.vec = rep(NA, length(a.vec))# initialize
# xhat.vec = rep(NA, length(a.vec))# initialize
# profita.vec = rep(NA, length(a.vec))# initialize
# profitb.vec = rep(NA, length(a.vec))# initialize
# 
# for (i in 1:length(a.vec)){
#   # the function is a 2-dim vector corresponding to equations (8a) and (8b) in the paper (B does not undercut A and A does not undercut B). x[1] is pa, x[2] is pb
#   upe.fn = function(x){c(x[2]*(L-(tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[1]-tau*(L-b-a.vec[i]))*L, x[1]*((tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[2]-tau*(L-b-a.vec[i]))*L)
#   }
#   # solve for UPE prices and place them in a 2-dim vector for pa and pb
#   papb.vec = unlist(nleqslv(c(0,0), upe.fn)[1]);
#   # place it in pa.vec and pb.vec to be plotted
#   pa.vec[i] = papb.vec[1]; # UPE pa
#   pb.vec[i] = papb.vec[2]  # UPE pb
#   # market share of firm A
#   xhat.vec[i] = (tau*(L+a.vec[i]-b)-papb.vec[1] + papb.vec[1])/(2*tau)
# }# end loop on all i (all a.vec)
# # profits firm A and firm B
# profita.vec = pa.vec*xhat.vec
# profitb.vec = pb.vec*(1-xhat.vec)# profit firm A
# # plot pa.vec and pb.vec as functions of a.vec
# # dataframe for pa and pb solution when b=0 (firm B is located at L)
# papb_0.df = data.frame(a.vec, pa.vec, pb.vec, xhat.vec, profita.vec, profitb.vec)
# #
# # plot of UPE prices and A's market share for b=0
# ggplot(papb_0.df, aes(x=a.vec)) + geom_line(aes(y=pa.vec), linetype="solid", size=1.2) + geom_line(aes(y=pb.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xhat.vec), linetype="dotted", size=1.5) +labs(x="a = location of firm A", y="UPE prices and firm A's market share")+ scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,2,0.1)) + theme(axis.text.x = element_text(size = 18, color = "black"),   axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.5, y = 0.45, label =TeX("$p_A$"), size = 10) + annotate("text", x = 0.72, y = 0.65, label =TeX("$p_B$"), size = 10) + annotate("text", x = 0.8, y = 0.98, label =TeX("$\\hat{x}$"), size = 10)
# #
# # plot of UPE profits and A's market share for b=0
# ggplot(papb_0.df, aes(x=a.vec)) + geom_line(aes(y=xhat.vec), linetype="dotted", size=1.5) + geom_line(aes(y=profita.vec), linetype="solid", size=1.2) + geom_line(aes(y=profitb.vec), linetype="longdash", size=1.2) +labs(x="a = location of firm A", y="UPE profits and firm A's market share") + scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,1.2,0.1)) + theme(axis.text.x = element_text(size = 18, color = "black"),   axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.7, y = 0.29, label =TeX("$\\pi_A$"), size = 10) + annotate("text", x = 0.55, y = 0.15, label =TeX("$\\pi_B$"), size = 10) + annotate("text", x = 0.8, y = 0.95, label =TeX("$\\hat{x}$"), size = 10)
# 
# ## b=0.2 => 1-b=0.8 ##
# # plotting pa and pb as functions of a\in[0,1-b]
# L = 1 #Length of the linear street
# b = 0.2 # distance of firm B from 1 (from the right)
# tau = 1 # transportation cost parameter
# #
# # vector of "a" as firm A gets closer to firm B located at "L-b"
# (a.vec = seq(0, L-b, (L-b)/100))
# # vectors of pa and pb solutions to be plotted
# pa.vec = rep(NA, length(a.vec))# initialize
# pb.vec = rep(NA, length(a.vec))# initialize
# xhat.vec = rep(NA, length(a.vec))# initialize
# profita.vec = rep(NA, length(a.vec))# initialize
# profitb.vec = rep(NA, length(a.vec))# initialize
# 
# for (i in 1:length(a.vec)){
#   # the function is a 2-dim vector corresponding to equations (8a) and (8b) in the paper (B does not undercut A and A does not undercut B). x[1] is pa, x[2] is pb
#   upe.fn = function(x){c(x[2]*(L-(tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[1]-tau*(L-b-a.vec[i]))*L, x[1]*((tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[2]-tau*(L-b-a.vec[i]))*L)
#   }
#   # solve for UPE prices and place them in a 2-dim vector for pa and pb
#   papb.vec = unlist(nleqslv(c(0,0), upe.fn)[1]);
#   # place it in pa.vec and pb.vec to be plotted
#   pa.vec[i] = papb.vec[1]; # UPE pa
#   pb.vec[i] = papb.vec[2]  # UPE pb
#   # market share of firm A
#   xhat.vec[i] = (tau*(L+a.vec[i]-b)-papb.vec[1] + papb.vec[1])/(2*tau)
# }# end loop on all i (all a.vec)
# # profits firm A and firm B
# profita.vec = pa.vec*xhat.vec
# profitb.vec = pb.vec*(1-xhat.vec)# profit firm A
# # plot pa.vec and pb.vec as functions of a.vec
# # dataframe for pa and pb solution when b=0 (firm B is located at L)
# papb_02.df = data.frame(a.vec, pa.vec, pb.vec, xhat.vec, profita.vec, profitb.vec)
# #
# # plot of UPE prices and A's market share for b=0.2
# ggplot(papb_02.df, aes(x=a.vec)) + geom_line(aes(y=pa.vec), linetype="solid", size=1.2) + geom_line(aes(y=pb.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xhat.vec), linetype="dotted", size=1.5) +labs(x="a = location of firm A", y="UPE prices and firm A's market share")+ scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,2,0.1)) + theme(axis.text.x = element_text(size = 18, color = "black"),   axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.3, y = 0.83, label =TeX("$p_A$"), size = 10) + annotate("text", x = 0.4, y = 0.94, label =TeX("$p_B$"), size = 10) + annotate("text", x = 0.6, y = 0.76, label =TeX("$\\hat{x}$"), size = 10)
# #
# # plot of UPE profits and A's market share for b=0.2 => 1-b=0.8
# ggplot(papb_02.df, aes(x=a.vec)) + geom_line(aes(y=xhat.vec), linetype="dotted", size=1.5) + geom_line(aes(y=profita.vec), linetype="solid", size=1.2) + geom_line(aes(y=profitb.vec), linetype="longdash", size=1.2) +labs(x="a = location of firm A", y="UPE profits and firm A's market share") + scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,1.2,0.1)) + theme(axis.text.x = element_text(size = 18, color = "black"),   axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.61, y = 0.23, label =TeX("$\\pi_A$"), size = 10) + annotate("text", x = 0.54, y = 0.15, label =TeX("$\\pi_B$"), size = 10) + annotate("text", x = 0.5, y = 0.69, label =TeX("$\\hat{x}$"), size = 10)
# 
# ## b=0.4 => 1-b=0.6 ##
# # plotting pa and pb as functions of a\in[0,1-b]
# L = 1 #Length of the linear street
# b = 0.4 # distance of firm B from 1 (from the right)
# tau = 1 # transportation cost parameter
# #
# # vector of "a" as firm A gets closer to firm B located at "L-b"
# (a.vec = seq(0, L-b, (L-b)/100))
# # vectors of pa and pb solutions to be plotted
# pa.vec = rep(NA, length(a.vec))# initialize
# pb.vec = rep(NA, length(a.vec))# initialize
# xhat.vec = rep(NA, length(a.vec))# initialize
# profita.vec = rep(NA, length(a.vec))# initialize
# profitb.vec = rep(NA, length(a.vec))# initialize
# 
# for (i in 1:length(a.vec)){
#   # the function is a 2-dim vector corresponding to equations (8a) and (8b) in the paper (B does not undercut A and A does not undercut B). x[1] is pa, x[2] is pb
#   upe.fn = function(x){c(x[2]*(L-(tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[1]-tau*(L-b-a.vec[i]))*L, x[1]*((tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[2]-tau*(L-b-a.vec[i]))*L)
#   }
#   # solve for UPE prices and place them in a 2-dim vector for pa and pb
#   papb.vec = unlist(nleqslv(c(0,0), upe.fn)[1]);
#   # place it in pa.vec and pb.vec to be plotted
#   pa.vec[i] = papb.vec[1]; # UPE pa
#   pb.vec[i] = papb.vec[2]  # UPE pb
#   # market share of firm A
#   xhat.vec[i] = (tau*(L+a.vec[i]-b)-papb.vec[1] + papb.vec[1])/(2*tau)
# }# end loop on all i (all a.vec)
# # profits firm A and firm B
# profita.vec = pa.vec*xhat.vec
# profitb.vec = pb.vec*(1-xhat.vec)# profit firm A
# # plot pa.vec and pb.vec as functions of a.vec
# # dataframe for pa and pb solution when b=0 (firm B is located at L)
# papb_04.df = data.frame(a.vec, pa.vec, pb.vec, xhat.vec, profita.vec, profitb.vec)
# #
# # plot of UPE prices and A's market share for b=0.4
# ggplot(papb_04.df, aes(x=a.vec)) + geom_line(aes(y=pa.vec), linetype="solid", size=1.2) + geom_line(aes(y=pb.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xhat.vec), linetype="dotted", size=1.5) +labs(x="a = location of firm A", y="UPE prices and firm A's market share")+ scale_x_continuous(breaks = seq(0,0.6,0.05)) + scale_y_continuous(breaks = seq(0,2,0.1)) + theme(axis.text.x = element_text(size = 18, color = "black"),   axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.15, y = 1.03, label =TeX("$p_A$"), size = 10) + annotate("text", x = 0.15, y = 0.71, label =TeX("$p_B$"), size = 10) + annotate("text", x = 0.5, y = 0.6, label =TeX("$\\hat{x}$"), size = 10)
# #
# # plot of UPE profits and A's market share for b=0.4 => 1-b=0.6
# ggplot(papb_04.df, aes(x=a.vec)) + geom_line(aes(y=xhat.vec), linetype="dotted", size=1.5) + geom_line(aes(y=profita.vec), linetype="solid", size=1.2) + geom_line(aes(y=profitb.vec), linetype="longdash", size=1.2) +labs(x="a = location of firm A", y="UPE profits and firm A's market share") + scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,1.2,0.1)) + theme(axis.text.x = element_text(size = 18, color = "black"),   axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.2, y = 0.32, label =TeX("$\\pi_A$"), size = 10) + annotate("text", x = 0.25, y = 0.38, label =TeX("$\\pi_B$"), size = 10) + annotate("text", x = 0.5, y = 0.58, label =TeX("$\\hat{x}$"), size = 10)
### b = 0.2 case (not in paper)

## b=0.2 => 1-b=0.8 ##
# plotting pa and pb as functions of a\in[0,1-b]
# L = 1 #Length of the linear street
# b = 0.2 # distance of firm B from 1 (from the right)
# tau = 1 # transportation cost parameter
# #
# # vector of "a" as firm A gets closer to firm B located at "L-b"
# (a.vec = seq(0, L-b, (L-b)/100))
# # vectors of pa and pb solutions to be plotted
# pa.vec = rep(NA, length(a.vec))# initialize
# pb.vec = rep(NA, length(a.vec))# initialize
# xhat.vec = rep(NA, length(a.vec))# initialize
# profita.vec = rep(NA, length(a.vec))# initialize
# profitb.vec = rep(NA, length(a.vec))# initialize
# 
# for (i in 1:length(a.vec)){
#   # the function is a 2-dim vector corresponding to equations (8a) and (8b) in the paper (B does not undercut A and A does not undercut B). x[1] is pa, x[2] is pb
#   upe.fn = function(x){c(x[2]*(L-(tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[1]-tau*(L-b-a.vec[i]))*L, x[1]*((tau*(L+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[2]-tau*(L-b-a.vec[i]))*L)
#   }
#   # solve for UPE prices and place them in a 2-dim vector for pa and pb
#   papb.vec = unlist(nleqslv(c(0,0), upe.fn)[1]);
#   # place it in pa.vec and pb.vec to be plotted
#   pa.vec[i] = papb.vec[1]; # UPE pa
#   pb.vec[i] = papb.vec[2]  # UPE pb
#   # market share of firm A
#   xhat.vec[i] = (tau*(L+a.vec[i]-b)-papb.vec[1] + papb.vec[1])/(2*tau)
# }# end loop on all i (all a.vec)
# # profits firm A and firm B
# profita.vec = pa.vec*xhat.vec
# profitb.vec = pb.vec*(1-xhat.vec)# profit firm A
# # plot pa.vec and pb.vec as functions of a.vec
# # dataframe for pa and pb solution when b=0 (firm B is located at L)
# papb_02.df = data.frame(a.vec, pa.vec, pb.vec, xhat.vec, profita.vec, profitb.vec)
# #
# # plot of UPE prices, profits, and A's market share for b=0
# ggplot(papb_02.df, aes(x=a.vec)) + geom_line(aes(y=pa.vec), linetype="solid", size=1.2) + geom_line(aes(y=pb.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xhat.vec), linetype="dotted", size=1.5) +labs(x="a = location of firm A", y="UPE prices, profits, and firm A's market share")+ scale_x_continuous(breaks = seq(0,0.8,0.05)) + scale_y_continuous(breaks = seq(0,2,0.1)) + theme(axis.text.x = element_text(size = 18, color = "black"),   axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.29, y = 0.85, label =TeX("$p_A$"), size = 10) + annotate("text", x = 0.35, y = 1.05, label =TeX("$p_B$"), size = 10) + annotate("text", x = 0.7, y = 0.83, label =TeX("$\\hat{x}$"), size = 10)+ geom_line(aes(y=profita.vec), linetype="solid", size=1.2, color="blue") + geom_line(aes(y=profitb.vec), linetype="longdash", size=1.2, color="blue") + annotate("text", x = 0.45, y = 0.40, label =TeX("$\\pi_A$"), size = 10, color="blue") + annotate("text", x = 0.55, y = 0.11, label =TeX("$\\pi_B$"), size = 10, color="blue")
# #

