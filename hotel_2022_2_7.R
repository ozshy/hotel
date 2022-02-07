### R packages needed
library(nleqslv)# package numerical solution of a system of equations
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot

#### Hotelling paper begins ####

### Section 6,Figure 3 (prices, profits, xhat)
## b=0.25 => 1-b=0.75
b = 0.25 # distance of firm B from 1 (from the right) expressed as a fraction of L
tau = 1 # transportation cost parameter
#
# vector of "a" as firm A gets closer to firm B located at "1-b" Location of firm A
(a.vec = seq(0, 1-b, (1-b)/100))
# Initialize UPE vectors of pa and pb solutions to be plotted
pa_ug.vec = rep(NA, length(a.vec))# initialize
pb_ug.vec = rep(NA, length(a.vec))# initialize
xhat_ug.vec = rep(NA, length(a.vec))# initialize
profita_ug.vec = rep(NA, length(a.vec))# initialize
profitb_ug.vec = rep(NA, length(a.vec))# initialize
#
# Initialize Nash prices xhat and profits
pa_n.vec = rep(NA, length(a.vec))# initialize
pb_n.vec = rep(NA, length(a.vec))# initialize
xhat_n.vec = rep(NA, length(a.vec))# initialize
profita_n.vec = rep(NA, length(a.vec))# initialize
profitb_n.vec = rep(NA, length(a.vec))# initialize

## Run a loop over all firm A's location. This loop solves for Nash prices and UPEG (Regions I and III in the chart). Region II is computed in separate loop
#
for (i in 1:length(a.vec)){
  # the function is a 2-dim vector corresponding to equations (7) and (8) in the paper  (binding global undercut-proof property). x[1] is pa, x[2] is pb
  upeg.fn = function(x){c(x[2]*(1-(tau*(1+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[1]-tau*(1-b-a.vec[i])), x[1]*((tau*(1+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[2]-tau*(1-b-a.vec[i])))
  }
  # solve for UPEG prices and place them in a 2-dim vector for pa and pb
  papb_ug.vec = unlist(nleqslv(c(0,0), upeg.fn)[1]);
  # place it in pa.vec and pb.vec to be plotted
  pa_ug.vec[i] = papb_ug.vec[1]; # UPEG pa
  pb_ug.vec[i] = papb_ug.vec[2]  # UPEG pb
  # market share of firm A
  xhat_ug.vec[i] = (tau*(1+a.vec[i]-b)-pa_ug.vec[i] + pb_ug.vec[i])/(2*tau)
# continue loop with Nash equilibrium
pa_n.vec[i] = tau*(3+a.vec[i]-b)/3 
pb_n.vec[i] = tau*(3-a.vec[i]+b)/3 
xhat_n.vec[i] = (3+a.vec[i]-b)/6
}## end loop on all i (all a.vec)
#
# UPEG profits firm A and firm B
profita_ug.vec = pa_ug.vec*xhat_ug.vec
profitb_ug.vec = pb_ug.vec*(1-xhat_ug.vec)# profit firm B
# Nash eql profits
profita_n.vec = pa_n.vec*xhat_n.vec
profitb_n.vec = pb_n.vec*(1-xhat_n.vec)# profit firm B
#
## Now, check whether UPEG marginal conditions hold
# creating horizontal lines indicating whether the UPEG prices adhere to the marginal deviation constraints
marg_a.vec = (tau*(1+a.vec-b)-2*pa_ug.vec + pb_ug.vec)/(2*tau)# should be >=0 to satisfy the marginal property
marg_b.vec = (tau*(1-a.vec+b)+pa_ug.vec - 2*pb_ug.vec)/(2*tau)# should be >=0 to satisfy the marginal property
#
(marg_a_max_red_index = max(which(marg_a.vec < 0)))
(marg_a_min_green_index = max(which(marg_a.vec < 0)+1))# the first a.vec that satisfies the marginal deviation of firm A
(marg_a_max_red_value = a.vec[marg_a_max_red_index])
(marg_a_min_green_value = a.vec[marg_a_min_green_index])#value of a.vec
#
# marge_b.vec has 3 regions: starts positive, then negative, then positive!
(marg_b_max_green_index = min(which(marg_b.vec < 0)-1))
(marg_b_min_red_index = min(which(marg_b.vec < 0)))
(marg_b_max_red_index = max(which(marg_b.vec < 0)))
(marg_b_min_green_index = max(which(marg_b.vec < 0)+1))# 
# marge_b.vec has 3 regions: starts positive, then negative, then positive!
(marg_b_max_green_value = a.vec[marg_b_max_green_index])
(marg_b_min_red_value = a.vec[marg_b_min_red_index])#value of a.vec
(marg_b_max_red_value = a.vec[marg_b_max_red_index])#value of a.vec
(marg_b_min_green_value = a.vec[marg_b_min_green_index])#value of a.vec#
# for this graph, we plot UPE prices only when BOTH marginal conditions are satisfied
(marg_index_u = max(marg_a_min_green_index, marg_b_min_green_index))# 
(marg_value_u = max(marg_a_min_green_value, marg_b_min_green_value))

# conditions guaranteeing Nash equilibrium
(nash_1.vec = (3+a.vec-b)^2 - 12*(a.vec+2*b))
(nash_2.vec = (3-a.vec+b)^2 - 12*(2*a.vec+b))
(nash_1_max = a.vec[max(which(nash_1.vec >= 0))])
(nash_1_max_index = max(which(nash_1.vec >= 0)))
(nash_2_max = a.vec[max(which(nash_2.vec >= 0))])
(nash_2_max_index = max(which(nash_2.vec >= 0)))
(nash_max_index = min(nash_1_max_index, nash_2_max_index))# this is max a.vec below which Nash equilibria exist
(nash_max_value = a.vec[nash_max_index])

# construct vectors of pa and pb that combine Nash prices with UPE prices, with a gap for values of a.vec in which none of these are valid.

# recall max a.vec where Nash exists
nash_max_index
# recall min a.vec where UPE exist with global binding
marg_index_u
#
## Build final vector of prices that combine all ranges where Nash, UPE, and UPEG exist.
pa.vec = rep(NA, length(a.vec))# initialize
pb.vec = rep(NA, length(a.vec))# initialize
xhat.vec = rep(NA, length(a.vec))# initialize
profita.vec = rep(NA, length(a.vec))# initialize
profitb.vec = rep(NA, length(a.vec))# initialize
#
# add the Nash prices for the range when they exist
pa.vec[1:nash_max_index] = pa_n.vec[1:nash_max_index]
pb.vec[1:nash_max_index] = pb_n.vec[1:nash_max_index]
xhat.vec[1:nash_max_index] = xhat_n.vec[1:nash_max_index]
profita.vec[1:nash_max_index] = profita_n.vec[1:nash_max_index]
profitb.vec[1:nash_max_index] = profitb_n.vec[1:nash_max_index]
#
# add the UPEG prices for the range when they exist
pa.vec[marg_index_u: length(a.vec)] = pa_ug.vec[marg_index_u: length(a.vec)]
pb.vec[marg_index_u: length(a.vec)] = pb_ug.vec[marg_index_u: length(a.vec)]
xhat.vec[marg_index_u: length(a.vec)] = xhat_ug.vec[marg_index_u: length(a.vec)]
profita.vec[marg_index_u: length(a.vec)] = profita_ug.vec[marg_index_u: length(a.vec)]
profitb.vec[marg_index_u: length(a.vec)] = profitb_ug.vec[marg_index_u: length(a.vec)]

### New loop to compute UPE in Region II where the global undercut-proof property is not binding.
# There are 2 possibilities to be analyzed because the  global is binding for one firm and marginal is binding for the other firm.
# 
# Case "ambg" => 
# A's marginal is binding eq (9), B's global is binding eq (8)
# A marginal below =0, eq (9)
# tau*(1+a.vec-b)-2*pa_u.vec + pb_u.vec 
# B global below =0, eq (7)
# x[1]*((tau*(1+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[2]-tau*(1-b-a.vec[i])))
# vectors of pa and pb solutions to be plotted
pa_ambg.vec = rep(NA, length(a.vec))# initialize
pb_ambg.vec = rep(NA, length(a.vec))# initialize
xhat_ambg.vec = rep(NA, length(a.vec))# initialize
profita_ambg.vec = rep(NA, length(a.vec))# initialize
profitb_ambg.vec = rep(NA, length(a.vec))# initialize
#
# Case "agbm" => 
# A's global is binding eq (7), B's marginal is binding eq (10) 
# A global below =0, eq (7)
# x[2]*(1-(tau*(1+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[1]-tau*(1-b-a.vec[i]))
# B marginal below =0, eq (10)
# tau*(1-a.vec+b)+pa_u.vec - 2*pb_u.vec
# vectors of pa and pb solutions to be plotted
pa_agbm.vec = rep(NA, length(a.vec))# initialize
pb_agbm.vec = rep(NA, length(a.vec))# initialize
xhat_agbm.vec = rep(NA, length(a.vec))# initialize
profita_agbm.vec = rep(NA, length(a.vec))# initialize
profitb_agbm.vec = rep(NA, length(a.vec))# initialize

# Below is the loop that compute prices and verifies the two non-binding properties for cases "ambg" and "agbm" separately. 
for (i in 1:length(a.vec)){
# x[2]=pb, x[1]=pa
# ambg case: equations (9) and (8)
ambg.fn = function(x){c(tau*(1+a.vec[i]-b)-2*x[1] + x[2], x[1]*((tau*(1+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[2]-tau*(1-b-a.vec[i])))
  }
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_ambg.vec = unlist(nleqslv(c(0,0), ambg.fn)[1]);
  # place it in pa.vec and pb.vec to be plotted
  pa_ambg.vec[i] = papb_ambg.vec[1]; #
  pb_ambg.vec[i] = papb_ambg.vec[2]  #
  # market share of firm A
  xhat_ambg.vec[i] = (tau*(1+a.vec[i]-b)-pa_ambg.vec[i] + pb_ambg.vec[i])/(2*tau)
#
# agbm case: equations (7) and (10)
# Note: case "agbm" is an UPE (plotted in Region II)
agbm.fn = function(x){c(x[2]*(1-(tau*(1+a.vec[i]-b)-x[1]+x[2])/(2*tau)) - (x[1]-tau*(1-b-a.vec[i])),  tau*(1-a.vec[i] +b)+x[1] - 2*x[2])
  }
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_agbm.vec = unlist(nleqslv(c(0,0), agbm.fn)[1]);
  # place it in pa.vec and pb.vec to be plotted
  pa_agbm.vec[i] = papb_agbm.vec[1]; #
  pb_agbm.vec[i] = papb_agbm.vec[2]  #
  # market share of firm A
  xhat_agbm.vec[i] = (tau*(1+a.vec[i]-b)-pa_agbm.vec[i] + pb_agbm.vec[i])/(2*tau)
}### end loop on all i (all a.vec)
#
# Examine whether ambg holds or agbm hold by checking the 2 other non-binding properties
# ambg verify A global property eq (7)
(ambg_ag.vec = pb_ambg.vec*(1-(tau*(1+a.vec[i]-b)-pa_ambg.vec+pb_ambg.vec)/(2*tau)) - (pa_ambg.vec-tau*(1-b-a.vec[i]))) # => no, all <0
# ambg verify B marginal property eq (10)
(ambg_bm.vec = tau*(1-a.vec+b)+pa_ambg.vec - 2*pb_ambg.vec)
#
# agbm verify B global property eq (8)
(ambg_bg.vec = pb_agbm.vec*(1-(tau*(1+a.vec[i]-b)-pa_agbm.vec+pb_agbm.vec)/(2*tau)) - (pa_agbm.vec-tau*(1-b-a.vec[i])))# => all <0
# agbm verify A marginal property eq (9)
(agbm_am.vec = tau*(1+a.vec-b)-2*pa_agbm.vec + pb_agbm.vec)
# agbm verify B global property eq (10)
(agbm_bg.vec = pa_agbm.vec*((tau*(1+a.vec[i]-b)-pa_agbm.vec+pb_agbm.vec)/(2*tau)) - (pb_agbm.vec-tau*(1-b-a.vec[i])))

## now verify if agbm are equilibria [YES]
pa_agbm.vec
pb_agbm.vec
xhat_agbm.vec
(profita_agbm.vec = pa_agbm.vec*xhat_agbm.vec)
(profitb_agbm.vec = pb_agbm.vec*(1-xhat_agbm.vec))
#
# verify that A marginal eq (9) is satisfied
(agbm_am=tau*(1+a.vec[i]-b)-2*pa_agbm.vec + pb_agbm.vec)
# verify that B global eq (8) is satisfied
(agbm_bg=pa_agbm.vec*((tau*(1+a.vec-b)-pa_agbm.vec+pb_agbm.vec)/(2*tau)) - (pb_agbm.vec-tau*(1-b-a.vec)))

## Finalize by adding the UPE agbm prices for the range when they are valid (replacing remaining NAs)
which(is.na(pa.vec))
which(is.na(pb.vec))
pa.vec[which(is.na(pa.vec))] = pa_agbm.vec[which(is.na(pa.vec))]
pb.vec[which(is.na(pb.vec))] = pb_agbm.vec[which(is.na(pb.vec))]
xhat.vec[which(is.na(xhat.vec))] = xhat_agbm.vec[which(is.na(xhat.vec))]
profita.vec[which(is.na(profita.vec))] = profita_agbm.vec[which(is.na(profita.vec))]
profitb.vec[which(is.na(profitb.vec))] = profitb_agbm.vec[which(is.na(profitb.vec))]

## build final data frame for ggplot
# dataframe for pa and pb solution when b=0 (firm B is located at L)
papb_025_u_n.df = data.frame(a.vec, pa.vec, pb.vec, xhat.vec, profita.vec, profitb.vec)
# I define locations of vertical dotted lines indicating where each equilibrium type begins
(end_nash = a.vec[nash_max_index])# use 0.25 instead
(end_nash = 0.25)
(begin_global = a.vec[marg_index_u])# use 0.25 instead

# plot of UPE prices, profits, and A's market share for b=0.25 => 1-b = 0.75
ggplot(papb_025_u_n.df, aes(x=a.vec)) + geom_line(aes(y=pa.vec), linetype="solid", size=1.2) + geom_line(aes(y=pb.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xhat.vec), linetype="dotdash", size=1.5, color="red") +labs(x="a = location of firm A", y="Equilibrium prices, profits, and market share")+ scale_x_continuous(breaks = seq(0,0.75,0.05)) + scale_y_continuous(breaks = seq(0,2,0.1))+ theme(axis.text.x = element_text(size = 18, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.5, y = 0.47, label =TeX("$p_A^{ug}$"), size = 8) + annotate("text", x = 0.1, y = 0.89, label =TeX("$p_A^n=p_A^u$"), size = 8) + annotate("text", x = 0.5, y = 0.6, label =TeX("$p_B^{ug}$"), size = 8) + annotate("text", x = 0.1, y = 1.1, label =TeX("$p_B^n=p_B^u$"), size = 8) + annotate("text", x = 0.65, y = 0.78, label =TeX("$\\hat{x}^{ug}$"), size = 8, color="red") + annotate("text", x = 0.3, y = 0.62, label =TeX("$\\hat{x}^{u}$"), size = 8, color="red") + annotate("text", x = 0.04, y = 0.5, label =TeX("$\\hat{x}^n=\\hat{x}^u$"), size = 8, color="red") + geom_line(aes(y=profita.vec), linetype="solid", size=1.2, color="blue") + geom_line(aes(y=profitb.vec), linetype="longdash", size=1.2, color="blue") + annotate("text", x = 0.05, y = 0.38, label =TeX("$\\pi_A^n=\\pi_A^u$"), size = 8, color="blue") + annotate("text", x = 0.05, y = 0.62, label =TeX("$\\pi_B^n=\\pi_B^u$"), size = 8, color="blue") + annotate("text", x = 0.5, y = 0.33, label =TeX("$\\pi_A^{ug}$"), size = 8, color="blue") + annotate("text", x = 0.5, y = 0.22, label =TeX("$\\pi_B^{ug}$"), size = 8, color="blue") + annotate("text", x = 0.67, y = 0.5, label =TeX("$p_A^n=p_A^{ug}=p_B^n=p_B^{ug}$"), size = 8) + geom_segment(aes(x = 0.67, y = 0.48, xend = 0.75, yend = 0.02), arrow = arrow(length = unit(0.5, "cm")), size =1) + geom_point(x=0.75, y=0, shape = 19, size=4, color="black", stroke=2) +  geom_vline(xintercept = end_nash, linetype = "dotted", size=1.2)+ geom_vline(xintercept = begin_global, linetype = "dotted", size=1.2)  + annotate("text", x = 0.35, y = 0.37, label =TeX("$\\pi_B^{u}$"), size = 8, color="blue") + annotate("text", x = 0.35, y = 0.48, label =TeX("$\\pi_A^{u}$"), size = 8, color="blue") + annotate("text", x = 0.35, y = 0.78, label =TeX("$p_A^u$"), size = 8, color="black") + annotate("text", x = 0.35, y = 0.9, label =TeX("$p_B^u$"), size = 8, color="black") + annotate("text", x = 0.1, y = 0.05, label ="Region I: Nash & UPE", size = 6, color="black") + annotate("text", x = 0.33, y = 0.05, label ="Region II: UPE", size = 6, color="black") + annotate("text", x = 0.52, y = 0.05, label ="Region III: UPEG", size = 6, color="black") 

# info for the caption
# Nash equilibrium holds for a=[0, 0.25]
nash_max_value
# undercut-proof ug (global binding) holds for a=(0.25, 0.42] half-open interval
marg_b_max_red_value
marg_b_min_green_value

### End of code 



