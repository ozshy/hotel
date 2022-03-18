# Code for three figures in a paper entitled: "Hotelling's model when firms are located close to each other"
#
# Figure 4 starts on line 15
# Figure 5 starts on line 318
# Figure 6 starts on line 618
# Figure (not in the paper) line 946: Testing mirror image of figure 6.
#
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

### New loop to compute UPE in Region II where the global and marginal undercut-proof properties is binding for one firm and not the other. 
# Recall, Region II applies for
(regionII_b_025 = which(is.na(pa.vec)))
a.vec[regionII_b_025]

# 4 cases are looped below: 
# I.   agam, eqs (8) and (9), A global and marginal
# II.  bgbm, eqs (7) and (10), B global and marginal
# III. agbm, eqs (8) and (10), A global, B marginal
# IV.  bgam, eqs (7) and (9), B global, A marginal

# 
# Case I "agam" => 
# A's global is binding eq (8), and marginal eq (9)
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_agam.vec = rep(NA, length(a.vec))# initialize
pb_agam.vec = rep(NA, length(a.vec))# initialize
xhat_agam.vec = rep(NA, length(a.vec))# initialize
profita_agam.vec = rep(NA, length(a.vec))# initialize
profitb_agam.vec = rep(NA, length(a.vec))# initialize
#
# Below is the loop that computes agam UPE prices using equations (8) and (9) 
for (i in 1:length(a.vec)){
# x[2]=pb, x[1]=pa
# use below for easier typing (to be pasted)
# xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
#
agam.fn = function(x){
  c(x[1]*(tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau) - (x[2] - tau*(1-b-a.vec[i])) , tau*(1+a.vec[i] -b) -2*x[1] +x[2])
}  
#
# solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_agam.vec = unlist(nleqslv(c(0,0), agam.fn)[1]);
  # candidate UPE prices: 
  pa_agam.vec[i] = papb_agam.vec[1]; #
  pb_agam.vec[i] = papb_agam.vec[2]  #
  # market share of firm A
  xhat_agam.vec[i] = (tau*(1+a.vec[i]-b)-pa_agam.vec[i] + pb_agam.vec[i])/(2*tau)
#
}### end loop on all i (all a.vec) for case agam
# check prices
pa_agam.vec # constant! Rule out
pb_agam.vec
xhat_agam.vec # constant! Rule out
# End of case I: agam

# Case II "bgbm" => is an UPE for b=0.25 !
# B's global is binding eq (7), and marginal eq (10) 
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_bgbm.vec = rep(NA, length(a.vec))# initialize
pb_bgbm.vec = rep(NA, length(a.vec))# initialize
xhat_bgbm.vec = rep(NA, length(a.vec))# initialize
profita_bgbm.vec = rep(NA, length(a.vec))# initialize
profitb_bgbm.vec = rep(NA, length(a.vec))# initialize
#
# Below is the loop that computes agam UPE prices using equations (7) and (10) 
for (i in 1:length(a.vec)){
  # x[2]=pb, x[1]=pa
  # use below for easier typing (to be pasted)
  # xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
  #
  bgbm.fn = function(x){
    c(x[2]*(1-(tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)) - (x[1]-tau*(1-b-a.vec[i])) , tau*(1-a.vec[i]+b) +x[1] -2*x[2])
  }  
  #
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_bgbm.vec = unlist(nleqslv(c(0,0), bgbm.fn)[1]);
  # candidate UPE prices: 
  pa_bgbm.vec[i] = papb_bgbm.vec[1]; #
  pb_bgbm.vec[i] = papb_bgbm.vec[2]  #
  # market share of firm A
  xhat_bgbm.vec[i] = (tau*(1+a.vec[i]-b)-pa_bgbm.vec[i] + pb_bgbm.vec[i])/(2*tau)
  #
}### end loop on all i (all a.vec) for case bgbm
# check prices
pa_bgbm.vec[regionII_b_025] # 
pb_bgbm.vec[regionII_b_025]
xhat_bgbm.vec[regionII_b_025] # 
profita_bgbm.vec = pa_bgbm.vec*xhat_bgbm.vec
profitb_bgbm.vec = pb_bgbm.vec*(1-xhat_bgbm.vec)
#
# Verify that A's global (8) >=0 
bgbm_8.vec = pa_bgbm.vec*xhat_bgbm.vec - (pb_bgbm.vec - tau*(1-b-a.vec))
bgbm_8.vec[regionII_b_025] # >0, yes
# Verify that A's marginal (9) >=0
bgbm_9.vec = tau*(1+a.vec-b) - 2*pa_bgbm.vec + pb_bgbm.vec
bgbm_9.vec[regionII_b_025] # >0, Yes~
# End of case II: bgbm: This is an UPE for b=0.25

# Case III "agbm" => 
# A's global is binding eq (8), B's marginal is binding (10)
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_agbm.vec = rep(NA, length(a.vec))# initialize
pb_agbm.vec = rep(NA, length(a.vec))# initialize
xhat_agbm.vec = rep(NA, length(a.vec))# initialize
profita_agbm.vec = rep(NA, length(a.vec))# initialize
profitb_agbm.vec = rep(NA, length(a.vec))# initialize
#
# Below is the loop that computes agbm UPE prices using equations (8) and (10) 
for (i in 1:length(a.vec)){
  # x[2]=pb, x[1]=pa
  # use below for easier typing (to be pasted)
  # xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
  #
  agbm.fn = function(x){
    c(x[1]*(tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau) - (x[2] - tau*(1-b-a.vec[i])) , tau*(1-a.vec[i] +b) +x[1] - 2*x[2])
  }  
  #
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_agbm.vec = unlist(nleqslv(c(0,0), agbm.fn)[1]);
  # candidate UPE prices: 
  pa_agbm.vec[i] = papb_agbm.vec[1]; #
  pb_agbm.vec[i] = papb_agbm.vec[2]  #
  # market share of firm A
  xhat_agbm.vec[i] = (tau*(1+a.vec[i]-b)-pa_agbm.vec[i] + pb_agbm.vec[i])/(2*tau)
  #
}### end loop on all i (all a.vec) for case agbm
# check prices
pa_agbm.vec # 
pb_agbm.vec
xhat_agbm.vec # 
#
# Verify that A's marginal (9) >=0 
agbm_9.vec = tau*(1+a.vec-b) - 2*pa_agbm.vec + pb_agbm.vec
agbm_9.vec[regionII_b_025] # >0, yes
# Verify that B's global (7) >=0
agbm_7.vec = pb_agbm.vec*(1-xhat_agbm.vec) - (pa_agbm.vec -tau*(1-b-a.vec))
agbm_7.vec[regionII_b_025] # <0, No!
# End of case III: agbm

# Case IV "bgam" => 
# B's global is binding eq (7), A's marginal is binding (9)
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_bgam.vec = rep(NA, length(a.vec))# initialize
pb_bgam.vec = rep(NA, length(a.vec))# initialize
xhat_bgam.vec = rep(NA, length(a.vec))# initialize
profita_bgam.vec = rep(NA, length(a.vec))# initialize
profitb_bgam.vec = rep(NA, length(a.vec))# initialize
#
# Below is the loop that computes agbm UPE prices using equations (7) and (9) 
for (i in 1:length(a.vec)){
  # x[2]=pb, x[1]=pa
  # use below for easier typing (to be pasted)
  # xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
  #
  bgam.fn = function(x){
    c(x[2]*(1 - (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)) - (x[1] - tau*(1-b-a.vec[i])) , tau*(1+a.vec[i]-b) -2*x[1]+ x[2])
  }  
  #
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_bgam.vec = unlist(nleqslv(c(0,0), bgam.fn)[1]);
  # candidate UPE prices: 
  pa_bgam.vec[i] = papb_bgam.vec[1]; #
  pb_bgam.vec[i] = papb_bgam.vec[2]  #
  # market share of firm A
  xhat_bgam.vec[i] = (tau*(1+a.vec[i]-b)-pa_bgam.vec[i] + pb_bgam.vec[i])/(2*tau)
  #
}### end loop on all i (all a.vec) for case agbm
# check prices
pa_bgam.vec # 
pa_bgam.vec[regionII_b_025]# rising pa?
pb_bgam.vec[regionII_b_025]# non-monotonic?
xhat_bgam.vec # 
#
# End of case IV: bgam

## Finalize by adding the UPE bgbm prices (case II) for the range when they are valid (replacing remaining NAs)
#which(is.na(pa.vec))
#which(is.na(pb.vec))
pa.vec[regionII_b_025] = pa_bgbm.vec[regionII_b_025]
pb.vec[regionII_b_025] = pb_bgbm.vec[regionII_b_025]
xhat.vec[regionII_b_025] = xhat_bgbm.vec[regionII_b_025]
profita.vec[regionII_b_025] = profita_bgbm.vec[regionII_b_025]
profitb.vec[regionII_b_025] = profitb_bgbm.vec[regionII_b_025]

## build final data frame for ggplot
# dataframe for pa and pb solution when b=0 (firm B is located at L)
papb_025_u_n.df = data.frame(a.vec, pa.vec, pb.vec, xhat.vec, profita.vec, profitb.vec)
# I define locations of vertical dotted lines indicating where each equilibrium type begins
(end_nash = a.vec[nash_max_index])# use 0.25 instead
(end_nash = 0.25)
(begin_global = a.vec[marg_index_u])# use 0.25 instead

# plot of UPE prices, profits, and A's market share for b=0.25 => 1-b = 0.75
ggplot(papb_025_u_n.df, aes(x=a.vec)) + geom_line(aes(y=pa.vec), linetype="solid", size=1.2) + geom_line(aes(y=pb.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xhat.vec), linetype="dotdash", size=1.5, color="red") +labs(x="a = location of firm A", y="Equilibrium prices, profits, and market share")+ scale_x_continuous(breaks = seq(0,0.75,0.05)) + scale_y_continuous(breaks = seq(0,2,0.1))+ theme(axis.text.x = element_text(size = 18, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.45, y = 0.45, label =TeX("$p_A^{ug}$"), size = 8) + annotate("text", x = 0.1, y = 0.89, label =TeX("$p_A^n=p_A^u$"), size = 8) + annotate("text", x = 0.5, y = 0.6, label =TeX("$p_B^{ug}$"), size = 8) + annotate("text", x = 0.1, y = 1.1, label =TeX("$p_B^n=p_B^u$"), size = 8) + annotate("text", x = 0.65, y = 0.78, label =TeX("$\\hat{x}^{ug}$"), size = 8, color="red") + annotate("text", x = 0.3, y = 0.6, label =TeX("$\\hat{x}^{u}$"), size = 8, color="red") + annotate("text", x = 0.04, y = 0.5, label =TeX("$\\hat{x}^n=\\hat{x}^u$"), size = 8, color="red") + geom_line(aes(y=profita.vec), linetype="solid", size=1.2, color="blue") + geom_line(aes(y=profitb.vec), linetype="longdash", size=1.2, color="blue") + annotate("text", x = 0.05, y = 0.38, label =TeX("$\\pi_A^n=\\pi_A^u$"), size = 8, color="blue") + annotate("text", x = 0.05, y = 0.62, label =TeX("$\\pi_B^n=\\pi_B^u$"), size = 8, color="blue") + annotate("text", x = 0.5, y = 0.33, label =TeX("$\\pi_A^{ug}$"), size = 8, color="blue") + annotate("text", x = 0.5, y = 0.12, label =TeX("$\\pi_B^{ug}$"), size = 8, color="blue") + annotate("text", x = 0.67, y = 0.5, label =TeX("$p_A^n=p_A^{ug}=p_B^n=p_B^{ug}$"), size = 8) + geom_segment(aes(x = 0.67, y = 0.48, xend = 0.75, yend = 0.02), arrow = arrow(length = unit(0.5, "cm")), size =1) + geom_point(x=0.75, y=0, shape = 19, size=4, color="black", stroke=2) +  geom_vline(xintercept = end_nash, linetype = "dotted", size=1.2)+ geom_vline(xintercept = begin_global, linetype = "dotted", size=1.2)  + annotate("text", x = 0.35, y = 0.27, label =TeX("$\\pi_B^{u}$"), size = 8, color="blue") + annotate("text", x = 0.35, y = 0.48, label =TeX("$\\pi_A^{u}$"), size = 8, color="blue") + annotate("text", x = 0.35, y = 0.67, label =TeX("$p_A^u$"), size = 8, color="black") + annotate("text", x = 0.35, y = 0.9, label =TeX("$p_B^u$"), size = 8, color="black") + annotate("text", x = 0.1, y = 0.0, label ="Region I: Nash & UPE", size = 6, color="black") + annotate("text", x = 0.33, y = 0.0, label ="Region II: UPE", size = 6, color="black") + annotate("text", x = 0.52, y = 0.0, label ="Region III: UPEG", size = 6, color="black") 


# info for the caption or text
# Nash equilibrium holds for a=[0, 0.25]
nash_max_value
#UPE (Region II) holds for a=[0.25, 0.425] 
marg_b_max_red_value
marg_b_min_green_value

### Section 6,Figure 5 (prices, profits, xhat)
## b=0 => 1-b=1
b = 0 # distance of firm B from 1 (from the right) expressed as a fraction of L
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
# creating horizontal lines indicating whether the UPEG prices adhere to the marginal deviation constraints => Below, show that UPEG do NOT exist! (no Region III when b=0)
marg_a.vec = (tau*(1+a.vec-b)-2*pa_ug.vec + pb_ug.vec)/(2*tau)# should be >=0 to satisfy the marginal property
marg_b.vec = (tau*(1-a.vec+b)+pa_ug.vec - 2*pb_ug.vec)/(2*tau)# should be >=0 to satisfy the marginal property => all negative, no UPEG !!!
#
(marg_a_max_red_index = max(which(marg_a.vec < 0)))
(marg_a_min_green_index = max(which(marg_a.vec < 0)+1))# the first a.vec that satisfies the marginal deviation of firm A
(marg_a_max_red_value = a.vec[marg_a_max_red_index])
(marg_a_min_green_value = a.vec[marg_a_min_green_index])#value of a.vec
#
# marge_b.vec all negative! No UPEG!
(marg_b_max_green_index = min(which(marg_b.vec < 0)-1))
(marg_b_min_red_index = min(which(marg_b.vec < 0)))
(marg_b_max_red_index = max(which(marg_b.vec < 0)))
(marg_b_min_green_index = max(which(marg_b.vec < 0)+1))# 
# marge_b.vec Negative, below irrelevant!
#(marg_b_max_green_value = a.vec[marg_b_max_green_index])
#(marg_b_min_red_value = a.vec[marg_b_min_red_index])#value of a.vec
#(marg_b_max_red_value = a.vec[marg_b_max_red_index])#value of a.vec
#(marg_b_min_green_value = a.vec[marg_b_min_green_index])#value of a.vec#

# for this graph, we plot UPE prices only when BOTH marginal conditions are satisfied
#(marg_index_u = max(marg_a_min_green_index, marg_b_min_green_index))# 
#(marg_value_u = max(marg_a_min_green_value, marg_b_min_green_value))

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
#marg_index_u
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
# add the UPEG prices for the range when they exist => do not exist! No Region III when b=0
#pa.vec[marg_index_u: length(a.vec)] = pa_ug.vec[marg_index_u: length(a.vec)]
#pb.vec[marg_index_u: length(a.vec)] = pb_ug.vec[marg_index_u: length(a.vec)]
#xhat.vec[marg_index_u: length(a.vec)] = xhat_ug.vec[marg_index_u: length(a.vec)]
#profita.vec[marg_index_u: length(a.vec)] = profita_ug.vec[marg_index_u: length(a.vec)]
#profitb.vec[marg_index_u: length(a.vec)] = profitb_ug.vec[marg_index_u: length(a.vec)]

### New loop to compute UPE in Region II where the global and marginal undercut-proof properties is binding for one firm and not the other. 
# Recall, Region II applies for
(regionII_b_0 = which(is.na(pa.vec)))
a.vec[regionII_b_0]

# 4 cases are looped below: 
# I.   agam, eqs (8) and (9), A global and marginal
# II.  bgbm, eqs (7) and (10), B global and marginal
# III. agbm, eqs (8) and (10), A global, B marginal
# IV.  bgam, eqs (7) and (9), B global, A marginal

# 
# Case I "agam" => 
# A's global is binding eq (8), and marginal eq (9)
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_agam.vec = rep(NA, length(a.vec))# initialize
pb_agam.vec = rep(NA, length(a.vec))# initialize
xhat_agam.vec = rep(NA, length(a.vec))# initialize
profita_agam.vec = rep(NA, length(a.vec))# initialize
profitb_agam.vec = rep(NA, length(a.vec))# initialize
#
# Below is the loop that computes agam UPE prices using equations (8) and (9) 
for (i in 1:length(a.vec)){
  # x[2]=pb, x[1]=pa
  # use below for easier typing (to be pasted)
  # xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
  #
  agam.fn = function(x){
    c(x[1]*(tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau) - (x[2] - tau*(1-b-a.vec[i])) , tau*(1+a.vec[i] -b) -2*x[1] +x[2])
  }  
  #
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_agam.vec = unlist(nleqslv(c(0,0), agam.fn)[1]);
  # candidate UPE prices: 
  pa_agam.vec[i] = papb_agam.vec[1]; #
  pb_agam.vec[i] = papb_agam.vec[2]  #
  # market share of firm A
  xhat_agam.vec[i] = (tau*(1+a.vec[i]-b)-pa_agam.vec[i] + pb_agam.vec[i])/(2*tau)
  #
}### end loop on all i (all a.vec) for case agam
# check prices
pa_agam.vec # constant! Rule out
pb_agam.vec
xhat_agam.vec # constant! Rule out
# End of case I: agam

# Case II "bgbm" => is an UPE for b=0.25 !
# B's global is binding eq (7), and marginal eq (10) 
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_bgbm.vec = rep(NA, length(a.vec))# initialize
pb_bgbm.vec = rep(NA, length(a.vec))# initialize
xhat_bgbm.vec = rep(NA, length(a.vec))# initialize
profita_bgbm.vec = rep(NA, length(a.vec))# initialize
profitb_bgbm.vec = rep(NA, length(a.vec))# initialize
#
# Below is the loop that computes agam UPE prices using equations (7) and (10) 
for (i in 1:length(a.vec)){
  # x[2]=pb, x[1]=pa
  # use below for easier typing (to be pasted)
  # xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
  #
  bgbm.fn = function(x){
    c(x[2]*(1-(tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)) - (x[1]-tau*(1-b-a.vec[i])) , tau*(1-a.vec[i]+b) +x[1] -2*x[2])
  }  
  #
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_bgbm.vec = unlist(nleqslv(c(0,0), bgbm.fn)[1]);
  # candidate UPE prices: 
  pa_bgbm.vec[i] = papb_bgbm.vec[1]; #
  pb_bgbm.vec[i] = papb_bgbm.vec[2]  #
  # market share of firm A
  xhat_bgbm.vec[i] = (tau*(1+a.vec[i]-b)-pa_bgbm.vec[i] + pb_bgbm.vec[i])/(2*tau)
  #
}### end loop on all i (all a.vec) for case bgbm
# check prices
pa_bgbm.vec[regionII_b_0] # 
pb_bgbm.vec[regionII_b_0]
xhat_bgbm.vec[regionII_b_0] # 
profita_bgbm.vec = pa_bgbm.vec*xhat_bgbm.vec
profitb_bgbm.vec = pb_bgbm.vec*(1-xhat_bgbm.vec)
#
# Verify that A's global (8) >=0 
bgbm_8.vec = pa_bgbm.vec*xhat_bgbm.vec - (pb_bgbm.vec - tau*(1-b-a.vec))
bgbm_8.vec[regionII_b_0] # >0, yes
# Verify that A's marginal (9) >=0
bgbm_9.vec = tau*(1+a.vec-b) - 2*pa_bgbm.vec + pb_bgbm.vec
bgbm_9.vec[regionII_b_0] # >0, Yes~
# End of case II: bgbm: This is an UPE for b=0 !

# Case III "agbm" => 
# A's global is binding eq (8), B's marginal is binding (10)
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_agbm.vec = rep(NA, length(a.vec))# initialize
pb_agbm.vec = rep(NA, length(a.vec))# initialize
xhat_agbm.vec = rep(NA, length(a.vec))# initialize
profita_agbm.vec = rep(NA, length(a.vec))# initialize
profitb_agbm.vec = rep(NA, length(a.vec))# initialize
#
# Below is the loop that computes agbm UPE prices using equations (8) and (10) 
for (i in 1:length(a.vec)){
  # x[2]=pb, x[1]=pa
  # use below for easier typing (to be pasted)
  # xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
  #
  agbm.fn = function(x){
    c(x[1]*(tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau) - (x[2] - tau*(1-b-a.vec[i])) , tau*(1-a.vec[i] +b) +x[1] - 2*x[2])
  }  
  #
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_agbm.vec = unlist(nleqslv(c(0,0), agbm.fn)[1]);
  # candidate UPE prices: 
  pa_agbm.vec[i] = papb_agbm.vec[1]; #
  pb_agbm.vec[i] = papb_agbm.vec[2]  #
  # market share of firm A
  xhat_agbm.vec[i] = (tau*(1+a.vec[i]-b)-pa_agbm.vec[i] + pb_agbm.vec[i])/(2*tau)
  #
}### end loop on all i (all a.vec) for case agbm
# check prices
pa_agbm.vec # <0 => not an UPE
pb_agbm.vec
xhat_agbm.vec # 
#
# Verify that A's marginal (9) >=0 
agbm_9.vec = tau*(1+a.vec-b) - 2*pa_agbm.vec + pb_agbm.vec
agbm_9.vec[regionII_b_0] # >0, yes
# Verify that B's global (7) >=0
agbm_7.vec = pb_agbm.vec*(1-xhat_agbm.vec) - (pa_agbm.vec -tau*(1-b-a.vec))
agbm_7.vec[regionII_b_0] # 
# End of case III: agbm

# Case IV "bgam" => 
# B's global is binding eq (7), A's marginal is binding (9)
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_bgam.vec = rep(NA, length(a.vec))# initialize
pb_bgam.vec = rep(NA, length(a.vec))# initialize
xhat_bgam.vec = rep(NA, length(a.vec))# initialize
profita_bgam.vec = rep(NA, length(a.vec))# initialize
profitb_bgam.vec = rep(NA, length(a.vec))# initialize
#
# Below is the loop that computes agbm UPE prices using equations (7) and (9) 
for (i in 1:length(a.vec)){
  # x[2]=pb, x[1]=pa
  # use below for easier typing (to be pasted)
  # xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
  #
  bgam.fn = function(x){
    c(x[2]*(1 - (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)) - (x[1] - tau*(1-b-a.vec[i])) , tau*(1+a.vec[i]-b) -2*x[1]+ x[2])
  }  
  #
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_bgam.vec = unlist(nleqslv(c(0,0), bgam.fn)[1]);
  # candidate UPE prices: 
  pa_bgam.vec[i] = papb_bgam.vec[1]; #
  pb_bgam.vec[i] = papb_bgam.vec[2]  #
  # market share of firm A
  xhat_bgam.vec[i] = (tau*(1+a.vec[i]-b)-pa_bgam.vec[i] + pb_bgam.vec[i])/(2*tau)
  #
}### end loop on all i (all a.vec) for case agbm
# check prices
pa_bgam.vec # 
pa_bgam.vec[regionII_b_0]# 
pb_bgam.vec[regionII_b_0]# non-monotonic some <0?
xhat_bgam.vec # 
#
# End of case IV: bgam

## Finalize by adding the UPE bgbm prices for the range when they are valid (replacing remaining NAs)
#which(is.na(pa.vec))
#which(is.na(pb.vec))
pa.vec[regionII_b_0] = pa_bgbm.vec[regionII_b_0]
pb.vec[regionII_b_0] = pb_bgbm.vec[regionII_b_0]
xhat.vec[regionII_b_0] = xhat_bgbm.vec[regionII_b_0]
profita.vec[regionII_b_0] = profita_bgbm.vec[regionII_b_0]
profitb.vec[regionII_b_0] = profitb_bgbm.vec[regionII_b_0]

## build final data frame for ggplot
# dataframe for pa and pb solution when b=0 (firm B is located at L)
papb_0_u_n.df = data.frame(a.vec, pa.vec, pb.vec, xhat.vec, profita.vec, profitb.vec)
# I define locations of vertical dotted lines indicating where each equilibrium type begins
(end_nash = a.vec[nash_max_index])# use 0.25 instead
(end_nash = 0.3)
#(begin_global = a.vec[marg_index_u])# use 0.25 instead

# plot of UPE prices, profits, and A's market share for b=0 => 1-b = 1
ggplot(papb_0_u_n.df, aes(x=a.vec)) + geom_line(aes(y=pa.vec), linetype="solid", size=1.2) + geom_line(aes(y=pb.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xhat.vec), linetype="dotdash", size=1.5, color="red") +labs(x="a = location of firm A", y="Equilibrium prices, profits, and market share")+ scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,2,0.1))+ theme(axis.text.x = element_text(size = 18, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.48, y = 0.56, label =TeX("$p_B^{u}$"), size = 8) + annotate("text", x = 0.1, y = 0.9, label =TeX("$p_B^n=p_B^u$"), size = 8) + annotate("text", x = 0.55, y = 0.64, label =TeX("$p_A^{u}$"), size = 8) + annotate("text", x = 0.1, y = 1.1, label =TeX("$p_A^n=p_A^u$"), size = 8) + annotate("text", x = 0.65, y = 0.84, label =TeX("$\\hat{x}^{u}$"), size = 8, color="red")  + annotate("text", x = 0.2, y = 0.5, label =TeX("$\\hat{x}^n=\\hat{x}^u$"), size = 8, color="red") + geom_line(aes(y=profita.vec), linetype="solid", size=1.2, color="blue") + geom_line(aes(y=profitb.vec), linetype="longdash", size=1.2, color="blue") + annotate("text", x = 0.2, y = 0.38, label =TeX("$\\pi_B^n=\\pi_B^u$"), size = 8, color="blue") + annotate("text", x = 0.2, y = 0.62, label =TeX("$\\pi_A^n=\\pi_A^u$"), size = 8, color="blue") + annotate("text", x = 0.55, y = 0.385, label =TeX("$\\pi_A^{u}$"), size = 8, color="blue") + annotate("text", x = 0.55, y = 0.18, label =TeX("$\\pi_B^{u}$"), size = 8, color="blue") + annotate("text", x = 0.87, y = 0.5, label =TeX("$p_A^n=p_A^{u}=p_B^n=p_B^{u}$"), size = 8) + geom_segment(aes(x = 0.87, y = 0.48, xend = 1, yend = 0.02), arrow = arrow(length = unit(0.5, "cm")), size =1) + geom_point(x=1, y=0, shape = 19, size=4, color="black", stroke=2) +  geom_vline(xintercept = end_nash, linetype = "dotted", size=1.2) + annotate("text", x = 0.11, y = 0.01, label ="Region I: Nash & UPE", size = 6, color="black") + annotate("text", x = 0.6, y = 0.01, label ="Region II: UPE", size = 6, color="black")  

# info for the caption
# Nash equilibrium holds for a=[0, 0.3]
nash_max_value

### Section 6,Figure 6 (prices, profits, xhat)
## b=0.5 => 1-b=0.5
b = 0.5 # distance of firm B from 1 (from the right) expressed as a fraction of L
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
# Nash eql profits => No NE when b=0.5
# profita_n.vec = pa_n.vec*xhat_n.vec
# profitb_n.vec = pb_n.vec*(1-xhat_n.vec)# profit firm B
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
# marg_b.vec All positive [all green]
marg_b.vec
# So, only A's has a red zone (blow the green where marginal does hold)
(marg_index_u = marg_a_min_green_index)# 
(marg_value_u = marg_a_min_green_value)

# conditions guaranteeing Nash equilibrium => No NE, No Region 1
(nash_1.vec = (3+a.vec-b)^2 - 12*(a.vec+2*b)) # all <0 => no NE
(nash_2.vec = (3-a.vec+b)^2 - 12*(2*a.vec+b))
(nash_1_max = a.vec[max(which(nash_1.vec >= 0))])
(nash_1_max_index = max(which(nash_1.vec >= 0)))
(nash_2_max = a.vec[max(which(nash_2.vec >= 0))])
(nash_2_max_index = max(which(nash_2.vec >= 0)))
(nash_max_index = min(nash_1_max_index, nash_2_max_index))# this is max a.vec below which Nash equilibria exist
(nash_max_value = a.vec[nash_max_index])

# construct vectors of pa and pb that combine Nash [don't exist, do not include] prices with UPE prices, with a gap for values of a.vec in which none of these are valid.

# recall max a.vec where Nash exists
nash_max_index # does not exist!
# recall min a.vec where UPEG exist with global binding
marg_index_u # start of UPEG
#
## Build final vector of prices that combine all ranges where Nash [no!], UPE, and UPEG exist.
pa.vec = rep(NA, length(a.vec))# initialize
pb.vec = rep(NA, length(a.vec))# initialize
xhat.vec = rep(NA, length(a.vec))# initialize
profita.vec = rep(NA, length(a.vec))# initialize
profitb.vec = rep(NA, length(a.vec))# initialize
#
# add the Nash prices for the range when they exist
#pa.vec[1:nash_max_index] = pa_n.vec[1:nash_max_index]
#pb.vec[1:nash_max_index] = pb_n.vec[1:nash_max_index]
#xhat.vec[1:nash_max_index] = xhat_n.vec[1:nash_max_index]
#profita.vec[1:nash_max_index] = profita_n.vec[1:nash_max_index]
#profitb.vec[1:nash_max_index] = profitb_n.vec[1:nash_max_index]
#
# add the UPEG prices for the range when they exist
pa.vec[marg_index_u: length(a.vec)] = pa_ug.vec[marg_index_u: length(a.vec)]
pb.vec[marg_index_u: length(a.vec)] = pb_ug.vec[marg_index_u: length(a.vec)]
xhat.vec[marg_index_u: length(a.vec)] = xhat_ug.vec[marg_index_u: length(a.vec)]
profita.vec[marg_index_u: length(a.vec)] = profita_ug.vec[marg_index_u: length(a.vec)]
profitb.vec[marg_index_u: length(a.vec)] = profitb_ug.vec[marg_index_u: length(a.vec)]

### New loop to compute UPE in Region II where the global and marginal undercut-proof properties is binding for one firm and not the other. 
# Recall, Region II applies for
(regionII_b_05 = which(is.na(pa.vec)))
a.vec[regionII_b_05]

# 4 cases are looped below to compute UPE in Region II: 
# I.   agam, eqs (8) and (9), A global and marginal
# II.  bgbm, eqs (7) and (10), B global and marginal
# III. agbm, eqs (8) and (10), A global, B marginal
# IV.  bgam, eqs (7) and (9), B global, A marginal

# 
# Case I "agam" => 
# A's global is binding eq (8), and marginal eq (9)
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_agam.vec = rep(NA, length(a.vec))# initialize
pb_agam.vec = rep(NA, length(a.vec))# initialize
xhat_agam.vec = rep(NA, length(a.vec))# initialize
profita_agam.vec = rep(NA, length(a.vec))# initialize
profitb_agam.vec = rep(NA, length(a.vec))# initialize
#
# Below is the loop that computes agam UPE prices using equations (8) and (9) 
for (i in 1:length(a.vec)){
  # x[2]=pb, x[1]=pa
  # use below for easier typing (to be pasted)
  # xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
  #
  agam.fn = function(x){
    c(x[1]*(tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau) - (x[2] - tau*(1-b-a.vec[i])) , tau*(1+a.vec[i] -b) -2*x[1] +x[2])
  }  
  #
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_agam.vec = unlist(nleqslv(c(0,0), agam.fn)[1]);
  # candidate UPE prices: 
  pa_agam.vec[i] = papb_agam.vec[1]; #
  pb_agam.vec[i] = papb_agam.vec[2]  #
  # market share of firm A
  xhat_agam.vec[i] = (tau*(1+a.vec[i]-b)-pa_agam.vec[i] + pb_agam.vec[i])/(2*tau)
  #
}### end loop on all i (all a.vec) for case agam
# check prices
pa_agam.vec # constant! 
pb_agam.vec
xhat_agam.vec # constant! 
(profita_agam.vec = pa_agam.vec*xhat_agam.vec) # constant
(profitb_agam.vec = pb_agam.vec*(1-xhat_agam.vec))
#
# verify that B's global (7) >=0
agam_7.vec = pb_agam.vec*(1-xhat_agam.vec) - (pa_agam.vec - tau*(1-b-a.vec))
agam_7.vec[regionII_b_05] #> 0
# verify that B's marginal (10) is satisfied
agam_10.vec = tau*(1 - a.vec + b) +pa_agam.vec -2*pb_agam.vec
(agam_10.vec[regionII_b_05]) # > 0
#
# End of case I: agam (which is an UPE in Region II)

# Case II "bgbm" => is an UPE for b=0.25 !
# B's global is binding eq (7), and marginal eq (10) 
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_bgbm.vec = rep(NA, length(a.vec))# initialize
pb_bgbm.vec = rep(NA, length(a.vec))# initialize
xhat_bgbm.vec = rep(NA, length(a.vec))# initialize
profita_bgbm.vec = rep(NA, length(a.vec))# initialize
profitb_bgbm.vec = rep(NA, length(a.vec))# initialize
#
# Below is the loop that computes agam UPE prices using equations (7) and (10) 
for (i in 1:length(a.vec)){
  # x[2]=pb, x[1]=pa
  # use below for easier typing (to be pasted)
  # xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
  #
  bgbm.fn = function(x){
    c(x[2]*(1-(tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)) - (x[1]-tau*(1-b-a.vec[i])) , tau*(1-a.vec[i]+b) +x[1] -2*x[2])
  }  
  #
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_bgbm.vec = unlist(nleqslv(c(0,0), bgbm.fn)[1]);
  # candidate UPE prices: 
  pa_bgbm.vec[i] = papb_bgbm.vec[1]; #
  pb_bgbm.vec[i] = papb_bgbm.vec[2]  #
  # market share of firm A
  xhat_bgbm.vec[i] = (tau*(1+a.vec[i]-b)-pa_bgbm.vec[i] + pb_bgbm.vec[i])/(2*tau)
  #
}### end loop on all i (all a.vec) for case bgbm
# check prices
pa_bgbm.vec[regionII_b_05] # 
pb_bgbm.vec[regionII_b_05]
xhat_bgbm.vec[regionII_b_05] # 
profita_bgbm.vec = pa_bgbm.vec*xhat_bgbm.vec
profitb_bgbm.vec = pb_bgbm.vec*(1-xhat_bgbm.vec)
#
# Verify that A's global (8) >=0 
bgbm_8.vec = pa_bgbm.vec*xhat_bgbm.vec - (pb_bgbm.vec - tau*(1-b-a.vec))
bgbm_8.vec[regionII_b_05] # <0, No!
# Verify that A's marginal (9) >=0
bgbm_9.vec = tau*(1+a.vec-b) - 2*pa_bgbm.vec + pb_bgbm.vec
bgbm_9.vec[regionII_b_05] # mostly <0, No!
# End of case II: bgbm: This is an UPE for b=0.25

# Case III "agbm" => 
# A's global is binding eq (8), B's marginal is binding (10)
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_agbm.vec = rep(NA, length(a.vec))# initialize
pb_agbm.vec = rep(NA, length(a.vec))# initialize
xhat_agbm.vec = rep(NA, length(a.vec))# initialize
profita_agbm.vec = rep(NA, length(a.vec))# initialize
profitb_agbm.vec = rep(NA, length(a.vec))# initialize
#
# Below is the loop that computes agbm UPE prices using equations (8) and (10) 
for (i in 1:length(a.vec)){
  # x[2]=pb, x[1]=pa
  # use below for easier typing (to be pasted)
  # xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
  #
  agbm.fn = function(x){
    c(x[1]*(tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau) - (x[2] - tau*(1-b-a.vec[i])) , tau*(1-a.vec[i] +b) +x[1] - 2*x[2])
  }  
  #
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_agbm.vec = unlist(nleqslv(c(0,0), agbm.fn)[1]);
  # candidate UPE prices: 
  pa_agbm.vec[i] = papb_agbm.vec[1]; #
  pb_agbm.vec[i] = papb_agbm.vec[2]  #
  # market share of firm A
  xhat_agbm.vec[i] = (tau*(1+a.vec[i]-b)-pa_agbm.vec[i] + pb_agbm.vec[i])/(2*tau)
  #
}### end loop on all i (all a.vec) for case agbm
# check prices
pa_agbm.vec # 
pb_agbm.vec
xhat_agbm.vec # Something wrong, xhat > 0.5
#
# Verify that A's marginal (9) >=0 
agbm_9.vec = tau*(1+a.vec-b) - 2*pa_agbm.vec + pb_agbm.vec
agbm_9.vec[regionII_b_05] # >0, yes
# Verify that B's global (7) >=0
agbm_7.vec = pb_agbm.vec*(1-xhat_agbm.vec) - (pa_agbm.vec -tau*(1-b-a.vec))
agbm_7.vec[regionII_b_05] # <0,
# End of case III: agbm => Not an UPE because xhat > 0.5

# Case IV "bgam" => 
# B's global is binding eq (7), A's marginal is binding (9)
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_bgam.vec = rep(NA, length(a.vec))# initialize
pb_bgam.vec = rep(NA, length(a.vec))# initialize
xhat_bgam.vec = rep(NA, length(a.vec))# initialize
profita_bgam.vec = rep(NA, length(a.vec))# initialize
profitb_bgam.vec = rep(NA, length(a.vec))# initialize
#
# Below is the loop that computes agbm UPE prices using equations (7) and (9) 
for (i in 1:length(a.vec)){
  # x[2]=pb, x[1]=pa
  # use below for easier typing (to be pasted)
  # xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
  #
  bgam.fn = function(x){
    c(x[2]*(1 - (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)) - (x[1] - tau*(1-b-a.vec[i])) , tau*(1+a.vec[i]-b) -2*x[1]+ x[2])
  }  
  #
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_bgam.vec = unlist(nleqslv(c(0,0), bgam.fn)[1]);
  # candidate UPE prices: 
  pa_bgam.vec[i] = papb_bgam.vec[1]; #
  pb_bgam.vec[i] = papb_bgam.vec[2]  #
  # market share of firm A
  xhat_bgam.vec[i] = (tau*(1+a.vec[i]-b)-pa_bgam.vec[i] + pb_bgam.vec[i])/(2*tau)
  #
}### end loop on all i (all a.vec) for case agbm
# check prices
pa_bgam.vec # 
pa_bgam.vec[regionII_b_05]# rising pa?
pb_bgam.vec[regionII_b_05]# <0 for small a
which(pb_bgam.vec[regionII_b_05] <0) # mostly <0
xhat_bgam.vec # 
(profita_bgam.vec = pa_bgam.vec*xhat_bgam.vec)
(profitb_bgam.vec = pb_bgam.vec*(1-xhat_bgam.vec)) # Mostly < 0 
#

## now verify if bgam are equilibria 
#
# verify that A's global (8) is satisfied
bgam_8.vec = pa_bgam.vec*xhat_bgam.vec - (pb_bgam.vec-tau*(1-b-a.vec))
bgam_8.vec[regionII_b_05]# > 0 yes!
# verify that B's marginal (10) is satisfied
bgam_10.vec = tau*(1-a.vec+b)+pa_bgam.vec-2*pb_bgam.vec
bgam_10.vec[regionII_b_05] # >0, Yes!
# Hence bgam is an in a limited range of a.vec (not near zero)
#
# End of case IV: bgam

## Finalize by adding the UPE agam 
pa.vec[regionII_b_05] = pa_agam.vec[regionII_b_05]
pb.vec[regionII_b_05] = pb_agam.vec[regionII_b_05]
xhat.vec[regionII_b_05] = xhat_agam.vec[regionII_b_05]
profita.vec[regionII_b_05] = profita_agam.vec[regionII_b_05]
profitb.vec[regionII_b_05] = profitb_agam.vec[regionII_b_05]

# pa.vec[regionII_b_05_bgam] = pa_bgam.vec[regionII_b_05_bgam]
# pb.vec[regionII_b_05_bgam] = pb_bgam.vec[regionII_b_05_bgam]
# xhat.vec[regionII_b_05_bgam] = xhat_bgam.vec[regionII_b_05_bgam]
# profita.vec[regionII_b_05_bgam] = profita_bgam.vec[regionII_b_05_bgam]
# profitb.vec[regionII_b_05_bgam] = profitb_bgam.vec[regionII_b_05_bgam]

# # Now, fill in the rest (leftover) with pa and pb agam
# (regionII_b_05_leftover = which(is.na(pa.vec)))
# (pa.vec[regionII_b_05_leftover] = pa_agam.vec[regionII_b_05_leftover])
# (pb.vec[regionII_b_05_leftover] = pb_agam.vec[regionII_b_05_leftover])
# (xhat.vec[regionII_b_05_leftover] = xhat_agam.vec[regionII_b_05_leftover])
# (profita.vec[regionII_b_05_leftover] = profita_agam.vec[regionII_b_05_leftover])
# (profitb.vec[regionII_b_05_leftover] = profitb_agam.vec[regionII_b_05_leftover])

## build final data frame for ggplot
# dataframe for pa and pb solution when b=0 (firm B is located at L)
papb_05_u_n.df = data.frame(a.vec, pa.vec, pb.vec, xhat.vec, profita.vec, profitb.vec)
# I define locations of vertical dotted lines indicating where each equilibrium type begins
(end_II = max(a.vec[regionII_b_05]))

# plot of UPE prices, profits, and A's market share for b=0.25 => 1-b = 0.75
ggplot(papb_05_u_n.df, aes(x=a.vec)) + geom_line(aes(y=pa.vec), linetype="solid", size=1.2) + geom_line(aes(y=pb.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xhat.vec), linetype="dotdash", size=1.5, color="red") +labs(x="a = location of firm A", y="Equilibrium prices, profits, and market share")+ scale_x_continuous(breaks = seq(0,0.5,0.05)) + scale_y_continuous(breaks = seq(0,2,0.1))+ theme(axis.text.x = element_text(size = 18, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.25, y = 0.58, label =TeX("$p_A^{ug}$"), size = 8) + annotate("text", x = 0.15, y = 0.63, label =TeX("$p_A^u$"), size = 8) + annotate("text", x = 0.25, y = 0.38, label =TeX("$p_B^{ug}$"), size = 8) + annotate("text", x = 0.15, y = 0.48, label =TeX("$p_B^u$"), size = 8) + annotate("text", x = 0.4, y = 0.47, label =TeX("$\\hat{x}^{ug}$"), size = 8, color="red")  + annotate("text", x = 0.05, y = 0.26, label =TeX("$\\hat{x}^u$"), size = 8, color="red") + geom_line(aes(y=profita.vec), linetype="solid", size=1.2, color="blue") + geom_line(aes(y=profitb.vec), linetype="longdash", size=1.2, color="blue") + annotate("text", x = 0.05, y = 0.14, label =TeX("$\\pi_A^u$"), size = 8, color="blue") + annotate("text", x = 0.05, y = 0.47, label =TeX("$\\pi_B^u$"), size = 8, color="blue") + annotate("text", x = 0.25, y = 0.14, label =TeX("$\\pi_A^{ug}$"), size = 8, color="blue") + annotate("text", x = 0.25, y = 0.24, label =TeX("$\\pi_B^{ug}$"), size = 8, color="blue") + annotate("text", x = 0.67-0.2, y = 0.3, label =TeX("$p_A^n=p_A^{ug}=p_B^n=p_B^{ug}$"), size = 8) + geom_segment(aes(x = 0.67-0.2, y = 0.28, xend = 0.75-0.25, yend = 0.02), arrow = arrow(length = unit(0.5, "cm")), size =1) + geom_point(x=0.5, y=0, shape = 19, size=4, color="black", stroke=2) +  geom_vline(xintercept = end_II, linetype = "dotted", size=1.2) + annotate("text", x = 0.1, y = 0.05, label ="Region II: UPE", size = 6, color="black") + annotate("text", x = 0.35, y = 0.05, label ="Region III: UPEG", size = 6, color="black") 

# info for the caption
# Nash equilibrium holds for a=[0, 0.25]
nash_max_value
# undercut-proof ug (global binding) holds for a=(0.25, 0.42] half-open interval
marg_b_max_red_value
marg_b_min_green_value

### Not in the paper: Constructing a chart where a=0.50 and b varies between 0.50 and 1. 
# Purpose of this exercise: to plot a mirror image of Figure 6 where b varies and a is fixed. I want to verify that pbu and profitbu are constant for b between 0.775 and 1. Success!!!

a = 0.5 # distance of firm A from 0 (from the left) expressed as a fraction of L
tau = 1 # transportation cost parameter
#
# vector of "b" as firm B gets closer to firm A located at "a" 
(b.vec = seq(1-a, 0, -a/100))
# Initialize UPE vectors of pa and pb solutions to be plotted
pa_ug.vec = rep(NA, length(b.vec))# initialize
pb_ug.vec = rep(NA, length(b.vec))# initialize
xhat_ug.vec = rep(NA, length(b.vec))# initialize
profita_ug.vec = rep(NA, length(b.vec))# initialize
profitb_ug.vec = rep(NA, length(b.vec))# initialize
#
## Run a loop over all firm A's location. This loop solves for Nash prices and UPEG (Regions I and III in the chart). Region II is computed in separate loop
#
for (i in 1:length(b.vec)){
  # the function is a 2-dim vector corresponding to equations (7) and (8) in the paper  (binding global undercut-proof property). x[1] is pa, x[2] is pb
  upeg.fn = function(x){c(x[2]*(1-(tau*(1+a-b.vec[i])-x[1]+x[2])/(2*tau)) - (x[1]-tau*(1-b.vec[i]-a)), x[1]*((tau*(1+a-b.vec[i])-x[1]+x[2])/(2*tau)) - (x[2]-tau*(1-b.vec[i]-a)))
  }
  # solve for UPEG prices and place them in a 2-dim vector for pa and pb
  papb_ug.vec = unlist(nleqslv(c(0,0), upeg.fn)[1]);
  # place it in pa.vec and pb.vec to be plotted
  pa_ug.vec[i] = papb_ug.vec[1]; # UPEG pa
  pb_ug.vec[i] = papb_ug.vec[2]  # UPEG pb
  # market share of firm A
  xhat_ug.vec[i] = (tau*(1+a-b.vec[i])-pa_ug.vec[i] + pb_ug.vec[i])/(2*tau)
}## end loop on all i (all v.vec)
# CHECK WHY I GET NEGATIVE PA AND PB !!!
# UPEG profits firm A and firm B
profita_ug.vec = pa_ug.vec*xhat_ug.vec
profitb_ug.vec = pb_ug.vec*(1-xhat_ug.vec)# profit firm B
#
## Now, check whether UPEG marginal conditions hold
# creating horizontal lines indicating whether the UPEG prices adhere to the marginal deviation constraints
(marg_a.vec = (tau*(1+a-b.vec)-2*pa_ug.vec + pb_ug.vec)/(2*tau))# should be >=0 to satisfy the marginal property [all >0]
(marg_b.vec = (tau*(1-a+b.vec)+pa_ug.vec - 2*pb_ug.vec)/(2*tau))# should be >=0 to satisfy the marginal property
#
(marg_a_max_red_index = min(which(marg_a.vec < 0)))# none
#
# marg_b.vec
marg_b.vec
(marg_b_min_red_index = min(which(marg_b.vec < 0)))#
(marg_b_max_green_index = max(which(marg_b.vec >= 0)))# #
# So, only B's has a red zone (blow the green where marginal does hold)
(marg_index_u = marg_b_max_green_index)# 
(marg_value_u = b.vec[marg_b_max_green_index])

# construct vectors of pa and pb that combine Nash [don't exist, do not include] prices with UPE prices, with a gap for values of a.vec in which none of these are valid.

# recall max a.vec where Nash exists
#nash_max_index # does not exist!
# recall min a.vec where UPEG exist with global binding
marg_index_u # start of UPEG
#
## Build final vector of prices that combine all ranges where Nash [no!], UPE, and UPEG exist.
pa.vec = rep(NA, length(b.vec))# initialize
pb.vec = rep(NA, length(b.vec))# initialize
xhat.vec = rep(NA, length(b.vec))# initialize
profita.vec = rep(NA, length(b.vec))# initialize
profitb.vec = rep(NA, length(b.vec))# initialize
#
# add the Nash prices for the range when they exist
#pa.vec[1:nash_max_index] = pa_n.vec[1:nash_max_index]
#pb.vec[1:nash_max_index] = pb_n.vec[1:nash_max_index]
#xhat.vec[1:nash_max_index] = xhat_n.vec[1:nash_max_index]
#profita.vec[1:nash_max_index] = profita_n.vec[1:nash_max_index]
#profitb.vec[1:nash_max_index] = profitb_n.vec[1:nash_max_index]
#
# add the UPEG prices for the range when they exist
(pa.vec[1:marg_index_u] = pa_ug.vec[1:marg_index_u])
(pb.vec[1:marg_index_u] = pb_ug.vec[1:marg_index_u])
(xhat.vec[1:marg_index_u] = xhat_ug.vec[1:marg_index_u])
(profita.vec[1:marg_index_u] = profita_ug.vec[1:marg_index_u])
(profitb.vec[1:marg_index_u] = profitb_ug.vec[1:marg_index_u])

### New loop to compute UPE in Region II where the global and marginal undercut-proof properties is binding for one firm and not the other. 
# Recall, Region II applies for
(regionII_a_05 = which(is.na(pa.vec)))
b.vec[regionII_a_05]

# 4 cases are looped below to compute UPE in Region II: 
# I.   agam, eqs (8) and (9), A global and marginal
# II.  bgbm, eqs (7) and (10), B global and marginal
# III. agbm, eqs (8) and (10), A global, B marginal
# IV.  bgam, eqs (7) and (9), B global, A marginal

# 
# Case I "agam" => 
# A's global is binding eq (8), and marginal eq (9)
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_agam.vec = rep(NA, length(b.vec))# initialize
pb_agam.vec = rep(NA, length(b.vec))# initialize
xhat_agam.vec = rep(NA, length(b.vec))# initialize
profita_agam.vec = rep(NA, length(b.vec))# initialize
profitb_agam.vec = rep(NA, length(b.vec))# initialize
#
# Below is the loop that computes agam UPE prices using equations (8) and (9) 
for (i in 1:length(b.vec)){
  # x[2]=pb, x[1]=pa
  # use below for easier typing (to be pasted)
  # xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
  #
  agam.fn = function(x){
    c(x[1]*(tau*(1+a-b.vec[i]) -x[1] +x[2])/(2*tau) - (x[2] - tau*(1-b.vec[i]-a)) , tau*(1+a -b.vec[i]) -2*x[1] +x[2])
  }  
  #
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_agam.vec = unlist(nleqslv(c(0,0), agam.fn)[1]);
  # candidate UPE prices: 
  pa_agam.vec[i] = papb_agam.vec[1]; #
  pb_agam.vec[i] = papb_agam.vec[2]  #
  # market share of firm A
  xhat_agam.vec[i] = (tau*(1+a-b.vec[i])-pa_agam.vec[i] + pb_agam.vec[i])/(2*tau)
  #
}### end loop on all i (all a.vec) for case agam
# check prices
pa_agam.vec #
pb_agam.vec
xhat_agam.vec # 
(profita_agam.vec = pa_agam.vec*xhat_agam.vec) # constant
(profitb_agam.vec = pb_agam.vec*(1-xhat_agam.vec))
#
# verify that B's global (7) >=0
agam_7.vec = pb_agam.vec*(1-xhat_agam.vec) - (pa_agam.vec - tau*(1-b.vec-a))
agam_7.vec[regionII_a_05] # all < 0
# verify that B's marginal (10) is satisfied
agam_10.vec = tau*(1 - a + b.vec) +pa_agam.vec -2*pb_agam.vec
(agam_10.vec[regionII_a_05]) # mostly < 0
#
# End of case I: agam (NOT an UPE in Region II)

# Case II "bgbm" => 
# B's global is binding eq (7), and marginal eq (10) 
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_bgbm.vec = rep(NA, length(b.vec))# initialize
pb_bgbm.vec = rep(NA, length(b.vec))# initialize
xhat_bgbm.vec = rep(NA, length(b.vec))# initialize
profita_bgbm.vec = rep(NA, length(b.vec))# initialize
profitb_bgbm.vec = rep(NA, length(b.vec))# initialize
#
# Below is the loop that computes agam UPE prices using equations (7) and (10) 
for (i in 1:length(b.vec)){
  # x[2]=pb, x[1]=pa
  # use below for easier typing (to be pasted)
  # xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
  #
  bgbm.fn = function(x){
    c(x[2]*(1-(tau*(1+a-b.vec[i]) -x[1] +x[2])/(2*tau)) - (x[1]-tau*(1-b.vec[i]-a)) , tau*(1-a+b.vec[i]) +x[1] -2*x[2])
  }  
  #
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_bgbm.vec = unlist(nleqslv(c(0,0), bgbm.fn)[1]);
  # candidate UPE prices: 
  pa_bgbm.vec[i] = papb_bgbm.vec[1]; #
  pb_bgbm.vec[i] = papb_bgbm.vec[2]  #
  # market share of firm A
  xhat_bgbm.vec[i] = (tau*(1+a-b.vec[i])-pa_bgbm.vec[i] + pb_bgbm.vec[i])/(2*tau)
  #
}### end loop on all i (all a.vec) for case bgbm
# check prices
pa_bgbm.vec[regionII_a_05] # 
pb_bgbm.vec[regionII_a_05]
xhat_bgbm.vec[regionII_a_05] # 
profita_bgbm.vec = pa_bgbm.vec*xhat_bgbm.vec
profitb_bgbm.vec = pb_bgbm.vec*(1-xhat_bgbm.vec)
#
# Verify that A's global (8) >=0 
bgbm_8.vec = pa_bgbm.vec*xhat_bgbm.vec - (pb_bgbm.vec - tau*(1-b.vec-a))
bgbm_8.vec[regionII_a_05] # All > 0, Yes!
# Verify that A's marginal (9) >=0
bgbm_9.vec = tau*(1+a-b.vec) - 2*pa_bgbm.vec + pb_bgbm.vec
bgbm_9.vec[regionII_a_05] # All >0, Yes!
# End of case II: bgbm: This is an UPE for a=0.5

# Case III "agbm" => 
# A's global is binding eq (8), B's marginal is binding (10)
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_agbm.vec = rep(NA, length(b.vec))# initialize
pb_agbm.vec = rep(NA, length(b.vec))# initialize
xhat_agbm.vec = rep(NA, length(b.vec))# initialize
profita_agbm.vec = rep(NA, length(b.vec))# initialize
profitb_agbm.vec = rep(NA, length(b.vec))# initialize
#
# Below is the loop that computes agbm UPE prices using equations (8) and (10) 
for (i in 1:length(b.vec)){
  # x[2]=pb, x[1]=pa
  # use below for easier typing (to be pasted)
  # xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
  #
  agbm.fn = function(x){
    c(x[1]*(tau*(1+a-b.vec[i]) -x[1] +x[2])/(2*tau) - (x[2] - tau*(1-b.vec[i]-a)) , tau*(1-a +b.vec[i]) +x[1] - 2*x[2])
  }  
  #
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_agbm.vec = unlist(nleqslv(c(0,0), agbm.fn)[1]);
  # candidate UPE prices: 
  pa_agbm.vec[i] = papb_agbm.vec[1]; #
  pb_agbm.vec[i] = papb_agbm.vec[2]  #
  # market share of firm A
  xhat_agbm.vec[i] = (tau*(1+a-b.vec[i])-pa_agbm.vec[i] + pb_agbm.vec[i])/(2*tau)
  #
}### end loop on all i (all a.vec) for case agbm
# check prices
pa_agbm.vec 
pa_agbm.vec[regionII_a_05]# => mostly <0 in region II
pb_agbm.vec
xhat_agbm.vec # 
#
# Verify that A's marginal (9) >=0 
agbm_9.vec = tau*(1+a-b.vec) - 2*pa_agbm.vec + pb_agbm.vec
agbm_9.vec[regionII_a_05] # >0, yes
# Verify that B's global (7) >=0
agbm_7.vec = pb_agbm.vec*(1-xhat_agbm.vec) - (pa_agbm.vec -tau*(1-b.vec-a))
agbm_7.vec[regionII_a_05] # >0,
# End of case III: agbm => Not an UPE because pa < for most of region II. 

# Case IV "bgam" => 
# B's global is binding eq (7), A's marginal is binding (9)
# Initialize vectors of pa and pb (later the computed values will be inserted into Region II of pa.vec and pb.vec)
pa_bgam.vec = rep(NA, length(b.vec))# initialize
pb_bgam.vec = rep(NA, length(b.vec))# initialize
xhat_bgam.vec = rep(NA, length(b.vec))# initialize
profita_bgam.vec = rep(NA, length(b.vec))# initialize
profitb_bgam.vec = rep(NA, length(b.vec))# initialize
#
# Below is the loop that computes agbm UPE prices using equations (7) and (9) 
for (i in 1:length(b.vec)){
  # x[2]=pb, x[1]=pa
  # use below for easier typing (to be pasted)
  # xhat.vec[i] = (tau*(1+a.vec[i]-b) -x[1] +x[2])/(2*tau)
  #
  bgam.fn = function(x){
    c(x[2]*(1 - (tau*(1+a-b.vec[i]) -x[1] +x[2])/(2*tau)) - (x[1] - tau*(1-b.vec[i]-a)) , tau*(1+a-b.vec[i]) -2*x[1]+ x[2])
  }  
  #
  # solve for UPE prices and place them in a 2-dim vector for pa and pb
  papb_bgam.vec = unlist(nleqslv(c(0,0), bgam.fn)[1]);
  # candidate UPE prices: 
  pa_bgam.vec[i] = papb_bgam.vec[1]; #
  pb_bgam.vec[i] = papb_bgam.vec[2]  #
  # market share of firm A
  xhat_bgam.vec[i] = (tau*(1+a-b.vec[i])-pa_bgam.vec[i] + pb_bgam.vec[i])/(2*tau)
  #
}### end loop on all i (all a.vec) for case agbm
# check prices
pa_bgam.vec # 
pa_bgam.vec[regionII_a_05]# 
pb_bgam.vec[regionII_a_05]# 
which(pb_bgam.vec[regionII_a_05] <0) #
xhat_bgam.vec # 
(profita_bgam.vec = pa_bgam.vec*xhat_bgam.vec)
(profitb_bgam.vec = pb_bgam.vec*(1-xhat_bgam.vec)) # Mostly < 0 
#

## now verify if bgam are equilibria 
#
# verify that A's global (8) is satisfied
bgam_8.vec = pa_bgam.vec*xhat_bgam.vec - (pb_bgam.vec-tau*(1-b.vec-a))
bgam_8.vec[regionII_a_05]# > 0 yes!
# verify that B's marginal (10) is satisfied
bgam_10.vec = tau*(1-a+b.vec)+pa_bgam.vec-2*pb_bgam.vec
bgam_10.vec[regionII_a_05] # >0, Yes!
# Hence bgam is an in a limited range of a.vec (not near zero)
#
# End of case IV: bgam

## Finalize by adding the UPE agam 
(pa.vec[regionII_a_05] = pa_bgbm.vec[regionII_a_05])
(pb.vec[regionII_a_05] = pb_bgbm.vec[regionII_a_05])
(xhat.vec[regionII_a_05] = xhat_bgbm.vec[regionII_a_05])
profita.vec[regionII_a_05] = profita_bgbm.vec[regionII_a_05]
profitb.vec[regionII_a_05] = profitb_bgbm.vec[regionII_a_05]

## build final data frame for ggplot
# dataframe for pa and pb solution when a=0.5 
papb_a_05_u_n.df = data.frame(b.vec, pa.vec, pb.vec, xhat.vec, profita.vec, profitb.vec)
# I define locations of vertical dotted lines indicating where each equilibrium type begins
(end_II = min(1-b.vec[regionII_a_05]))

# plot of UPE prices, profits, and A's market share for b=0.25 => 1-b = 0.75
ggplot(papb_a_05_u_n.df, aes(x=1-b.vec)) + geom_line(aes(y=pa.vec), linetype="solid", size=1.2) + geom_line(aes(y=pb.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xhat.vec), linetype="dotdash", size=1.5, color="red") +labs(x="b = location of firm B", y="Equilibrium prices, profits, and market share")+ scale_x_continuous(breaks = seq(0.5,1,0.05)) + scale_y_continuous(breaks = seq(0,2,0.1))+ theme(axis.text.x = element_text(size = 18, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + annotate("text", x = 0.25, y = 0.58, label =TeX("$p_A^{ug}$"), size = 8) + annotate("text", x = 0.15, y = 0.63, label =TeX("$p_A^u$"), size = 8) + annotate("text", x = 0.25, y = 0.38, label =TeX("$p_B^{ug}$"), size = 8) + annotate("text", x = 0.15, y = 0.48, label =TeX("$p_B^u$"), size = 8) + annotate("text", x = 0.4, y = 0.47, label =TeX("$\\hat{x}^{ug}$"), size = 8, color="red")  + annotate("text", x = 0.05, y = 0.26, label =TeX("$\\hat{x}^u$"), size = 8, color="red") + geom_line(aes(y=profita.vec), linetype="solid", size=1.2, color="blue") + geom_line(aes(y=profitb.vec), linetype="longdash", size=1.2, color="blue") + annotate("text", x = 0.05, y = 0.14, label =TeX("$\\pi_A^u$"), size = 8, color="blue") + annotate("text", x = 0.05, y = 0.47, label =TeX("$\\pi_B^u$"), size = 8, color="blue") + annotate("text", x = 0.25, y = 0.14, label =TeX("$\\pi_A^{ug}$"), size = 8, color="blue") + annotate("text", x = 0.25, y = 0.24, label =TeX("$\\pi_B^{ug}$"), size = 8, color="blue") + annotate("text", x = 0.67-0.2, y = 0.3, label =TeX("$p_A^n=p_A^{ug}=p_B^n=p_B^{ug}$"), size = 8) + geom_segment(aes(x = 0.67-0.2, y = 0.28, xend = 0.75-0.25, yend = 0.02), arrow = arrow(length = unit(0.5, "cm")), size =1) + geom_point(x=0.5, y=0, shape = 19, size=4, color="black", stroke=2) +  geom_vline(xintercept = end_II, linetype = "dotted", size=1.2) + annotate("text", x = 0.1, y = 0.05, label ="Region II: UPE", size = 6, color="black") + annotate("text", x = 0.35, y = 0.05, label ="Region III: UPEG", size = 6, color="black") 

# info for the caption
# Nash equilibrium holds for a=[0, 0.25]
#nash_max_value
# undercut-proof ug (global binding) holds for a=(0.25, 0.42] half-open interval
#marg_b_max_red_value
#marg_b_min_green_value


### End of code 



