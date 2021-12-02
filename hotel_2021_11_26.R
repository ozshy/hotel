library(nleqslv)
### testing the function
test_fn = function(x){c(x[1]+1/x[2]-4, x[1]+x[2]^2-10)}
nleqslv(c(1,2), test_fn)
# check solution 3.604574 2.528918
3.604574 + 1/2.528918
3.604574 + 2.528918^2

#### Hotelling paper begins ####
### Characterizing the area of non existence of NE





L = 1 #Length of the linear street
a = 1/3 # distance of firm A from the origin 0 (from the left)
b = 1/3 # distance of firm A from 1 (from the right)
tau = 1 # transportation cost parameter
# Notation: 
# x[1] = pa (price set by firm A) 
# x[2] = pb (price set by firm B)

# System to be solved (just prices)
upe.fn = function(x){c(x[1]*(tau*(L+a-b)-x[1]+x[2])/(2*tau) - L*(x[2]-tau*(L-b-a)), x[2]*(L-(tau*(L+a-b)-x[1]+x[2])/(2*tau)) - L*(x[1]-tau*(L-b-a)))
}
# solve it
nleqslv(c(1,1), upe.fn)
