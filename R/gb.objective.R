gb.objective <-
function(theta, r, te, y, s0, market.calls, call.strikes, market.puts, put.strikes, lambda = 1)
{
  a     = theta[1]
  b     = theta[2]   
  v     = theta[3]
  w     = theta[4]  
  discount.factor  = exp(-r * te)
  expected.value   = b * beta(v + 1/a, w - 1/a)/beta(v,w)   ### See page 395 of jpk

  ###
  ### Calls
  ###

  # Remove this later....
  #prob.1 = pgb(call.strikes, a = a, b = b, v = (v + 1/a) , w = (w - 1/a) )
  #prob.2 = pgb(call.strikes, a = a, b = b, v = v ,         w = w )
  #theoretical.calls = discount.factor * ( expected.value * (1 - prob.1)  - call.strikes * (1 - prob.2) )

  theoretical.calls = gb.option.price(r = r, te = te, s0 = s0, k = call.strikes, y = y, a = a, b = b, v = v, w = w)$call

  ###
  ### Puts
  ###

  # Remove this later....
  #prob.tmp.1 = pgb(put.strikes, a = a, b = b, v = (v + 1/a) , w = (w - 1/a) )
  #prob.tmp.2 = pgb(put.strikes, a = a, b = b, v = v ,         w = w )
  #tmp.calls  = discount.factor * ( expected.value * (1 - prob.tmp.1)  - put.strikes * (1 - prob.tmp.2) )
  #theoretical.puts  = tmp.calls + discount.factor * ( put.strikes - expected.value)

  theoretical.puts = gb.option.price(r = r, te = te, s0 = s0, k = put.strikes, y = y, a = a, b = b, v = v, w = w)$put
   
  ###
  ### Finally ... the objective function
  ###

  if ( (a <= 0) | (a > 20) | (b <= 0) | (v <= 0) | (w <= 0) | (w <= 4/a) ) { obj = 10^7 } else { obj = sum((theoretical.calls - market.calls)^2) + sum((theoretical.puts - market.puts)^2) + lambda * (s0*exp(-y*te) - expected.value * discount.factor )^2 }
 
  obj

}
