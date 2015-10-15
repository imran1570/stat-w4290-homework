CDF = function (x, dist) {
  integrate(f = dist, lower = -Inf, upper = x)$value
}
objective = function (x, quantile, dist) {
  (CDF(x, dist) - quantile)^2
}
find_quantile = function (dist, quantile) {
  result = nlminb(start = 0, objective = objective, quantile = quantile, dist = dist)$par
  return (result)
}

crazy_eqn = function (x) {
  abs(x+1)/((x^2+1)^2)
}

z = integrate(crazy_eqn, lower=-Inf, upper=Inf)

crazy_eqn_to_integrate = function(x) {
  z$value * (abs(x+1)/((x^2+1)^2))
}

find_quantile(dist = crazy_eqn_to_integrate, quantile = 0.95)

integrate(crazy_eqn_to_integrate, lower=-Inf, upper=0.03161596)
