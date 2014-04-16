### Copulas in Finance - Applied Math Seminar
### Dan Rieman - djr2144@columbia.edu

spy = read.csv(url("http://www.columbia.edu/~djr2144/Seminar/SPY_Historical_Prices.csv"), header = T)
iwm = read.csv(url("http://www.columbia.edu/~djr2144/Seminar/IWM_Historical_Prices.csv"), header = T)

spy = spy[!is.na(spy[,2]),]
iwm = iwm[!is.na(iwm[,2]),]

if(ncol(spy) == ncol(iwm) && nrow(spy) == nrow(iwm)){
	n = nrow(spy)
	m = ncol(spy)
}

require(fBasics)
require(copula)

r.spy = (spy[2:n, m] / spy[1:(n-1), m]) - 1
r.iwm = (iwm[2:n, m] / iwm[1:(n-1), m]) - 1

k.tau = cor.test(r.spy, r.iwm, method = "kendall")
p.cor = cor.test(r.spy, r.iwm, method = "pearson")
print(paste("Kendall's tau: ", k.tau$est))
print(paste("Pearson's correlation: ", p.cor$est))

edata = cbind(rank(r.spy)/(n+1), rank(r.iwm)/(n+1))
ncop = ellipCopula("normal", param = c(0), dim = 2, dispstr = "un")
fn = fitCopula(ncop, edata, method = "ml")
# gives rho = 0.904724
fn.new = ellipCopula("normal", 0.904724, dispstr = "un")
plot(rCopula(1000, fn.new))





### Normal copula - general examples:
rho.n = c(-0.99, -0.7, 0.05, 0.3, 0.7, 0.90)
par(mfrow = c(2, 3))
for(i in 1:6){
	n.copula = ellipCopula("normal", rho.n[i], dispstr = "un")
	plot(rCopula(1000, n.copula), main = paste("rho = ", rho.n[i]),
		ylab = "Y", xlab = "X")
}

### t copula - general examples:
rho.t = c(-0.99, -0.7, 0.05, 0.3, 0.7, 0.90)
par(mfrow = c(2, 3))
for(i in 1:6){
	t.copula = ellipCopula("t", rho.t[i], dispstr = "un", df = 5)
	plot(rCopula(1000, t.copula), main = paste("rho = ", rho.t[i]),
		ylab = "Y", xlab = "X")
}

### Clayton copula - general examples
rho.c = c(-0.99, -0.6, 0.1, 0.9, 10, 25)
par(mfrow = c(2, 3))
for(i in 1:6){
	clay.copula = archmCopula("clayton", param = rho.c[i])
	plot(rCopula(1000, clay.copula), main = paste("param = ", rho.c[i]),
		ylab = "Y", xlab = "X")
}


### Frank copula - general examples
rho.f = c(-40, -10, -1, 1, 10, 40)
par(mfrow = c(2, 3))
for(i in 1:6){
	frank.copula = archmCopula("frank", param = rho.f[i])
	plot(rCopula(1000, frank.copula), main = paste("param = ", rho.f[i]),
		ylab = "Y", xlab = "X")
}



### Gumbel copula - general examples
rho.g = c(1, 2, 3, 5, 7, 10)
par(mfrow = c(2, 3))
for(i in 1:6){
	gumbel.copula = archmCopula("gumbel", param = rho.g[i], dim = 2)
	plot(rCopula(1000, gumbel.copula), main = paste("param = ", rho.g[i]),
		ylab = "Y", xlab = "X")
}


### SPY & IWM example
f.norm = fitCopula(data = edata, method = "ml", n.copula)
f.t = fitCopula(data = edata, method = "ml", t.copula)
f.clay = fitCopula(data = edata, method = "ml", clay.copula)
f.fran = fitCopula(data = edata, method = "ml", frank.copula)
f.gumb = fitCopula(data = edata, method = "ml", gumbel.copula)

p.norm = 0.904724
p.t = c(0.907695, 4.882040)
p.clay = 3.47694
p.fran = 12.213
p.gumb = 3.42397

norm.cop = ellipCopula("normal", param = p.norm, dispstr = "un")
t.cop = ellipCopula("t", param = p.t[1], df = p.t[2], dispstr = "un")
clay.cop = archmCopula("clayton", param = p.clay)
fran.cop = archmCopula("frank", param = p.fran)
gumb.cop = archmCopula("gumbel", param = p.gumb)

norm.u = rCopula(1000, norm.cop)
t.u = rCopula(1000, t.cop)
clay.u = rCopula(1000, clay.cop)
fran.u = rCopula(1000, fran.cop)
gumb.u = rCopula(1000, gumb.cop)

par(mfrow = c(2, 3)
plot(norm.u, main = paste("Normal copula, param = ", p.norm),
	xlab = "X", ylab = "Y")
persp(norm.cop, dCopula, xlab = "X", ylab = "Y", 
	zlab = "P(X < x, Y < y)", main = "Normal copula")
persp(norm.cop, pCopula, xlab = "X", ylab = "Y",
	main = "Normal copula")



par(mfrow = c(2, 3))
set.seed(23)
plot(rCopula(1000, ellipCopula("normal", param = p.norm, dispstr = "un")),
	main = paste("Normal copula: rho = ", p.norm),
	xlab = "SPY", ylab = "IWM")
plot(rCopula(1000, ellipCopula("t", param = p.t[1], df = p.t[2], dispstr = "un")),
	main = paste("t copula: rho = ", p.t[1], " df = ", p.t[2]),
	xlab = "SPY", ylab = "IWM")
plot(rCopula(1000, archmCopula("clayton", param = p.clay)),
	main = paste("Clayton copula: param = ", p.clay),
	xlab = "SPY", ylab = "IWM")
plot(rCopula(1000, archmCopula("frank", param = p.fran)),
	main = paste("Frank copula: param = ", p.fran),
	xlab = "SPY", ylab = "IWM")
plot(rCopula(1000, archmCopula("gumbel", param = p.gumb)),
	main = paste("Gumbel copula: param = ", p.gumb),
	xlab = "SPY", ylab = "IWM")



