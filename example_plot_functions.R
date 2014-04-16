### Copulas in Finance - Applied Math Seminar
### Daniel Rieman - djr2144@columbia.edu
### April 16, 2014

### the following copula plotting function was written by Markus Gesmann
my3dplot <- function(z, zlab="copula",	# a matrix
                  ncol=20,          # the number of colors to use
                  zlim=c(0,max(z)), # limits in z coordinates
                  nlevels=20,       # see option nlevels in contour
		      theta=30,         # see option theta in persp
		      phi=30,           # see option phi in persp
		      main="Hello")
		      {
	nrz <- nrow(z)
	ncz <- ncol(z)
	couleurs  <- tail(topo.colors(trunc(1.4 * ncol)),ncol)
	fcol      <- couleurs[trunc(z/zlim[2]*(ncol-1))+1]
	dim(fcol) <- c(nrz,ncz)
	fcol      <- fcol[-nrz,-ncz]
	persp(z,col=fcol,zlim=zlim,theta=theta,phi=phi,zlab=zlab, main=main)
	image(z,col=couleurs)
	contour(z,add=T,nlevels=nlevels)
	box()
	invisible(fcol)
}



Generate.Plots = function(copula, n = 50, name = "", type = "both"){
	ind = expand.grid(xi = 1:n, xj = 1:n)
	if(type == "both"){
		zd = dCopula(as.matrix(ind/n), copula)
		zp = pCopula(as.matrix(ind/n), copula)
		Md = data.frame(ind, z = zd)
		Mp = data.frame(ind, z = zp)
		zd = as.matrix(reshape(Md, timevar = "xi", idvar = "xj",
			direction = "wide")[,-1])
		zp = as.matrix(reshape(Mp, timevar = "xi", idvar = "xj",
			direction = "wide")[,-1])
		par(mfrow = c(2,2))
		my3dplot(zd, main = paste(name, " pdf"))
		my3dplot(zp, main = paste(name, " cdf"), theta = -30)
	} else if(type == "pdf"){
		zd = dCopula(as.matrix(ind/n), copula)
		Md = data.frame(ind, z = zd)
		zd = as.matrix(reshape(Md, timevar = "xi", idvar = "xj",
			direction = "wide")[,-1])
		par(mfrow = c(1,2))
		my3dplot(zd, main = paste(name, " pdf"))
	} else{
		zp = pCopula(as.matrix(ind/n), copula)
		Mp = data.frame(ind, z = zp)
		zp = as.matrix(reshape(Mp, timevar = "xi", idvar = "xj",	
			direction = "wide")[,-1])
		par(mfrow = c(1,2))
		my3dplot(zp, main = paste(name, " cdf"), theta = -30)
	}
}

### starting with copulas already fitted to SPY and IWM

### Gaussian copulas of IWM and SPY example
Generate.Plots(norm.cop, name = "Normal copula", type = "both")
Generate.Plots(t.cop, name = "t copula", type = "pdf")		# pCopula won't accept non-integer df
Generate.Plots(tCopula(p.t[1], df = 5), name = "t copula", type = "cdf")


### Archimedean copulas of IWM and SPY example
Generate.Plots(clay.cop, name = "Clayton copula", type = "both")
Generate.Plots(fran.cop, name = "Frank copula", type = "both")
Generate.Plots(gumb.cop, name = "Gumbel copula", type = "both")

