###Plot
library(ggplot2)
z = fortify(bact_e)
conf.reg=function(x,LCL,UCL,...) {
  x.sort <- order(x)
  x <- x[x.sort]
  LCL <- LCL[x.sort]
  UCL <- UCL[x.sort]
  polygon(c(x,rev(x)),c(LCL,rev(UCL)), ...)
}
SITE <- unique(z$Assemblage)
ORDER <- unique(z$Order.q)
for(j in 1:length(ORDER)){
  {
    tmp.sub <- z[z$Order.q==ORDER[j],]
    
    tmp.j <- data.frame(Assemblage=tmp.sub$Assemblage, Order.q=tmp.sub$Order.q,
                        Method=tmp.sub$Method, 
                        x=tmp.sub$x, y=tmp.sub$y)
    
    plot(y~x, data=tmp.j, type="n", xlab="", ylab="")
  }}
for(i in 1:length(SITE)){
  # tmp <- subset(tmp.j, Assemblage==SITE[i])
  tmp <- tmp.j[tmp.j$Assemblage==SITE[i],]
  conf.reg(x=tmp$x, LCL=tmp$y.lwr, UCL=tmp$y.upr, border=NA, 0.25)
  lines(y~x, data=tmp[tmp$Method=="Rarefaction",], lty=1, lwd=2, col ="blue")
  lines(y~x, data=tmp[tmp$Method=="Extrapolation",], lty=2, lwd=2, col = "red")
}


