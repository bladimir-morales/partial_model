
set.seed(01012022)
m=function(x1,x2) sin(2*pi*x1)+x2
x1=seq(0,1,0.01)
x2=seq(0,1,0.01)
z=outer(x1,x2,m)

error=rnorm(101,0,1/4)
y=m(x1,x2)+error

# Con lambda igual a 0.3
mod1=loess(y~x1+x2,span=0.3,degree = 0)
z1=expand.grid(x1,x2)

mod2=loess(y~x1+x2,span=0.3,degree = 1)
z2=expand.grid(x1,x2)

par(mfrow=c(1,3))
persp(x1,x2,z,phi=30,col = "green",main="Función",ticktype = "detailed")
persp(x1,x2,predict(mod1,z1),phi=30,col = "green",main="Nadaraya Watson-lambda=0.3",ticktype = "detailed")
persp(x1,x2,predict(mod2,z2),phi=30,col = "green",main="Lineal local-lambda=0.3",ticktype = "detailed")

# Sin lambda dado
mod3=loess(y~x1+x2,degree = 0)
z3=expand.grid(x1,x2)

mod4=loess(y~x1+x2,degree = 1)
z4=expand.grid(x1,x2)

par(mfrow=c(1,3))
persp(x1,x2,z,phi=30,col = "green",main="Función",ticktype = "detailed")
persp(x1,x2,predict(mod3,z3),phi=30,col = "green",main="Nadaraya Watson-lambda=automatico",ticktype = "detailed")
persp(x1,x2,predict(mod4,z4),phi=30,col = "green",main="Lineal local-lambda=automatico",ticktype = "detailed")



