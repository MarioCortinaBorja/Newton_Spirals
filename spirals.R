
library(tidyverse)
library(wesanderson)

darj3<- wes_palette("Darjeeling1",3)[1:3]

newton.spirals<- function(a=1/2, b=0.3, k0=40, 
                          col.line=darj3[2], col.area=darj3[3])
{
#References:
#https://www.johndcook.com/blog/2021/01/23/newtons-method-spirals/
#https://www.tandfonline.com/doi/pdf/10.4169/college.math.j.43.5.365?needAccess=true
### converges to 0 if a>1/2, diverges if a<1/2, circles if a=1/2

k<- 1:k0 ###sequence
p<- complex(real=a, imaginary=b)
z<- (1 - 1/p)^k
df<- data.frame(re=Re(z),  im=Im(z))
eqn<- sprintf("Newton~spiral: ~ italic(z)^{%.2g  + %.2g ~ italic(i)}", 
                a, b, k0)
g1<-
df %>% ggplot(aes(x=re, y=im)) +
       geom_path() +
       geom_polygon(col=col.line, fill=col.area) +
       theme_minimal() +
       annotate(geom='text', size=5, col='black',
                x=-Inf, y=Inf, hjust = 0, vjust = 1,
                label=eqn, 
                parse=TRUE) +
       theme(axis.title.x=element_blank(),
             axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
       NULL

print(g1)
invisible(g1)
}


### examples

newton.spirals(a=0.500112, b=0.33, k=400)
newton.spirals(a=0.43, b=0.33, k=400)
newton.spirals(a=1/2, b=0.4, k=200)

newton.spirals(a=0.525, b=0.475, k=200)
