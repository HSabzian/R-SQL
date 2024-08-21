########Perspective of 3D 

rm(list =ls())

#################################################perspective
x <- seq(-10, 10, length= 30)
y <- x
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
m <- outer(x, y, f)
op <- par(bg = "white")
persp(x, y, m, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Payoff"
)




x <- seq(10, 40, length= 30)
y <- x
f <- function(x, y) { r <- exp (x ) + exp(y) }
m <- outer(x, y, f)
op <- par(bg = "white")
persp(x, y, m, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Payoff"
)




x = seq(0.1,1,0.1)
y = exp(x)
f <- function(x,y){ r <- x * y}
z <- outer(x,y,f)
op <- par (bg ="white") ### Color of Environment
persp( x, y, z, theta = 120, phi = 30, expand = 0.5, col = "lightblue",
      ltheta = 120, shade = 0.10, ticktype = "detailed",
      xlab = "Data", ylab = "P(Data)", zlab = "Operation" )



#############################Poission in Perspective
x = seq(1,100,1)
Lambda = seq(1,50,1)
f <- function(x,Lambda){ output <- dpois(x,Lambda)}
z = outer(x, Lambda, f)
par(mfrow = c(2,2) , bg ="white")
persp( x, Lambda, z, theta = 120, phi = 30, expand = 0.5, col = "lightblue",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="1", sub = "One" )

persp( x, Lambda, z, theta = 100, phi = 30, expand = 0.5, col = "lightblue",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="2", sub = "Two" )

persp( x, Lambda, z, theta = 80, phi = 30, expand = 0.5, col = "lightblue",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="3", sub = "Three")

persp( x, Lambda, z, theta = 60, phi = 30, expand = 0.5, col = "lightblue",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="4", sub = "Four" )



#############################Power in perspective

x = seq(1,100,1)
gamma = seq(1,50,1)
f <- function(x, gamma) { output <- x ^ (- gamma) }
z <- outer(x,gamma,f)
par(mfrow = c(2,2) , bg ="white")
persp( x, gamma, z, theta = 120, phi = 30, expand = 0.5, col = "red",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="1", sub = "One" )

persp( x, gamma, z, theta = 100, phi = 30, expand = 0.5, col = "red",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="2", sub = "Two" )

persp( x, gamma, z, theta = 80, phi = 30, expand = 0.5, col = "red",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="3", sub = "Three")

persp( x, gamma, z, theta = 60, phi = 30, expand = 0.5, col = "red",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="4", sub = "Four" )


############################ Total of poission and Power

x = seq(1,100,1)
Lambda = seq(1,50,1)
f <- function(x,Lambda){ output <- dpois(x,Lambda)}
z = outer(x, Lambda, f)

X = seq(1,100,1)
gamma = seq(1,50,1)
ff <- function(X, gamma) { output <- X ^ (- gamma) }
Z <- outer(X,gamma,ff)

par(mfrow = c(2,4) , bg ="white")
persp( x, Lambda, z, theta = 120, phi = 30, expand = 0.5, col = "lightblue",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="1", sub = "One" )

persp( x, Lambda, z, theta = 100, phi = 30, expand = 0.5, col = "lightblue",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="2", sub = "Two" )

persp( x, Lambda, z, theta = 80, phi = 30, expand = 0.5, col = "lightblue",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="3", sub = "Three")

persp( x, Lambda, z, theta = 60, phi = 30, expand = 0.5, col = "lightblue",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="4", sub = "Four" )


persp( X, gamma, Z, theta = 120, phi = 30, expand = 0.5, col = "red",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="1", sub = "One" )

persp( X, gamma, Z, theta = 100, phi = 30, expand = 0.5, col = "red",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="2", sub = "Two" )

persp( X, gamma, Z, theta = 80, phi = 30, expand = 0.5, col = "red",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="3", sub = "Three")

persp( X, gamma, Z, theta = 60, phi = 30, expand = 0.5, col = "red",
       ltheta = 120, shade = 0.10, ticktype = "detailed",
       xlab = "Data", ylab = "P(Data)", zlab = "Operation" , main ="4", sub = "Four" )


######Histogram of normal,. poisson, unif

x<- seq(1,100,0.2)
par(mfrow = c(1,3))
hist(rnorm(10000,6,2),main = "Normality", col = "red", breaks= 10)
hist(rpois(10000,6),main = "Poission", col = "red", breaks= 10)
hist(runif(10000,1,2),main = "Unif", col = "red", breaks= 10 )


##### ###################### POWER OF LATTICE
str(VADeaths)
help(VADeaths)
class(VADeaths)  #### Matrix
dotchart(VADeaths, main = "Death Rates in Virginia - 1940")
barchart(VADeaths, main = "Death Rates in Virginia - 1940")

 #######################Some data
#library(lattice)
singer       #LATTICE package
ToothGrowth #LATTICE
iris       ###Datasets package
Animals   # MASS package
Pima.te   # MASS package

## return the mean of glu in women with 1 preg
mean( Pima.te$glu[Pima.te$npreg==1] )   

## return the mean of glu in women with 4 preg
mean( Pima.te$glu[Pima.te$npreg==4] )

mean( Pima.te$bmi[Pima.te$type=="Yes"] )
mean( Pima.te$bmi[Pima.te$type=="No"] )


head(ToothGrowth)
head(iris)

xyplot(Sepal.Length ~ Petal.Length, data = iris)

xyplot(Sepal.Length ~ Petal.Length, group = Species, 
       data = iris, auto.key = TRUE)

xyplot(Sepal.Length ~ Petal.Length, data = iris,
       type = c("p", "g", "smooth"),
       xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)" )

xyplot(Sepal.Length ~ Petal.Length | Species, 
       group = Species, data = iris,
       type = c("p", "smooth"),
       scales = "free")
###############Cloud
cloud(Sepal.Length ~ Sepal.Length * Petal.Width, 
      data = iris)

cloud(Sepal.Length ~ Sepal.Length * Petal.Width, 
      group = Species, data = iris,
      auto.key = TRUE)
################Boxplot, Violin box plot, dotplot, stripplot
# Basic box plot
bwplot(len ~ dose,  data = ToothGrowth,
       xlab = "Dose", ylab = "Length")

bwplot(len ~ supp | dose,  data = ToothGrowth,
       layout = c(3, 1),
       xlab = "Dose", ylab = "Length")


# Violin plot using panel = panel.violin
bwplot(len ~ dose,  data = ToothGrowth,
       panel = panel.violin,
       xlab = "Dose", ylab = "Length")


bwplot(len ~ supp | dose,  data = ToothGrowth,
       layout = c(3, 1), panel = panel.violin,
       xlab = "Dose", ylab = "Length")


# Basic dot plot
dotplot(len ~ dose,  data = ToothGrowth,
        xlab = "Dose", ylab = "Length")

dotplot(len ~ supp | dose,  data = ToothGrowth,
        layout = c(3, 1),
        xlab = "Dose", ylab = "Length")

# Basic stripplot
stripplot(len ~ dose,  data = ToothGrowth,
          jitter.data = TRUE, pch = 19,
          xlab = "Dose", ylab = "Length")

stripplot(len ~ supp | dose,  data = ToothGrowth,
          layout = c(3, 1), jitter.data = TRUE,
          xlab = "Dose", ylab = "Length")

###densityplot



densityplot(~ len, data = ToothGrowth,
            plot.points = FALSE)

densityplot(~ len, groups = dose, data = ToothGrowth,
            plot.points = FALSE, auto.key = TRUE)
###histogram

histogram(~ len, data = ToothGrowth,
          breaks = 20)
histogram(~ height| voice.part, data = singer)



##########################
# par is just for graphics package

par(mfrow=c(1,3), bg = "white")
plot(x, avepan2g, type = "l", col ="red")
plot(x, avepan2gg, type = "l", col = "blue")
plot(x, avepan2ggg, type = "l", col ="green")


######################### Multiple graphes in one plot
x  <- seq(-2, 2, 0.05)
y1 <- pnorm(x)
y2 <- pnorm(x, 1, 1)
plot(x,y1,type="l",col="red")
lines(x,y2,col="green")

########################
# Generate some data
x<-1:10; y1=x*x; y2=2*y1
plot(x, y1, type="b", pch=19, col="red", xlab="x", ylab="y")
# Add a line
lines(x, y2, pch=18, col="blue", type="b", lty=2)
# Add a legend
legend(1, 95, legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty=1:2, cex=0.8)



x <- 1:50 ; y1 = log(x) + 1;y2 = log(x)

plot(x,y1, type = "l", col ="red")
lines(x, y2, type ="l", col ="blue")
legend(2,5, legend=c("GPD of Iran" , "GDP of USA"), col = c("red","blue"), lty = 1, cex = 0.9 )
##################################################################################################


library(ggplot2); library(maps)

county_df <- map_data('county')  #mappings of counties by state
ny <- subset(county_df, region=="new york")   #subset just for NYS
ny$county <- ny$subregion
cnames <- aggregate(cbind(long, lat) ~ subregion, data=ny, FUN=mean)

p <- ggplot(ny, aes(long, lat, group=group)) +  geom_polygon(colour='black', fill=NA) 
p #p of course plots as expected

ggplot(ny, aes(long, lat)) +  
  geom_polygon(aes(group=group), colour='black', fill=NA) +
  geom_text(data=cnames, aes(long, lat, label = subregion), size=2)

cnames <- aggregate(cbind(long, lat) ~ subregion, data=ny, 
                    FUN=function(x)mean(range(x)))

ggplot(ny, aes(long, lat)) +  
  geom_polygon(aes(group=group), colour='black', fill=NA) +
  geom_text(data=cnames, aes(long, lat, label = subregion), size=3) +
  coord_map()


#########################################################

county_df <- map_data('county')  #mappings of counties by state
cali <- subset(county_df, region== "california")   #subset just californai
cali$county <- cali$subregion
cnames <- aggregate(cbind(long, lat) ~ subregion, data=cali, FUN=mean)

p <- ggplot(cali, aes(long, lat, group=group)) +  geom_polygon(colour='black', fill=NA) 
p #p of course plots as expected

ggplot(cali, aes(long, lat)) +  
  geom_polygon(aes(group=group), colour='black', fill=NA) +
  geom_text(data=cnames, aes(long, lat, label = subregion), size=2)

cnames <- aggregate(cbind(long, lat) ~ subregion, data=cali, 
                    FUN=function(x)mean(range(x)))

ggplot(cali, aes(long, lat)) +  
  geom_polygon(aes(group=group), colour='black', fill=NA) +
  geom_text(data=cnames, aes(long, lat, label = subregion), size=3) +
  coord_map()


#####maps
map('world2', xlim = c(100, 300))
map.axes()
# xlim is performed before wrapping:
map('world', wrap=c(0,360), xlim = c(100, 300))
# so to emulate "world2":
ww2 <- map('world', wrap=c(0,360), plot=FALSE, fill=TRUE)
map(ww2, xlim = c(100, 300), fill=TRUE)


#################################MULTI LINES IN ONE PLOT USING ggplot
# define 3 data sets
xdata <- c(1,2,3,4,5,6,7)
y1 <- c(1,4,9,16,25,36,49)
y2 <- c(1, 5, 12, 21, 34, 51, 72)
y3 <- c(1, 6, 14, 28, 47, 73, 106 )

##plot the first curve by calling plot() function
# First curve is plotted
plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,110) )

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(xdata, y2, col="red", pch="*")
lines(xdata, y2, col="red",lty=2)

# Add Third curve to the same plot by calling points() and lines()
# Use symbol '+' for points.
points(xdata, y3, col="dark red",pch="+")
lines(xdata, y3, col="dark red", lty=3)
legend(1,100, legend=c("GPD of Iran" , "GDP of USA", "Calibration"), col = c("blue","red","dark red"), lty = 1, cex = 0.5 )



############################################### plot the first curve by calling plot() function


nmonths = 24
x = seq(as.Date("2015/1/1"), by = "month", length.out = nmonths)

prescription1 <- data.frame(x,Percent.Change = 25 + runif(nmonths,1,100))

prescription2 <- data.frame(x,Percent.Change = 75 + runif(nmonths,1,50))

head(prescription1)
head(prescription2)

cols = c("dates", "Difference")
colnames(prescription1) = cols
colnames(prescription2) = cols

head(prescription1)
head(prescription2)


p = ggplot() + 
  geom_line(data = prescription1, aes(x = dates, y = Difference), color = "blue") +
  geom_line(data = prescription2, aes(x = dates, y = Difference), color = "red") +
  xlab('Dates') +
  ylab('percent.change')

print(p)


prescription = merge(prescription1, prescription2, by="dates")
head(prescription)
prescriptionMelted <- reshape2::melt(prescription, id.var='dates')
head(prescriptionMelted)
ggplot(prescriptionMelted, aes(x=dates, y=value, col=variable)) + geom_line()








write.csv(prescription, file = "prescription.csv" )




##################
n <- 20
dfr <- data.frame( id=rep(1:n, 2), group=rep(c("1","2"), each=n),
                   value=c (rnorm(n), rnorm(n, sd=1.1)         )        )
head(dfr)

ggplot(data=dfr, mapping=aes(x=id, y=value)) +
  geom_line(mapping=aes(colour=group))

ggplot(data = resahpedfour, mapping = aes(x=runsv, y=value)) +
  geom_line(mapping=aes(colour=variable))

