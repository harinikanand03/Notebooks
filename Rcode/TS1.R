library(fBasics)

# Load the data
# header=T means 1st row of the data file contains
# variable names. The default is header=F, i.e., no names.
da=read.table("/Users/harini-mac/Desktop/Northwestern University/MSDS-413/Week1/d-ibm3dx7008.txt",header=T)

# Find size of the data: 9845 rows and 5 columns.
dim(da)

# See the first row of the data
da[1,]
# Obtain IBM simple returns 
ibm=da[,2]

# Percentage simple returns
sibm=ibm*100

# Compute the summary statistics
basicStats(sibm)

# Alternatively, one can use individual commands as follows:
mean(sibm)
var(sibm)
sqrt(var(sibm))
skewness(sibm)
attr(,"method")
kurtosis(sibm)
attr(,"method")

# Simple tests
s1<-skewness(sibm)
t1<-s1/sqrt(6/9845)
pval <- 2*(1-pnorm(t1))


# Turn to log returns in percentages
libm <- log(ibm+1)*100
# Test mean being zero.
t.test(libm)

# Normality test
normalTest(libm,method="jb")

###################################################################
###################################################################
###################################################################
# Load the data
# header=T means 1st row of the data file contains
# variable names. The default is header=F, i.e., no names.
da=read.table("/Users/harini-mac/Desktop/Northwestern University/MSDS-413/Week1//m-gm3dx7508.txt",header=T)

# Find size of the data: 
dim(da)

# Obtain the gm stock
gm <- da[,2]
gm1 <- ts(gm, frequency=12, start=c(1975,1))

# Creates a ts object
par(mfcol=c(2,1))
plot(gm,type="l")
plot(gm1,type="l")

acf(gm,lag=24)
acf(gm1,lag=24)
