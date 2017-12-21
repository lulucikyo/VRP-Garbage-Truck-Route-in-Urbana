data <- read.csv(file="UrbanaData.csv",header = TRUE)
depot <- c(40.117844, -88.199203)

data$LAT <- (data$LAT-depot[1])*111.7
data$LON <- (data$LON-depot[2])*111.7*cos(40.1/360*2*pi)

plot(data$LON, data$LAT)
points(0, 0, col = "red")
n <- length(data$LON)
x <- cbind(data$LON, data$LAT)
d <- matrix(0, nrow = n, ncol = n)
for (i in 1:n)
  for (j in 1:n)
  {
    d[i, j] = abs(x[i,1]-x[j,1])+abs(x[i,2]-x[j,2])
  }
    
r <- abs(data$LON)+abs(data$LAT)
alpha <- atan(data$LAT/data$LON)
alpha <- ifelse(data$LAT<0 & data$LON<0, alpha-pi, alpha)
alpha <- ifelse(data$LAT>0 & data$LON<0, alpha+pi, alpha)
dmdori = data$Pop*4.4*7
dmd = qnorm(0.95, mean = data$Pop*4.4*7, sd = data$Pop*4.4*7*0.1)

p <- as.data.frame(cbind(data$BID, x, r, alpha, dmd))
colnames(p) <- c("id","lon","lat","r","alpha","demand")
plot(p$r, p$demand)
plot(data$LON, data$LAT, xlab = "X", ylab = "Y")
points(0, 0, col = "red", pch = 17)
high <- which(p$demand>=10000)
mid <- which(p$demand>=5000 & p$demand<10000)
points(p$lon[high], p$lat[high], col = "green")
points(p$lon[mid], p$lat[mid], col = "blue")

########################################

searchnode <- function(x, conn, node, N)
{
  for (j in 1:N)
    if (conn[x, j]==1)
    {
      if (node[j]==FALSE) return(j)
    }
  return(0)
}

renode <- function(x, dist, node)
{
  l <- order(dist[x, ], decreasing = FALSE)
  for (j in l)
    if (node[j]==FALSE) return(j)
}

getsolu <- function(ppp, mark)
{
  print(mark)
  x <- c(0, ppp$lon)
  y <- c(0, ppp$lat)
  N <- length(x)
  dot <- cbind(x, y)
  #creating dist matrix for dist(i,j)
  dist <- matrix(0, nrow = N, ncol = N)
  for (i in 1:N)
    for (j in 1:N)
    {
      dist[i, j] = abs(x[i]-x[j])+abs(y[i]-y[j])
    }
  
  # use prim algorithm to find MST, set point 1 as the start point 
  conn <- matrix(0, nrow = N, ncol = N) # if city i & j are connected 
  cost <- dist[1,] #distance from the tree to other nodes
  node <- c(TRUE, rep(FALSE, N-1)) #whether node is in the set
  father <- c(NA, rep(1, N-1)) 
  
  for (i in 1:(N-1))
  {
    newdist <- Inf
    for (j in 1:N)
    {
      #find a new node to join
      if (node[j]==FALSE & cost[j]<newdist)
      {
        newnode <- j
        newdist <- cost[j]
      }
    }
    node[newnode] <- TRUE
    conn[father[newnode], newnode] <- 1
    conn[newnode, father[newnode]] <- 1
    #update the cost and father using node j
    for (k in 1:N)
    {
      if (node[k]==FALSE & dist[newnode, k]<cost[k])
      {
        cost[k] = dist[newnode, k]
        father[k] = newnode
      }
    }
  }
  
  lstt <- c()
  node <- c(TRUE, rep(FALSE, N-1)) #whether node is in the set
  front <- 1
  totd <- 0
  for (i in 1:(N-1))
  {
    tail<- searchnode(front, conn, node, N)
    if (tail==0) {tail <- renode(front, dist, node)}
    lstt <- c(lstt, mark[tail-1])
    node[tail] <- TRUE
    if (i!=1)
    {segments(x[front], y[front], x[tail], y[tail], col = 2, lwd = "1")}
    totd <- totd+dist[front, tail]
    front <- tail
  }
  totd <- totd+dist[1, tail]
  print(lstt)
  #segments(x[1], y[1], x[tail], y[tail], col = "red", lwd = "1")
  return(c(totd, lstt))
}

########################3333
plot(data$LON, data$LAT, xlab = "X", ylab = "Y")
points(0, 0, col = "red", pch = 17)

ctg <- rep(0, n)
flag <- 1
tmp <- 0
pp <- p[order(p$alpha, decreasing = TRUE),]
#create data in the form suitable for Simulated Anealing
write.csv(pp, file="SA.csv")
lstt <- c()
for (i in 1:n)
{
  #i <- i+1
  if (tmp+pp$demand[i]>60000)
  {
    if (flag==1)
    {
      sol <- getsolu(pp[which(ctg==flag),], which(ctg==flag))
      solu <- sol[1]
      lstt <- sol[2:length(sol)]
    } else {
      sol <- c(getsolu(pp[which(ctg==flag),], which(ctg==flag)))
      solu <- c(solu, sol[1])
      lstt <- c(lstt, sol[2:length(sol)])
    }
    lstt <- c(lstt, flag+n)
    lines(c(0, pp$lon[i-1]*10), c(0, pp$lat[i-1]*10))
    flag <- flag+1
    tmp <- pp$demand[i]
    lines(c(0, pp$lon[i-1]*10), c(0, pp$lat[i-1]*10))
    
  } else {
    tmp <- tmp+pp$demand[i]
  }
  ctg[i] <- flag
}
lines(c(0, pp$lon[n]*10), c(0, pp$lat[n]*10))
sol <- c(getsolu(pp[which(ctg==flag),], which(ctg==flag)))
solu <- c(solu, sol[1])
lstt <- c(lstt, sol[2:length(sol)])
solu <- solu*0.621371

final <- data.frame(
  routelength = solu,
  demandpoint = sapply(1:flag, function(i) length(which(ctg==i))),
  totaldemand = sapply(1:flag, function(i) sum(pp$demand[which(ctg==i)]))
)
sum(final$routelength)


#####Cost Computation ##############33

cost <- (sum(solu)/25+sum(dmd)/5000/6)*15+
1.86*sum(solu)+0.1*sum(solu)+sum(solu*final$totaldemand*0.1/5000/2)+
0.2*(n+20)

cc <- rep(1,n)
cc <- ifelse(dmd>5000 & dmd<=10000, 5, cc)
cc <- ifelse(dmd>10000, 25, cc)
print(sum(cc*dmd)/5000)
print(sum(cc*p$demand)/5000+cost)

#provide initial solution for Simulated Anealing
write.table(t(as.matrix(lstt)), "ini2.txt")