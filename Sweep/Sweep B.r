data <- read.csv(file="UrbanaData.csv",header = TRUE)
depot <- c(40.117844, -88.199203)

data$LAT <- (data$LAT-depot[1])*111.7
data$LON <- (data$LON-depot[2])*111.7*cos(40.1/360*2*pi)

plot(data$LON, data$LAT)
points(0, 0, col = "red")
n1 <- length(data$LON)
n2 <- length(data$LON)
    
r <- abs(data$LON)+abs(data$LAT)
alpha <- atan(data$LAT/data$LON)
alpha <- ifelse(data$LAT<0 & data$LON<0, alpha-pi, alpha)
alpha <- ifelse(data$LAT>0 & data$LON<0, alpha+pi, alpha)
dmdori = data$Pop*4.4*7
dmd = qnorm(0.95, mean = data$Pop*4.4*7, sd = data$Pop*4.4*7*0.1)

p <- as.data.frame(cbind(data$BID, x, r, alpha, dmd))
colnames(p) <- c("id","lon","lat","r","alpha","demand")
p$id <- rep(1:n1)
p$newid <- rep(1:n1)
p$spec <- rep(0,n1)
plot(p$r, p$demand, xlab = "Distance", ylab = "Demand")

plot(data$LON, data$LAT, xlab = "X", ylab = "Y")
points(0, 0, col = "red", pch = 17)
high <- which(p$demand>=10000)
mid <- which(p$demand>=5000 & p$demand<10000)
points(p$lon[high], p$lat[high], col = "green", pch = 16)
points(p$lon[mid], p$lat[mid], col = "blue", pch = 16)
req <- matrix(0, nrow = n2, ncol = 5)

#############split points###################
for (i in high)
{
  req[i, 5] <- 1
  p$demand[i] <- p$demand[i]/5
  p$spec[i] <- 1
  newp <- p[i, ]
  newp$id[1] <- i
  for (j in 1:4)
  {
    n2 <- n2+1
    req <- rbind(req, rep(0, 5))
    req[n2, j] <- 1
    newp$newid[1] <- n2
    p <- rbind(p, newp)
  }
}

for (i in mid)
{
  req[i,] <- c(0, 1, 0, 0, 0)
  p$demand[i] <- p$demand[i]/2
  p$spec[i]<- 1
  newp <- p[i, ]
  newp$id[1] <- i
  
  n2 <- n2+1
  req <- rbind(req, rep(0, 5))
  req[n2, 4] <- 1
  newp$newid[1] <- n2
  p <- rbind(p, newp)
}

#data input for SA
write.table(p, "V2.csv")

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
  #print(mark)
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
  #segments(x[1], y[1], x[tail], y[tail], col = "red", lwd = "1")
  print(totd)
  return(c(totd, lstt))
}
#################################
rangemust <- function(case, p)
{
  if (case==1)
  {
    tmp <- p$newid[which(p$alpha>=-pi & p$alpha<=-3*pi/4)]
    return(tmp)
  }
  if (case==2)
  {
    tmp <- p$newid[which(p$alpha>=-3*pi/4 & p$alpha<=-pi/4)]
    return(tmp)
  }
  if (case==3)
  {
    tmp <- p$newid[which(p$alpha>=-pi/4 & p$alpha<=pi/4)]
    return(tmp)
  }
  if (case==4)
  {
    tmp <- p$newid[which(p$alpha>=pi/4 & p$alpha<=pi)]
    return(tmp)
  }
}

rarange <- function(case, p)
{
  pp <- p
  if (case==1)
  {
    pp <- pp[order(pp$alpha, decreasing = FALSE),]
    return(pp)
  }
  if (case==2)
  {
    tmp <- which(pp$alpha>=-pi & pp$alpha<=-pi/2)
    pp$alpha[tmp] <- pp$alpha[tmp]+2*pi
    pp <- pp[order(pp$alpha, decreasing = FALSE),]
    return(pp)
  }
  if (case==3)
  {
    tmp <- which(pp$alpha>=-pi & pp$alpha<=0)
    pp$alpha[tmp] <- pp$alpha[tmp]+2*pi
    pp <- pp[order(pp$alpha, decreasing = FALSE),]
    return(pp)
  }
  if (case==4)
  {
    tmp <- which(pp$alpha>=-pi & pp$alpha<=pi/2)
    pp$alpha[tmp] <- pp$alpha[tmp]+2*pi
    pp <- pp[order(pp$alpha, decreasing = FALSE),]
    return(pp)
  }
}

################# sweep ################
plot(p$lon, p$lat, xlab = "X", ylab = "Y")
points(0, 0, col = "red", pch = 16)

ctg <- rep(0, n2)
solu <- c()
lstt <- c()

rr<-0
newid <-c()
for (i in 1:5) #sweep for days
{
  for (j in 1:4) #sweep for tours in a day
  {
    flag <- (i-1)*4+j
    print("flag")
    print(flag)
    
    rg <- rangemust(j, p)
    must <- which((req[,i]==1) & (p$newid %in% rg)) 
    #points required to visit this tour, specified by newid
    ctg[must] <- flag
    newid <- c(newid, must)
    points(p$lon[must], p$lat[must], col = flag, pch = 16)
    pp <- rarange(j, p)
    tmp <- sum(p$demand[must])
    if (tmp>60000) {print("xxxxxx")}
  
    for (k in 1:n2)
    {
      if (ctg[pp$newid[k]]==0 & pp$spec[k]==0)
      {
        if (tmp+pp$demand[k]<=60000)
        {
          ctg[pp$newid[k]] <- flag
          tmp <- tmp+pp$demand[k]
          points(p$lon[pp$newid[k]], p$lat[pp$newid[k]], col = flag, pch = 16)
          newid <- c(newid, pp$newid[k])
        } else{
          break
        }
      }
    }
    rr <- rr+tmp
    sol <- getsolu(p[which(ctg==flag),], which(ctg==flag))
    solu <- c(solu, sol[1])
    lstt <- c(lstt, sol[2:length(sol)], flag+n2)
  }
}

solu <- solu*0.621371

final <- data.frame(
  routelength = solu,
  demandpoint = sapply(1:flag, function(i) length(which(ctg==i))),
  totaldemand = sapply(1:flag, function(i) sum(p$demand[which(ctg==i)]))
)
sum(final$routelength)


(sum(solu)/25+sum(p$demand)/5000/6)*15
1.86*sum(solu)+0.1*sum(solu)+sum(solu*final$totaldemand*0.1/5000/2)
0.2*(n2+20)

cost <- (sum(solu)/25+sum(dmd)/5000/6)*15+
  1.86*sum(solu)+0.1*sum(solu)+sum(solu*final$totaldemand*0.1/5000/2)+
  0.2*(n2+20)
#compute penalty
cc <- rep(1,n2)
cc <- ifelse(p$demand>5000 & p$demand<=10000, 5, cc)
cc <- ifelse(p$demand>10000, 25, cc)
print(sum(cc*p$demand)/5000)
print(sum(cc*p$demand)/5000+cost)

#initial solution for SA
write.table(t(as.matrix(lstt)), "sol2.txt")
