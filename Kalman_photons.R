library(markovchain)
library(xtable)

 array_to_LaTeX <- function(arr){
  rows <- apply(arr, MARGIN=1, paste, collapse = " & ")
   matrix_string <- paste(rows, collapse = " \\ ")
   return(paste("\\begin{bmatrix}", matrix_string, "\\end{bmatrix}"))
   }

# transition matrices
P1 <- matrix(c(0.95, 0.05, 0.05, 0.95), nrow = 2, byrow = TRUE)
P2 <- matrix(c(0.85, 0.15, 0.15, 0.85), nrow = 2, byrow = TRUE)


lambda1 <- 3
lambda2 <- 5

scalar1 <- function(x) {x / abs(sum(x))}

set.seed(123)
n=20
mc1 <- new("markovchain", states = c("active", "inactive"), transitionMatrix = P1)
mc2 <- new("markovchain", states = c("active", "inactive"), transitionMatrix = P2)
# Simulace 
z1 <- rmarkovchain(n , object = mc1, t0 = "active")
z2 <- rmarkovchain(n , object = mc2, t0 = "active")

x1 = rep(0,n);x2 = rep(0,n)
for(i in 1:n){
  if(z1[i]=="active")
    x1[i] <- rpois(1,lambda1)
  else
    x1[i] <- 0
}
for(i in 1:n){
  if(z2[i]=="active")
    x2[i] <- rpois(1,lambda2)
  else
    x2[i] <- 0
}
# Pocet fotonu
Y <- x1+x2
#df = data.frame(z1,z2,x1,x2,Y)
df1 = data.frame(z1,z2,x1,x2,Y)
plot(Y, type = "l", xlab = "t", ylab = "Počet fotonů")


z1 = factor(z1, levels=c("inactive","active")); z2 = factor(z2, levels=c("inactive","active"))
z1 = as.numeric(z1)-1; z2 = as.numeric(z2)-1

p1 = 0.95; p2 = 0.85
A= matrix(c( p1*p2 , p1*(1-p2) , (1-p1) * p2 , (1-p2) * (1-p1),
             p1*(1-p2) , p1*p2 , (1-p2) * (1-p1) , p2 * (1-p1),
             (1-p1) * p2 , (1-p1)*(1-p2) , p2 * p1 , p1 * (1-p2),
             (1-p1)*(1-p2) , (1-p1) * p2 , (1-p2)*p1,  p1*p2), nrow =4, ncol=4)
# Observation matrix
H <- matrix(c(1, 0,0,1,0, 0, 0,0), nrow = 2)
H <- diag(c(1,1,1,1))
# Noise covariance matrix
R <- diag(c(lambda_1+lambda_2, lambda_1,lambda_2,0.1))
#R <- diag(c(lambda_1, lambda_2))
# Initialize values
x_0 <- c(0.25, 0.25, 0.25, 0.25)
x_hat <- x_0
P <- diag(rep(0.5, 4))
Zt_hist <- array(dim = c(4, 4, n+1))
zt_hist <- array(dim = c(4, n+1))
# Kalman filter
for (t in 1:20) {
  # Update state
  x_hat_pred <- A %*% x_hat
  
  x_hat <- scalar1( c(dpois(Y[t],lambda1+lambda2) /x_hat_pred[1], dpois(Y[t],lambda1)/x_hat_pred[2] 
             , dpois(Y[t],lambda2)/x_hat_pred[3], dpois(Y[t],0 )/x_hat_pred[4]) )
  #save
  zt_hist[,t] <- x_hat
  Zt_hist[,,t] <- P
}

# Ohady stavů
zt_hist

x_hat_pred <- A %*% x_hat
P_pred <- A %*% P %*% t(A)
K <- P_pred %*% t(H) %*% solve(H %*% P_pred %*% t(H) + R)

pred_stav = rep(0,20)
real_stav = rep(0,20)
for(t in 1:n){
pred_stav[t] = which.max(zt_hist[,t])
if(z1[t] == "active" & z2[t] == "active"){
  real_stav[t] = 1
}else if( z1[t] == "active" & z2[t] == "inactive" ) {
  real_stav[t] = 2
}else if( z1[t] == "inactive" & z2[t] == "active" ) {
  real_stav[t] = 3
}else {
  real_stav[t] = 4
}
}
pred_stav
real_stav



states <- matrix(c(1, 1, 0, 0, 1, 0, 1, 0), ncol = 2)
target <- c(1, 0, 0, 0)


plot(c(-0.1, 1.1), c(-0.1, 1.1), type = "n", xlab = "", ylab = "")

points(states, pch = 19, col = "black")

text(states, labels = c("(1,1)", "(1,0)", "(0,1)", "(0,0)"), pos = 3)

points(target, pch = 16, col = "red", cex = 2)
zt_hist



