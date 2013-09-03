library(shiny)

if (!"MASS" %in% installed.packages()) install.packages("MASS")
library(MASS) 

if (!"mixtools" %in% installed.packages()) install.packages("mixtools")
library(mixtools)

shinyServer(function(input,output){

  output$shot_plot <- renderPlot({

# Bivariate normal distribution parameters
mu1  <- 0
mu2  <- 30
mu_vec <- c(mu1,mu2)
sig1 <- as.numeric(input$stdev)^2/4
sig2 <- as.numeric(input$stdev)^2
rho  <- 0
sigma_mat <- matrix(c(sig1,rho,rho,sig2),2)

# Generate random shots
shots <- mvrnorm(n=as.numeric(input$nr_shots),mu=mu_vec,Sigma=sigma_mat)

# Plot the shots
plot(shots,xlim=c(-20,20),ylim=c(0,45),pch=20,xlab="x (kilometers)",ylab="y (kilometers)",main = bquote("An Application of the Bivariate Normal Distribution with" ~ sigma[x]==sigma[y]/2 ~ "and" ~ rho==0),cex.main=1.5,col="azure4")


# Add grid lines
lines(c(-20,-20),c(-10,60),lty=3)
lines(c(-15,-15),c(-10,60),lty=3)
lines(c(-10,-10),c(-10,60),lty=3)
lines(c(-5,-5),c(-10,60),lty=3)
lines(c(0,0),c(-10,60),lty=3)
lines(c(5,5),c(-10,60),lty=3)
lines(c(10,10),c(-10,60),lty=3)
lines(c(15,15),c(-10,60),lty=3)
lines(c(20,20),c(-10,60),lty=3)
lines(c(-50,50),c(50,50),lty=3)
lines(c(-50,50),c(45,45),lty=3)
lines(c(-50,50),c(40,40),lty=3)
lines(c(-50,50),c(35,35),lty=3)
lines(c(-50,50),c(30,30),lty=3)
lines(c(-50,50),c(25,25),lty=3)
lines(c(-50,50),c(20,20),lty=3)
lines(c(-50,50),c(15,15),lty=3)
lines(c(-50,50),c(10,10),lty=3)
lines(c(-50,50),c(5,5),lty=3)
lines(c(-50,50),c(0,0),lty=3)

# Illustrate the artillery piece
rect(-0.6, -1, 0.6, 1, col="dark green", border=par("fg"), lty=NULL, xpd=FALSE)
rect(-0.15, 0, 0.15, 1.5, col="black", border=par("fg"), lty=NULL, xpd=FALSE)
rect(0.7, -1, 0.9, 1, col="dark gray", border="black", lty=NULL, xpd=FALSE)
rect(-0.7, -1, -0.9, 1, col="dark gray", border="black", lty=NULL, xpd=FALSE)
lines(c(0.8,0.8),c(-1,1),lwd=3,lty=3)
lines(c(-0.8,-0.8),c(-1,1),lwd=3,lty=3)
text(15,0,"Artillery",cex=2,col="dark green")
arrows(11,0,7,0,length=0.15,lwd=3,col="dark green")

# Add the aim point
points(mu1,mu2,pch="x",col="red",cex=3)
text(15,30,"Aim point",cex=2,col="red")
arrows(11,30,7,30,length=0.15,lwd=3,col="red")

# Add CEP contour
if(input$cep_contour==TRUE){
	ellipse(mu_vec, sigma_mat, alpha = 0.5, npoints = 250, newplot = FALSE, draw = TRUE,lwd=4,col="blue")
	
	# Count the number of points within 1 CEP
	dist <-vector(length=as.numeric(input$nr_shots))
	for(i in 1:as.numeric(input$nr_shots)){
	    dist[i] <- t(shots[i,] - mu_vec) %*% solve(sigma_mat) %*% (shots[i,] - mu_vec)
        }
	frac <- sum(as.numeric(dist < qchisq((1-2*pnorm(-0.675)),2)))/as.numeric(input$nr_shots)
	
	# Add a legend that displays the number within 1 CEP
	text(-20,8,paste("Fraction of shots within the CEP:",frac,sep=" "),cex=1.3,pos=4)
    }

# Add sigma contours
if(input$sigma_contour==TRUE){
	ellipse(mu_vec, sigma_mat, alpha = 0.606, npoints = 250, newplot = FALSE, draw = TRUE,lwd=4,col="blue4")	
	ellipse(mu_vec, sigma_mat, alpha = 0.135, npoints = 250, newplot = FALSE, draw = TRUE,lwd=4,col="cornflowerblue")	
	ellipse(mu_vec, sigma_mat, alpha = 0.011, npoints = 250, newplot = FALSE, draw = TRUE,lwd=4,col="light blue")	
	
	# Count the number of points within 1-, 2-, and 2-SDs
	dist <-vector(length=as.numeric(input$nr_shots))
	for(i in 1:as.numeric(input$nr_shots)){
	    dist[i] <- t(shots[i,] - mu_vec) %*% solve(sigma_mat) %*% (shots[i,] - mu_vec)
        }
	
	frac1 <- sum(as.numeric(dist < qchisq(0.394,2)))/as.numeric(input$nr_shots)
	frac2 <- sum(as.numeric(dist < qchisq(0.865,2)))/as.numeric(input$nr_shots)
	frac3 <- sum(as.numeric(dist < qchisq(0.989,2)))/as.numeric(input$nr_shots)

	# Add a legend that displays the numbers within 1-, 2-, and 3-SDs	
	text(-20,13,paste("Fraction of shots within 1 std. deviation:",frac1,sep=" "),cex=1.3,pos=4)
	text(-20,11.5,paste("Fraction of shots within 2 std. deviations:",frac2,sep=" "),cex=1.3,pos=4)
	text(-20,10,paste("Fraction of shots within 3 std. deviations:",frac3,sep=" "),cex=1.3,pos=4)
    }

  })  
})



