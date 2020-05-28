#load package

library(png)

#generate a random set of weights

W = empty_list <- vector(mode = "list", length = 100)

for(i in 1:100) { 
W[[i]] = matrix(data = runif(25, min = 0, max = 1), nrow = 10, ncol = 10)
}


#Create a function that calculates the euclidean distance

U = empty_matrix <- matrix(data = NA, nrow = 10, ncol = 10, byrow = T)

D = empty_list <- vector(mode = "list", length = 100)

for(i in 1:100) { 

D[[i]] = matrix(data = NA, nrow = 10, ncol = 10)

}


Eu_dist <- function(inp, weight, unit, dif) { 
	for(i in 1:100) { 
		for(n in 1:100){
		dif[[i]][n] <- (inp[n] - weight[[i]][n])^2
		}
	unit[i] <- (sum(dif[[i]]))^(1/2) 
	}
return(unit)
}
							 


#create a function that changes the weights based on the proximity to the BMU

neighbor <- matrix(data = U, nrow = 10, ncol = 10, byrow = T)


W_morph <- function(unity, neib, rad_rate, learn_rate, weight, inp, Lo, colour) {

t <- 1 

bmu <- unity[which.min(unity)]

print(bmu)

bmu_position = which(unity == bmu, arr.ind = T)

bmu_position[]

	while(t < 40) {
	rad <- 7 * exp(-t/rad_rate)
		for(i in 1:nrow(unity)) {
			for(j in 1:ncol(unity)) {  
			distance <- ((bmu_position[1] - i)^2 + (bmu_position[2] - j)^2)^(1 / 2)
				if(distance < rad){ neib[i, j] <- unity[i, j] 
				L <- Lo * exp(-t/learn_rate) 
				R <- exp( -(distance^2) / (2 * rad^2) )
				w_redct <- which(unity == unity[i, j])
				weight[[w_redct]] <- weight[[w_redct]] + (L * (inp - weight[[w_redct]]) * R)
				points(i,j, col = colour)
				}
					else { neib[i,j] <- NA
					}
			}
		}
	t = t + 1
	}
return(weight)
}

#extract the png information and put it in a list

inputJ1 <- readPNG("\\Users\\leona\\OneDrive\\Imagens\\J - Copia\\KK.png")
inputJ2 <- readPNG("\\Users\\leona\\OneDrive\\Imagens\\J - Copia\\J2.png")
inputA1 <- readPNG("\\Users\\leona\\OneDrive\\Imagens\\A\\A1.png")
inputA2 <- readPNG("\\Users\\leona\\OneDrive\\Imagens\\A\\A2.png")
inputG1 <- readPNG("\\Users\\leona\\OneDrive\\Imagens\\G\\G1.png")
inputG2 <- readPNG("\\Users\\leona\\OneDrive\\Imagens\\G\\G2.png")

inputs <- list(inputJ1, inputA1, inputG1)

length(inputs)
#create colour function

sgn <- c("P", "L", "H")

#create a function that run the entire SOM based on input 

training_SOM <- function(inpt, neib, rad_rate, learn_rate, weights, Lo, sign) {

plot(1:10, 1:10)

	for(n in 1:length(sign)){
		
		#calculating and organizing the matrix of distances
		U = Eu_dist(inp = inpt[[n]], weight = W, unit = U, dif = D)
		bmu <- U[which.min(U)]
		print(bmu)
		bmu_position = which(U == bmu, arr.ind = T)
		bmu_position[]
			#mapping the BMU region and changing weights
			t <- 1
			while(t < 270) {
			rad <- 8 * exp(-t/rad_rate)
				for(i in 1:nrow(U)) {
					for(j in 1:ncol(U)) {  
					distance <- ((bmu_position[1] - i)^2 + (bmu_position[2] - j)^2)^(1 / 2)
						if(distance < rad){ neib[i, j] <- U[i, j] 
						L <- Lo * exp(-t/learn_rate) 
						R <- exp( -(distance^2) / (2 * rad^2) )
						w_redct <- which(U == U[i, j])
						weights[[w_redct]] <- weights[[w_redct]] + (L * (inpt[[n]] - weights[[w_redct]]) * R)
						text(i,j, sign[n])
						}
							else { neib[i,j] <- NA 
							}
					}
				}
			t = t + 1
			}
	}
return(weights)
}

W = training_SOM(inpt = inputs, neib = neighbor, rad_rate = 1, learn_rate = 1, weights = W, Lo = 1, sign = sgn)

 
