#building weight matrix

weight_mtx <- matrix(data = NA, ncol= 10, nrow = 10)

#creating an random input for the pattern 

input_sample <- function(x = 1, y = -1, length, input = c()) {
t <- 1
polar <- c(x, y )
	while(t <= length){
	input[t] <- sample(polar, 1)
	t <- t + 1
	}
return(input)
}

input_sla <- as.matrix(input_sample(length = 10))


# building the pattern stabilization

standardization <- function(input, weights) {
	for(i in 1:length(input)) {
		for(n in 1:length(input)) {
		weights[i, n] <- input[i] * input[n] 
		}
	}
weights <- weights - diag(length(input))
return(weights)
}

# creating the energy function

energy <- function(input, weights, E = list()){
	E <- -1/2 * (t(input) %*% weights %*% input)
return(E)
}

energy(corrupt_inp, weight_mtx)


diference <- function(x){
t <- x[1]
	for(d in 2:length(x)){ t <- t - x[d] 
		if( t == 0){ t <- x[d]
		}
	}
if( t == x[1]){ t <- 0 }
return(t)
}
diference(c(3,3,3,3,3,3)) == 0


#tests

print(weight_mtx)

weight_mtx <- standardization(input = input_sla, weights = weight_mtx)

weight_mtx2 <- standardization(input = corrupt_inp, weights = weight_mtx)


print(weight_mtx)

# creating the sign fucntion

sign <- function(x) { 
	if(x >= 0) { 
	x = 1 
	}
		else {  x = -1
		}
return(x)
}

sign(-14)


# creating the neuron activation function as an asynchronous network

sample_neuron <- function(weight, noise_input, engy = c(), e_dif = c()) {	
i <- 1
	repeat {
	engy[i] <- energy(input = noise_input, weights = weight)
	print(engy)
		if(i > 50) { 
		e_dif <-  engy[(i - 30):i]
		print(e_dif)
			if(diference(e_dif) == 0) { return(noise_input)
			break
			}
		}
				inp <- sample(1:length(noise_input),1)
		
				W <- t(noise_input) %*% as.matrix(weight[,inp])
		
					if(sign(W) != noise_input[inp]){
					noise_input[inp] <- sign(W)
					}
	i <- i + 1
	}
}


# creating an corrupt input

input_sla

corrupt_inp <- as.matrix(c(-1,1,-1,1,-1,1,1,1,1,1))

corrupt_inp

corrupt_inp <- sample_neuron(weight = weight_mtx, noise_input = corrupt_inp)
sample_neuron(weight = weight_mtx, noise_input = corrupt_inp)

energy(input_sla, weight_mtx)

energy(corrupt_inp, weight_mtx)
