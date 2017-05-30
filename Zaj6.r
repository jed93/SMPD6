library("neuralnet")

#Going to create a neural network to perform prediction
#Type ?neuralnet for more information on the neuralnet library

#Generate training data
#And store them as a dataframe
traininginput <- as.data.frame(matrix(c(30, 4.8, 18,
                                        29, 4.1, 17,
                                        24, 3.9, 17,
                                        27, 4.4, 14,
                                        26, 5.5, 16,
                                        22, 3.9, 14,
                                        37, 7.1, 17,
                                        22, 3.7, 17,
                                        42, 19.8, 14,
                                        10, 11.5, 20,
                                        7, 2.4, 20,
                                        41, 7.52, 18,
                                        21, 2.83, 18,
                                        20, 4.1, 20,
                                        30, 4.7, 18,
                                        32, 2.9, 15,
                                        24, 5, 15,
                                        20, 4.3, 20,
                                        4, 2.3, 19,
                                        32, 2.9, 15), nrow=20, ncol=3))
trainingoutput <- c(269, 379, 349, 299, 299, 160, 499, 259, 2699, 419, 269, 489, 189, 369, 319, 399, 200, 269, 159, 419)

#Column bind the data into one variable
trainingdata <- cbind(traininginput, trainingoutput)

# Create Vector of Column Max and Min Values
maxs <- apply(trainingdata[,], 2, max)
mins <- apply(trainingdata[,], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.trainingdata <- as.data.frame(scale(trainingdata[,], center=mins, scale=maxs-mins))
trainingdata <- scaled.trainingdata

# Check out results
print(head(trainingdata, 10))

colnames(trainingdata) <- c("Pojemnosc", "Waga_w_kg", "Utrzymywanie_temperatury_do_st_c", "Cena") 
print(trainingdata)

#Train the neural network
#Going to have C(5, 4, 3) hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.price <- neuralnet(Cena~Pojemnosc+Waga_w_kg+Utrzymywanie_temperatury_do_st_c, trainingdata, hidden=c(5, 4, 3), threshold=0.001)
print(net.price)

#Plot the neural network
plot(net.price)

#Test the neural network on some training data
testdata <- as.data.frame(matrix(c(262, 64, 28,
                                   206, 75, 16,
                                   361, 56, 20), nrow=3, ncol=3))
scaled.testdata <- as.data.frame(scale(testdata[,], center=mins[1:3], scale=maxs[1:3]-mins[1:3]))
net.results <- compute(net.price, scaled.testdata) #Run them through the neural network

#Lets see what properties net.price has
ls(net.results)

#Lets see the results
print(net.results$net.result)
