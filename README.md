# MLfinalproject-taxis

## Motivation

There is an interesting question (several questions actually...) to be asked in the service industry that tends to be defined for restaurants and bars but not Taxis - what should the tip be?

1. As a driver for FHV (for-hire vehicles), are certain customers worth more than others in terms of tip percentage?
2. Can we predict the tip percentage? 
3. What factors contribute most to larger tips?

## Data Set

* NYC taxi & Limousine Commision 
* Training set : 3,884,620
* Testing set : 863,248
* Dataset contains twenty-one variables such as time of transaction, pickup/dropoff date and time, transaction amount, etc.

**Feature  Engineering**
* Day of the week  
* Hour of pick up 
* Hour of drop off
* Duration of the trip (in minutes) 
* Speed 
* Part of day (early morning, morning, etc.) 

## Quick Methods
1. Linear Regression as a baseline
2. Gradient Boosted Trees 
  * Optimized / Tuned using Grid Search 
  * Models scored after every 10 trees
  * Stopped once the MSE value was optimized
  * Optimized parameters include number of trees, max leaves, etc.

**Final Model Parameters **
  Number_of_trees: 114 
  Number_of_internal_trees: 114
  Min_depth: 4
  Learning Rate: .05
  Max_depth: 9
  Mean_depth: 8.86
  Min_leaves: 7
  Max_leaves:189
  Mean_leaves: 81.84
