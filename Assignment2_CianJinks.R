n_stay <- 0 #Variable for the number of times the person got the car by staying
n_switch <- 0 #Variable for the number of times the person got the car by switching
random_wins <- 0

for ( i in 1:100) {
  door <- c(1,2,3) #A vector with 3 options (one for each door)
  cardoor <- sample(door,1) #Makes a random door the one with the car behind it
  choice <- sample(door,1) #Makes a random door the persons choice
  goatdoors <- setdiff(door, cardoor) #Makes goatdoors equal to the two doors that are not the one with the car
  reveal_options <- setdiff(goatdoors, choice) #Make the possible doors to reveal the ones with goats and that the user did not already pick
  #If the user picked the cardoor
  if (choice == cardoor) { 
    reveal <- sample(reveal_options,1)  }  #Make the reveal one of the reveal options
  else {
    reveal <- reveal_options #If the user did not pick the door then make the reveal equal to the reveal options
  }
  remaining_doors <-setdiff(door, reveal) #Make a vector that has the remaining unrevealed doors
  random_choice <- sample(remaining_doors, 1) #Choose a random door of the unrevealed ones for the user to pick (Part 3)
  newchoice <- setdiff(remaining_doors, choice) #Store the new door after the contestant switches   
  
  #If the user picked the car door originally
  if (choice == cardoor) {
    n_stay <- n_stay + 1 #Increase the number of time they got the car by staying
  }
  
  #If the user had to switch the achieve the cardoor
  if (newchoice == cardoor) {
    n_switch <- n_switch + 1 #Increase the number of times they got the car by switching
  }
  
  #If statement to check if the random user (part 3) won the car or not
  if(random_choice == cardoor) {
    random_wins <- random_wins + 1 #Increase the number of times the random user one the car
  }
}
print(n_stay/100) #Print the percentage of times they got the car by staying
print(n_switch/100) #Print the percentage of times they got the car by switching
print(random_wins/100) #Print the percentage of times the random user won the car
