# Rank Hospitals #

This project allows the user to rank hospitals in all 50 states based on their success rate in treating three different illnesses: heart attack, heart failure, and pneumonia.

##best

This function requires two inputs: a two-letter state abbreviation and one of three conditions ("heart attack", "heart failure", "pneumonia").  It then outputs the name of the hospital with the highest success rate for that condition.

##rankhospital

This function works the same as best, but allows the user to specify "best" or "worst".

##rankall

This function requires a condition ("heart attack", "heart failure", "pneumonia") and, optionally, "best" or "worst".  The function then outputs a data frame with the best (or worst) ranked hospital in every state for that condition.