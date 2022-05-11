
# JRodoni_HW04_script.R
# C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/
#   STAT 604 - STAT Computation/Homeworks/JRodoni_HW04_script.R
# Created By: Jack Rodoni
# Creation Date: 09/14/2021
# Purpose: STAT 604 Homework 4
# Last Executed: 09/14/2021
Sys.time()

# 1.) Perform housekeeping steps to ensure you start with a clean workspace.The 
#     first housekeeping function should display the contents of the workspace.
#     The second housekeeping function should clear the workspace but it is to 
#     be commented out so it will not be run automatically should you execute 
#     the entire script.  Add a step to show which libraries are loaded in your 
#     session.

ls()
rm(list = ls())
search()



# 2.) Use a function to set up your R session so that everything written to the 
#     console will also be directed to a separate text file while still 
#     appearing in the console.  Include the full path to show where the 
#     textfile will be written.

sink(file = "C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/STAT 604 - STAT Computation/Homeworks/JRodoni_HW4.txt", split = TRUE)


# 3.) Invoke R help to research the seq function in the available documentation.  
#     This command is not to be part of your program script but will be 
#     referenced as the answer to one of the questions at the end of the 
#     assignment.


# 4.) Unless you are specifically instructed to give an object a certain name, 
#     you are expected to use a name of your own choosing.  Write a single 
#     line of code to create in the workspace and display a vector of numeric 
#     values from 5 to 80 with an increment of 5.  Show the type of data in the 
#     vector.  Show the length.

(seq1 = seq(from = 5, to = 80, by = 5))
class(seq1)
length(seq1)

# 5.) Create in the workspace and display a vector of numeric values from 0.4 
#     to 20 with an increment of 0.4.  Show the type of data inthe vector. 
#     Show the length.

(seq2 = seq(from = 0.4, to = 20, by = 0.4))
class(seq2)
length(seq2)

# 6.) Use the firstvector to create and display a matrix by columns that is 4 
#     columns wide

(matrix1 = matrix(data = seq1, ncol = 4, byrow = FALSE))

# 7.) Combine the two vectors as rows to create and display a new matrix.

(matrix2 = rbind(seq1, seq2))

# 8.) Combine the two vectors as columns to create and display a new matrix.

(matrix3 = cbind(seq1, seq2))

# 9.) Create a vector that contains the ninenumeric values 67, 72, 75,95,58, 
#     82, 88, 93 and 100. Execute a command that will display only the second, 
#     fourth,fifth and sixth members of the vector.

vect1 = c(67, 72, 75, 95, 58, 82, 88, 93, 100)
vect1[c(2,4,5,6)]

# 10.) Create another vector that contains character strings with values of 
#      Dasher, Dancer, Prancer, Donder, Blitzen, Vixen, Comet, Cupid,and 
#      Rudolph. Execute a command that will display only the first four members 
#      of the vector.

vect2 = c("Dasher", "Dancer", "Prancer", "Donder", "Blitzen", "Vixen", "Comet",
          "Cupid", "Rudolph")
vect2[1:4]

# 11.) Combine the character vector with the numeric vector to create and 
#      display a data frame.  Execute a function to show the data storage 
#      type of the new data frame.  Show the contents of the workspace

(df1 = data.frame(vect1,vect2))
mode(df1)
ls()


# 12.) Load the states workspace that you downloaded from Canvas. You may use 
#      the R menu to load the workspace initially,but your script must contain 
#      a line of code that will load the workspace the next time you run the 
#      script.  Some versions of R will make an entry in the console log 
#      showing the command that loaded the workspace.  If you get this line, 
#      you may copy it into your script.  Otherwise, you will need to find 
#      the command syntax inthe course slides orR documentation and write the 
#      command yourself. Show the contents of the workspace with the newly 
#      loaded object(s).

load("C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/STAT 604 - STAT Computation/RData/states.RData")
ls()

# 13.) Display the object type and the type of data in Texas.
class(Texas) #object type
mode(Texas)  #data type


# 14.) Display the object type and type of data in column 1 from Texas.
class(Texas[,1]) #object type
mode(Texas[,1])  # data type

# 15.) Display the structure of Texas
str(Texas)

# 16.) Display a summary of Texas
summary(Texas)

# 17.) Display the first 20 rows and all but column 3 from Texas.Use a 
#      negative index value.
Texas[1:20,-3]
# or alternatively
Texas[-(21:nrow(Texas)),-3]

# 18.) Create and display a new object from Texas using the first 15 rows, 
#      the first column and third column
(TexasB = Texas[1:15,c(1,3)])

# 19.) Add a command that closes the text file and stops sending output to it.
sink()

# 20.) After you have run your script for the final time, answer the following 
#      questions in a series of comments at the bottom of the script.

# a.) What command did you use to invoke help on seq?
#       ?seq

# b.) How many packages are loaded in your R Session?  
#     (Count only those listed as "package:").
#     7

# c.) What type of data is inthe vector created in step 4?
#     Numeric

# d.) Explain how the values from the first vector are used 
#     in the creation of the matrix in step 7.

#     The values of the first vector are used as the values in 
#     the first row of the matrix. It is important to note that
#     becasue the length of vector 2 is greater than the length 
#     of vector 1, the values of vector 1 are recycled inorder to
#     make both rows of the matrix the same length.

# e.) What is the type of data in the data frame created in step 11?
#     list

# f.) What is the class and data type of column 1 from Texas?
#     Class - factor
#     Data Type - numeric

# g.) How many observations and variables are in the Texas data frame?
#     254 observations of 3 variables

# h.) Explain the relationship between the median and mean of the Pop column?
#     The mean pop is much higher than the median pop as the mean is skewed 
#     heavily by the high population counties.




























