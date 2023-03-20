library(readxl)
Homework <- read_excel("Desktop/Homework.xls", 
                       col_names = FALSE, col_types = c("text"))

table(Homework$...1)  # Creating a frequency table
library('plyr')
count(Homework, '...1') # More beatiful frequency table

table(Homework$...1)/length(Homework$...1) # Relative frequency table
# Creating a bar plot
H <-  c(12, 4, 9)
B <- c("Internet", "Newspaper", "TV")
barplot (H, names.arg=B , xlab = "Media", ylab = "Amount", col = "Pink" , main = "Where you watch news?")

# Creating a pie chart
pie(x=H, labels = B, radius=1, main = "Where you watch news?", clockwise = TRUE)
