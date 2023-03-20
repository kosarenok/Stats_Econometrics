mtcars$even_gear <- mtcars$gear %% 2 == 1
my1 <- subset(mtcars, even_gear == 'FALSE' )
my2 <- subset(mtcars, even_gear == 'TRUE' )

mtcars$even_gear <- cbind(my1 , my2)
?subset
