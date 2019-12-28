
#Load in libraries
library(Rcpp) #to iterate fast
library(tidyverse) #to plot
library(reshape2) #to melt matrix into data frame
library(colourlovers) #to color drawings with nice colors

# Import C++ code
sourceCpp('cpp_funcs.cpp')

# Default aesthetics of the ggplot
opt <-  theme(panel.border = element_rect(color="black", fill = NA),
              legend.position = "none",
              axis.ticks       = element_blank(),
              panel.grid       = element_blank(),
              axis.title       = element_blank(),
              axis.text        = element_blank())

# The plot will be a 400x400 raster
pixels <- 400

# Initialization of two matrix A and B that will form the tensor. Changing the way
# of initializing B you will obtain different patterns. Try yourself!
A <- matrix(1, pixels, pixels) #A is a zero matrix
B <- matrix(sample(c(0, 1), 
                   size = pixels^2, 
                   replace = TRUE, 
                   prob = c(99,1)), 
            ncol = pixels) #B is a binary one with much more 0's than 1's

# Matrix L to perform convolutions
L <- matrix(c(0.05, 0.2, 0.05, 
               0.2,  -1, 0.2, 
              0.05, 0.2, 0.05), nrow = 3)

# DA and DB parameters
DA <- 1
DB <- 0.5

# f and k parameters: play with them to obtain different patterns
f <- 0.0545
k <- 0.062

# Create the tensor X
X <- array(c(A, B) , dim = c(pixels, pixels, 2))

# Perform interations of Gray-Scott algorithm (it may take a while)
X <- iterate_Gray_Scott(X, L, DA, DB, f, k, 5000)

# Convert matrix B into  data frame preserving indexes
df <- melt(X[,,2])
colnames(df) <- c("x","y","B") # to name columns
    
# Pick a random palette from colourlovers
palette <- sample(clpalettes('top'), 1)[[1]] %>% 
      swatch %>% .[[1]] %>% unique() %>% colorRampPalette()
    
# Do the plot
ggplot(data = df, aes(x = x, y = y, fill= B)) + 
  geom_raster(interpolate = T) +
  coord_equal() +
  scale_fill_gradientn(colours = palette(40)) +
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0)) +
  opt -> plot
    
# Do you like it? Save it!
ggsave("choose_a_name.png", plot, height = 3, width = 3, units = "in")        
