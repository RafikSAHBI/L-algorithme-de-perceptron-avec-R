data = data.frame( x1 = c( 3, 1, 1 ),
                   x2 = c( 2, 1, 2 ),
                   x3 = c( 1, 1, 3 ),
                    y = c( 0, 1, 1 ) )

print( data )

# plot( data$x1, data$x2, data$x3, type = 'n', main = 'Dataset', xlab = "x1", 
#      ylab = "x2", zlab = "x3" )
# text( data$x1, data$x2, data$x3, labels = data$y ), col = 'black' )
# grid( nx = length( data$x1 ) + 1, ny = length( data$x1 ), col = 'black' )

#w = c( 1, 1, 1, 1 )

#print( w )

heaviside = function( net ){
  if( net < 0 )
    return( 0 )
  return( 1 )
}




#data = as.matrix( data )
#net = c( data[1, 1:3 ], 1 ) %*% w
#net

#y_hat = heaviside( net )
#y_hat

#error = y_hat - data[1,4]
#error

#delta =  0.1
#delta

#w = w - delta * ( error ) * c( data[ 1, 1:3 ], 1 )

#print( w )


# With initial random weights
perceptron = function( dataset, delta = 0.1, threshold =1e-5 ){
  data = as.matrix( dataset )
  num.features = ncol( data ) - 1
  target = ncol( data )
  
   # Initial random  weights
   w = rnorm( mean = 0, sd = 0.1, n = ncol( data ) )
   w
  # Add bias and compute multiplications
  mse = threshold * 2
  while( mse > threshold ){
    mse = 0
    for( i in 1:nrow( data ) ){
      net = c( data[ i, 1:num.features ], 1 ) %*% w
      
      # Activation function
      y_hat = heaviside( net )
      
      # Compute mse
      error = ( y_hat - data[ i, target ] )
      mse = mse + error^2
      cat( paste( "Mean square error = ", mse, "\n" ) )
      
      # Update weights
      w = w - delta * error * c( data[i, 1:num.features ], 1 )
    }
  }
  return( w )
}

w = perceptron( data, delta=0.1, threshold=1e-5 )

w