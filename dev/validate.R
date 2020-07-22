
validate <- function(object, newdata, y, method="all"){

  y_pred <- predict(object, newdata)[,1]

  if(method == "RMSE"){
    error <- data.frame(RMSE=sqrt(mean((y - y_pred)^2)))
  }
  if(method == "MAE"){
    error <- data.frame(MAE=sqrt(mean((y - y_pred)^2)))
  }
  if(method == "MedianAE"){
    error <- data.frame(MedianAE=sqrt(mean((y - y_pred)^2)))
  }
  if(method == "all"){
    error <- data.frame(RMSE=sqrt(mean((y - y_pred)^2)), MAE=mean(abs(y - y_pred)),
                           MedianAE = median(abs(y - y_pred)))

  }
  return(error)

}


validate(sd, new, y1, method="RMSE")

