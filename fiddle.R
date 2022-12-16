get_accuracy <- function(x, vector_length) {
  count = 0
  max = 0
  for (val in 1:(vector_length-1)) {
    for (val_2 in  (val+1):vector_length){
       a = x[val]
       b = x[val_2]
       #print(c(a, b, a < b))
       weight = 1
       # weight = 1/(val_2 - val) # doesn't yield better result
       # weight = 1-(val_2 - val)/vector_length # doesn't yield better result
       if(a < b){
         count = count + weight
       }
       if(a > b){
         count = count - weight
       }
       max = max + weight
    }
  }
  count / max
}

get_cor <- function(x, vector_length) {
  target=1:vector_length
  cor(x, target, method="spearman")
}

test <- function(vector_length, test_number, cor_method="spearman") {
  cors = c()
  accs = c()
  
  for (val in 1:test_number) {
    x <- sample(x = 1:vector_length, size=vector_length, replace = FALSE)
    cors <- append(cors, get_cor(x, vector_length))
    accs <- append(accs, get_accuracy(x, vector_length))
  }

  cor(cors, accs, method=cor_method)
}

eval <- function(vector_range, cor_method="spearman"){
  res_list = c()
  for (val in 1:30) {
    sample = sample(vector_range, 1)
    if (length(vector_range) == 1){
      sample = vector_range
    }
    res_list <- append(res_list, test(sample, 1000, cor_method))
  }
  
  print(mean(res_list))
  print(var(res_list))
}

print("ranked correlation of accuracy and ranked correlation: (mean, var)")
eval(2:100)
eval(3)

print("---")
print("plain correlation (not ranked) of accuracy and ranked correlation: (mean, var)")
eval(2:100, "pearson")
eval(3, "pearson")
