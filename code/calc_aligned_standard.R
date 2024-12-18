
# Calculate aligned standard
# fv = "Maize flour"; nutr = "Vitamin B12"; intake_g_d = 210
calc_aligned_standard <- function(fv, nutr, intake_g_d){
  
  # NA
  std <- NA
  
  # Maize flour
  if(fv=="Maize flour"){
    if(nutr=="Vitamin B12"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(0.04, 0.02, 0.01, 0), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Vitamin A"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(6, 3, 1.5, 0), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Folate"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(5, 2.6, 1.3, 0), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Riboflavin"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(2, 2, 2, 0), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Vitamin B6"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(6.2, 6.2, 6.21, 0), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Thiamin"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(3.9, 3.9, 3.9, 0), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Iron"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(40, 40, 20, 0), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Niacin"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(36, 36, 36, 0), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Zinc"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(95, 55, 40, 0), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Vitamin B5"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(4.2, 4.2, 4.2, 0), right=F) %>% as.character() %>% as.numeric()
    }
    
  }
  
  # Oil
  if(fv=="Oil"){
    if(nutr=="Vitamin A"){std <- 18}
    if(nutr=="Vitamin E"){std <- 105}
  }
  
  # Rice
  if(fv=="Rice"){
    if(nutr=="Vitamin B12"){std <- 0.01}
    if(nutr=="Folate"){std <- 1.1}
    if(nutr=="Riboflavin"){std <- 3}
    if(nutr=="Vitamin B6"){std <- 4}
    if(nutr=="Thiamin"){std <- 5}
    if(nutr=="Zinc"){std <- 28.5}
    if(nutr=="Niacin"){std <- 41}
    if(nutr=="Iron"){std <- 42.45}
  }
  
  # Salt
  if(fv=="Salt"){
    if(nutr=="Iodine"){
      std <- cut(x=intake_g_d,
               breaks=c(0, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, Inf),
               labels=c(65, 65, 49, 39, 33, 28, 24, 22, 20, 18, 16, 15, 14), right=F) %>% as.character() %>% as.numeric()
    }
  }
  
  # Wheat flour
  if(fv=="Wheat flour"){
    if(nutr=="Vitamin B12"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(0.04, 0.02, 0.01, 0.008), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Vitamin A"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(5.9, 3, 1.5, 1), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Folate"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(5, 2.6, 1.3, 1), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Riboflavin"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(2, 2, 2, 2), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Vitamin B6"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(2, 2, 2, 2), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Thiamin"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(3, 3, 3, 3), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Zinc"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(95, 55, 40, 30), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Iron"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(40, 40, 20, 15), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Niacin"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(40, 40, 40, 40), right=F) %>% as.character() %>% as.numeric()
    }
    if(nutr=="Calcium"){
      std <- cut(x=intake_g_d,
                 breaks=c(0, 75, 149, 300, Inf),
                 labels=c(3125, 2112, 1250, 1250), right=F) %>% as.character() %>% as.numeric()
    }
    
  }
  
  # Return
  return(std)  
  
}


