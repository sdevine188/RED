# https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf
# wilson score is equation #10 in paper

n1 <- 1000
x1 <- 50
p1 <- x1 / n1
p1
z <- 1.96

first_term <- (1 / (2 * (n1 + z^2)) )  
first_term

upper_limit <- first_term * ( (2 * n1 * p1 + z^2) + (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
upper_limit

lower_limit <- first_term * ( (2 * n1 * p1 + z^2) - (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
lower_limit

# without breaking up terms
(1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) + (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
(1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) - (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )


####################################################


# newcombe conf. int. for difference in proportions

# example 1
output_node_p_weighted_combined %>% data.frame()

p1 <- 0.4135501
upper1 <- 0.64151620
lower1 <- 0.21745359

p2 <- 0.06783049
upper2 <- 0.1113119
lower2 <- 0.04055891

p1 - p2

diff_lower_limit <- (p1 - p2) - sqrt((p1 - lower1)^2 + (upper2 - p2)^2)
diff_lower_limit 

diff_upper_limit <- (p1 - p2) + sqrt((upper1 - p1)^2 + (p2 - lower2)^2)
diff_upper_limit        


#################################################


# newcombde example 2
# note the diff can be negative
p1 <- 0.1750581
upper1 <- 0.47000712 
lower1 <- 0.04832501 

p2 <- 0.08948235
upper2 <- 0.1362107
lower2 <- 0.05771353

p1 - p2

diff_lower_limit <- (p1 - p2) - sqrt((p1 - lower1)^2 + (upper2 - p2)^2)
diff_lower_limit 

diff_upper_limit <- (p1 - p2) + sqrt((upper1 - p1)^2 + (p2 - lower2)^2)
diff_upper_limit        


###################################################
##################################################
##################################################


# full example with mtcars without weights

# get diff in rate of cars which are manual in domain of 8-cyl cars, and the rate of cars with are manual overall (am = 1)
head(mtcars)
mtcars %>% group_by(cyl) %>% count()
mtcars %>% group_by(cyl, am) %>% count()

p_overall <- mtcars %>% summarize(p_overall = mean(am))
p_overall

p_domain <- mtcars %>% filter(cyl == 8) %>% summarize(p_domain = mean(am))
p_domain

mtcars_data <- mtcars %>% mutate(cyl8_dummy = ifelse(cyl == 8, 1, 0))
head(mtcars_data)

# get wilson conf. int. for domain 
n1 <- mtcars_data %>% filter(cyl8_dummy == 1) %>% nrow()
n1
x1 <- mtcars_data %>% filter(cyl8_dummy == 1, am == 1) %>% nrow()
x1
p1 <- x1 / n1
p1
z <- 1.96

upper1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) + (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
upper1

lower1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) - (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
lower1


################################


# get wilson conf. int. for complement
n2 <- mtcars_data %>% filter(cyl8_dummy == 0) %>% nrow()
n2
x2 <- mtcars_data %>% filter(cyl8_dummy == 0, am == 1) %>% nrow()
x2
p2 <- x2 / n2
p2
z <- 1.96

upper2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) + (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
upper2

lower2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) - (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
lower2


####################################


# get diff in domain vs complement using newcombe
p1
p2
diff_dc <- p1 - p2
diff_dc

diff_dc_lower_limit <- (p1 - p2) - sqrt((p1 - lower1)^2 + (upper2 - p2)^2)
diff_dc_lower_limit 

diff_dc_upper_limit <- (p1 - p2) + sqrt((upper1 - p1)^2 + (p2 - lower2)^2)
diff_dc_upper_limit    


##########################################


# get diff in domain vs overall using reid's derivation
p_domain
p_overall
p_domain - p_overall

diff_overall <- (1 - (n1 / (n1 + n2))) * diff_dc
diff_overall

diff_overall_upper_limit <- (1 - (n1 / (n1 + n2))) * diff_dc_upper_limit 
diff_overall_upper_limit

diff_overall_lower_limit <- (1 - (n1 / (n1 + n2))) * diff_dc_lower_limit 
diff_overall_lower_limit


####################################################
###################################################
##################################################


# full example with mtcars with weights

# get diff in rate of cars which are manual in domain of 8-cyl cars, and the rate of cars with are manual overall (am = 1)
head(mtcars)
mtcars %>% group_by(cyl) %>% count()
mtcars %>% group_by(cyl, am) %>% count()

mtcars_data <- mtcars %>% mutate(cyl8_dummy = ifelse(cyl == 8, 1, 0))
head(mtcars_data)

# add weights and created weighted_obs
mtcars_data <- mtcars_data %>% mutate(weights = case_when(gear == 4 ~ 1.22, gear == 3 ~ 1.04, gear == 5 ~ 1.11), 
                                      weighted_obs = am * weights)
head(mtcars_data)

# get p_overall and p_domain as check
p_overall <- mtcars_data %>% summarize(p_overall = sum(weighted_obs) / sum(weights))
p_overall

p_domain <- mtcars_data %>% filter(cyl == 8) %>% summarize(p_weighted = sum(weighted_obs) / sum(weights))
p_domain

# get wilson conf. int. for domain 
n1 <- mtcars_data %>% filter(cyl8_dummy == 1) %>% summarize(weight_sum = sum(weights))
n1
x1 <- mtcars_data %>% filter(cyl8_dummy == 1, am == 1) %>% summarize(weight_sum = sum(weights))
x1
p1 <- x1 / n1
p1
z <- 1.96

upper1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) + (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
upper1

lower1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) - (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
lower1


################################


# get wilson conf. int. for complement
n2 <- mtcars_data %>% filter(cyl8_dummy == 0) %>% summarize(weight_sum = sum(weights))
n2
x2 <- mtcars_data %>% filter(cyl8_dummy == 0, am == 1) %>% summarize(weight_sum = sum(weights))
x2
p2 <- x2 / n2
p2
z <- 1.96

upper2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) + (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
upper2

lower2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) - (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
lower2


####################################


# get diff in domain vs complement using newcombe
p1
p2
diff_dc <- p1 - p2
diff_dc

diff_dc_lower_limit <- (p1 - p2) - sqrt((p1 - lower1)^2 + (upper2 - p2)^2)
diff_dc_lower_limit 

diff_dc_upper_limit <- (p1 - p2) + sqrt((upper1 - p1)^2 + (p2 - lower2)^2)
diff_dc_upper_limit    


##########################################


# get diff in domain vs overall using reid's derivation
p_domain
p_overall
p_domain - p_overall

diff_overall <- (1 - (n1 / (n1 + n2))) * diff_dc
diff_overall

diff_overall_upper_limit <- (1 - (n1 / (n1 + n2))) * diff_dc_upper_limit 
diff_overall_upper_limit

diff_overall_lower_limit <- (1 - (n1 / (n1 + n2))) * diff_dc_lower_limit 
diff_overall_lower_limit





























        
        
        
        