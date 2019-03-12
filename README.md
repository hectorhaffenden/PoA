### Game Theory - Routing Theory Solver in R
The raw code can be found in Workflow.R

# What does this do?
This provides a simple interface to enter your routing network, and returns the price of anarchy, as well as the optimal and nash costs with their respective parameters. The following examples illustrate this. Then run the PriceOfAnarchy function with the tibble of cost and flow, and variable.limits (what each variables constraints are, for example, for the first network, the alpha variables maximum value is 0.5, and must be greater than 0, same with beta, so enter c(0.5,0.5) as that argument).



## Examples

For the following routing network (Input the cost and flow values, as functions)

# 2 player 
The following example was used in development of this algorithm. Provided by https://vknight.org/Year_3_game_theory_course/Content/Chapter_17_Routing_games/

player2 <- tibble(cost = c(function(x){0}, function(x){0}, function(x){NA}, function(x){x^2}, function(x){(3/2)*x},  function(x){x}),
                            flow = c(function(alpha,beta){(1/2)-alpha},function(alpha,beta){(1/2)-beta},  function(alpha,beta){NA}, function(alpha,beta){alpha},  function(alpha,beta){beta},
                                     function(alpha, beta){1-alpha-beta}))

PriceOfAnarchy(player2, c((1/2), (1/2)))


# 3 player
player3 <- tibble(cost = c(function(x){x}, function(x){x}, function(x){x}, function(x){x^2}, function(x){x}, function(x){x^2}, function(x){NA}, function(x){x}),
                  flow = c(function(alpha,beta,theta){(3/12)-alpha}, function(alpha,beta,theta){alpha}, function(alpha,beta,theta){(4/12)-beta}, function(alpha,beta,theta){beta}, function(alpha,beta,theta){(5/12)-theta}, function(alpha,beta,theta){theta}, function(alpha,beta,theta){NA}, function(alpha,beta,theta){1-alpha-beta-theta}))

PriceOfAnarchy(player3, c((3/12), (4/12), (5/12)))




# 3 player - 2x2 middle
player3middle2x2 <- tibble(cost = c(function(x){0}, function(x){x},function(x){x^2},function(x){x},function(x){x},function(x){0},function(x){x},function(x){x^2},function(x){x^2},function(x){x},function(x){(1/2)*x},function(x){x}),
                           flow = c(function(d1,d2,d3,d4,d5){d1},function(d1,d2,d3,d4,d5){1-d1},function(d1,d2,d3,d4,d5){d2},function(d1,d2,d3,d4,d5){1-d2},function(d1,d2,d3,d4,d5){d3},function(d1,d2,d3,d4,d5){1-d3},function(d1,d2,d3,d4,d5){d4},function(d1,d2,d3,d4,d5){d1+d2+d3-d4},function(d1,d2,d3,d4,d5){d5},function(d1,d2,d3,d4,d5){1-d1-d2-d3-d5},function(d1,d2,d3,d4,d5){d4+d5},function(d1,d2,d3,d4,d5){1-d4-d5}))



PriceOfAnarchy(player3middle2x2, c((6/10),(1/10),(3/10),1,1))

               
