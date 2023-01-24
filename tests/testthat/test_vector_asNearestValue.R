data<- c(0.8,1,0.9,2.8,-1,-0.5,-2.4)
values<- c(-3,-2,-1,1,2,3)
ans<- c(1,1,1,3,-1,-1,-2)

testthat::expect_equivalent(vector_asNearestValue(data, values), ans)
