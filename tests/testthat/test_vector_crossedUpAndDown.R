load("data/testCross0And20And80.rda")

result<- c("-8", "-5", "-4", "-2", "crossUp0", "-3", "-2", "crossUp0", "10", "17", "18", "19", "crossUp20", "21", "crossDn20", "19",
            "crossUp20", "76", "77", "crossDn20", "crossUp20", "crossUp80", "78", "79", "crossUp80")

fitted<- vector_crossedUpAndDown(testCross0And20And80,c(0,20,80),list(c('crossUp0','crossDn0'),c('crossUp20','crossDn20'),c('crossUp80','crossDn80')))
predicted<- predict(fitted, testCross0And20And80)

testthat::expect_equivalent(predicted, result)

result2<- c('-8', '-5', '-4', '-2', 'crossDnToUp0', '-3', '-2', 'crossDnToUp0', '10', '17', '18', '19',
            'crossDnToUp20', '21', 'crossUpToDn20', '19', 'crossDnToUp20', '76', '77', 'crossUpToDn20',
            'crossDnToUp20', 'crossDnToUp80', '78', '79', 'crossDnToUp80')

fitted2<- vector_crossedUpAndDown(testCross0And20And80, c(0,20,80))
predicted2<- predict(fitted2, testCross0And20And80)

testthat::expect_equivalent(predicted2, result2)
