library(RUnit)
source('crmp-class.r')

testsuite <- defineTestSuite("CRMP-nightly",
                                 dirs = c("./"),
                                 testFileRegexp = "^(crmp.load.data|spatial.inventory|crmp-class|flags-class).r$",
                                 testFuncRegexp = "^(long.)?test.+")

test.result   <- runTestSuite(testsuite, useOwnErrorHandler=F)
printTextProtocol(test.result)
printHTMLProtocol(test.result, "RUnit_crmp_test_log.html")
