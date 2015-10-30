library(RUnit)
library(crmp)

testsuite <- defineTestSuite("CRMP",
#                             dirs = c("../../R/"),
#                             system.file("R", package = "crmp"),
                             dirs = "./",
#                             testFileRegexp = "^(crmp.load.data|spatial.inventory|crmp-class|flags-class).r$",
                             testFileRegexp = "^test\\..*\\.r$",
                             testFuncRegexp = "^test.+")

test.result   <- runTestSuite(testsuite, useOwnErrorHandler=F)
printTextProtocol(test.result)
printHTMLProtocol(test.result, "RUnit_crmp_test_log.html")
