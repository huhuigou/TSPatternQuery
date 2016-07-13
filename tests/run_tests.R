library("RUnit")
#Add Source File Name

test.suit <- defineTestSuite("TSPatternQuery",
                             dirs = file.path("tests"),
                             testFileRegexp = '^\\d+\\.R')

test.result <- runTestSuite(test.suit)

printTextProtocol(test.result)
