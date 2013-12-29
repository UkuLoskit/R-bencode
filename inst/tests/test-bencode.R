
library(bencode)

test_that("bencode parses integers, strings and lists as expected",{
    expect_identical(bencode("li1ei2ee"), 1:2)
    expect_identical(bencode("l2:ab3:abc4:abcde"), c("ab", "abc", "abcd"))
    expect_identical(bencode("lli1ei2eel2:ab3:abc4:abcdee"), list(1:2, c("ab", "abc", "abcd")))
    expect_identical(bencode("lli1ei2eli1ei2eee"), list(list(1L, 2L, 1:2)))
    expect_identical(bencode("lli1ei2eel1:a2:ab3:abcee"), list(1:2, c("a", "ab", "abc")))
    expect_identical(bencode("lli1ei2eel1:a2:ab3:abci3eee"), list(1:2, list("a", "ab", "abc", 3L)))
})

