library(bencode)

test_that("bencode parses integers, strings and lists as expected",{
    expect_identical(bencode("li1ei2ee"), 1:2)
    expect_identical(bencode("l2:ab3:abc4:abcde"), c("ab", "abc", "abcd"))
    expect_identical(bencode("lli1ei2eel2:ab3:abc4:abcdee"), list(1:2, c("ab", "abc", "abcd")))
    expect_identical(bencode("lli1ei2eli1ei2eeee"), list(list(1L, 2L, 1:2)))
    expect_identical(bencode("lli1ei2eel1:a2:ab3:abcee"), list(1:2, c("a", "ab", "abc")))
    expect_identical(bencode("lli1ei2eel1:a2:ab3:abci3eee"), list(1:2, list("a", "ab", "abc", 3L)))
})

test_that("bencode parses dictionaries as expected", {
    expect_identical(bencode("d4:abcdli1ei2ei3ee4:bbcdli1ei2ei3ei4ei5eee"),
                     structure(list(abcd = 1:3, bbcd = 1:5), .Names = c("abcd", "bbcd"), class = "bendict"))
    expect_identical(bencode("d4:abcdli1ei2ei3ee4:bbcdli1ei2ei3ei4ei5ee10:inner_dictd4:abcdli1ei2ei3ee4:bbcdli1ei2ei3ei4ei5eeee"),
                     structure(list(abcd = 1:3, bbcd = 1:5,
                                    inner_dict = structure(list(abcd = 1:3, bbcd = 1:5),
                                        .Names = c("abcd", "bbcd"), class = "bendict")),
                               .Names = c("abcd", "bbcd", "inner_dict"), class = "bendict"))
})
