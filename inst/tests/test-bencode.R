library(bencode)
library(testthat)

test_that("bdecode parses integers, strings and lists",{
    expect_identical(bdecode("li1ei2ee"), 1:2)
    expect_identical(bdecode("l2:ab3:abc4:abcde"), c("ab", "abc", "abcd"))
    expect_identical(bdecode("lli1ei2eel2:ab3:abc4:abcdee"), list(1:2, c("ab", "abc", "abcd")))
    expect_identical(bdecode("lli1ei2eli1ei2eeee"), list(list(1L, 2L, 1:2)))
    expect_identical(bdecode("lli1ei2eel1:a2:ab3:abcee"), list(1:2, c("a", "ab", "abc")))
    expect_identical(bdecode("lli1ei2eel1:a2:ab3:abci3eee"), list(1:2, list("a", "ab", "abc", 3L)))
})

test_that("bdecode parses dictionaries", {
    expect_identical(bdecode("d4:abcdli1ei2ei3ee4:bbcdli1ei2ei3ei4ei5eee"),
                     structure(list(abcd = 1:3, bbcd = 1:5), .Names = c("abcd", "bbcd"), class = "bendict"))
    expect_identical(bdecode("d4:abcdli1ei2ei3ee4:bbcdli1ei2ei3ei4ei5ee10:inner_dictd4:abcdli1ei2ei3ee4:bbcdli1ei2ei3ei4ei5eeee"),
                     structure(list(abcd = 1:3, bbcd = 1:5,
                                    inner_dict = structure(list(abcd = 1:3, bbcd = 1:5),
                                        .Names = c("abcd", "bbcd"), class = "bendict")),
                               .Names = c("abcd", "bbcd", "inner_dict"), class = "bendict"))
})

test_that("encode -> decode loop works", {
          X <- c("aaaa", "bbbb")
          expect_identical(X, bdecode(bencode(X)))
          X <- 1:50
          expect_identical(X, bdecode(bencode(X)))
          X <- list(1L, 2L, 3L, "abcd")
          expect_identical(X, bdecode(bencode(X)))
          X <- list(1L, 2L, 1:3, "abcd")
          expect_identical(X, bdecode(bencode(X)))
          X <- bendict(list(a = 1L, b = 2L, c = 1:3, d = "abcd"))
          expect_identical(X, bdecode(bencode(X)))
          X <- bendict(list(a = 1L, b = 2L, c = 1:3, d = "abcd"))
          expect_identical(X, bdecode(bencode(X)))
          X <- bendict(list(a = 1L, b = 2L, c = 1:3, d = "abcd",
                            e = bendict(list(a = 1L, b = 2L, c = c("a", "b", "c"), d = "abcd"))))
          expect_identical(X, bdecode(bencode(X)))
      })


bdecode(bencode(list(op = "bbb", status = c("err", "unknown", "done"))))

bdecode(bencode(list(empty = list(), empty2 = bendict())))

bdecode(bencode(character(0))) == "NULL"

## bdecode(bencode(iris$Sepal.Length))
## library(microbenchmark)
## microbenchmark(bdecode(bencode(iris$Sepal.Length)), bdecode(str) , times = 1000)
## library(rbenchmark)
## benchmark(bdecode(bencode(iris$Sepal.Length)), replications = 10000)
