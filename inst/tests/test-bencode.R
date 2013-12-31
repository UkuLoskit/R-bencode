library(bencode)

test_that("ben_decode parses integers, strings and lists",{
    expect_identical(ben_decode("li1ei2ee"), 1:2)
    expect_identical(ben_decode("l2:ab3:abc4:abcde"), c("ab", "abc", "abcd"))
    expect_identical(ben_decode("lli1ei2eel2:ab3:abc4:abcdee"), list(1:2, c("ab", "abc", "abcd")))
    expect_identical(ben_decode("lli1ei2eli1ei2eeee"), list(list(1L, 2L, 1:2)))
    expect_identical(ben_decode("lli1ei2eel1:a2:ab3:abcee"), list(1:2, c("a", "ab", "abc")))
    expect_identical(ben_decode("lli1ei2eel1:a2:ab3:abci3eee"), list(1:2, list("a", "ab", "abc", 3L)))
})

test_that("ben_decode parses dictionaries", {
    expect_identical(ben_decode("d4:abcdli1ei2ei3ee4:bbcdli1ei2ei3ei4ei5eee"),
                     structure(list(abcd = 1:3, bbcd = 1:5), .Names = c("abcd", "bbcd"), class = "bendict"))
    expect_identical(ben_decode("d4:abcdli1ei2ei3ee4:bbcdli1ei2ei3ei4ei5ee10:inner_dictd4:abcdli1ei2ei3ee4:bbcdli1ei2ei3ei4ei5eeee"),
                     structure(list(abcd = 1:3, bbcd = 1:5,
                                    inner_dict = structure(list(abcd = 1:3, bbcd = 1:5),
                                        .Names = c("abcd", "bbcd"), class = "bendict")),
                               .Names = c("abcd", "bbcd", "inner_dict"), class = "bendict"))
})

test_that("encode -> decode loop works", {
          X <- c("aaaa", "bbbb")
          expect_identical(X, ben_decode(ben_encode(X)))
          X <- 1:50
          expect_identical(X, ben_decode(ben_encode(X)))
          X <- list(1L, 2L, 3L, "abcd")
          expect_identical(X, ben_decode(ben_encode(X)))
          X <- list(1L, 2L, 1:3, "abcd")
          expect_identical(X, ben_decode(ben_encode(X)))
          X <- bendict(list(a = 1L, b = 2L, c = 1:3, d = "abcd"))
          expect_identical(X, ben_decode(ben_encode(X)))
          X <- bendict(list(a = 1L, b = 2L, c = 1:3, d = "abcd"))
          expect_identical(X, ben_decode(ben_encode(X)))
          X <- bendict(list(a = 1L, b = 2L, c = 1:3, d = "abcd",
                            e = bendict(list(a = 1L, b = 2L, c = c("a", "b", "c"), d = "abcd"))))
          expect_identical(X, ben_decode(ben_encode(X)))
      })


## ben_decode(ben_encode(iris$Sepal.Length))
## library(microbenchmark)
## microbenchmark(ben_decode(ben_encode(iris$Sepal.Length)), ben_decode(str) , times = 1000)
## library(rbenchmark)
## benchmark(ben_decode(ben_encode(iris$Sepal.Length)), replications = 10000)
