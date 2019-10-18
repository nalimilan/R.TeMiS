library(tm)
library(tm.plugin.lexisnexis)

# English example
file <- system.file("texts", "lexisnexis_test_en.html",
                    package = "tm.plugin.lexisnexis")
corpus <- Corpus(LexisNexisSource(file))

# French example
file <- system.file("texts", "lexisnexis_test_fr.html",
                    package = "tm.plugin.lexisnexis")
corpus <- Corpus(LexisNexisSource(file))

# Two malformed examples: one which can be parsed but with a missing copyright
# notice (and with a warning), and one which should be dropped (with a warning).
# We suppress warnings to avoid breaking the build, then test the consequences.
file <- system.file("texts", "lexisnexis_test_copyright_error.html",
                    package = "tm.plugin.lexisnexis")
corpus <- suppressWarnings(Corpus(LexisNexisSource(file)))
stopifnot(length(corpus) == 1,
          corpus[[1]]$meta$id == "resize201812311",
          length(corpus[[1]]$meta$rights) == 0)
