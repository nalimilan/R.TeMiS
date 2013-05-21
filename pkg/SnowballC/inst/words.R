# The following code can be used to read vocabulary lists from
# the data directory of svn://svn.tartarus.org/snowball/
# Manual fixes are needed to replace empty values by ""
for(lang in getStemLanguages()) {
    voc <- scan(file.path("words", lang, "diffs.txt"),
                list("character", "character"), quote="\"", quiet=TRUE)

    stopifnot(all(wordStem(voc[[1]], lang) == voc[[2]]))
}

# Save individual files in the most compressed form
for(lang in getStemLanguages()) {
    voc <- scan(file.path("words", lang, "diffs.txt"),
                list("character", "character"), quote="\"", quiet=TRUE)

    save(voc, file=file.path("words", paste0(lang, ".RData")), compress="xz")
}

