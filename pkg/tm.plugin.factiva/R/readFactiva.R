readFactiva <- tm::readXML(
    spec = list(Author = list("function", function(node)
                toupper(gsub("^\\s+|\\s+$", "",
                             gsub("\n|\\s+", " ",
                                  sapply(XML::getNodeSet(node, "/ppsarticle/article/byline"), xmlValue))))),
    Content = list("function", function(node)
                   paste(sapply(XML::getNodeSet(node, "/ppsarticle/article/headline"), xmlValue),
                         sapply(XML::getNodeSet(node, "/ppsarticle/article/leadParagraph"), xmlValue),
                         sapply(XML::getNodeSet(node, "/ppsarticle/article/tailParagraphs"), xmlValue),
                         sep="\n\n")),
    DateTimeStamp = list("function", function(node)
                         strptime(sapply(XML::getNodeSet(node, "/ppsarticle/article/publicationDate/date"), xmlValue),
                                  format="%Y-%m-%d")),
    Heading = list("node", "/ppsarticle/article/headline"),
    ID = list("function", function(node)
              gsub("^distdoc:archive/ArchiveDoc::Article/", "",
                   sapply(XML::getNodeSet(node, "/ppsarticle/article/reference"), xmlValue))),
    Origin = list("node", "/ppsarticle/article/sourceName"),
    Language = list("function", function(node)
                    tolower(sapply(XML::getNodeSet(node, "/ppsarticle/article/baseLanguage"), xmlValue))),
    Edition = list("node", "/ppsarticle/article/edition"),
    Section = list("node", "/ppsarticle/article/sectionName"),
    Subject = list("node", "/ppsarticle/article/newsSubject/name"),
    Coverage = list("node", "/ppsarticle/article/region/name"),
    WordCount = list("function", function(node)
                     as.numeric(sapply(XML::getNodeSet(node, "/ppsarticle/article/wordCount"), xmlValue))),
    Pages = list("function", function(node)
                 as.numeric(sapply(XML::getNodeSet(node, "/ppsarticle/article/pages"), xmlValue))),
    Publisher = list("node", "/ppsarticle/article/publisherName"),
    Rights = list("function", function(node)
                  gsub("^\\s+|\\s+$", "",
                       gsub("\n|\\s+", " ",
                            sapply(XML::getNodeSet(node, "/ppsarticle/article/copyright"), xmlValue))))),
    doc = PlainTextDocument())

