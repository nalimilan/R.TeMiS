readFactiva <- tm::readXML(
    spec = list(Author = list("function", function(node)
                toupper(gsub("^\\s+|\\s+$", "",
                             gsub("\n|\\s+", " ",
                                  sapply(XML::getNodeSet(node, "/article/byline"), xmlValue))))),
    Content = list("function", function(node)
                   paste(sapply(XML::getNodeSet(node, "/article/headline"), xmlValue),
                         sapply(XML::getNodeSet(node, "/article/leadParagraph"), xmlValue),
                         paste(sapply(XML::getNodeSet(node, "/article/tailParagraphs/paragraph"), xmlValue), collapse="\n"),
                         sep="\n\n")),
    DateTimeStamp = list("function", function(node)
                         strptime(sapply(XML::getNodeSet(node, "/article/publicationDate/date"), xmlValue),
                                  format="%Y-%m-%d")),
    Heading = list("node", "/article/headline"),
    ID = list("function", function(node)
              gsub("^distdoc:archive/ArchiveDoc::Article/", "",
                   sapply(XML::getNodeSet(node, "/article/reference"), xmlValue))),
    Origin = list("node", "/article/sourceName"),
    Language = list("function", function(node)
                    tolower(sapply(XML::getNodeSet(node, "/article/baseLanguage"), xmlValue))),
    Edition = list("node", "/article/edition"),
    Section = list("node", "/article/sectionName"),
    Subject = list("node", "/article/newsSubject/name"),
    Coverage = list("node", "/article/region/name"),
    WordCount = list("function", function(node)
                     as.numeric(sapply(XML::getNodeSet(node, "/article/wordCount"), xmlValue))),
    Pages = list("function", function(node)
                 as.numeric(sapply(XML::getNodeSet(node, "/article/pages"), xmlValue))),
    Publisher = list("node", "/article/publisherName"),
    Rights = list("function", function(node)
                  gsub("^\\s+|\\s+$", "",
                       gsub("\n|\\s+", " ",
                            sapply(XML::getNodeSet(node, "/article/copyright"), xmlValue))))),
    doc = PlainTextDocument())

