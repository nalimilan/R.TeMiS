readFactivaXML <- tm::readXML(
    spec = list(Author = list("function", function(node)
                toupper(gsub("^\\s+|\\s+$", "",
                             gsub("\n|\\s+", " ",
                                  sapply(XML::getNodeSet(node, "/article/byline"), xmlValue))))),
    Content = list("function", function(node)
                   c(sapply(XML::getNodeSet(node, "/article/headline"), xmlValue),
                         sapply(XML::getNodeSet(node, "/article/leadParagraph"), xmlValue),
                         sapply(XML::getNodeSet(node, "/article/tailParagraphs/paragraph"), xmlValue))),
    DateTimeStamp = list("function", function(node)
                         strptime(sapply(XML::getNodeSet(node, "/article/publicationDate/date"), xmlValue),
                                  format="%Y-%m-%d")),
    Heading = list("node", "/article/headline"),
    ID = list("function", function(node) {
              str <- gsub("^distdoc:archive/ArchiveDoc::Article/", "",
                           sapply(XML::getNodeSet(node, "/article/reference"), xmlValue))
              # Extract useful information: origin, date, and two last characters to avoid collisions
              m <- regmatches(str, regexec("^([A-Za-z]+)0*[1-9][0-9]([0-9][0-9][0-3][0-9][0-3][0-9]).*([A-Za-z0-9]{2})$", str))[[1]]
              # If extraction failed for some reason, make sure we return a unique identifier
              if(length(m) == 4)
                  paste(toupper(m[2]), "-", m[3], "-", m[4], sep="")
              else
                  paste(sample(LETTERS, 10), collapse="")
    }),
    Origin = list("node", "/article/sourceName"),
    Language = list("function", function(node)
                    tolower(sapply(XML::getNodeSet(node, "/article/baseLanguage"), xmlValue))),
    Edition = list("node", "/article/edition"),
    Section = list("node", "/article/sectionName"),
    Subject = list("node", "/article/newsSubject/name"),
    Coverage = list("node", "/article/region/name"),
    Company = list("node", "/article/company/name"),
    Industry = list("node", "/article/industry/name"),
    InfoCode = list("node", "/article/descField[@code!='ipd']"),
    InfoDesc = list("function", function(node) {
                    str <- sapply(XML::getNodeSet(node, "/article/descField[@code='ipd']"), xmlValue)
                    if(length(str) > 0)
                        strsplit(str, "( +\\| +| +-+ +| +--+|--+ +|\\._)")[[1]]
                    else
                        character(0)
    }),
    WordCount = list("function", function(node)
                     as.numeric(sapply(XML::getNodeSet(node, "/article/wordCount"), xmlValue))),
    Publisher = list("node", "/article/publisherName"),
    Rights = list("function", function(node)
                  gsub("^\\s+|\\s+$", "",
                       gsub("\n|\\s+", " ",
                            sapply(XML::getNodeSet(node, "/article/copyright"), xmlValue))))),
    doc = PlainTextDocument())

