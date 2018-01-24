readFactivaXML <- readXML(spec = list(
    author = list("function", function(node)
                  toupper(gsub("^\\s+|\\s+$", "",
                               gsub("\n|\\s+", " ",
                                    xml_text(xml_find_all(node, ".//byline")))))),
    content = list("function", function(node)
                   c(xml_text(xml_find_all(node, ".//headline")),
                     xml_text(xml_find_all(node, ".//leadParagraph")),
                     xml_text(xml_find_all(node, ".//tailParagraphs/paragraph")))),
    datetimestamp = list("function", function(node)
                         strptime(xml_text(xml_find_all(node, ".//publicationDate/date")),
                                  format="%Y-%m-%d")),
    heading = list("node", ".//headline"),
    id = list("function", function(node) {
              str <- gsub("^distdoc:archive/ArchiveDoc::Article/", "",
                          xml_text(xml_find_all(node, ".//reference")))
              # Extract useful information: origin, date, and two last characters to avoid collisions
              m <- regmatches(str, regexec("^([A-Za-z]+)0*[1-9][0-9]([0-9][0-9][0-3][0-9][0-3][0-9]).*([A-Za-z0-9]{2})$", str))[[1]]
              # If extraction failed for some reason, make sure we return a unique identifier
              if(length(m) == 4)
                  paste(toupper(m[2]), "-", m[3], "-", m[4], sep="")
              else
                  paste(sample(LETTERS, 10), collapse="")
    }),
    origin = list("node", ".//sourceName"),
    language = list("function", function(node)
                    tolower(xml_text(xml_find_all(node, ".//baseLanguage")))),
    edition = list("node", ".//edition"),
    section = list("node", ".//sectionName"),
    subject = list("node", ".//newsSubject/name"),
    coverage = list("node", ".//region/name"),
    company = list("node", ".//company/name"),
    industry = list("node", ".//industry/name"),
    infocode = list("node", ".//descField[@code!='ipd']"),
    infodesc = list("function", function(node) {
                    str <- xml_text(xml_find_all(node, ".//descField[@code='ipd']"))
                    if(length(str) > 0)
                        strsplit(str, "( +\\| +| +-+ +| +--+|--+ +|\\._)")[[1]]
                    else
                        character(0)
    }),
    page = list("function", function(node) {
                str <- xml_text(xml_find_all(node, ".//page"))
                if(length(str) > 0)
                    as.numeric(str)
                else
                    NA
    }),
    wordcount = list("function", function(node)
                     as.numeric(xml_text(xml_find_all(node, ".//wordCount")))),
    publisher = list("node", ".//publisherName"),
    rights = list("function", function(node)
                  gsub("^\\s+|\\s+$", "",
                       gsub("\n|\\s+", " ",
                            xml_text(xml_find_all(node, ".//copyright")))))),
    doc = PlainTextDocument())

