#!/usr/bin/awk -f

# Convert the IANA Content Type registry to Ada
# Usage: convert_content_formats.awk <iana_content_type_registry.xml

BEGIN {
    FS=" *[<>]"
    content_list[contentcoding] = ""
    print "with Interfaces;"
    print ""
    print "package CoAP_SPARK.Content_Formats"
    print "   with SPARK_Mode => On"
    print "is"
    print ""
    print "   -- Generated from Constrained RESTful Environments (CoRE) Parameters"
    print "   -- https://www.iana.org/assignments/core-parameters/core-parameters.xhtml"
    print "   -- From the registry with id=\"content-formats\""
    print ""
}

/<contenttype>/ {
    if ($3 != "Unassigned" && $3 !="type=\"uri\"" && !match($3, /Reserved/)) {
        contenttype=1
        comment=$3
        gsub(/-/, "_")
        gsub(/+/, "_Plus_")
        gsub(/\./, "_Dot_")
        split($3, mime, /[/; ]/)
        attribute[1] = ""
        attribute[2] = ""
        if (match(mime[4], /=/)) {
            split(mime[4], attribute, /=/)
        }
        package=mime[1]
        if (attribute[2] != "") {
            gsub("\"", "", attribute[2])
            if (match(attribute[2], mime[2])) {
                identifier = attribute[2]
            } else {
                identifier = mime[2] "_" attribute[1] "_" attribute[2]
            }
        } else {
            identifier = mime[2]
        }

    }
 }
 
 /contentcoding/ {
    if ($3) {
        contentcoding = "_" $3
    }
 }

 /<id>/ {
    if (contenttype && match($3, /^[0-9]+$/)) {
        full_id = identifier contentcoding
        full_constant = package "." full_id
        if (package == "text") {
            text_list[identifier] = full_constant
        }
        package_list[package] = package_list[package] \
            sprintf("      -- %s\n", comment) \
            sprintf("      %s : constant := %s;\n\n", full_id, $3)
        gsub("\"", "\"\"", comment)
        content_list[full_constant] = content_list[full_constant] \
            sprintf("         when %s =>\n           \"%s\",", full_constant, comment)
    }
    contenttype=0
    contentcoding=""
}

END	{
    for (i in package_list) {
        printf("   package %s is\n\n", i)
        printf("%s", package_list[i])
        printf("   end %s;\n\n", i)
    }
    print "   function Is_Text (Content_Type : Interfaces.Unsigned_32) return Boolean"
    print "   is (case Content_Type is"
    print ""
    for (i in text_list) {
        printf("          when %s => True,\n", text_list[i])
    }
    print "          when others => False);"
    print ""
    print "   function To_String (Content_Type : Interfaces.Unsigned_32) return String"
    printf("   is (case Content_Type is")
    for (i in content_list) {
        print content_list[i]
    }
    print "         when others => \"application/octet-stream\");"
    print ""
    print "end CoAP_SPARK.Content_Formats;"
}