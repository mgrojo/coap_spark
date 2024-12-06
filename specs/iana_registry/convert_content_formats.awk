#!/usr/bin/awk -f

# Convert the IANA Content Type registry to Ada
# Usage: convert_content_formats.awk <iana_content_type_registry.xml

BEGIN {
    FS=" *[<>]"
    print "package CoAP_SPARK.Content_Formats"
    print "   with SPARK_Mode => On"
    print "is"
    print
    print "   -- Generated from Constrained RESTful Environments (CoRE) Parameters"
    print "   -- https://www.iana.org/assignments/core-parameters/core-parameters.xhtml"
    print "   -- From the registry with id=\"content-formats\""
    print
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
        package_list[package] = package_list[package] \
            sprintf("      -- %s\n", comment) \
            sprintf("      %s%s : constant := %s;\n\n", identifier, contentcoding, $3)
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
    print "end CoAP_SPARK.Content_Formats;"
}