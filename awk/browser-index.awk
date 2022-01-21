### Commentary
#
# This file implements a parser that reads org files, extracts data
# relevant to org-fc and prints it as an S-expression so it can be
# parsed with EmacsLisp's read function.
#
# The org format is mostly line based.
# A small state machine is used to keep track of where we are in a file,
# (e.g. inside a card, reading heading properties, reading review data).
#
# Some parsing of review data columns is done.
#
# The position is escaped as a string and the due date is converted
# into Emacs's date format because it's a bit faster in AWK than in
# EmacsLisp.
#
# All other columns of the review data table are assumed to be numeric
# values and included in the output S-expression without any escaping.
#
# Because of the complicated rules used by org-mode to determine a
# heading's tags, inherited (file / parent heading) and local tags are
# tracked separately and later combined using an org-mode function.
#
### Code

BEGIN {
    # The only time we're interested in multiple fields is when
    # parsing the review data drawer.
    #
    # Treating whitespace as part of the field separator instead of
    # stripping it from the fields afterwards is a bit faster.
    FS="[ \t]*|[ \t]*";

    now = strftime("%FT%TZ", systime(), 1);

    fc_tag = ":" or_default(fc_tag, "fc") ":";
    suspended_tag = ":" or_default(suspended_tag, "suspended") ":";
    review_data_drawer = ":" or_default(review_data_drawer, "REVIEW_DATA") ":";
    type_property = or_default(type_property, "FC_TYPE");
    cloze_type_property = or_default(cloze_type_property, "FC_CLOZE_TYPE");
    created_property = or_default(created_property, "FC_CREATED");

    # Small state machine to make sure cards are in the correct format
    state = 0;
    state_file = 0;
    state_properties = 1;
    state_properties_done = 2;
    state_review_data = 3;
    state_review_data_body = 4;
    state_review_data_done = 5;

    print "(";
}

## File Parsing

BEGINFILE {
    # Reset filetags
    delete parent_tags;
    file_title = "";
    title = "";
    parent_tags[0] = "";
    state = state_file;
}

## File Title

match($0, /^#\+(TITLE|title):[ \t]+(.*)/, a) {
    # Combine tags to handle multiple FILETAGS lines
    file_title = a[2]
    next;
}

## File Tags

match($0, /^#\+(FILETAGS|filetags):[ \t]+(.*)/, a) {
    # Combine tags to handle multiple FILETAGS lines
    parent_tags[0] = combine_tags(a[2], parent_tags[0]);
    suspended = (parent_tags[0] ~ suspended_tag);
    file_suspended = suspended;
    next;
}


## Heading Parsing

match($0, /^(\*+)[ \t]+(.*)$/, a) {
    level = length(a[1]);
    title = a[2];
    tags = "";

    # tag re based on org-tag-re
    # this only guarantees that there is at least one tab/space
    # between the headline text and the tags.
    # TODO: Do this in a single match
    if (match(title, /^(.*)[ \t]+(:([[:alnum:]_@#%]+:)+)$/, b) != 0) {
        title = b[1];
        # remove trailing tabs/spaces
        sub(/[ \t]*$/, "", title);
        tags = b[2];
    }
    parent_tags[level] = tags;

    id = "none";

    if (tags ~ fc_tag) {
        suspended = (tags ~ suspended_tag);
    }
    next;
}

## Drawer Parsing

/:PROPERTIES:/ {
    # if (state == state_file) {
        state = state_properties;
        delete properties;
    # }
    next;
}

$0 ~ review_data_drawer {
    # Make sure the review data comes after the property drawer
    if (state == state_properties_done) {
        delete review_data_columns;
        review_data_ncolumns = 0;

        delete review_data;
        review_index = 1;

        state = state_review_data;
    }
    next;
}

/:END:/ {
    if (state == state_properties) {
        state = state_properties_done;
    } else if (state == state_review_data_body) {
        state = state_review_data_done;
        # Card header
        inherited_tags = "";
        for (i = 0; i < level; i++) {
            inherited_tags = combine_tags(inherited_tags, parent_tags[i]);
        }
        local_tags = parent_tags[level];

        cloze_type = ""
        if (cloze_type_property in properties)
            cloze_type = " :cloze-type " properties[cloze_type_property]

        # Card positions
        for (i = 1; i < review_index; i++) {
            print "      (" \
                escape_string(properties["ID"])  \
                " [" escape_string(title) ;
            for (j = 4; j <= review_data_ncolumns; j++) {
                col = review_data_columns[j];
                val = review_data[i][col];

                if (j == 7 || j == 9 ) {
                    val = escape_string(val);
                    print " "  val;
                }
            }
            print " " escape_string(properties[type_property])       \
                "])";
        }
    }
    next;
}

## Property Parsing

(state == state_properties) && match($0, /^[ \t]*:([a-zA-Z0-9_]+):[ \t]*(.+)$/, a)  {
    properties[a[1]] = trim_surrounding(a[2]);
    next;
}

## Review data parsing

# Table separator
(state == state_review_data) && /^\|[-+]+\|$/ {
    state = state_review_data_body;
    next;
}

# Column Names
# NOTE: This line comes before the table separator in the file but to
# keep the regex simple, we match it later.
(state == state_review_data) && /^\|.*\|$/ {
    # Skip the first and last empty fields
    for (i = 2; i <= (NF - 1); i++) {
        review_data_columns[i - 1] = $i;
    }
    review_data_ncolumns = NF - 2;
    next;
}

# Positions are collected in an array first,
# in case the review drawer is broken.
(state == state_review_data_body) && /^\|.*\|$/ {
    if (NF == (review_data_ncolumns + 2)) {
        for (i = 2; i <= (NF - 1); i++) {
            column = review_data_columns[i - 1];
            review_data[review_index][column] = $i;
        }
        review_index += 1;
    }
    next;
}

END {
    print ")";
}
