BEGIN {
    FS = "\t"
    print "("
}

{
    date = $1;
    path = $2;
    id = $3;
    position = $4;
    ease = $5
    box = $6;
    interval = $7;
    
    if (NF == 10) { rating = $8; }
    else if (NF == 11) { rating = $9; }

    }

# card_id is given by the org-fc-awk-history-for-id function
id == card_id {
    # Validation check
    print "(:date " escape_string(date)         \
        " :path " escape_string(path)           \
        " :position " escape_string(position)   \
        " :ease " ease \
        " :box " box \
        " :interval " interval \
        " :rating " escape_string(rating) ")"
}

END {
    print ")"
}
