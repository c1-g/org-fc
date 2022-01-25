BEGIN {
    FS = "\t"
    print "("
}

{
    date = $1;
    path = $2;
    id = $3;
    algo = $4;
    time = $5;
    rating = $6;

    }

# card_id is given by the org-fc-awk-history-for-id function
id == card_id {
    # Validation check
    print "(:date " escape_string(date)         \
        " :path " escape_string(path)           \
        " :algo " escape_string(algo) \
        " :time " escape_string(time) \
        " :rating " escape_string(rating) \
        " :params (";
    for (i = 7; i <= NF; i++) {
        print  " " escape_string($i);
    }
    print "))";
}

END {
    print ")"
}
