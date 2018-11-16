git --no-pager log --all | grep "Author" | sed -E 's/[^<]*<([^>]+)>/\1/' | sort | uniq -c
