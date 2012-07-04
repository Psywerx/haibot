#change name, cut
cat wn_hyp.pl | cut -b 5- | awk 'sub("..$", "")' > wn_hyp.db
